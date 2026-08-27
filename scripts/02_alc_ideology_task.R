# 02_alc_ideology_task.R
#
# Processes ONE topic's worth of terms for the ideology (liberal/conservative)
# ALC embedding. Meant to be launched once per SLURM array task; the task
# index selects which topic to process, so all topics run in parallel
# instead of one long serial loop.
#
# Which topic to run is read from the SLURM_ARRAY_TASK_ID environment
# variable (1-indexed), falling back to a command-line argument for local
# testing: Rscript 02_alc_ideology_task.R 3
#
# Requires the outputs of 01_build_subsample_anchors.R plus:
#   Results_Files/dict_terms_final_0825.rds
#
# Output:
#   ALC_results_topic/topic_<name>_result.rds

library(conText)
library(quanteda)
library(dplyr)

source("scripts/utils_alc.R")

task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = NA))
if (is.na(task_id)) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) stop("No SLURM_ARRAY_TASK_ID set and no topic index argument given.")
  task_id <- as.integer(args[1])
}

glove  <- readRDS("input_files/glove.rds")
khodak <- readRDS("input_files/khodakA.rds")
subsampled_tokens <- readRDS("subsampled_tokens.rds")
subsample_sizes   <- readRDS("subsample_sizes.rds")
lib_wv_subsamples <- readRDS("lib_wv_subsamples.rds")
con_wv_subsamples <- readRDS("con_wv_subsamples.rds")
dict_terms <- readRDS("Results_Files/dict_terms_final_0825.rds")

topics <- sort(unique(dict_terms$topic))
if (task_id < 1 || task_id > length(topics)) {
  stop(sprintf("Task id %d out of range (1..%d topics).", task_id, length(topics)))
}
tp <- topics[task_id]
message("Task ", task_id, " -> topic: ", tp)

compute_term_metrics <- function(t) {
  years   <- as.character(1980:2024)
  results <- list()

  for (n in 1:20) {
    term_wv <- build_anchor_wv(subsampled_tokens[[n]], pattern = t, filter_terms = NULL)
    lib_wv <- lib_wv_subsamples[[n]]
    con_wv <- con_wv_subsamples[[n]]

    for (year in years) {
      year_present <- year %in% term_wv@Dimnames$docs &
                      year %in% lib_wv@Dimnames$docs  &
                      year %in% con_wv@Dimnames$docs

      if (year_present) {
        avg_emb <- term_wv[year, ]
        cos_lib <- as.numeric(lsa::cosine(lib_wv[year, ], avg_emb))
        cos_con <- as.numeric(lsa::cosine(con_wv[year, ], avg_emb))
      } else {
        cos_lib <- NA
        cos_con <- NA
      }

      results[[length(results) + 1]] <- data.frame(
        term        = t,
        year        = as.integer(year),
        subsample   = n,
        text_n      = subsample_sizes[n],
        cos_liberal = cos_lib,
        cos_con     = cos_con
      )
    }
  }

  do.call(rbind, results) %>%
    group_by(term, year) %>%
    mutate(
      cos_liberal_avg = mean(cos_liberal, na.rm = TRUE),
      cos_con_avg     = mean(cos_con,     na.rm = TRUE),
      error_lib = sqrt(text_n) * (cos_liberal - cos_liberal_avg) / sqrt(1350000),
      error_con = sqrt(text_n) * (cos_con     - cos_con_avg)     / sqrt(1350000)
    ) %>%
    summarise(
      cos_liberal_mean  = mean(cos_liberal, na.rm = TRUE),
      cos_liberal_lower = cos_liberal_mean - quantile(error_lib, 0.95, na.rm = TRUE),
      cos_liberal_upper = cos_liberal_mean - quantile(error_lib, 0.05, na.rm = TRUE),
      cos_con_mean      = mean(cos_con,     na.rm = TRUE),
      cos_con_lower     = cos_con_mean - quantile(error_con, 0.95, na.rm = TRUE),
      cos_con_upper     = cos_con_mean - quantile(error_con, 0.05, na.rm = TRUE),
      .groups = "drop"
    )
}

topic_terms  <- subset(dict_terms, topic == tp)$term
term_metrics <- lapply(topic_terms, compute_term_metrics)
result       <- do.call(rbind, term_metrics)

dir.create("ALC_results_topic", showWarnings = FALSE)
saveRDS(result, paste0("ALC_results_topic/topic_", tp, "_result.rds"))
message("Saved topic: ", tp)
