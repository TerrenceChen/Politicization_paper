# 03_alc_party_task.R
#
# Same idea as 02_alc_ideology_task.R but for the democrat/republican anchors.
# One SLURM array task = one topic.
#
# Output:
#   ALC_results_party/topic_<name>_result.rds

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
dem_wv_subsamples <- readRDS("dem_wv_subsamples.rds")
rep_wv_subsamples <- readRDS("rep_wv_subsamples.rds")
dict_terms <- readRDS("Results_Files/dict_terms_final_0825.rds")

topics <- sort(unique(dict_terms$topic))
if (task_id < 1 || task_id > length(topics)) {
  stop(sprintf("Task id %d out of range (1..%d topics).", task_id, length(topics)))
}
tp <- topics[task_id]
message("Task ", task_id, " -> topic: ", tp)

compute_party_metrics <- function(t) {
  years   <- as.character(1980:2024)
  results <- list()

  for (n in 1:20) {
    term_wv <- build_anchor_wv(subsampled_tokens[[n]], pattern = t, filter_terms = NULL)
    dem_wv <- dem_wv_subsamples[[n]]
    rep_wv <- rep_wv_subsamples[[n]]

    for (year in years) {
      year_present <- year %in% term_wv@Dimnames$docs &
                      year %in% dem_wv@Dimnames$docs  &
                      year %in% rep_wv@Dimnames$docs

      if (year_present) {
        avg_emb  <- term_wv[year, ]
        cos_dem  <- as.numeric(lsa::cosine(dem_wv[year, ], avg_emb))
        cos_rep  <- as.numeric(lsa::cosine(rep_wv[year, ], avg_emb))
        cos_diff <- as.numeric(lsa::cosine((dem_wv[year, ] - rep_wv[year, ]), avg_emb))
      } else {
        cos_dem  <- NA
        cos_rep  <- NA
        cos_diff <- NA
      }

      results[[length(results) + 1]] <- data.frame(
        term      = t,
        year      = as.integer(year),
        subsample = n,
        text_n    = subsample_sizes[n],
        cos_dem   = cos_dem,
        cos_rep   = cos_rep,
        cos_diff  = cos_diff
      )
    }
  }

  do.call(rbind, results) %>%
    group_by(term, year) %>%
    mutate(
      cos_dem_avg  = mean(cos_dem, na.rm = TRUE),
      cos_rep_avg  = mean(cos_rep, na.rm = TRUE),
      cos_diff_avg = mean(cos_diff, na.rm = TRUE),
      error_dem  = sqrt(text_n) * (cos_dem  - cos_dem_avg)  / sqrt(1350000),
      error_rep  = sqrt(text_n) * (cos_rep  - cos_rep_avg)  / sqrt(1350000),
      error_diff = sqrt(text_n) * (cos_diff - cos_diff_avg) / sqrt(1350000)
    ) %>%
    summarise(
      cos_dem_mean   = mean(cos_dem, na.rm = TRUE),
      cos_dem_lower  = cos_dem_mean - quantile(error_dem, 0.95, na.rm = TRUE),
      cos_dem_upper  = cos_dem_mean - quantile(error_dem, 0.05, na.rm = TRUE),
      cos_rep_mean   = mean(cos_rep, na.rm = TRUE),
      cos_rep_lower  = cos_rep_mean - quantile(error_rep, 0.95, na.rm = TRUE),
      cos_rep_upper  = cos_rep_mean - quantile(error_rep, 0.05, na.rm = TRUE),
      cos_diff_mean  = mean(cos_diff, na.rm = TRUE),
      cos_diff_lower = cos_diff_mean - quantile(error_diff, 0.95, na.rm = TRUE),
      cos_diff_upper = cos_diff_mean - quantile(error_diff, 0.05, na.rm = TRUE),
      .groups = "drop"
    )
}

topic_terms  <- subset(dict_terms, topic == tp)$term
term_metrics <- lapply(topic_terms, compute_party_metrics)
result       <- do.call(rbind, term_metrics)

dir.create("ALC_results_party", showWarnings = FALSE)
saveRDS(result, paste0("ALC_results_party/topic_", tp, "_result.rds"))
message("Saved topic: ", tp)
