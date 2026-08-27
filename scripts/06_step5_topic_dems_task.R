# 06_step5_topic_dems_task.R
#
# Processes ONE topic: builds the raw (ungrouped) DEM matrix for every term
# in that topic, across all 20 subsamples, preserving docvars (Article_ID,
# Year, Source, Type, subsample, text_n) for later close-reading and
# outlet-level analysis (AppendixF, AppendixG). Meant to be launched once
# per SLURM array task, exactly like Step3's 02/03 scripts.
#
# Which topic to run comes from SLURM_ARRAY_TASK_ID (1-indexed)
#
# Output:
#   ALC_dems/topic_<name>.rds

library(conText)
library(quanteda)
library(dplyr)

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
dict_terms <- readRDS("Results_Files/dict_terms_final_0825.rds")

topics <- sort(unique(dict_terms$topic))
if (task_id < 1 || task_id > length(topics)) {
  stop(sprintf("Task id %d out of range (1..%d topics).", task_id, length(topics)))
}
tp <- topics[task_id]
message("Task ", task_id, " -> topic: ", tp)

# Returns the raw dem (one row per context), preserving all docvars
build_anchor_dem <- function(toks_subsample, pattern, filter_terms = NULL) {

  anchor_toks <- tokens_context(
    x       = toks_subsample,
    pattern = pattern,
    window  = 6L
  )

  if (!is.null(filter_terms)) {
    has_noise <- sapply(anchor_toks, function(ctx) any(filter_terms %in% ctx))
    anchor_toks <- anchor_toks[!has_noise]
  }

  anchor_dfm <- dfm(anchor_toks)

  anchor_dem <- dem(
    x                = anchor_dfm,
    pre_trained      = glove,
    transform        = TRUE,
    transform_matrix = khodak,
    verbose          = FALSE
  )

  return(anchor_dem)   # raw dem, NOT dem_group'd
}

dem_topic <- function(tp) {

  topic_terms <- subset(dict_terms, topic == tp)$term
  message("Processing topic: ", tp)

  term_dems <- lapply(setNames(topic_terms, topic_terms), function(t) {

    sub_dems <- lapply(1:20, function(n) {
      dem_obj <- build_anchor_dem(
        toks_subsample = subsampled_tokens[[n]],
        pattern        = t,
        filter_terms   = NULL
      )
      dem_obj@docvars$subsample <- n
      dem_obj@docvars$text_n    <- subsample_sizes[n]

      list(
        matrix  = as.matrix(dem_obj),
        docvars = dem_obj@docvars
      )
    })

    list(
      matrix  = do.call(rbind, lapply(sub_dems, `[[`, "matrix")),
      docvars = do.call(rbind, lapply(sub_dems, `[[`, "docvars"))
    )
  })

  dir.create("ALC_dems", showWarnings = FALSE)
  saveRDS(term_dems, paste0("ALC_dems/topic_", tp, ".rds"))
  message("Saved topic: ", tp)
}

dem_topic(tp)
