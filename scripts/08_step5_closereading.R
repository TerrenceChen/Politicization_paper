# 08_step5_facevalidity_closereading.R
#
# Close-reading helper functions. NOTE: close reading is inherently exploratory -- you pick a
# term/topic/year to inspect by hand. This script defines the functions and
# runs the one example from the notebook ("comedian", topic_33, 2024); edit
# the bottom section to inspect whatever term/topic you actually want to
# read closely
#
# Requires:
#   Results_Files/results_df_label_combined.rds   (from Step3)
#   ALC_dems/anchors.rds                          (from 02_step5_anchor_dems.R)
#   ALC_dems/topic_<n>.rds                         (from 01_step5_topic_dems_task.R array)
#   subsampled_tokens.rds                          (from 00_setup_step5.R)
#   input/glove.rds, input/khodakA.rds

library(conText)
library(quanteda)
library(dplyr)
library(ggplot2)

glove  <- readRDS("input/glove.rds")
khodak <- readRDS("input/khodakA.rds")
subsampled_tokens <- readRDS("subsampled_tokens.rds")

# --- Close reading ---
anchor_dems <- readRDS("ALC_dems/anchors.rds")

close_reading <- function(term_dem, anchor_dems, n_top = 20,
                           filter_source = NULL, filter_year = NULL) {

  mat     <- term_dem$matrix
  docvars <- term_dem$docvars

  keep <- rep(TRUE, nrow(mat))
  if (!is.null(filter_source)) keep <- keep & docvars$Source == filter_source
  if (!is.null(filter_year))   keep <- keep & docvars$Year   == filter_year

  mat     <- mat[keep, ]
  docvars <- docvars[keep, ]

  lib_by_year <- rowsum(anchor_dems$liberal$matrix,
                         anchor_dems$liberal$docvars$Year) /
                 as.numeric(table(anchor_dems$liberal$docvars$Year))

  con_by_year <- rowsum(anchor_dems$conservative$matrix,
                         anchor_dems$conservative$docvars$Year) /
                 as.numeric(table(anchor_dems$conservative$docvars$Year))

  cos_scores <- sapply(seq_len(nrow(mat)), function(i) {
    yr <- as.character(docvars$Year[i])
    if (!yr %in% rownames(lib_by_year) | !yr %in% rownames(con_by_year)) return(NA)
    cos_lib <- lsa::cosine(mat[i, ], lib_by_year[yr, ])
    cos_con <- lsa::cosine(mat[i, ], con_by_year[yr, ])
    (cos_lib + cos_con) / 2
  })

  med         <- max(cos_scores, na.rm = TRUE)
  dist_to_med <- abs(cos_scores - med)
  median_idx  <- order(dist_to_med)[1:n_top]

  data.frame(
    Article_ID = docvars$Article_ID[median_idx],
    Year       = docvars$Year[median_idx],
    Source     = docvars$Source[median_idx],
    Type       = docvars$Type[median_idx],
    subsample  = docvars$subsample[median_idx],
    pol_score  = cos_scores[median_idx]
  )
}

get_contexts <- function(article_ids, subsample_n, pattern, anchor_dems) {

  toks_sub <- subsampled_tokens[[subsample_n]]

  ctx <- tokens_context(toks_sub, pattern = pattern, window = 6L)

  ctx_docvars <- docvars(ctx)
  match_idx   <- ctx_docvars$Article_ID %in% article_ids

  ctx_matched  <- ctx[match_idx, ]
  dvars_matched <- ctx_docvars[match_idx, ]

  ctx_dfm <- dfm(ctx_matched)
  ctx_dem <- dem(
    x                = ctx_dfm,
    pre_trained      = glove,
    transform        = TRUE,
    transform_matrix = khodak,
    verbose          = FALSE
  )

  lib_by_year <- rowsum(anchor_dems$liberal$matrix,
                         anchor_dems$liberal$docvars$Year) /
                 as.numeric(table(anchor_dems$liberal$docvars$Year))

  con_by_year <- rowsum(anchor_dems$conservative$matrix,
                         anchor_dems$conservative$docvars$Year) /
                 as.numeric(table(anchor_dems$conservative$docvars$Year))

  cos_scores <- sapply(seq_len(nrow(ctx_dem)), function(i) {
    yr <- as.character(dvars_matched$Year[i])
    if (!yr %in% rownames(lib_by_year) | !yr %in% rownames(con_by_year)) return(NA)
    cos_lib <- lsa::cosine(as.numeric(ctx_dem[i, ]), lib_by_year[yr, ])
    cos_con <- lsa::cosine(as.numeric(ctx_dem[i, ]), con_by_year[yr, ])
    (cos_lib + cos_con) / 2
  })

  df <- data.frame(
    context    = sapply(ctx_matched, paste, collapse = " "),
    dvars_matched
  ) %>%
    group_by(Article_ID) %>%
    mutate(occurrence = row_number()) %>%
    ungroup() %>%
    mutate(pol_score = cos_scores) %>%
    arrange(Article_ID, desc(pol_score))

  df
}

# --- EXAMPLE (edit for whatever you actually want to inspect) ---
term_dems <- readRDS("ALC_dems/topic_33.rds")

closereading_results <- close_reading(term_dems[["comedian"]], anchor_dems, n_top = 20, filter_year = 2024)
print(closereading_results)

example_contexts <- get_contexts(closereading_results$Article_ID[[1]],
                                  closereading_results$subsample[[1]],
                                  "comedian",
                                  anchor_dems = anchor_dems)
print(example_contexts)
