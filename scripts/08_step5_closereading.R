# 08_step5_closereading.R
#
# Close-reading helper functions. NOTE: close reading is inherently exploratory -- you pick a
# term/topic/year to inspect by hand. This script defines the functions and
# runs the one example from the notebook ("comedian", topic_33, 2024); edit
# the bottom section to inspect whatever term/topic you actually want to
# read closely.
#
# Requires:
#   Results_Files/results_df_label_combined.rds
#   ALC_dems/anchors.rds                           (from 07_step5_anchor_dems.R)
#   ALC_dems/topic_<n>.rds                         (from 06_step5_topic_dems_task.R array)
#   subsampled_tokens.rds                          (from 01_build_subsample_anchors.R)
#   input_files/glove.rds, input_files/khodakA.rds

library(conText)
library(quanteda)
library(dplyr)

glove  <- readRDS("input_files/glove.rds")
khodak <- readRDS("input_files/khodakA.rds")
subsampled_tokens <- readRDS("subsampled_tokens.rds")

anchor_dems <- readRDS("ALC_dems/anchors.rds")


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Mean anchor embedding within each year. rowsum() orders rows by sorted group
# value and table() uses the same ordering, so the division aligns row-wise.
year_means <- function(anchor) {
  rowsum(anchor$matrix, anchor$docvars$Year) /
    as.numeric(table(anchor$docvars$Year))
}

# Politicization score for one embedding vector, against that year's anchors.
score_vector <- function(v, yr, lib_by_year, con_by_year) {
  yr <- as.character(yr)
  if (!yr %in% rownames(lib_by_year) || !yr %in% rownames(con_by_year)) return(NA_real_)
  cos_lib <- lsa::cosine(as.numeric(v), lib_by_year[yr, ])
  cos_con <- lsa::cosine(as.numeric(v), con_by_year[yr, ])
  (cos_lib + cos_con) / 2
}

# Document names retained by dem(). dem() silently drops contexts with no
# features in the pre-trained vocabulary, so callers must re-align docvars
# against this rather than against the input dfm.
dem_docnames <- function(d) {
  nm <- rownames(d)
  if (is.null(nm)) nm <- d@Dimnames$docs
  nm
}

# Pick up to n_top positions from a score vector. NAs are dropped, not ranked.
#   "top"    -- highest-scoring contexts (face validity at the high end)
#   "middle" -- contexts nearest the median (typical usage)
#   "bottom" -- lowest-scoring contexts
select_idx <- function(scores, n_top, select = c("top", "middle", "bottom")) {
  select <- match.arg(select)
  ok <- which(!is.na(scores))
  if (length(ok) == 0) return(integer(0))
  
  ranked <- switch(
    select,
    top    = ok[order(scores[ok], decreasing = TRUE)],
    bottom = ok[order(scores[ok], decreasing = FALSE)],
    middle = ok[order(abs(scores[ok] - median(scores[ok])))]
  )
  head(ranked, n_top)
}


# ---------------------------------------------------------------------------
# Close reading
# ---------------------------------------------------------------------------

close_reading <- function(term_dem, anchor_dems, n_top = 20,
                          filter_source = NULL, filter_year = NULL,
                          select = c("top", "middle", "bottom")) {
  
  select  <- match.arg(select)
  mat     <- term_dem$matrix
  docvars <- term_dem$docvars
  
  keep <- rep(TRUE, nrow(mat))
  if (!is.null(filter_source)) keep <- keep & docvars$Source == filter_source
  if (!is.null(filter_year))   keep <- keep & docvars$Year   == filter_year
  keep[is.na(keep)] <- FALSE
  
  # drop = FALSE: a single surviving context must stay a matrix, or nrow()
  # returns NULL and seq_len() below errors.
  mat     <- mat[keep, , drop = FALSE]
  docvars <- docvars[keep, , drop = FALSE]
  
  if (nrow(mat) == 0) {
    warning("close_reading(): no contexts survive the filters; returning 0 rows.")
    return(data.frame(Article_ID = character(0), Year = character(0),
                      Source = character(0), Type = character(0),
                      subsample = integer(0), pol_score = numeric(0)))
  }
  
  lib_by_year <- year_means(anchor_dems$liberal)
  con_by_year <- year_means(anchor_dems$conservative)
  
  cos_scores <- vapply(
    seq_len(nrow(mat)),
    function(i) score_vector(mat[i, ], docvars$Year[i], lib_by_year, con_by_year),
    numeric(1)
  )
  
  idx <- select_idx(cos_scores, n_top, select)
  if (length(idx) == 0) {
    warning("close_reading(): no context could be scored (no overlapping anchor years).")
  }
  
  data.frame(
    Article_ID = docvars$Article_ID[idx],
    Year       = docvars$Year[idx],
    Source     = docvars$Source[idx],
    Type       = docvars$Type[idx],
    subsample  = docvars$subsample[idx],
    pol_score  = cos_scores[idx]
  )
}


get_contexts <- function(article_ids, subsample_n, pattern, anchor_dems) {
  
  toks_sub <- subsampled_tokens[[subsample_n]]
  
  ctx <- tokens_context(toks_sub, pattern = pattern, window = 6L)
  
  ctx_docvars <- docvars(ctx)
  match_idx   <- which(ctx_docvars$Article_ID %in% article_ids)
  
  if (length(match_idx) == 0) {
    warning("get_contexts(): no contexts found for the requested Article_ID(s).")
    return(data.frame())
  }
  
  # tokens objects take a single index
  ctx_matched   <- ctx[match_idx]
  dvars_matched <- ctx_docvars[match_idx, , drop = FALSE]
  
  ctx_dfm <- dfm(ctx_matched)
  ctx_dem <- dem(
    x                = ctx_dfm,
    pre_trained      = glove,
    transform        = TRUE,
    transform_matrix = khodak,
    verbose          = FALSE
  )
  
  # Re-align to the contexts dem() actually kept. Without this, dvars_matched
  # rows and ctx_dem rows drift apart and contexts get scored against the
  # wrong year's anchors.
  kept_pos <- match(dem_docnames(ctx_dem), docnames(ctx_dfm))
  kept_pos <- kept_pos[!is.na(kept_pos)]
  
  if (length(kept_pos) == 0) {
    warning("get_contexts(): dem() retained no contexts.")
    return(data.frame())
  }
  if (length(kept_pos) < length(match_idx)) {
    message(sprintf("get_contexts(): dem() dropped %d of %d contexts with no embedded features.",
                    length(match_idx) - length(kept_pos), length(match_idx)))
  }
  
  ctx_kept   <- ctx_matched[kept_pos]
  dvars_kept <- dvars_matched[kept_pos, , drop = FALSE]
  
  lib_by_year <- year_means(anchor_dems$liberal)
  con_by_year <- year_means(anchor_dems$conservative)
  
  cos_scores <- vapply(
    seq_len(nrow(ctx_dem)),
    function(i) score_vector(ctx_dem[i, ], dvars_kept$Year[i], lib_by_year, con_by_year),
    numeric(1)
  )
  
  data.frame(
    context = vapply(ctx_kept, paste, character(1), collapse = " "),
    dvars_kept,
    stringsAsFactors = FALSE
  ) %>%
    group_by(Article_ID) %>%
    mutate(occurrence = row_number()) %>%
    ungroup() %>%
    mutate(pol_score = cos_scores) %>%
    arrange(Article_ID, desc(pol_score))
}


# ---------------------------------------------------------------------------
# EXAMPLE (edit for whatever you actually want to inspect)
# ---------------------------------------------------------------------------

term_dems <- readRDS("ALC_dems/topic_33.rds")

# Highest-scoring contexts: shows the measure picks out recognizably political
# usage at the top of the range.
closereading_results <- close_reading(term_dems[["comedian"]], anchor_dems,
                                      n_top = 20, filter_year = 2024,
                                      select = "top")
print(closereading_results)

# Contexts nearest the median, for contrast. Reporting both supports a face
# validity claim about the whole range rather than only the extreme.
closereading_median <- close_reading(term_dems[["comedian"]], anchor_dems,
                                     n_top = 20, filter_year = 2024,
                                     select = "middle")
print(closereading_median)

example_contexts <- get_contexts(closereading_results$Article_ID[[1]],
                                 closereading_results$subsample[[1]],
                                 "comedian",
                                 anchor_dems = anchor_dems)
print(example_contexts)