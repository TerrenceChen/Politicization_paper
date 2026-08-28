# 07_step5_anchor_dems.R
#
# Builds the raw (ungrouped) anchor DEMs -- liberal, conservative, democrat,
# republican -- with per-context text preserved, needed for AppendixF,
# AppendixG, and the close-reading functions later. 
#
# Output:
#   ALC_dems/anchors.rds

library(conText)
library(quanteda)
library(dplyr)

glove  <- readRDS("input_files/glove.rds")
khodak <- readRDS("input_files/khodakA.rds")
subsampled_tokens <- readRDS("subsampled_tokens.rds")
subsample_sizes   <- readRDS("subsample_sizes.rds")

build_anchor_dem_new <- function(toks_subsample, pattern, filter_terms = NULL) {

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

  return(list(dem = anchor_dem, toks = anchor_toks))
}

liberal_patterns <- c("liberal", "liberals", "liberalism", "liberal-leaning")
conservative_patterns <- c("conservative", "conservatives",
                            "conservativism", "conservative-leaning")
conservative_noise_terms <- c(
  "estimate", "estimates", "projection", "projections",
  "forecast", "forecasts", "investment", "investments",
  "investor", "investors"
)

Democrat_patterns <- c("democrat", "democrats")
Republican_patterns <- c("republican", "republicans")

anchor_specs <- list(
  list(name = "liberal",      patterns = liberal_patterns,      filter = NULL),
  list(name = "conservative", patterns = conservative_patterns, filter = conservative_noise_terms),
  list(name = "democrat",     patterns = Democrat_patterns,     filter = NULL),
  list(name = "republican",   patterns = Republican_patterns,   filter = NULL)
)

anchor_dems <- lapply(anchor_specs, function(spec) {

  sub_dems <- lapply(1:20, function(n) {
    result  <- build_anchor_dem_new(
      toks_subsample = subsampled_tokens[[n]],
      pattern        = spec$patterns,
      filter_terms   = spec$filter
    )
    dem_obj <- result$dem
    toks    <- result$toks

    dem_obj@docvars$subsample <- n
    dem_obj@docvars$text_n    <- subsample_sizes[n]

    list(
      matrix   = as.matrix(dem_obj),
      docvars  = dem_obj@docvars,
      contexts = sapply(toks, paste, collapse = " ")
    )
  })

  list(
    matrix   = do.call(rbind,  lapply(sub_dems, `[[`, "matrix")),
    docvars  = do.call(rbind,  lapply(sub_dems, `[[`, "docvars")),
    contexts = do.call(c,      lapply(sub_dems, `[[`, "contexts"))
  )
})

names(anchor_dems) <- sapply(anchor_specs, `[[`, "name")

dir.create("ALC_dems", showWarnings = FALSE)
saveRDS(anchor_dems, "ALC_dems/anchors.rds")

message("Anchor DEMs saved.")
