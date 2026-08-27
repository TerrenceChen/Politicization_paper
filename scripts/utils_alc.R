# utils_alc.R
# Shared helper, extracted verbatim from Step3_ALC_Embedding.ipynb (cell "build_anchor_wv").
# Sourced by every script below. Expects `glove` and `khodak` to already be
# loaded in the calling environment.

build_anchor_wv <- function(toks_subsample, pattern, filter_terms = NULL) {

  # 1. Build token contexts
  anchor_toks <- tokens_context(
    x       = toks_subsample,
    pattern = pattern,
    window  = 6L
  )

  # 2. Optionally filter out contexts where noise terms appear nearby
  if (!is.null(filter_terms)) {
    has_noise <- sapply(anchor_toks, function(ctx) any(filter_terms %in% ctx))
    anchor_toks <- anchor_toks[!has_noise]
  }

  # 3. Build DFM
  anchor_dfm <- dfm(anchor_toks)

  # 4. Build DEM
  anchor_dem <- dem(
    x                = anchor_dfm,
    pre_trained      = glove,
    transform        = TRUE,
    transform_matrix = khodak,
    verbose          = FALSE
  )

  # 5. Group by year
  anchor_wv <- dem_group(anchor_dem, groups = anchor_dem@docvars$Year)

  return(anchor_wv)
}
