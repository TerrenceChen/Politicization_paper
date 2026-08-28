# 01_build_subsample_anchors.R
#
# One-time setup step. Builds the 20 token subsamples and the four anchor
# embeddings (liberal, conservative, democrat, republican) that every
# per-topic task in the job arrays needs. Runs once so the array tasks don't
# each redo this work.
#
# Inputs  (must be present in the working directory):
#   all_toks_ngram_0412.rds
#   glove.rds
#   khodakA.rds
#
# Outputs:
#   subsampled_tokens.rds, subsample_sizes.rds
#   lib_wv_subsamples.rds, con_wv_subsamples.rds
#   dem_wv_subsamples.rds, rep_wv_subsamples.rds

library(conText)
library(quanteda)
library(dplyr)

source("scripts/utils_alc.R")

all_toks_final <- readRDS("all_toks_ngram_0412.rds")
glove          <- readRDS("input_files/glove.rds")
khodak         <- readRDS("input_files/khodakA.rds")

# --- Build subsamples (seed fixed for reproducibility) ---
indices <- seq_len(ndoc(all_toks_final))
set.seed(7)
subsamples <- sample(1:20, size = length(indices), replace = TRUE, prob = rep(0.05, times = 20))
subsampled_tokens <- split(all_toks_final, subsamples)
subsample_sizes   <- sapply(subsampled_tokens, length)

saveRDS(subsampled_tokens, "subsampled_tokens.rds")
saveRDS(subsample_sizes, "subsample_sizes.rds")

# --- Ideology anchors: liberal / conservative ---
conservative_noise_terms <- c(
  "estimate", "estimates", "projection", "projections",
  "forecast", "forecasts", "investment", "investments",
  "investor", "investors"
)
liberal_patterns     <- c("liberal", "liberals", "liberalism", "liberal-leaning")
conservative_patterns <- c("conservative", "conservatives", "conservatism", "conservative-leaning")

lib_wv_subsamples <- lapply(1:20, function(n) {
  build_anchor_wv(subsampled_tokens[[n]], pattern = liberal_patterns, filter_terms = NULL)
})
con_wv_subsamples <- lapply(1:20, function(n) {
  build_anchor_wv(subsampled_tokens[[n]], pattern = conservative_patterns, filter_terms = conservative_noise_terms)
})

saveRDS(lib_wv_subsamples, "lib_wv_subsamples.rds")
saveRDS(con_wv_subsamples, "con_wv_subsamples.rds")

# --- Party anchors: democrat / republican ---
Democrat_patterns   <- c("democrat", "democrats")
Republican_patterns <- c("republican", "republicans")

dem_wv_subsamples <- lapply(1:20, function(n) {
  build_anchor_wv(subsampled_tokens[[n]], pattern = Democrat_patterns, filter_terms = NULL)
})
rep_wv_subsamples <- lapply(1:20, function(n) {
  build_anchor_wv(subsampled_tokens[[n]], pattern = Republican_patterns, filter_terms = NULL)
})

saveRDS(dem_wv_subsamples, "dem_wv_subsamples.rds")
saveRDS(rep_wv_subsamples, "rep_wv_subsamples.rds")

message("Anchor setup complete.")
