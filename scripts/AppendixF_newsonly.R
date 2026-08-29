# News-Only politicization score, using Step5's own cached raw DEM matrices
# (ALC_dems/topic_*.rds, ALC_dems/anchors.rds) rather than recomputing embeddings.
#
# Requires:
#   Results_Files/dict_terms_final_0825.rds
#   ALC_dems/anchors.rds                  
#   ALC_dems/topic_<n>.rds                  
#   freq_by_year_source.rds                   
#   Results_Files/ALC_ideology_results_df.rds, ALC_party_results_df.rds  

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(quanteda)

dict_terms  <- readRDS("Results_Files/dict_terms_final_0825.rds")
anchor_dems <- readRDS("ALC_dems/anchors.rds")

# --- News-only sample sizes -------------------------------------------------

NEWS_PATTERN <- "News|Article|Feature|Front Page|Cover Story"

all_toks_final <- readRDS("all_toks_ngram_0412.rds")

set.seed(7)
subsamples <- sample(1:20, size = ndoc(all_toks_final),
                     replace = TRUE, prob = rep(0.05, times = 20))

type_vec <- docvars(all_toks_final, "Type")
is_news  <- !is.na(type_vec) & str_detect(type_vec, NEWS_PATTERN)

subsample_sizes <- as.integer(table(factor(subsamples[is_news], levels = 1:20)))
tau_news        <- sum(subsample_sizes)

message("News-only corpus: ", tau_news, " articles (",
        round(100 * tau_news / 1350000, 1), "% of the full sample)")


# Focus on news only

group_dem_by_year_newsonly <- function(mat, docvars, include_pattern = NEWS_PATTERN) {

  keep    <- !is.na(docvars$Type) &
              str_detect(docvars$Type, include_pattern)
  mat     <- mat[keep, , drop = FALSE]
  docvars <- docvars[keep, ]

  if (nrow(mat) == 0) return(NULL)

  year_sum   <- rowsum(mat, group = docvars$Year)
  year_count <- table(docvars$Year)
  sweep(year_sum, 1, as.numeric(year_count[rownames(year_sum)]), "/")
}

build_newsonly_anchor_wv_subsamples <- function(anchor_name) {
  ad <- anchor_dems[[anchor_name]]
  lapply(1:20, function(n) {
    sub_rows <- ad$docvars$subsample == n
    group_dem_by_year_newsonly(ad$matrix[sub_rows, , drop = FALSE], ad$docvars[sub_rows, ])
  })
}

lib_wv_subsamples_newsonly <- build_newsonly_anchor_wv_subsamples("liberal")
con_wv_subsamples_newsonly <- build_newsonly_anchor_wv_subsamples("conservative")
dem_wv_subsamples_newsonly <- build_newsonly_anchor_wv_subsamples("democrat")
rep_wv_subsamples_newsonly <- build_newsonly_anchor_wv_subsamples("republican")

compute_term_metrics_newsonly <- function(t, term_data) {

  years   <- as.character(1980:2024)
  results <- list()

  for (n in 1:20) {

    sub_rows <- term_data$docvars$subsample == n
    term_wv  <- group_dem_by_year_newsonly(term_data$matrix[sub_rows, , drop = FALSE],
                                            term_data$docvars[sub_rows, ])

    lib_wv <- lib_wv_subsamples_newsonly[[n]]
    con_wv <- con_wv_subsamples_newsonly[[n]]

    for (year in years) {

      year_present <- !is.null(term_wv) && !is.null(lib_wv) && !is.null(con_wv) &&
                       year %in% rownames(term_wv) &&
                       year %in% rownames(lib_wv)  &&
                       year %in% rownames(con_wv)

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
      error_lib = sqrt(text_n) * (cos_liberal - cos_liberal_avg) / sqrt(tau_news),
      error_con = sqrt(text_n) * (cos_con     - cos_con_avg)     / sqrt(tau_news)
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

compute_party_metrics_newsonly <- function(t, term_data) {
  
  years   <- as.character(1980:2024)
  results <- list()
  
  for (n in 1:20) {
    
    sub_rows <- term_data$docvars$subsample == n
    term_wv  <- group_dem_by_year_newsonly(term_data$matrix[sub_rows, , drop = FALSE],
                                           term_data$docvars[sub_rows, ])
    
    dem_wv <- dem_wv_subsamples_newsonly[[n]]
    rep_wv <- rep_wv_subsamples_newsonly[[n]]
    
    for (year in years) {
      
      year_present <- !is.null(term_wv) && !is.null(dem_wv) && !is.null(rep_wv) &&
        year %in% rownames(term_wv) &&
        year %in% rownames(dem_wv)  &&
        year %in% rownames(rep_wv)
      
      if (year_present) {
        avg_emb <- term_wv[year, ]
        cos_dem <- as.numeric(lsa::cosine(dem_wv[year, ], avg_emb))
        cos_rep <- as.numeric(lsa::cosine(rep_wv[year, ], avg_emb))
      } else {
        cos_dem <- NA
        cos_rep <- NA
      }
      
      results[[length(results) + 1]] <- data.frame(
        term      = t,
        year      = as.integer(year),
        subsample = n,
        text_n    = subsample_sizes[n],
        cos_dem   = cos_dem,
        cos_rep   = cos_rep
      )
    }
  }
  
  do.call(rbind, results) %>%
    group_by(term, year) %>%
    mutate(
      cos_dem_avg = mean(cos_dem, na.rm = TRUE),
      cos_rep_avg = mean(cos_rep, na.rm = TRUE),
      error_dem = sqrt(text_n) * (cos_dem - cos_dem_avg) / sqrt(tau_news),
      error_rep = sqrt(text_n) * (cos_rep - cos_rep_avg) / sqrt(tau_news)
    ) %>%
    summarise(
      cos_dem_mean  = mean(cos_dem, na.rm = TRUE),
      cos_dem_lower = cos_dem_mean - quantile(error_dem, 0.95, na.rm = TRUE),
      cos_dem_upper = cos_dem_mean - quantile(error_dem, 0.05, na.rm = TRUE),
      cos_rep_mean  = mean(cos_rep, na.rm = TRUE),
      cos_rep_lower = cos_rep_mean - quantile(error_rep, 0.95, na.rm = TRUE),
      cos_rep_upper = cos_rep_mean - quantile(error_rep, 0.05, na.rm = TRUE),
      .groups = "drop"
    )
}

# --- Apply to all topics ---
newsonly_topics <- unique(dict_terms$topic)
newsonly_topics <- newsonly_topics[sapply(newsonly_topics, function(tp)
  file.exists(paste0("ALC_dems/topic_", tp, ".rds")))]

results_df_newsonly <- do.call(rbind, lapply(newsonly_topics, function(tp) {
  topic_terms <- subset(dict_terms, topic == tp)$term
  term_dems   <- readRDS(paste0("ALC_dems/topic_", tp, ".rds"))
  message("Processing topic (ideology, news-only): ", tp)
  do.call(rbind, lapply(topic_terms, function(term) compute_term_metrics_newsonly(term, term_dems[[term]])))
}))

results_df_party_newsonly <- do.call(rbind, lapply(newsonly_topics, function(tp) {
  topic_terms <- subset(dict_terms, topic == tp)$term
  term_dems   <- readRDS(paste0("ALC_dems/topic_", tp, ".rds"))
  message("Processing topic (party, news-only): ", tp)
  do.call(rbind, lapply(topic_terms, function(term) compute_party_metrics_newsonly(term, term_dems[[term]])))
}))

# --- Merge with dict_terms + frequency, compute politicization score ---
freq_by_year <- readRDS("freq_by_year_source.rds") %>%
  group_by(term, Year) %>%
  summarise(frequency = sum(frequency, na.rm = TRUE), .groups = "drop") %>%
  rename(year = Year) %>%
  mutate(year = as.numeric(year))

results_df_newsonly <- results_df_newsonly %>%
  left_join(dict_terms, by = "term") %>%
  filter(!is.na(label)) %>%
  left_join(freq_by_year, by = c("term", "year")) %>%
  filter(frequency >= 10) %>%
  mutate(pol_score       = (cos_liberal_mean  + cos_con_mean)  / 2,
         pol_score_upper = (cos_liberal_upper + cos_con_upper) / 2,
         pol_score_lower = (cos_liberal_lower + cos_con_lower) / 2,
         group = "ideology") %>%
  select(term, year, topic, label, pol_score, pol_score_upper, pol_score_lower, group)

results_df_party_newsonly <- results_df_party_newsonly %>%
  left_join(dict_terms, by = "term") %>%
  filter(!is.na(label)) %>%
  left_join(freq_by_year, by = c("term", "year")) %>%
  filter(frequency >= 10) %>%
  mutate(pol_score       = (cos_dem_mean  + cos_rep_mean)  / 2,
         pol_score_upper = (cos_dem_upper + cos_rep_upper) / 2,
         pol_score_lower = (cos_dem_lower + cos_rep_lower) / 2,
         group = "party") %>%
  select(term, year, topic, label, pol_score, pol_score_upper, pol_score_lower, group)

saveRDS(results_df_newsonly,       "Results_Files/ALC_ideology_results_df_newsonly.rds")
saveRDS(results_df_party_newsonly, "Results_Files/ALC_party_results_df_newsonly.rds")

# --- Load original All-Articles results, processed the same way ---

results_df_newsonly <- readRDS("Results_Files/ALC_ideology_results_df_newsonly.rds")
results_df_party_newsonly <- readRDS("Results_Files/ALC_party_results_df_newsonly.rds")

results_df_original <- readRDS("Results_Files/ALC_ideology_results_df.rds") %>%
  filter(frequency >= 10) %>%
  mutate(pol_score       = (cos_liberal_mean  + cos_con_mean)  / 2,
         pol_score_upper = (cos_liberal_upper + cos_con_upper) / 2,
         pol_score_lower = (cos_liberal_lower + cos_con_lower) / 2,
         group = "ideology") %>%
  select(term, year, topic, label, pol_score, pol_score_upper, pol_score_lower, group)

results_df_party_original <- readRDS("Results_Files/ALC_party_results_df.rds") %>%
  filter(frequency >= 10) %>%
  mutate(pol_score       = (cos_dem_mean  + cos_rep_mean)  / 2,
         pol_score_upper = (cos_dem_upper + cos_rep_upper) / 2,
         pol_score_lower = (cos_dem_lower + cos_rep_lower) / 2,
         group = "party") %>%
  select(term, year, topic, label, pol_score, pol_score_upper, pol_score_lower, group)

# --- Category mapping (Step3's exact case_when block) ---
assign_category <- function(df) {
  df %>%
    mutate(Category = case_when(
      label %in% c("Abortion", "Gender", "LGBTQ", "Marriage", "Parenting") ~ "Gender and Family",
      label %in% c("Catholicism", "Religion") ~ "Religion",
      label %in% c("Minority", "Native_American", "Racial", "Confederate", "Immigration") ~ "Race and Immigration",
      label %in% c("Campaigns & Donors", "Protest", "PublicOpinion", "Senate", "Elections") ~ "Politics",
      label %in% c("CriminalJustice", "Investigation", "Criminal", "Burglary",
                   "Homicide", "Police") ~ "Crime and Policing",
      label %in% c("Constitution_Law", "Judicial", "Lawsuit", "SupremeCourt") ~ "Legal Institution",
      label %in% c("Higher_Education", "Education") ~ "Education",
      label %in% c("Science") ~ "Science",
      label %in% c("Gun_Control") ~ "Gun",
      label %in% c("Bankruptcy", "Banks", "Commodity_Market", "Corporate", "CorporateFinance",
                   "Earnings", "ExecutiveCompensation", "Financial", "Investing", "Money_Markets",
                   "Retail", "Securities",
                   "Stock_market", "Foreign_exchange") ~ "Financial Market",
      label %in% c("Welfare & Poverty", "Taxation", "Economics", "Agriculture", "Business",
                   "Economy", "Fiscal", "Labor", "Workers", "MonetaryPolicy", "Trade") ~ "Economic Issues",
      label %in% c("Housing", "Urban_development", "Mortgage", "Architecture") ~ "Housing",
      label %in% c("China", "Espionage", "Latin_America", "Middle_East",
                   "MiddleEastConflict", "Military", "NuclearArms", "Terrorism", "Russia & Ukraine") ~ "Foreign Affairs",
      label %in% c("ClimateChange", "Disaster", "Air Pollution",
                   "Weather", "Wildfires") ~ "Environment",
      label %in% c("Healthcare", "Hospital", "MentalHealth", "Nutrition", "Pharmaceuticals",
                   "Substance Abuse", "Vaccine", "Disease") ~ "Health",
      label %in% c("Art",  "Broadway",  "Classical_Music",
                      "Museum", "Fashion",
                      "Literature", "Movie", "Music") ~ "Creative Arts",
      label %in% c("Festival", "TravelIndustry", "Horticulture", "Nature", "Gaming") ~ "Leisure",
      label %in% c("Culinary", "Dining",  "Wine", "Beverage") ~ "Food & Drinks",
      label %in% c("Journalism", "Radio", "Television", "TV_Entertainment") ~ "Media",
      label %in% c("AmericanFootball", "Baseball", "NBA", "NFL", "Boxing",
                   "CollegeSports", "Golf", "NHL", "Soccer",  "Tennis") ~ "Sports",
      label %in% c("Electricity", "Energy", "Postal_service",  "Telecommunications", "Water") ~ "Infrastructure",
      label %in% c("Maritime",  "Mass_Transit", "Airline", "Automotive",
                    "Railroad", "Traffic", "Transportation") ~ "Transportation",
      label %in% c("Animal", "Marine_Biology",  "Wildlife") ~ "Animals",
      label %in% c("Home", "Household", "Cooking") ~ "Home",
      label == "Space" ~ "Outer Space",
      TRUE ~ "Other"))
}

results_df_cat_newsonly <- assign_category(rbind(results_df_newsonly, results_df_party_newsonly)) %>%
  mutate(Corpus = "News-Only")

results_df_cat_original <- assign_category(rbind(results_df_original, results_df_party_original)) %>%
  mutate(Corpus = "All Articles")

results_df_cat_combined <- rbind(results_df_cat_original, results_df_cat_newsonly)

# --- Overall trend comparison (Figure 1 style) ---
results_df_cat_combined %>%
    group_by(year, group, Corpus) %>%
    summarise(value_avg = mean(pol_score, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = year, y = value_avg, color = Corpus)) +
    geom_point() +
    geom_smooth(method = "gam", se = FALSE) +
    facet_wrap(~ group) +
    theme_classic() +
    labs(x = "Year", y = "Politicization Score") +
    guides(color = guide_legend(title = ""))

ggsave("Graphs/FigureF1_pol_trend_compare.png", width = 6.5, height =7)

# --- Category-level early vs. late period (Figures 3/4 style) ---
category_year <- results_df_cat_combined %>%
  group_by(year, Category, group, Corpus) %>%
  summarise(
    value_avg   = mean(pol_score,       na.rm = TRUE),
    value_upper = mean(pol_score_upper, na.rm = TRUE),
    value_lower = mean(pol_score_lower, na.rm = TRUE),
    .groups = "drop"
  )

category_period <- category_year %>%
  group_by(Category, group, Corpus) %>%
  summarise(
    late_value  = mean(value_avg[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_upper  = mean(value_upper[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_lower  = mean(value_lower[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(value_avg[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_upper = mean(value_upper[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_lower = mean(value_lower[year >= 1980 & year <= 1989], na.rm = TRUE),
    .groups = "drop"
  )

category_period %>%
  filter(group == "ideology") %>%
  ggplot(aes(y = reorder(Category, late_value))) +
  geom_point(aes(x = late_value, color = "2015-2024 Average")) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper, color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper, color = "1980-1989 Average"), width = 0.3) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  facet_wrap(~ Corpus) +
  labs(x = "Politicization Score", y = "Issue Domains") +
  guides(color = guide_legend(title = "Period")) +
  theme_minimal(base_size = 11)

ggsave("Graphs/FigureF2_politicization_ideo_newsonly.png", width =6.5, height = 7)

category_period %>%
  filter(group == "party") %>%
  ggplot(aes(y = reorder(Category, late_value))) +
  geom_point(aes(x = late_value, color = "2015-2024 Average")) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper, color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper, color = "1980-1989 Average"), width = 0.3) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  facet_wrap(~ Corpus) +
  labs(x = "Politicization Score", y = "Issue Domains") +
  guides(color = guide_legend(title = "Period")) +
  theme_minimal(base_size = 11)

ggsave("Graphs/FigureF3_politicization_party_newsonly.png", width =6.5, height = 7)

# --- Rank comparison ---
compare_rank_order <- function(period_data, value_col, g) {
  ranked <- period_data %>%
    filter(group == g) %>%
    select(Category, Corpus, value = all_of(value_col)) %>%
    group_by(Corpus) %>%
    mutate(rank = rank(-value)) %>%
    ungroup() %>%
    select(-value) %>%
    pivot_wider(names_from = Corpus, values_from = rank) %>%
    mutate(rank_diff = abs(`All Articles` - `News-Only`)) %>%
    arrange(desc(rank_diff))

  list(
    spearman_rho = cor(ranked$`All Articles`, ranked$`News-Only`, method = "spearman"),
    table = ranked
  )
}

rank_ideo_late  <- compare_rank_order(category_period, "late_value", "ideology")
rank_party_late <- compare_rank_order(category_period, "late_value", "party")

message("Ideology rank correlation (all vs news-only): ", round(rank_ideo_late$spearman_rho, 3))
message("Party rank correlation (all vs news-only): ", round(rank_party_late$spearman_rho, 3))
