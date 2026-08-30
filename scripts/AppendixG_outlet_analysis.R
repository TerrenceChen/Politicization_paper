# AppendixG_outlet_analysis.R (speed-optimized, no parallelization)
#
# Per-outlet (NYT/WaPo/WSJ) politicization comparison
#
#
# Requires:
#   ALC_dems/anchors.rds
#   ALC_dems/topic_<n>.rds
#   Results_Files/dict_terms_final_0825.rds
#   freq_by_year_source.rds
#   all_toks_ngram_0412.rds

library(conText)
library(quanteda)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lme4)
library(texreg)

anchor_dems <- readRDS("ALC_dems/anchors.rds")
dict_terms  <- readRDS("Results_Files/dict_terms_final_0825.rds")

# Subsamples by News outlet
all_toks_final <- readRDS("all_toks_ngram_0412.rds")

set.seed(7)
subsamples <- sample(1:20, size = ndoc(all_toks_final),
                     replace = TRUE, prob = rep(0.05, times = 20))
source_vec <- docvars(all_toks_final, "Source")

outlets <- c("New York Times", "The Washington Post", "Wall Street Journal")
outlet_subsample_sizes <- setNames(lapply(outlets, function(src) {
  as.integer(table(factor(subsamples[!is.na(source_vec) & source_vec == src], levels = 1:20)))
}), outlets)
outlet_tau <- sapply(outlet_subsample_sizes, sum)

message(paste(outlets, round(100 * outlet_tau / 1350000, 1), "%", collapse = "; "))

stopifnot(abs(sum(outlet_tau) - 1350000) < 100)  # sanity check: outlets should partition the corpus


# ===================== Shared helpers =====================

# Aggregate a matrix's rows to one embedding per year. Returns NULL if empty.
group_by_year <- function(mat, years) {
  if (nrow(mat) == 0) return(NULL)
  ys  <- rowsum(mat, group = years)
  cnt <- table(years)
  sweep(ys, 1, as.numeric(cnt[rownames(ys)]), "/")
}

# ===================== Step 1: build each anchor's per-year embedding,
#                       once per (anchor, source, subsample) =====================

build_outlet_anchor_wv <- function(anchor_name, source) {
  ad      <- anchor_dems[[anchor_name]]
  src_idx <- ad$docvars$Source == source
  mat_src <- ad$matrix[src_idx, , drop = FALSE]
  dv_src  <- ad$docvars[src_idx, ]
  lapply(1:20, function(n) {
    rows_n <- dv_src$subsample == n
    group_by_year(mat_src[rows_n, , drop = FALSE], dv_src$Year[rows_n])
  })
}

anchor_wv <- list()
for (source in outlets) {
  message("Building anchor embeddings for: ", source)
  anchor_wv[[source]] <- list(
    liberal      = build_outlet_anchor_wv("liberal", source),
    conservative = build_outlet_anchor_wv("conservative", source),
    democrat     = build_outlet_anchor_wv("democrat", source),
    republican   = build_outlet_anchor_wv("republican", source)
  )
}

# ===================== Step 2: per-term cosine computation, filtering by
#                       Source once and splitting into subsamples/years efficiently =====================

compute_term_outlet <- function(term, term_data, source, sizes,
                                 lib_wv, con_wv, dem_wv, rep_wv) {

  src_idx <- term_data$docvars$Source == source
  mat_src <- term_data$matrix[src_idx, , drop = FALSE]
  dv_src  <- term_data$docvars[src_idx, ]

  years <- 1980:2024
  rows  <- vector("list", 20)

  for (n in 1:20) {
    rows_n  <- dv_src$subsample == n
    term_wv <- group_by_year(mat_src[rows_n, , drop = FALSE], dv_src$Year[rows_n])

    row <- data.frame(term = term, year = years, subsample = n, text_n = sizes[n],
                       cos_lib = NA_real_, cos_con = NA_real_,
                       cos_dem = NA_real_, cos_rep = NA_real_)

    if (!is.null(term_wv)) {
      yrs_present <- intersect(rownames(term_wv), as.character(years))
      lwv <- lib_wv[[n]]; cwv <- con_wv[[n]]; dwv <- dem_wv[[n]]; rwv <- rep_wv[[n]]

      for (y in yrs_present) {
        tv <- term_wv[y, ]
        i  <- match(as.integer(y), years)

        if (!is.null(lwv) && y %in% rownames(lwv)) row$cos_lib[i] <- as.numeric(lsa::cosine(lwv[y, ], tv))
        if (!is.null(cwv) && y %in% rownames(cwv)) row$cos_con[i] <- as.numeric(lsa::cosine(cwv[y, ], tv))
        if (!is.null(dwv) && y %in% rownames(dwv)) row$cos_dem[i] <- as.numeric(lsa::cosine(dwv[y, ], tv))
        if (!is.null(rwv) && y %in% rownames(rwv)) row$cos_rep[i] <- as.numeric(lsa::cosine(rwv[y, ], tv))
      }
    }
    rows[[n]] <- row
  }
  do.call(rbind, rows)
}

summarise_term_year <- function(df, tau) {
  df %>%
    group_by(term, year) %>%
    mutate(
      cos_lib_avg = mean(cos_lib, na.rm = TRUE), cos_con_avg = mean(cos_con, na.rm = TRUE),
      cos_dem_avg = mean(cos_dem, na.rm = TRUE), cos_rep_avg = mean(cos_rep, na.rm = TRUE),
      err_lib = sqrt(text_n) * (cos_lib - cos_lib_avg) / sqrt(tau),
      err_con = sqrt(text_n) * (cos_con - cos_con_avg) / sqrt(tau),
      err_dem = sqrt(text_n) * (cos_dem - cos_dem_avg) / sqrt(tau),
      err_rep = sqrt(text_n) * (cos_rep - cos_rep_avg) / sqrt(tau)
    ) %>%
    summarise(
      cos_lib = mean(cos_lib, na.rm = TRUE),
      cos_lib_lower = cos_lib - quantile(err_lib, 0.95, na.rm = TRUE),
      cos_lib_upper = cos_lib - quantile(err_lib, 0.05, na.rm = TRUE),
      cos_con = mean(cos_con, na.rm = TRUE),
      cos_con_lower = cos_con - quantile(err_con, 0.95, na.rm = TRUE),
      cos_con_upper = cos_con - quantile(err_con, 0.05, na.rm = TRUE),
      cos_dem = mean(cos_dem, na.rm = TRUE),
      cos_dem_lower = cos_dem - quantile(err_dem, 0.95, na.rm = TRUE),
      cos_dem_upper = cos_dem - quantile(err_dem, 0.05, na.rm = TRUE),
      cos_rep = mean(cos_rep, na.rm = TRUE),
      cos_rep_lower = cos_rep - quantile(err_rep, 0.95, na.rm = TRUE),
      cos_rep_upper = cos_rep - quantile(err_rep, 0.05, na.rm = TRUE),
      .groups = "drop"
    )
}

# ===================== Step 3: main loop -- topic outer, outlet inner,
#                       each topic file read exactly once =====================

topics_available <- unique(dict_terms$topic)
topics_available <- topics_available[sapply(topics_available, function(tp)
  file.exists(paste0("ALC_dems/topic_", tp, ".rds")))]

all_results <- vector("list", length(topics_available) * length(outlets))
k <- 1

for (topic in topics_available) {

  topic_dem <- readRDS(paste0("ALC_dems/topic_", topic, ".rds"))

  for (source in outlets) {

    sizes <- outlet_subsample_sizes[[source]]
    tau   <- outlet_tau[[source]]
    lib_wv <- anchor_wv[[source]]$liberal;      con_wv <- anchor_wv[[source]]$conservative
    dem_wv <- anchor_wv[[source]]$democrat;     rep_wv <- anchor_wv[[source]]$republican

    term_rows <- lapply(names(topic_dem), function(term)
      compute_term_outlet(term, topic_dem[[term]], source, sizes, lib_wv, con_wv, dem_wv, rep_wv))

    df <- summarise_term_year(do.call(rbind, term_rows), tau)
    df$Source <- source

    all_results[[k]] <- df
    k <- k + 1
  }
  message("Processed topic (all 3 outlets): ", topic)
}

cos_df <- do.call(rbind, all_results)

# Save the per-outlet .rds outputs
saveRDS(filter(cos_df, Source == "New York Times"),      "Results_Files/nyt_cos_df.rds")
saveRDS(filter(cos_df, Source == "The Washington Post"),  "Results_Files/wp_cos_df.rds")
saveRDS(filter(cos_df, Source == "Wall Street Journal"),  "Results_Files/wsj_cos_df.rds")

# ===================== Combine & Plot =====================

all_df <- cos_df %>%
            left_join(dict_terms, by = "term") %>%
            filter(!is.na(label))

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

all_df_cat <-
    all_df %>%
    assign_category() %>%
    mutate(pol_score_ideo = (cos_lib + cos_con)/2,
           pol_score_party = (cos_dem + cos_rep)/2)

freq_by_year_source <- readRDS("freq_by_year_source.rds")
freq_wide <- freq_by_year_source %>%
  filter(term %in% dict_terms$term) %>%
  mutate(year = as.numeric(Year)) %>%
  select(term, year, Source, frequency) %>%
  pivot_wider(names_from = Source, values_from = frequency, values_fill = 0)

freq_wide <- freq_wide %>%
  mutate(pooled     = `New York Times` + `The Washington Post` + `Wall Street Journal`,
         min_outlet = pmin(`New York Times`, `The Washington Post`, `Wall Street Journal`))

eligible_terms <- freq_wide %>%
  filter(pooled >= 10, min_outlet >= 5) %>%
  select(term, year)

all_df_cat <- all_df_cat %>% semi_join(eligible_terms, by = c("term", "year"))

# --- Compare Increase: regression by outlet ---
results_df_cat_reg <-
   all_df_cat %>%
   group_by(year, label, Category, Source) %>%
   summarise(
    value_ideo_avg   = mean(pol_score_ideo,   na.rm = TRUE),
    value_party_avg  = mean(pol_score_party, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year0 = year - 1980)

model_nyt <- lme4::lmer(value_ideo_avg ~ year0 + (year0|label),
                         data=subset(results_df_cat_reg, Source == "New York Times"))
model_wp <- lme4::lmer(value_ideo_avg ~ year0 + (year0|label),
                         data=subset(results_df_cat_reg, Source == "The Washington Post"))
model_wsj <- lme4::lmer(value_ideo_avg ~ year0 + (year0|label),
                         data=subset(results_df_cat_reg, Source == "Wall Street Journal"))

texreg(list(model_nyt, model_wp, model_wsj), digits = 4,
       file = "Tables/outlet_models_ideo.tex")

modelP_nyt <- lme4::lmer(value_party_avg ~ year0 + (year0|label),
                         data=subset(results_df_cat_reg, Source == "New York Times"))
modelP_wp <- lme4::lmer(value_party_avg ~ year0 + (year0|label),
                         data=subset(results_df_cat_reg, Source == "The Washington Post"))
modelP_wsj <- lme4::lmer(value_party_avg ~ year0 + (year0|label),
                         data=subset(results_df_cat_reg, Source == "Wall Street Journal"))

texreg(list(modelP_nyt, modelP_wp, modelP_wsj), digits = 4,
       file = "Tables/outlet_models_party.tex")

# --- Graphs ---
category_year <- all_df_cat %>%
  group_by(year, Category, Source) %>%
  summarise(
    value_ideo_avg   = mean(pol_score_ideo,   na.rm = TRUE),
    value_party_avg  = mean(pol_score_party, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year0 = year - 1980)

custom_order <-
c('Politics','Race and Immigration', 'Legal Institution','Gender and Family', 'Religion',
  'Education','Media','Gun','Economic Issues','Foreign Affairs',
  'Crime and Policing', 'Science', 'Creative Arts', 'Health', 'Environment',
  'Housing', 'Leisure', 'Animals', 'Transportation', 'Food & Drinks',
  'Infrastructure', 'Sports','Financial Market', 'Outer Space', 'Home')

category_year_ideo <-
  category_year %>%
  group_by(Category, Source) %>%
  summarise(
    late_value  = mean(value_ideo_avg[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(value_ideo_avg[year >= 1980 & year <= 1989], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(late_value))

category_year_ideo %>%
  filter(Category != "Other") %>%
  mutate(Category = factor(Category, rev(custom_order))) %>%
  ggplot() +
  geom_segment(
    aes(x = early_value,
        xend = late_value,
        y = Category,
        color = Source),
    position = position_dodge(width = 0.7),
    arrow = arrow(length = unit(0.1, "inches")),
    linewidth = 0.8) +
  geom_point(aes(x = early_value, y = Category, color = Source),
    position = position_dodge(width = 0.7), size = 1.5) +
  geom_point(aes(x = late_value, y = Category, color = Source),
    position = position_dodge(width = 0.7), size = 1.5) +
  scale_color_manual(values = c(
    "New York Times" = "#648FFF",
    "The Washington Post" = "#FFB000",
    "Wall Street Journal" = "#FE6100")) +
  labs(x = "Politicization Score",
       y = "Issue Domains") +
  theme_minimal(base_size = 11)

ggsave("Graphs/FigureG1_newspaper_ideo_cat.png", width = 6.5, height = 7)

category_year %>%
  group_by(Category, Source) %>%
  summarise(
    late_value  = mean(value_party_avg[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(value_party_avg[year >= 1980 & year <= 1989], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(Category != "Other") %>%
  mutate(Category = factor(Category, rev(custom_order))) %>%
  ggplot() +
  geom_segment(
    aes(x = early_value,
        xend = late_value,
        y = Category,
        color = Source),
    position = position_dodge(width = 0.7),
    arrow = arrow(length = unit(0.1, "inches")),
    linewidth = 0.8) +
  geom_point(aes(x = early_value, y = Category, color = Source),
    position = position_dodge(width = 0.7), size = 1.5) +
  geom_point(aes(x = late_value, y = Category, color = Source),
    position = position_dodge(width = 0.7), size = 1.5) +
  scale_color_manual(values = c(
    "New York Times" = "#648FFF",
    "The Washington Post" = "#FFB000",
    "Wall Street Journal" = "#FE6100")) +
  labs(x = "Politicization Score",
       y = "Issue Domains") +
  theme_minimal(base_size = 11)

ggsave("Graphs/FigureG2_newspaper_party_cat.png", width = 6.5, height = 7)

message("AppendixG complete.")