# Standalone -- only requires the two Step3 output files below.
#
# Every figure is now a baseline-vs-robustness-check comparison rather than
# a single version:
#   FigureJ1 / J2 / J3  -- Approach 1 (raise frequency threshold to >=100)
#                           vs. the frequency>=10 baseline
#   FigureJ4 / J5 / J6  -- Approach 2 (frequency-adjusted / residualized score)
#                           vs. the raw baseline score, both on frequency>=10 data
#
# Requires:
#   Results_Files/ALC_ideology_results_df.rds
#   Results_Files/ALC_party_results_df.rds

library(dplyr)
library(ggplot2)
library(tidyr)

president <- data.frame(xintercepts = c(1981, 1989, 1993, 2001, 2009, 2017, 2021))

pres_scale <- list(
  scale_x_continuous(
    breaks = c(1981, 1989, 1993, 2001, 2009, 2017, 2021),
    labels = c("1981\nReagan", "1989\nBush", "1993\nClinton",
               "2001\nBush", "2009\nObama", "2017\nTrump", "2021\nBiden"))
)

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

# ======================================================================
# Baseline construction (frequency >= 10), shared by both approaches
# ======================================================================
results_df <- readRDS("Results_Files/ALC_ideology_results_df.rds") %>%
  filter(frequency >= 10) %>%
  mutate(pol_score = (cos_liberal_mean + cos_con_mean)/2,
         pol_score_upper = (cos_liberal_upper + cos_con_upper)/2,
         pol_score_lower = (cos_liberal_lower + cos_con_lower)/2,
         group = "ideology") %>%
  select(term, year, topic, label, pol_score, pol_score_upper, pol_score_lower, group, frequency)

results_df_party <- readRDS("Results_Files/ALC_party_results_df.rds") %>%
  filter(frequency >= 10) %>%
  mutate(pol_score = (cos_dem_mean + cos_rep_mean)/2,
         pol_score_upper = (cos_dem_upper + cos_rep_upper)/2,
         pol_score_lower = (cos_dem_lower + cos_rep_lower)/2,
         group = "party") %>%
  select(term, year, topic, label, pol_score, pol_score_upper, pol_score_lower, group, frequency)

# ======================================================================
# APPROACH 1: raise minimum frequency threshold to >= 100
# ======================================================================

results_df_cat_baseline <- assign_category(bind_rows(results_df, results_df_party)) %>%
  mutate(Threshold = "Frequency >= 10 (baseline)")

results_df_cat_highfreq <- assign_category(
  bind_rows(
    filter(results_df, frequency >= 100),
    filter(results_df_party, frequency >= 100)
  )
) %>%
  mutate(Threshold = "Frequency >= 100")

results_df_cat_thresh <- bind_rows(results_df_cat_baseline, results_df_cat_highfreq)

## --- FigureJ1: overall trend, baseline vs high-frequency ---
results_df_cat_thresh %>%
  group_by(year, group, Threshold) %>%
  summarise(value_avg = mean(pol_score, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = value_avg, color = Threshold)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam", se = FALSE) +
  geom_vline(data = president, aes(xintercept = xintercepts), linetype = "dotted") +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  pres_scale +
  facet_wrap(~ group) +
  theme_classic() +
  labs(x = "Year", y = "Politicization Score") +
  guides(color = guide_legend(title = ""))

ggsave("FigureJ1_politicization_overall_highfreq.png", width = 10, height = 5)

## --- Category-level early vs. late, for both thresholds ---
category_year_thresh <- results_df_cat_thresh %>%
  group_by(year, Category, group, Threshold) %>%
  summarise(
    value_avg   = mean(pol_score,       na.rm = TRUE),
    value_upper = mean(pol_score_upper, na.rm = TRUE),
    value_lower = mean(pol_score_lower, na.rm = TRUE),
    .groups = "drop"
  )

category_period_thresh <- category_year_thresh %>%
  group_by(Category, group, Threshold) %>%
  summarise(
    late_value  = mean(value_avg[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_upper  = mean(value_upper[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_lower  = mean(value_lower[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(value_avg[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_upper = mean(value_upper[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_lower = mean(value_lower[year >= 1980 & year <= 1989], na.rm = TRUE),
    .groups = "drop"
  )

## --- FigureJ2: ideology, baseline vs high-frequency, faceted ---
category_period_thresh %>%
  filter(group == "ideology") %>%
  ggplot(aes(y = reorder(Category, late_value))) +
  geom_point(aes(x = late_value, color = "2015-2024 Average")) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper, color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper, color = "1980-1989 Average"), width = 0.3) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  facet_wrap(~ Threshold) +
  labs(x = "Politicization Score", y = "Issue Domains") +
  guides(color = guide_legend(title = "Period")) +
  theme_minimal(base_size = 11)

ggsave("FigureJ2_politicization_ideo_HighFreq.png", width = 11, height = 7)

## --- FigureJ3: party, baseline vs high-frequency, faceted ---
category_period_thresh %>%
  filter(group == "party") %>%
  ggplot(aes(y = reorder(Category, late_value))) +
  geom_point(aes(x = late_value, color = "2015-2024 Average")) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper, color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper, color = "1980-1989 Average"), width = 0.3) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  facet_wrap(~ Threshold) +
  labs(x = "Politicization Score", y = "Issue Domains") +
  guides(color = guide_legend(title = "Period")) +
  theme_minimal(base_size = 11)

ggsave("FigureJ3_politicization_party_HighFreq.png", width = 11, height = 7)

# ======================================================================
# APPROACH 2: residualize on frequency, keep residuals (frequency-adjusted)
# Both "Raw" and "Adjusted" here use the SAME frequency>=10 baseline data --
# ======================================================================

freq_model_ideo  <- lm(pol_score ~ frequency, data = results_df,       na.action = na.exclude)
freq_model_party <- lm(pol_score ~ frequency, data = results_df_party, na.action = na.exclude)

results_df$pol_score_adj       <- resid(freq_model_ideo)  + mean(results_df$pol_score,       na.rm = TRUE)
results_df_party$pol_score_adj <- resid(freq_model_party) + mean(results_df_party$pol_score, na.rm = TRUE)

## Shift existing CI bounds by the same recentering delta -- the adjustment
## only corrects the point estimate's frequency-driven bias, not the
## underlying sampling variability, so the CI half-width is preserved.
results_df$pol_score_upper_adj       <- results_df$pol_score_upper       + (results_df$pol_score_adj       - results_df$pol_score)
results_df$pol_score_lower_adj       <- results_df$pol_score_lower       + (results_df$pol_score_adj       - results_df$pol_score)
results_df_party$pol_score_upper_adj <- results_df_party$pol_score_upper + (results_df_party$pol_score_adj - results_df_party$pol_score)
results_df_party$pol_score_lower_adj <- results_df_party$pol_score_lower + (results_df_party$pol_score_adj - results_df_party$pol_score)

## Sanity check: frequency correlation should collapse toward zero after adjustment
message("Ideology: cor(pol_score, frequency) = ", round(cor(results_df$pol_score, results_df$frequency, use = "complete.obs"), 4),
        " -> cor(pol_score_adj, frequency) = ", round(cor(results_df$pol_score_adj, results_df$frequency, use = "complete.obs"), 4))
message("Party: cor(pol_score, frequency) = ", round(cor(results_df_party$pol_score, results_df_party$frequency, use = "complete.obs"), 4),
        " -> cor(pol_score_adj, frequency) = ", round(cor(results_df_party$pol_score_adj, results_df_party$frequency, use = "complete.obs"), 4))

## Does the year trend survive frequency-adjustment? Yes
print(summary(lm(pol_score     ~ year, data = results_df))$coefficients["year", ])
print(summary(lm(pol_score_adj ~ year, data = results_df))$coefficients["year", ])

# Build long-format Raw vs Adjusted, matching the Threshold pattern above
results_df_raw_tagged <- results_df %>%
  mutate(group = "ideology", Adjustment = "Raw",
         value = pol_score, value_upper = pol_score_upper, value_lower = pol_score_lower) %>%
  select(term, year, topic, label, group, Adjustment, value, value_upper, value_lower)

results_df_adj_tagged <- results_df %>%
  mutate(group = "ideology", Adjustment = "Frequency-adjusted",
         value = pol_score_adj, value_upper = pol_score_upper_adj, value_lower = pol_score_lower_adj) %>%
  select(term, year, topic, label, group, Adjustment, value, value_upper, value_lower)

results_df_party_raw_tagged <- results_df_party %>%
  mutate(group = "party", Adjustment = "Raw",
         value = pol_score, value_upper = pol_score_upper, value_lower = pol_score_lower) %>%
  select(term, year, topic, label, group, Adjustment, value, value_upper, value_lower)

results_df_party_adj_tagged <- results_df_party %>%
  mutate(group = "party", Adjustment = "Frequency-adjusted",
         value = pol_score_adj, value_upper = pol_score_upper_adj, value_lower = pol_score_lower_adj) %>%
  select(term, year, topic, label, group, Adjustment, value, value_upper, value_lower)

results_df_cat_adj <- assign_category(
  bind_rows(results_df_raw_tagged, results_df_adj_tagged,
            results_df_party_raw_tagged, results_df_party_adj_tagged)
)

## --- FigureJ4: overall trend, raw vs frequency-adjusted ---
results_df_cat_adj %>%
  group_by(year, group, Adjustment) %>%
  summarise(value_avg = mean(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = value_avg, color = Adjustment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam", se = FALSE) +
  geom_vline(data = president, aes(xintercept = xintercepts), linetype = "dotted") +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  pres_scale +
  facet_wrap(~ group) +
  theme_classic() +
  labs(x = "Year", y = "Politicization Score") +
  guides(color = guide_legend(title = ""))

ggsave("FigureJ4_politicization_overall_freqadj.png", width = 10, height = 5)

## --- Category-level early vs. late, raw vs adjusted ---
category_year_adj <- results_df_cat_adj %>%
  group_by(year, Category, group, Adjustment) %>%
  summarise(
    value_avg   = mean(value,       na.rm = TRUE),
    value_upper = mean(value_upper, na.rm = TRUE),
    value_lower = mean(value_lower, na.rm = TRUE),
    .groups = "drop"
  )

category_period_adj <- category_year_adj %>%
  group_by(Category, group, Adjustment) %>%
  summarise(
    late_value  = mean(value_avg[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_upper  = mean(value_upper[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_lower  = mean(value_lower[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(value_avg[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_upper = mean(value_upper[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_lower = mean(value_lower[year >= 1980 & year <= 1989], na.rm = TRUE),
    .groups = "drop"
  )

## --- FigureJ5: ideology, raw vs adjusted, faceted ---
category_period_adj %>%
  filter(group == "ideology") %>%
  ggplot(aes(y = reorder(Category, late_value))) +
  geom_point(aes(x = late_value, color = "2015-2024 Average")) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper, color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper, color = "1980-1989 Average"), width = 0.3) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  facet_wrap(~ Adjustment) +
  labs(x = "Politicization Score", y = "Issue Domains") +
  guides(color = guide_legend(title = "Period")) +
  theme_minimal(base_size = 11)

ggsave("FigureJ5_politicization_ideo_freqadjusted.png", width = 11, height = 7)

## --- FigureJ6: party, raw vs adjusted, faceted ---
category_period_adj %>%
  filter(group == "party") %>%
  ggplot(aes(y = reorder(Category, late_value))) +
  geom_point(aes(x = late_value, color = "2015-2024 Average")) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper, color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper, color = "1980-1989 Average"), width = 0.3) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  facet_wrap(~ Adjustment) +
  labs(x = "Politicization Score", y = "Issue Domains") +
  guides(color = guide_legend(title = "Period")) +
  theme_minimal(base_size = 11)

ggsave("FigureJ6_politicization_party_freqadjusted.png", width = 11, height = 7)