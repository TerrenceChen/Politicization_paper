# 04_analysis_and_figures.R
#
# Everything after the two ALC embedding loops: term frequency counts,
# combining per-topic checkpoint files from the job arrays, computing
# politicization scores, regression models, and Figure 1-8 and H1.
#
# Requires: all_toks_ngram_0412.rds, Results_Files/dict_terms_final_0825.rds,
# and the completed ALC_results_topic/ and ALC_results_party/ directories.

library(conText)
library(quanteda)
library(dplyr)
library(ggplot2)
library(glue)
library(tidyr)
library(stringr)

all_toks_final <- readRDS("all_toks_ngram_0412.rds")
dict_terms <- readRDS("Results_Files/dict_terms_final_0825.rds")

dfm <-
    dfm(all_toks_final) %>%
    dfm_trim(min_termfreq = 500)

dfm_year <- dfm_group(dfm, 
                      groups = interaction(docvars(dfm, c("Year", "Source")), drop = TRUE))

freq_by_year <- dfm_year %>%
  dfm_select(pattern = dict_terms$term) %>%
  convert(to = "data.frame") %>%
  pivot_longer(
    cols = -doc_id,
    names_to = "term",
    values_to = "frequency"
  )

freq_by_year <-
  freq_by_year %>%
  mutate(
    Year = str_extract(doc_id, "\\d{4}"),
    Source = str_extract(doc_id, "(?<=\\.).*$")
  )

saveRDS(freq_by_year, "freq_by_year_source.rds")


# ======================================================================
# ### Merge
# ======================================================================
freq_by_year <- readRDS("freq_by_year_source.rds")

freq_by_year <- 
  freq_by_year %>%
  group_by(term, Year) %>%
  summarise(frequency = sum(frequency, na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(frequency)

freq_by_year <- rename(freq_by_year, "year" = Year)

results_df <- do.call(rbind, 
  lapply(list.files("ALC_results_topic", pattern = "topic_.*_result.rds",
                    full.names = TRUE), readRDS))

results_df <-
    results_df %>%
    left_join(dict_terms, by = "term") %>%
    filter(!is.na(label))

freq_by_year$year = as.numeric(freq_by_year$year)

results_df <-
    results_df %>%
    left_join(freq_by_year, by = c("term", "year"))

saveRDS(results_df, "Results_Files/ALC_ideology_results_df.rds")

results_df_party <- do.call(rbind, 
  lapply(list.files("ALC_results_party", pattern = "topic_.*_result.rds",
                    full.names = TRUE), readRDS))

results_df_party <-
    results_df_party %>%
    left_join(dict_terms, by = "term") %>%
    filter(!is.na(label))

results_df_party <-
    results_df_party %>%
    left_join(freq_by_year, by = c("term", "year"))

saveRDS(results_df_party, "Results_Files/ALC_party_results_df.rds")


# ======================================================================
# ### Politicization score
# ======================================================================
results_df <- readRDS("Results_Files/ALC_ideology_results_df.rds")
results_df_party <- readRDS("Results_Files/ALC_party_results_df.rds")

# Create politicization score
results_df <- results_df %>%
    filter(frequency >= 10) %>%   ## appear at least 10 times in any given year
    mutate(pol_score = (cos_liberal_mean + cos_con_mean)/2,
           pol_score_upper = (cos_liberal_upper + cos_con_upper)/2,
           pol_score_lower = (cos_liberal_lower + cos_con_lower)/2) %>%
    mutate(group = "ideology") %>%
    select("term", "year", "topic", "label", "pol_score", "pol_score_upper", "pol_score_lower", "group")

results_df_party <- results_df_party %>%
    filter(frequency >= 10) %>%
    mutate(pol_score = (cos_dem_mean + cos_rep_mean)/2,
           pol_score_upper = (cos_dem_upper + cos_rep_upper)/2,
           pol_score_lower = (cos_dem_lower + cos_rep_lower)/2) %>%
    mutate(group = "party") %>%
    select("term", "year", "topic", "label", "pol_score", "pol_score_upper", "pol_score_lower", "group")


# ======================================================================
# ### Create bigger categories
# ======================================================================
results_df_cat <- 
    rbind(results_df, results_df_party) %>%
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


# --- Dictionary appendix (LaTeX term list by category) ---
topic_words <-
  results_df_cat %>%
  distinct(term, label, Category)

topic_words <-
  topic_words %>%
  group_by(label, Category) %>%
  summarise(all_terms = paste0(term, collapse = ", ")) %>%
  ungroup() %>%
  arrange(Category)

latex_output <- c()

for(cat in unique(topic_words$Category)) {
  
  latex_output <- c(
    latex_output,
    glue("\\subsubsection*{{{cat}}}"),
    ""
  )
  
  sub <- filter(topic_words, Category == cat)
  
  for(i in 1:nrow(sub)) {
    latex_output <- c(
      latex_output,
      glue("\\textbf{{{sub$label[i]}}}: {sub$all_terms[i]}"),
      ""
    )
  }
}

writeLines(latex_output, "dictionary_appendix.txt")


# ======================================================================
# ### Face validity: vaccine trend
# ======================================================================

results_df_cat %>%
  filter(term == "vaccine") %>%
  print(n=45)

results_df_cat %>%
  filter(term == "vaccine") %>%
  ggplot(aes(x= year, y= pol_score, color = group)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(
    breaks = c(1981, 1989, 1993, 2001, 2009, 2017, 2021),
    labels = c("1981\nReagan", "1989\nBush", "1993\nClinton",
               "2001\nBush", "2009\nObama", "2017\nTrump", "2021\nBiden")) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  theme_classic() +
  labs(x = "Year",
       y = "Politicization Score") +
  guides(color = guide_legend(title = "")) +
  theme(plot.title = element_text(size= 20),
        plot.subtitle=element_text(size=18),
        strip.text = element_text(size = 15))

ggsave("Graphs/Figure1_vaccine_graph.png", width=6.5, height = 7)

# ======================================================================
# ### Overall Graph
# ======================================================================
results_df_cat %>%
    group_by(year, group) %>%
    summarise(value_avg = mean(pol_score, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(value_avg)

president <- data.frame(xintercepts = c(1981, 1989, 1993, 2001, 2009, 2017, 2021))

results_df_cat %>%
    group_by(year, group) %>%
    summarise(value_avg = mean(pol_score, na.rm=TRUE)) %>%
    ungroup() %>%
    ggplot(aes(x= year, y=value_avg, color = group)) +
    geom_point() +
    geom_smooth(method = "gam", se=FALSE) +
    geom_vline(data = president, aes(xintercept = xintercepts), linetype = "dotted") +
    scale_x_continuous(
        breaks = c(1981, 1989, 1993, 2001, 2009, 2017, 2021),
        labels = c("1981\nReagan", "1989\nBush", "1993\nClinton",
                   "2001\nBush", "2009\nObama", "2017\nTrump", "2021\nBiden")) +
    scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
    theme_classic() +
    labs(x = "Year",
         y = "Politicization Score") + 
    guides(color = guide_legend(title = "")) +
    theme(plot.title = element_text(size= 20), 
          plot.subtitle=element_text(size=18), 
          strip.text = element_text(size = 15))

ggsave("Graphs/Figure2_26-08-26-politicization_overall.png", width=6.5, height = 7)


# ======================================================================
# ### Regression
# ======================================================================
results_df_cat_reg <-
   results_df_cat %>%
   group_by(year, label, Category, group) %>%
   summarise(
    value_avg   = mean(pol_score,   na.rm = TRUE),
    value_upper = mean(pol_score_upper, na.rm = TRUE),
    value_lower = mean(pol_score_lower, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year0 = year - 1980)

saveRDS(results_df_cat_reg, "Results_Files/results_df_label_combined.rds")

library(lme4)
model_ideo <- lme4::lmer(value_avg ~ year0 + (year0|label), data=subset(results_df_cat_reg, group == "ideology"))
model_party <- lme4::lmer(value_avg ~ year0 + (year0|label), data=subset(results_df_cat_reg, group == "party"))

library(texreg)
texreg(list(model_ideo, model_party), digits = 4,
       file = "Tables/26-08-26-models.tex")

## Add a quadratic year term and compare to the linear model above
model_ideo_quad  <- lme4::lmer(value_avg ~ year0 + I(year0^2) + (year0|label),
                                data = subset(results_df_cat_reg, group == "ideology"))
model_party_quad <- lme4::lmer(value_avg ~ year0 + I(year0^2) + (year0|label),
                                data = subset(results_df_cat_reg, group == "party"))

texreg(list(model_ideo_quad, model_party_quad), digits = 4,
       file = "Tables/26-08-26-models_quadratic.tex")

category_year <- results_df_cat %>%
  group_by(year, Category, group) %>%
  summarise(
    value_avg   = mean(pol_score,   na.rm = TRUE),
    value_upper = mean(pol_score_upper, na.rm = TRUE),
    value_lower = mean(pol_score_lower, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year0 = year - 1980)


# ======================================================================
# ### Graph - scope
# ======================================================================
results_df_cat_reg <-
    results_df_cat_reg %>%
    group_by(label, group) %>%
    mutate(high_pol = case_when(mean(value_avg[year >= 1980 & year <= 1989], na.rm = TRUE) >= 0.2 ~ "High (≥0.2)",
                                mean(value_avg[year >= 1980 & year <= 1989], na.rm = TRUE) <= 0.15 ~ "Low (≤0.15)",
                                TRUE ~ "Medium (0.15-0.2)")) %>%
    ungroup() 

results_df_cat_reg %>%
    filter(year >= 1990) %>%
    group_by(high_pol, year, group) %>%
    summarise(value_avg = mean(value_avg, na.rm=TRUE)) %>%
    ungroup() %>%
    ggplot(aes(x=year, y=value_avg, color = high_pol, linetype = group)) +
    geom_smooth(method = "gam", se=FALSE) +
    scale_x_continuous(
        breaks = c(1989, 1993, 2001, 2009, 2017, 2021),
        labels = c("1989\nBush", "1993\nClinton",
                   "2001\nBush", "2009\nObama", "2017\nTrump", "2021\nBiden")) +
    theme_classic() +
    scale_color_manual(values = c("#566700", "#B6DB00", "#4B75FF")) +
    labs(x = "Year",
         y = "Politicization Score") + 
    guides(linetype = guide_legend(title = "Political label"),
           color = guide_legend(title = "Politicization in the 1980s")) +
    theme(plot.title = element_text(size= 20), 
          plot.subtitle=element_text(size=18))

ggsave("Graphs/Figure3_politicization_overall_strat_new.png", width = 6.5, height = 7)


# ======================================================================
# ### Graph - Most and Least Politicized
# ======================================================================
polscore_cat_rawvalues <- 
  category_year %>%
  group_by(Category, group) %>%
  summarise(
    early_value  = mean(value_avg[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_upper  = mean(value_upper[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_lower  = mean(value_lower[year >= 1980 & year <= 1989], na.rm = TRUE),
    late_value  = mean(value_avg[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_upper  = mean(value_upper[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_lower  = mean(value_lower[year >= 2015 & year <= 2024], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(late_value))

write.csv(polscore_cat_rawvalues, "Results_Files/polscore_cat_rawvalues.csv")

order_ideo <-
    polscore_cat_rawvalues %>% 
    filter(group == "ideology") %>%
    arrange(desc(late_value))

category_year %>%
  group_by(Category) %>%
  filter(group == "ideology") %>%
  summarise(
    late_value  = mean(value_avg[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(value_avg[year >= 1980 & year <= 1989], na.rm = TRUE),
    late_upper  = mean(value_upper[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_lower  = mean(value_lower[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_upper = mean(value_upper[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_lower = mean(value_lower[year >= 1980 & year <= 1989], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(early_rank = min_rank(desc(early_value)),
         late_rank = min_rank(desc(late_value))) %>%
  ggplot(aes(y = reorder(Category, late_value))) +
  geom_point(aes(x = late_value, color = "2015-2024 Average"),
             position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper,
                    color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper,
                    color = "1980-1989 Average"), width = 0.3) +
  scale_alpha_manual(values = c(0.5, 1.0)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  labs(x = "Politicization Score",
       y = "Issue Domains") +
  guides(color = guide_legend(title = "Period")) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(size= 20), 
          plot.subtitle=element_text(size=18), 
          strip.text = element_text(size = 15))

ggsave("Graphs/Figure4_politicization_cat_period_ideo.png", width =6.5, height = 7)

category_year %>%
  group_by(Category) %>%
  filter(group == "party") %>%
  summarise(
    late_value  = mean(value_avg[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(value_avg[year >= 1980 & year <= 1989], na.rm = TRUE),
    late_upper  = mean(value_upper[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_lower  = mean(value_lower[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_upper = mean(value_upper[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_lower = mean(value_lower[year >= 1980 & year <= 1989], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(early_rank = min_rank(desc(early_value)),
         late_rank = min_rank(desc(late_value))) %>%
  ggplot(aes(y = reorder(Category, late_value))) +
  geom_point(aes(x = late_value, color = "2015-2024 Average"),
             position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper,
                    color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper,
                    color = "1980-1989 Average"), width = 0.3) +
  scale_alpha_manual(values = c(0.5, 1.0)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  labs(x = "Politicization Score",
       y = "Issue Domains") +
  guides(color = guide_legend(title = "Period")) +
  theme_minimal() +
  theme(plot.title = element_text(size= 20), 
          plot.subtitle=element_text(size=18), 
          strip.text = element_text(size = 15))

ggsave("Graphs/Figure5_politicization_cat_period_party.png", width = 6.5, height = 7)


# ======================================================================
# ### Graph - Change over Time (Not used in paper)
# ======================================================================
## Correlation between two trends

cor_results <- category_year %>%
  select(year, Category, group, value_avg) %>%
  pivot_wider(
    names_from = group,
    values_from = value_avg
  ) %>%
  group_by(Category) %>%
  summarise(
    correlation = cor(ideology, party, use = "complete.obs"),
    n_years = sum(complete.cases(ideology, party))
  )

cor_results %>%
    arrange(correlation)

### Mixed effects model

model_cat_1 <- lme4::lmer(value_avg ~ year0 + (year0 | Category), 
                        data = category_year %>% filter(group == "ideology"))

model_cat_2 <- lme4::lmer(value_avg ~ year0 + (year0 | Category), 
                        data = category_year %>% filter(group == "party"))

### Extract category-specific slopes

category_intercept_1 <- coef(model_cat_1)$Category %>%
  tibble::rownames_to_column("Category") %>%
  arrange(desc(year0)) %>%
  rename("slope" = year0)  %>%
  filter(Category != "Other") %>%
  mutate(group = "ideology")

category_intercept_2 <- coef(model_cat_2)$Category %>%
  tibble::rownames_to_column("Category") %>%
  arrange(desc(year0)) %>%
  rename("slope" = year0)  %>%
  filter(Category != "Other") %>%
  mutate(group = "party")

## presidential inauguration year
president <- data.frame(xintercepts = c(1981, 1989, 1993, 2001, 2009, 2017, 2021))

category_year %>%
  ggplot(aes(x = year, y = value_avg, color = group)) +
  geom_point() +
  geom_line(aes(color=group)) +
  geom_vline(data = president, aes(xintercept = xintercepts), linetype = "dotted") +
  scale_x_continuous(
    breaks = c(1981, 1989, 1993, 2001, 2009, 2017, 2021),
    labels = c("1981\nReagan", "1989\nBush", "1993\nClinton",
               "2001\nBush", "2009\nObama", "2017\nTrump", "2021\nBiden")
  ) +
  theme_classic() +
  geom_text(data= category_intercept_1, 
            aes(label=paste("ideology slope=", format(round(slope, digits = 4), scientific = FALSE))),
            x = 1985, y = 0.42, size = 3) +
  geom_text(data= category_intercept_2, 
            aes(label=paste("party slope=", format(round(slope, digits = 4), scientific = FALSE))),
            x = 1985, y = 0.38, size = 3) +
  scale_color_manual(values = c("#D55E00", "#0072B2")) +
  facet_wrap(~factor(Category, category_intercept_1$Category), ncol = 5) +
  labs(y = "Politicization Score",
       color = "Political Term") +
  theme(
    strip.text    = element_text(size = 20),
    axis.text.y   = element_text(size = 15)
  )

ggsave("politicization_fulltrend.png", height =20, width = 24)


# ======================================================================
# ## Topic Level Graphs (Not used in paper)
# ======================================================================
graph_topic <- function(g){

results_graph <- 
  results_df_cat_reg %>%
  filter(!label %in% c("Party", "Campaigns & Donors", "Protest", "PublicOpinion", "Senate", "Elections")) %>%
  filter(group == g) %>%
  group_by(label) %>%
  summarise(
    late_value  = mean(value_avg[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(value_avg[year >= 1980 & year <= 1989], na.rm = TRUE),
    late_upper  = mean(value_upper[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_lower  = mean(value_lower[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_upper = mean(value_upper[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_lower = mean(value_lower[year >= 1980 & year <= 1989], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(diff = late_value - early_value)

bind_rows(
  results_graph %>% slice_max(order_by = diff, n = 15),
  results_graph %>% slice_min(order_by = diff, n = 15)
) %>%
  ggplot(aes(y = reorder(label, diff))) +
  geom_point(aes(x = late_value,  shape = "2015-2024 Average", color = "2015-2024 Average")) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper,
                    color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, shape = "1980-1989 Average", color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper,
                    color = "1980-1989 Average"), width = 0.3) +
  geom_hline(yintercept = 15.5, linetype = "dashed", color = "gray50", size =1) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  labs(x = "Politicization Score",
       y = "Topic") +
  guides(shape = guide_legend(title = ""),
         color = guide_legend(title = "")) +
  theme_minimal()
}

graph_topic("ideology")

graph_topic("party")


# ======================================================================
# ### Specific Issue Domain
# ======================================================================
results_df_cat_reg %>%
  filter(Category %in% c("Creative Arts", "Media", "Sports", "Food & Drinks")) %>%
  mutate(label = ifelse(label == "Dining", "Casual Dining", label)) %>%
  filter(group == "ideology") %>%
  group_by(label) %>%
  summarise(
    late_value  = mean(value_avg[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(value_avg[year >= 1980 & year <= 1989], na.rm = TRUE),
    late_upper  = mean(value_upper[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_lower  = mean(value_lower[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_upper = mean(value_upper[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_lower = mean(value_lower[year >= 1980 & year <= 1989], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(diff = late_value - early_value) %>%
  ggplot(aes(y = reorder(label, diff))) +
  geom_point(aes(x = late_value,  shape = "2015-2024 Average", color = "2015-2024 Average")) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper,
                    color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, shape = "1980-1989 Average", color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper,
                    color = "1980-1989 Average"), width = 0.3) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  labs(x = "Politicization Score",
       y = "Topic") +
  guides(shape = guide_legend(title = ""),
         color = guide_legend(title = "")) +
  theme_bw(base_size = 11)

ggsave("Graphs/Figure6_politicization_lifestyle_period.png", height = 6.5, width= 7)

results_df_cat_reg %>%
  filter(Category %in% c("Creative Arts", "Media", "Sports", "Food & Drinks")) %>%
  mutate(label = ifelse(label == "Dining", "Casual Dining", label)) %>%
  filter(group == "party") %>%
  group_by(label) %>%
  summarise(
    late_value  = mean(value_avg[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(value_avg[year >= 1980 & year <= 1989], na.rm = TRUE),
    late_upper  = mean(value_upper[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_lower  = mean(value_lower[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_upper = mean(value_upper[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_lower = mean(value_lower[year >= 1980 & year <= 1989], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(diff = late_value - early_value) %>%
  ggplot(aes(y = reorder(label, diff))) +
  geom_point(aes(x = late_value,  shape = "2015-2024 Average", color = "2015-2024 Average")) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper,
                    color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, shape = "1980-1989 Average", color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper,
                    color = "1980-1989 Average"), width = 0.3) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  labs(x = "Politicization Score",
       y = "Topic") +
  guides(shape = guide_legend(title = ""),
         color = guide_legend(title = "")) +
  theme_bw(base_size = 11)

ggsave("Graphs/Figure7_politicization_lifestyle_period_party.png", height = 6.5, width= 7)


### Detailed time trends (not used in paper)

graph_topic_full <- function(cat){

model_lab_1 <- lme4::lmer(value_avg ~ year0 + (year0 | label), 
                        data = results_df_cat_reg  %>% filter(group == "ideology" & Category %in% cat))

model_lab_2 <- lme4::lmer(value_avg ~ year0 + (year0 | label), 
                        data = results_df_cat_reg  %>% filter(group == "party" & Category %in% cat))

label_intercept_1 <- coef(model_lab_1)$label %>%
  tibble::rownames_to_column("label") %>%
  arrange(desc(year0)) %>%
  rename("slope" = year0)  %>%
  mutate(group = "ideology")

label_intercept_2 <- coef(model_lab_2)$label %>%
  tibble::rownames_to_column("label") %>%
  arrange(desc(year0)) %>%
  rename("slope" = year0)  %>%
  mutate(group = "party")

president <- data.frame(xintercepts = c(1981, 1989, 1993, 2001, 2009, 2017, 2021))

results_df_cat_reg  %>% 
  filter(Category %in% cat) %>%
  ggplot(aes(x = year, y = value_avg, color = group)) +
  geom_point() +
  geom_line(aes(color=group)) +
  geom_vline(data = president, aes(xintercept = xintercepts), linetype = "dotted") +
  scale_x_continuous(
    breaks = c(1981, 1989, 1993, 2001, 2009, 2017, 2021),
    labels = c("1981\nReagan", "1989\nBush", "1993\nClinton",
               "2001\nBush", "2009\nObama", "2017\nTrump", "2021\nBiden")
  ) +
  theme_classic() +
  geom_text(data= label_intercept_1, 
            aes(label=paste("ideology slope=\n", format(round(slope, digits = 4), scientific = FALSE))),
            x = 1985, y = 0.25, size = 2.5) +
  geom_text(data= label_intercept_2, 
            aes(label=paste("party slope=\n", format(round(slope, digits = 4), scientific = FALSE))),
            x = 1985, y = 0.21, size = 2.5) +
  facet_wrap(~factor(label, label_intercept_1$label), ncol = 5) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  labs(y = "Politicization Score",
       color = "Political Term") +
  theme(
    plot.title    = element_text(size = 20),
    plot.subtitle = element_text(size = 18),
    strip.text    = element_text(size = 15)
  )

}

graph_topic_full(c("Creative Arts", "Media", "Sports", "Food & Drinks"))


# ======================================================================
# ### Specific Terms
# ======================================================================
results_df_cat %>%
 filter(term %in% c("cnn", "radio-stations", "late-night", "designer",
                    "hollywood", "disney",  "poems",
                    "mozart", "fine-arts", "picasso", "rapper", "jazz",
                    "nba", "nfl", "soccer-team", "nhl", 
                     "chicken", "liquors", "fast-food", "coca-cola")) %>%
 group_by(term, group, Category) %>%
  summarise(
    late_value  = mean(pol_score[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(pol_score[year >= 1980 & year <= 1989], na.rm = TRUE),
    late_upper  = mean(pol_score_upper[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_lower  = mean(pol_score_lower[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_upper = mean(pol_score_upper[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_lower = mean(pol_score_lower[year >= 1980 & year <= 1989], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(group == "ideology") %>%
  mutate(diff = late_value - early_value) %>%
  ggplot(aes(y = reorder(term, diff), shape = Category)) +
  geom_point(aes(x = late_value,  color = "2015-2024 Average")) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper, color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper, color = "1980-1989 Average"), width = 0.3) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  labs(x = "Politicization Score",
       y = "Term") +
  guides(shape = guide_legend(title = "Category"),
         color = guide_legend(title = "Period")) +
  theme_bw(base_size = 11)

ggsave("Graphs/Figure8_lifestyle_term.png", width = 6.5, height = 7)


## Appendix H: Select Top 5 Terms

lifestyle_label <- 
    results_df_cat %>% 
    filter(Category %in% c("Creative Arts", "Media", "Sports", "Food & Drinks")) %>%
    distinct(label) %>%
    pull(label)

top_terms <- 
  dict_terms %>%
  filter(label %in% lifestyle_label) %>%
  group_by(label) %>%
  slice_max(order_by = ratio, n = 1) %>%
  ungroup()

results_df_cat %>%
  filter(term %in% top_terms$term) %>%
  mutate(term_label = paste0(term, " (", label, ")")) %>%
  group_by(term_label, group, Category) %>%
  summarise(
    late_value  = mean(pol_score[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_value = mean(pol_score[year >= 1980 & year <= 1989], na.rm = TRUE),
    late_upper  = mean(pol_score_upper[year >= 2015 & year <= 2024], na.rm = TRUE),
    late_lower  = mean(pol_score_lower[year >= 2015 & year <= 2024], na.rm = TRUE),
    early_upper = mean(pol_score_upper[year >= 1980 & year <= 1989], na.rm = TRUE),
    early_lower = mean(pol_score_lower[year >= 1980 & year <= 1989], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(group == "ideology") %>%
  mutate(diff = late_value - early_value) %>%
  ggplot(aes(y = reorder(term_label, diff), shape = Category)) +
  geom_point(aes(x = late_value,  color = "2015-2024 Average")) +
  geom_errorbar(aes(xmin = late_lower, xmax = late_upper, color = "2015-2024 Average"), width = 0.3) +
  geom_point(aes(x = early_value, color = "1980-1989 Average")) +
  geom_errorbar(aes(xmin = early_lower, xmax = early_upper, color = "1980-1989 Average"), width = 0.3) +
  scale_color_manual(values = c("#FFC20A", "#0C7BDC")) +
  labs(x = "Politicization Score",
       y = "Term") +
  guides(shape = guide_legend(title = "Category"),
         color = guide_legend(title = "Period")) +
  theme_bw(base_size = 11)

ggsave("Graphs/FigureH1_lifestyle_terms_unique.png", width = 6.5, height = 7)