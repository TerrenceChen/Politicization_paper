# AppendixG_outlet_analysis.R
#
# Per-outlet (NYT/WaPo/WSJ) politicization comparison
#
# Requires:
#   ALC_dems/anchors.rds                     
#   ALC_dems/topic_<n>.rds                    
#   Results_Files/dict_terms_final_0825.rds
#   freq_by_year_source.rds                  

library(conText)
library(quanteda)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(glue)
library(lme4)
library(texreg)

anchor_dems <- readRDS("ALC_dems/anchors.rds")
dict_terms  <- readRDS("Results_Files/dict_terms_final_0825.rds")

subset_by_source <- function(term_data, source, year, reclassify_editorials = FALSE) {

  if (reclassify_editorials) {
    term_data$docvars$Type[str_detect(term_data$docvars$Type,
                                      "Commentary|Editorial|Review")] <- "Editorial"
    idx <- which(term_data$docvars$Source == source &
                 term_data$docvars$Year == year &
                 term_data$docvars$Type == "Editorial")
  } else {
      idx <- which(term_data$docvars$Source == source  &
               term_data$docvars$Year == year)
      }

  list(
    matrix = term_data$matrix[idx, , drop = FALSE],
    docvars = term_data$docvars[idx, ]
  )
}

# ===================== All Articles by Outlet =====================

cosine_calculate <- function(topic, source){

    results <- list()

    file_name <- paste0("ALC_dems/topic_", topic, ".rds")
    topic_dem <- readRDS(file_name)

    for (y in 1980:2024){
        cor_lib <- c()
        cor_con <- c()
        cor_dem <- c()
        cor_rep <- c()

        topic_list <- lapply(topic_dem, subset_by_source, source = source, year = y, reclassify_editorials = FALSE)
        anchor_list <- lapply(anchor_dems, subset_by_source, source = source, year = y, reclassify_editorials = FALSE)

        for(i in 1:length(topic_list)){
            if(length(topic_list[[i]]$matrix)==0) {
                cor_lib[i] <- NA
                cor_con[i] <- NA
                cor_dem[i] <- NA
                cor_rep[i] <- NA
            } else{
                cor_lib[i] <- lsa::cosine(colMeans(anchor_list$liberal$matrix), colMeans(topic_list[[i]]$matrix))
                cor_con[i] <- lsa::cosine(colMeans(anchor_list$conservative$matrix), colMeans(topic_list[[i]]$matrix))
                cor_dem[i] <- lsa::cosine(colMeans(anchor_list$democrat$matrix), colMeans(topic_list[[i]]$matrix))
                cor_rep[i] <- lsa::cosine(colMeans(anchor_list$republican$matrix), colMeans(topic_list[[i]]$matrix))
            }
        }

        results[[length(results) + 1]] <- data.frame(
            term = names(topic_list),
            year = as.integer(y),
            cos_lib = cor_lib,
            cos_con = cor_con,
            cos_dem = cor_dem,
            cos_rep = cor_rep
        )
    }

    results_df <- do.call(rbind, results)
    return(results_df)
}

nyt_cos <- lapply(unique(dict_terms$topic), cosine_calculate, source = "New York Times")
nyt_cos_df <-  do.call(rbind, nyt_cos)
saveRDS(nyt_cos_df, "Results_Files/nyt_cos_df.rds")

wp_cos <- lapply(unique(dict_terms$topic), cosine_calculate, source = "The Washington Post")
wp_cos_df <-  do.call(rbind, wp_cos)
saveRDS(wp_cos_df, "Results_Files/wp_cos_df.rds")

wsj_cos <- lapply(unique(dict_terms$topic), cosine_calculate, source = "Wall Street Journal")
wsj_cos_df <-  do.call(rbind, wsj_cos)
saveRDS(wsj_cos_df, "Results_Files/wsj_cos_df.rds")

# ===================== Combine =====================
nyt_cos_df$Source = "New York Times"
wp_cos_df$Source = "The Washington Post"
wsj_cos_df$Source = "Wall Street Journal"

all_df <- rbind(nyt_cos_df, wp_cos_df, wsj_cos_df) %>%
            left_join(dict_terms, by = "term") %>%
            filter(!is.na(label))

all_df_cat <-
    all_df %>%
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

all_df_cat <-
    all_df_cat %>%
    mutate(pol_score_ideo = (cos_lib + cos_con)/2,
           pol_score_party = (cos_dem + cos_rep)/2)

freq_by_year_source <- readRDS("freq_by_year_source.rds")
freq_by_year_source <- rename(freq_by_year_source, "year" = Year)
freq_by_year_source$year = as.numeric(freq_by_year_source$year)

all_df_cat <-
    all_df_cat %>%
    left_join(freq_by_year_source, by= c("term", "year", "Source"))

# --- Compare Increase: regression by outlet ---
results_df_cat_reg <-
   all_df_cat %>%
   filter(frequency >=5) %>%
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
       file = "outlet_models_ideo.tex")

modelP_nyt <- lme4::lmer(value_party_avg ~ year0 + (year0|label),
                         data=subset(results_df_cat_reg, Source == "New York Times"))
modelP_wp <- lme4::lmer(value_party_avg ~ year0 + (year0|label),
                         data=subset(results_df_cat_reg, Source == "The Washington Post"))
modelP_wsj <- lme4::lmer(value_party_avg ~ year0 + (year0|label),
                         data=subset(results_df_cat_reg, Source == "Wall Street Journal"))

texreg(list(modelP_nyt, modelP_wp, modelP_wsj), digits = 4,
       file = "outlet_models_party.tex")

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

ggsave("FigureG1_newspaper_ideo_cat.png", width = 6.5, height = 7)

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

ggsave("FigureG2_newspaper_party_cat.png", width = 6.5, height = 7)

message("AppendixG complete.")
