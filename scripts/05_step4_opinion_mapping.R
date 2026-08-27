# 05_step4_opinion_mapping.R
#
# Merging ANES-derived opinion-sorting scores
# with politicization scores and producing comparison figures.
#
# Requires:
#   results_with_domains.rds          (ANES-derived opinion sorting scores)
#   Results_Files/results_df_label_combined.rds
#   Results_Files/dict_terms_final_0825.rds

library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(gridExtra)

sorting_df <- readRDS("input_files/results_with_domains.rds")
results_df <- readRDS("Results_Files/results_df_label_combined.rds")
dict_terms <- readRDS("Results_Files/dict_terms_final_0825.rds")

sorting_df_new <-
    sorting_df %>%
    mutate(label = case_when(issue == "abortion4RC" ~ "Abortion",
                             issue %in% c("affirmact", "blacks_deservemore", "blacks_fair", "aid2blacks",
                                          "blacks_specfav", "school_integ", "hard4blacks", "blacks_shldTry",
                                          "civrights2fast", "blacks_poschange") ~ "Race",
                             issue == "healthinsur" ~ "Healthcare",
                             issue %in% c("gays_adoptkids", "gayer_military", "protect_homo") ~ "LGBTQ",
                             issue %in% c("fedspend_envi", "envi_reg") ~ "Environment",
                             issue %in% c("fedspend_poor", "fedspend_homeless",
                                          "fedspend_welfare", "fedspend_foodstamp") ~ "Welfare & Poverty",
                             issue == "defense_spend" ~ "Military",
                             issue == "immigration_increase" ~ "Immigration",
                             issue == "fedspend_schools" ~ "Education",
                             issue %in% c("school_prayer", "bible_authRC") ~ "Religion",
                             issue == "fedspend_crimeRC" ~ "Crime",
                             issue == "gov_jobguar" ~ "Workers",
                             issue == "women_equals" ~ "Gender",
                             issue == "tax_wasteRC" ~ "Taxation",
                             issue == "fedspend_space" ~ "Space & Science",
                             TRUE ~ "Other")) %>%
      filter(label != "Other")

sorting_df_summary <-
    sorting_df_new %>%
    group_by(label, year) %>%
    summarise(cor_avg_ideo = mean(cor_with_ideo, na.rm=TRUE),
              cor_avg_party = mean(cor_with_party, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(cor_with_ideo_scaled = scale(cor_avg_ideo),
             cor_with_party_scaled = scale(cor_avg_party))

results_df_final <-
   results_df %>%
   mutate(label = ifelse(label %in% c("ClimateChange", "Air Pollution", "Wildfires", "Disaster"), "Environment", label),
          label = ifelse(label %in% c("Racial", "Minority"), "Race", label),
          label = ifelse(label %in% c("CriminalJustice", "Police"), "Crime", label),
          label = ifelse(label %in% c("Catholicism", "Religion"), "Religion", label),
          label = ifelse(label %in% c("Space", "Science"), "Space & Science", label)) %>%
   filter(label %in% unique(sorting_df_summary$label)) %>%
   group_by(year, label, group) %>%
   summarise(value_avg = mean(value_avg, na.rm=TRUE)) %>%
   ungroup() %>%
   group_by(group) %>%
   mutate(value_avg_scaled = scale(value_avg)) %>%
   ungroup()

combined <-
   results_df_final %>%
    pivot_wider(names_from = group, values_from = c("value_avg", "value_avg_scaled")) %>%
   left_join(sorting_df_summary, by = c("label", "year"))

# --- Overall trend ---
combined_all_wide <-
    combined %>%
    group_by(year) %>%
    summarise(
        year_pol_ideo  = mean(value_avg_scaled_ideology, na.rm = TRUE),
        year_sort_ideo = mean(cor_with_ideo_scaled, na.rm = TRUE),
        year_pol_party  = mean(value_avg_scaled_party, na.rm = TRUE),
        year_sort_party = mean(cor_with_party_scaled, na.rm = TRUE)
    ) %>%
    ungroup()

cor_ideo  <- round(cor(combined_all_wide$year_pol_ideo,  combined_all_wide$year_sort_ideo,  use = "complete.obs"), 3)
cor_party <- round(cor(combined_all_wide$year_pol_party, combined_all_wide$year_sort_party, use = "complete.obs"), 3)

long_ideo <- combined_all_wide %>%
    pivot_longer(cols = c(year_pol_ideo, year_sort_ideo),
                 names_to = "group", values_to = "value") %>%
    mutate(group = recode(group,
        "year_pol_ideo"  = "Politicization (discursive)",
        "year_sort_ideo" = "Ideological sorting"))

long_party <- combined_all_wide %>%
    pivot_longer(cols = c(year_pol_party, year_sort_party),
                 names_to = "group", values_to = "value") %>%
    mutate(group = recode(group,
        "year_pol_party"  = "Politicization (discursive)",
        "year_sort_party" = "Partisan sorting"))

p_ideo <- long_ideo %>%
    ggplot(aes(x = year, y = value, color = group)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_smooth(method = "gam", se = TRUE) +
    annotate("text", x = Inf, y = Inf,
             label = paste0("r = ", cor_ideo),
             hjust = 1.1, vjust = 1.5, size = 3.5, color = "gray30") +
    scale_color_manual(values = c(
        "Politicization (discursive)" = "firebrick",
        "Ideological sorting"         = "steelblue")) +
    labs(title = "Ideology", y = "Mean z-score", x = "Year", color = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")

p_party <- long_party %>%
    ggplot(aes(x = year, y = value, color = group)) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_smooth(method = "gam", se = TRUE) +
    annotate("text", x = Inf, y = Inf,
             label = paste0("r = ", cor_party),
             hjust = 1.1, vjust = 1.5, size = 3.5, color = "gray30") +
    scale_color_manual(values = c(
        "Politicization (discursive)" = "firebrick",
        "Partisan sorting"            = "steelblue")) +
    labs(title = "Party", y = "Mean z-score", x = "Year", color = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")

g <- arrangeGrob(p_ideo, p_party, ncol = 2)
ggsave("overall_trend.png", g, width = 8, height = 8.5, dpi = 300)

# --- Trends by issue: ideology ---
combined_ideo <- combined %>%
  group_by(label) %>%
  arrange(year) %>%
  mutate(
    first_common_year = min(year[!is.na(value_avg_ideology) & !is.na(cor_avg_ideo)]),
    last_common_year  = max(year[!is.na(value_avg_ideology) & !is.na(cor_avg_ideo)]),
    in_common_window = year >= first_common_year & year <= last_common_year,
    cosine_base  = value_avg_scaled_ideology[year == first_common_year],
    sorting_base = cor_with_ideo_scaled[year == first_common_year],
    cosine_rebase  = value_avg_scaled_ideology - cosine_base,
    sorting_rebase = cor_with_ideo_scaled - sorting_base
  ) %>%
  ungroup() %>%
  mutate(year0 = year - 1980)

combined_ideo %>%
  filter(label == "LGBTQ")

slopes_pol <- combined_ideo %>%
  filter(!is.na(cosine_rebase)) %>%
  group_by(label) %>%
  filter(year >= first_common_year & year <= last_common_year) %>%
  do(tidy(lm(cosine_rebase ~ year, data = .))) %>%
  filter(term == "year") %>%
  select(label, slope_pol = estimate, se_pol = std.error)

slopes_sort <- combined_ideo %>%
  filter(!is.na(sorting_rebase)) %>%
  group_by(label) %>%
  filter(year >= first_common_year & year <= last_common_year) %>%
  do(tidy(lm(sorting_rebase ~ year, data = .))) %>%
  filter(term == "year") %>%
  select(label, slope_sort = estimate, se_sort = std.error)

slopes <- left_join(slopes_pol, slopes_sort, by = "label") %>%
  mutate(slope_diff = slope_pol - slope_sort) %>%
  arrange(desc(slope_diff))

combined_ideo %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = cosine_rebase, color = "Politicization (discursive)"),
            linewidth = 0.8, alpha = 0.4, linetype = "longdash") +
  geom_line(data = filter(combined_ideo, in_common_window),
            aes(y = cosine_rebase,
                color = "Politicization (discursive)"),
            linewidth = 0.8) +
  geom_line(data = filter(combined_ideo, in_common_window & !is.na(sorting_rebase)),
            aes(y = sorting_rebase,
                color = "Ideological sorting"),
            linewidth = 0.8) +
  geom_text(data = slopes,
            aes(label=paste("Sorting=", round(slope_sort, digits = 3))), x = 1990, y = 2, size = 2) +
  geom_text(data = slopes,
            aes(label=paste("Politicization=", round(slope_pol, digits = 3))), x = 1990, y =3, size = 2) +
  facet_wrap(~factor(label, levels = slopes$label), ncol=3) +
  scale_color_manual(values = c("Politicization (discursive)" = "firebrick",
                                 "Ideological sorting" = "steelblue")) +
  labs(title = "Discursive Ideological Politicization vs. Ideological Sorting by Issue",
       y = "Standardized value", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("Figure9_politicization_comapre_ideo.png", width = 6.5, height =7)

# --- Trends by issue: party ---
combined_party <- combined %>%
  group_by(label) %>%
  arrange(year) %>%
  mutate(
    first_common_year = min(year[!is.na(value_avg_party) & !is.na(cor_avg_party)]),
    last_common_year  = max(year[!is.na(value_avg_party) & !is.na(cor_avg_party)]),
    in_common_window = year >= first_common_year & year <= last_common_year,
    cosine_base  = value_avg_scaled_party[year == first_common_year],
    sorting_base = cor_with_party_scaled[year == first_common_year],
    cosine_rebase  = value_avg_scaled_party - cosine_base,
    sorting_rebase = cor_with_party_scaled - sorting_base
  ) %>%
  ungroup()

slopes_pol <- combined_party %>%
  filter(!is.na(cosine_rebase)) %>%
  group_by(label) %>%
  filter(year >= first_common_year & year <= last_common_year) %>%
  do(tidy(lm(cosine_rebase ~ year, data = .))) %>%
  filter(term == "year") %>%
  select(label, slope_pol = estimate, se_pol = std.error)

slopes_sort <- combined_party %>%
  filter(!is.na(sorting_rebase)) %>%
  group_by(label) %>%
  filter(year >= first_common_year & year <= last_common_year) %>%
  do(tidy(lm(sorting_rebase ~ year, data = .))) %>%
  filter(term == "year") %>%
  select(label, slope_sort = estimate, se_sort = std.error)

slopes <- left_join(slopes_pol, slopes_sort, by = "label") %>%
  mutate(slope_diff = slope_pol - slope_sort) %>%
  arrange(desc(slope_diff))

combined_party %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = cosine_rebase, color = "Politicization (discursive)"),
            linewidth = 0.8, alpha = 0.4, linetype = "longdash") +
  geom_line(data = filter(combined_party, in_common_window),
            aes(y = cosine_rebase,
                color = "Politicization (discursive)"),
            linewidth = 0.8) +
  geom_line(data = filter(combined_party, in_common_window & !is.na(sorting_rebase)),
            aes(y = sorting_rebase,
                color = "Partisan sorting"),
            linewidth = 0.8) +
  geom_text(data = slopes,
            aes(label=paste("Sorting=", round(slope_sort, digits = 3))), x = 1990, y = 2, size = 2) +
  geom_text(data = slopes,
            aes(label=paste("Politicization=", round(slope_pol, digits = 3))), x = 1990, y =3, size = 2) +
  facet_wrap(~factor(label, levels = slopes$label), ncol=3) +
  scale_color_manual(values = c("Politicization (discursive)" = "firebrick",
                                 "Partisan sorting" = "steelblue")) +
  labs(title = "Discursive Partisan Politicization vs. Partisan Sorting by Issue",
       y = "Standardized value", color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("Figure10_politicization_comapre_party.png", width = 6.5, height =7)
