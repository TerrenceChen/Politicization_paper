########################################
#File: makeBaldGelData.R
#Desc: as in Baldassarri and Gelman AJS paper using ANES, 
#constructs data set of year-specific correlations
#between item pairs nested within issue
########################################

##########
##########      LIBRARIES
##########

library(dplyr)
library(data.table)
library(WGCNA)

##########
##########      RENAME VARIABLES
##########

#names of variables in original data and their new names by category
vec_ovars_core <- c("vcf0004", "vcf0803", "vcf0301")
vec_newnames_core <- c("year","ideoid","partyid")

vec_ovars_design <- c("vcf0017","vcf0009x", "vcf0009y", "vcf0009z")
vec_newnames_design <- c("mode","faceWT","webWT","pooledWT")

vec_ovars_econ <- c("vcf0839", "vcf0886", "vcf0809", "vcf0806", "vcf0887",
                  "vcf9050",  "vcf0893", "vcf0894", "vcf9046", "vcf0890",
                  "vcf9049", "vcf0889",  "vcf0891", "vcf9047")

vec_newnames_econ <- c("fedspend_govserv","fedspend_poor","gov_jobguar",
                     "healthinsur", "fedspend_childcare","fedspend_asstblacks",
                     "fedspend_homeless", "fedspend_welfare","fedspend_foodstamp",
                     "fedspend_schools", "fedspend_socsec","fedspend_aids",
                     "fedspend_college","fedspend_envi")

vec_ovars_civrights <- c("vcf9016", "vcf0867a", "vcf9018", "vcf9042", "vcf9037",
                       "vcf0830",  "vcf9014","vcf9040", "vcf9013","vcf9017",
                       "vcf0817", "vcf0816",  "vcf9039", "vcf9041", "vcf0814",
                       "vcf9015","vcf0813")


vec_newnames_civrights <- c("more_equal", "affirmact", "equal_treat",
                            "blacks_deservemore", "blacks_fair", "aid2blacks",
                            "rights2much", "blacks_specfav", "equal_opp",
                            "worryLessEqual", "school_bus", "school_integ",
                            "hard4blacks", "blacks_shldTry", "civrights2fast",
                            "chancesNEQ", "blacks_poschange")

vec_ovars_moral <- c("vcf0877", "vcf0876a", "vcf0852","vcf0854", "vcf0878",
                   "vcf0853", "vcf0851", "vcf0834", "vcf0837","vcf0838", "vcf9043")

vec_newnames_moral <- c("gayer_military", "protect_homo", "adjust_morals",
                      "tolerate_difvals", "gays_adoptkids", "tradvals",
                      "newLstylesBad", "women_equals", "abortion1", "abortion2",
                      "school_prayer")

vec_ovars_frgnp <- c("vcf0843", "vcf0811", "vcf0841", "vcf0892", "vcf0888","vcf9048")
vec_newnames_frgnp <- c("defense_spend","urban_unrest","ruskies","fedspend_frgnaid",
                      "fedspend_crime","fedspend_space")

vec_all_ovars <- c(vec_ovars_core, vec_ovars_design, vec_ovars_econ,
                   vec_ovars_civrights, vec_ovars_moral, vec_ovars_frgnp,
                   "vcf0879", "vcf0606", "vcf0842", "vcf0850")

vec_all_newnames <- c(vec_newnames_core, vec_newnames_design, vec_newnames_econ,
                      vec_newnames_civrights, vec_newnames_moral, vec_newnames_frgnp,
                      "immigration_increase", "tax_waste", "envi_reg", "bible_auth")

#final version of issue variables that will be used for correlations
#code dummies for issue areas after recodes
vec_econissues  <- c("fedspend_govservRC", "fedspend_poor", "gov_jobguar",
                   "healthinsur", "fedspend_childcare", "fedspend_asstblacks",
                   "fedspend_homeless", "fedspend_welfare", "fedspend_foodstamp",
                   "fedspend_schools", "fedspend_socsec", "fedspend_aids",
                   "fedspend_college", "fedspend_envi")

vec_civrissues <- c("more_equalRC", "affirmact", "equal_treat", "blacks_deservemore",
                    "blacks_fair", "aid2blacks", "rights2muchRC", "blacks_specfavRC",
                    "equal_opp", "worryLessEqualRC", "school_bus", "school_integ",
                    "hard4blacks", "blacks_shldTryRC", "civrights2fast", "chancesNEQ",
                    "blacks_poschange")

vec_moralissues <- c("gayer_military", "protect_homo", "adjust_morals",
                     "tolerate_difvals", "gays_adoptkids", "tradvalsRC",
                     "newLstylesBadRC", "women_equals", "abortion4RC",
                     "school_prayer")

vec_frgnpissues <- c("defense_spend","urban_unrest","ruskies","fedspend_frgnaid",
                   "fedspend_crimeRC","fedspend_space")

vec_issueVars <- c(vec_econissues, vec_civrissues, vec_moralissues, vec_frgnpissues,
                   "immigration_increase", "tax_wasteRC", "envi_reg", "bible_authRC")


##########
##########			MAIN CODE
##########

dt_orig <- read.csv("anes_timeseries_cdf_csv_20260205.csv") 
## Need to download from ANES website: https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/

names(dt_orig) <- tolower(names(dt_orig))


#the second condition excludes web respondents in 2016
dt_sub72 <- dt_orig[dt_orig$vcf0004 >= 1972, vec_all_ovars]

rm(dt_orig)

names(dt_sub72)[names(dt_sub72) %in% vec_all_ovars] <- vec_all_newnames

#convert numeric missing codes to NAs
vec_logrec <- !(vec_all_newnames %in% c("year", vec_newnames_design))
vec_rec1vars <- vec_all_newnames[vec_logrec]

#convert NA
f_conv2na <- function(p_var) {
	dt_sub72[[p_var]] <<- ifelse(dt_sub72[[p_var]] %in% 1:7,
                               yes=dt_sub72[[p_var]], no=NA)
}

invisible(lapply(vec_rec1vars, f_conv2na))

#additional recoding of variables where 7 is not a valid (or is questionable)
dt_sub72$affirmact <- ifelse(dt_sub72$affirmact %in% 1:5, dt_sub72$affirmact, NA)

f_conv2na_gt3 <- function(p_x){
  dt_sub72[[p_x]] <<- ifelse(dt_sub72[[p_x]] %in% 1:3, yes=dt_sub72[[p_x]], no=NA)
}

vec_naGT3 <- c("fedspend_asstblacks", "fedspend_foodstamp", "fedspend_socsec",
               "fedspend_space")
invisible(lapply(vec_naGT3, f_conv2na_gt3))


#reverse code that what needs reversing so that higher values 
#reflect more conservative attitudes
dt_sub72$fedspend_govservRC  <- -1 * dt_sub72$fedspend_govserv  + 8
dt_sub72$more_equalRC        <- -1 * dt_sub72$more_equal        + 6
dt_sub72$rights2muchRC       <- -1 * dt_sub72$rights2much       + 6
dt_sub72$blacks_specfavRC    <- -1 * dt_sub72$blacks_specfav    + 6
dt_sub72$worryLessEqualRC    <- -1 * dt_sub72$worryLessEqual    + 6
dt_sub72$blacks_shldTryRC    <- -1 * dt_sub72$blacks_shldTry    + 6
dt_sub72$tradvalsRC          <- -1 * dt_sub72$tradvals          + 6
dt_sub72$newLstylesBadRC     <- -1 * dt_sub72$newLstylesBad     + 6
dt_sub72$abortion1RC         <- -1 * dt_sub72$abortion1         + 5
dt_sub72$abortion2RC         <- -1 * dt_sub72$abortion2         + 5
dt_sub72$fedspend_crimeRC    <- -1 * dt_sub72$fedspend_crime    + 4
dt_sub72$tax_wasteRC         <- -1 * dt_sub72$tax_waste         + 4
dt_sub72$bible_authRC        <- -1 * dt_sub72$bible_auth        + 4

#new abortion variable combining original two
#abortion1 is from before 1980, abortion2 is after 1980
#but they overlapped in 1980 as two highly similar wordings
#two different combined versions: 
#FIRST (3RC) uses post-1980 version unless missing since it is the wording
#for most of the time series. If that's missing use the prior version
#SECOND (4RC) uses the pre-1980 version unless it's missing since Austin
#thinks it gets results closer to B&G's original
dt_sub72$abortion3RC <- ifelse(is.na(dt_sub72$abortion2RC), dt_sub72$abortion1RC, dt_sub72$abortion2RC)
dt_sub72$abortion4RC <- ifelse(is.na(dt_sub72$abortion1RC), dt_sub72$abortion2RC, dt_sub72$abortion1RC)

#remove old variables once recode done to avoid confusion
vec_vars2remove <- c("abortion2", "abortion1", "abortion2RC", "abortion1RC",
                     "newLstylesBad", "tradvals", "blacks_shldTry",
                     "worryLessEqual", "blacks_specfav", "more_equal",
                     "rights2much", "fedspend_govserv")
dt_sub72 <- dt_sub72[ , !(names(dt_sub72) %in% vec_vars2remove)]

dt_sub72 <- as.data.table(dt_sub72)



#### Correlation ####

vec_its2cor <- c("ideoid", "partyid", vec_issueVars)

years <- sort(unique(dt_sub72[mode == 0]$year))

results_list <- lapply(years, function(yr) {
  
  dt_year <- dt_sub72[year == yr & mode == 0]
  
  vec_weights <- dt_year$faceWT
  
  mat_cor <- WGCNA::cor(
    x = dt_year[, ..vec_its2cor],
    y = dt_year[, ..vec_its2cor],
    weights.x = vec_weights,
    weights.y = vec_weights,
    use = "pairwise.complete.obs"
  )
  
  # Extract only issue-vs-ideology pairs (upper triangle approach)
  ideo_idx <- which(colnames(mat_cor) == "ideoid")
  party_idx <- which(colnames(mat_cor) == "partyid")
  issue_idx <- which(colnames(mat_cor) %in% vec_issueVars)
  
  data.table(
    year    = yr,
    issue   = vec_issueVars,
    cor_with_ideo = mat_cor[issue_idx, ideo_idx],
    cor_with_party = mat_cor[issue_idx, party_idx]
  )
})

results <- rbindlist(results_list)


##########
##########      DOMAIN AVERAGING
##########

# Create a mapping table for issues to domains
domain_map <- rbind(
  data.table(issue = vec_econissues,  domain = "Economic"),
  data.table(issue = vec_civrissues,  domain = "Civil Rights"),
  data.table(issue = vec_moralissues, domain = "Moral"),
  data.table(issue = vec_frgnpissues, domain = "Foreign Policy")
)

# 2. Join the mapping to your results
results_with_domains <- merge(results, domain_map, by = "issue", all.x = TRUE)
saveRDS(results_with_domains, "results_with_domains.rds")

##########
##########      THE GAMM APPROACH
##########

library(gamm4)
library(ggplot2)

# 1. Prepare the data for the model
# The model requires domains and issues to be factors
dt_model_data <- results_with_domains[complete.cases(cor_with_ideo)]
dt_model_data[, `:=`(
  domain = as.factor(domain),
  issue = as.factor(issue) # This acts as our 'jointName'
)]

# 2. Fit the GAMM
# We model the correlation as a function of a smooth year term per domain,
# with a random intercept for each specific issue.
fit_ideo <- gamm4(
  cor_with_ideo ~ s(year, by = domain, bs = "tp", k = 10) + domain,
  random = ~(1 | issue), 
  data = dt_model_data
)

# We want to predict for every year in our range for each domain
plot_grid <- expand.grid(
  year = seq(min(dt_model_data$year), max(dt_model_data$year), length.out = 200),
  domain = unique(dt_model_data$domain)
)

preds <- predict(fit_ideo$gam, newdata = plot_grid, se.fit = TRUE)

# 3. Combine into a visualization data table
dt_viz <- data.table(
  year = plot_grid$year,
  domain = plot_grid$domain,
  fit = as.numeric(preds$fit),
  se.fit = as.numeric(preds$se.fit)
)

# 4. Calculate Confidence Interval bounds
dt_viz[, `:=`(
  upper = fit + (1.96 * se.fit),
  lower = fit - (1.96 * se.fit)
)]

dt_viz %>%
  filter(year <= 2016) %>%
  ggplot(aes(x = year, y = fit, color = domain, fill = domain)) +
  # Shaded Ribbon for 95% Confidence Interval
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.15, colour = NA) +
  # The smooth trend line
  geom_line(linewidth = 1) +
  # Aesthetics
  scale_x_continuous("Year", breaks = seq(1972, 2024, 10)) +
  coord_cartesian(ylim = c(0, 0.6)) +
  labs(
    title = "Smoothed Alignment: Issues and Ideology",
    subtitle = "GAMM predictions (1.96 * SE)",
    y = "Correlation Coefficient (r)",
    x = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")