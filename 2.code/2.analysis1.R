# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Analysis senior official of the Ministry of Education         
# Author: 
# Date: 23rd October 2025
# Editied by: DB
# Last edit: 
#  
#      
# Content
#     1. Prep data for the analysis 
#         a. Create variables useful for description and for better comparability
#         b. Weighted national mean reading & math by year
#         c. Non-Weighted national mean reading & math by year
#     2. Graph
#         a. Group summaries by household income quantile (for the chart)
#         b. Transform data to long format
#         c. Plot: mean reading and math score by income quantile across years
#     3. Transform variables before regression and do a check on RURAL
#         a. Center key variables: attendance and dropout
#         b. save merged and centered dataset
#         c. Weighted national mean in test scores 
#         d. Check correlation between rural (binary 0/1) and numeric covariates
#         e. Check association with categorical covariates
#         f. visualize distribution of numeric covariates by rural
#         g. balance check for specific covariates
#     4. Regression
#         a. Simple OLS (student-level) with year fixed effects and cluster at school level 
#         b. Multilevel model with random intercept for school, district, and student-level variance
#         c. Save regression results
#         d. Prediction math and reading score given an increase in attendance due to the proposed intervention


# 1. Prep data for the analysis ------------------------------------------------
#______________________
# a. Create variables useful for description and for better comparability
#______________________
df_merged <- df_merged %>%
  mutate(
    reading_z = scale(reading_proficiency_score),
    math_z = scale(maths_proficiency_score),
    low_income = household_income_quintile %in% c(1,2),
    female = if_else(gender == "Female", 1, 0),
    rural = if_else(rural_residence == 1, 1, 0),
    disability = if_else(disability_status == "Yes", 1, 0),
    hh_agriculture = if_else(household_agric_activity == "Yes", 1, 0),
    electricity = if_else(electricity_access == "Yes", 1, 0),
    water = if_else(water_access == "Yes", 1, 0),
    internet = if_else(internet_access == "Yes", 1, 0)
  )

df_merged %>%
  group_by(year) %>%
  summarise(
    median_r = median(reading_proficiency_score, na.rm = TRUE),
    mean_r = mean(reading_proficiency_score, na.rm = TRUE),
    median_m = median(maths_proficiency_score, na.rm = TRUE),
    mean_m = mean(maths_proficiency_score, na.rm = TRUE)
    
  )
# check the density of reading and math scores to ascertain if the if multilevel models is plausible to use
ggplot(df_merged, aes(x = reading_proficiency_score, fill = factor(year))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~year, scales = "free") +
  labs(title = "Distribution of Reading Proficiency by Year",
       x = "Reading Proficiency Score", y = "Density") +
  theme_minimal()
ggplot(df_merged, aes(x = maths_proficiency_score, fill = factor(year))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~year, scales = "free") +
  labs(title = "Distribution of maths Proficiency by Year",
       x = "maths Proficiency Score", y = "Density") +
  theme_minimal()
#______________________
# b. Weighted national mean reading & math by year
#______________________
national_summary <- df_merged %>%
  dplyr::group_by(year) %>%
  mutate(reading_proficiency_score = as.numeric(reading_proficiency_score),
         maths_proficiency_score = as.numeric(maths_proficiency_score)) %>%
  dplyr::summarise(
    mean_reading = weighted.mean(reading_proficiency_score, w = weight, na.rm = TRUE),
    mean_math = weighted.mean(maths_proficiency_score, w = weight, na.rm = TRUE),
    n = n()
  )

#______________________
# c. Non-Weighted national mean reading & math by year
#______________________
national_summary1 <- df_merged %>%
  dplyr::group_by(year) %>%
  mutate(reading_proficiency_score = as.numeric(reading_proficiency_score),
         maths_proficiency_score = as.numeric(maths_proficiency_score)) %>%
  dplyr::summarise(
    mean_reading = mean(reading_proficiency_score, na.rm = TRUE),
    mean_math = mean(maths_proficiency_score, na.rm = TRUE),
    n = n()
  )

print(national_summary)
print(national_summary1)


# 2. Graph -------------------------------------------------------------------
#______________________
# a. Group summaries by household income quantile (for the chart)
#______________________
quintile_summary <- df_merged %>%
  filter(!is.na(household_income_quintile)) %>%
  dplyr::group_by(year, household_income_quintile) %>%
  dplyr::summarise(
    mean_reading = weighted.mean(reading_proficiency_score, w = weight, na.rm = TRUE),
    mean_math = weighted.mean(maths_proficiency_score, w = weight, na.rm = TRUE), 
    se_reading = if (n() > 1) sqrt(Hmisc::wtd.var(reading_proficiency_score, weight, na.rm = TRUE) / n()) else NA,
    n = n(),
    .groups = "drop"
  )

print(quintile_summary )

#______________________
# b. Transform data to long format
#______________________
quantile_long <- quintile_summary %>%
  pivot_longer(
    cols = c(mean_reading, mean_math),
    names_to = "subject",
    values_to = "mean_score"
  ) %>%
  mutate(
    subject = recode(subject,
                     "mean_reading" = "Reading",
                     "mean_math" = "Mathematics")
  )
summary(quintile_summary)

# Filter for lowest (1) and highest (5) quintiles
lowest_vs_highest <- quantile_long %>%
  filter(household_income_quintile %in% c(1, 5)) %>%
  select(year, subject, household_income_quintile, mean_score) %>%
  pivot_wider(
    names_from = household_income_quintile,
    values_from = mean_score,
    names_prefix = "quintile_"
  ) %>%
  mutate(
    difference = quintile_5 - quintile_1,
    pct_difference = (difference / quintile_5) * 100
  )
lowest_vs_highest

#______________________
# c. Plot: mean reading and math score by income quantile across years
#______________________
plot = ggplot(quantile_long,
       aes(x = factor(household_income_quintile),
           y = mean_score,
           color = subject,
           linetype = factor(year),
           group = interaction(subject, year))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  labs(
    x = "Household income quintile (1 = poorest, 5 = richest)",
    y = "Mean proficiency score",
    title = "Reading and Mathematics proficiency by household income quantile (2023–2025)",
    color = "Subject",
    linetype = "Year",
    caption = "Source: Nyanda education dataset"
  ) +
  theme_minimal(base_size = 12) +
  scale_y_continuous(labels = number_format(accuracy = 1)) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

print(plot)
summary(quantile_long)
ggsave(filename = "math_reading_score.png",plot = plot, path = out_graph_fld)

# Convert income quintile to numeric
quantile_long <- quantile_long %>%
  mutate(household_income_quintile = as.numeric(as.character(household_income_quintile)))

# Fit linear models for each subject and year to get the slope of the curve
slopes <- quantile_long %>%
  group_by(subject, year) %>%
  summarise(
    slope = coef(lm(mean_score ~ household_income_quintile))[2], # coefficient for x
    .groups = "drop"
  )

print(slopes)

# 3. Transform variables before regression and do a check on RURAL ------------------------------------------------------
#______________________
# a. Center key variables: attendance and dropout - in this way each student attendance is now expressed relative to the average attendance across all students
#______________________
# 
df_merged_centered <- df_merged %>%
  mutate(
    attendance_centered = attendance_rate - mean(attendance_rate, na.rm = TRUE),
    dropout_centered = dropout_risk_score - mean(dropout_risk_score, na.rm = TRUE), 
    dropout_centered100 = dropout_centered * 100 # since the coefficents for dropout_risk_score are between 0.01 and 0.32, I decided to rescale them for a meaningul interpretation. Now it represents the effect per 1 percentage point increase in dropout risk.
  )
# export data to excel
#______________________
# b. save merged and centered dataset
#______________________
wb_new <- createWorkbook()

# Add worksheet and write data
addWorksheet(wb_new, sheetName = "merged_centered")
writeData(wb_new, sheet = "merged_centered", df_merged_centered)

# Save the Excel file
saveWorkbook(wb_new, file.path(out_data_fld,"df_merged_centered.xlsx"), overwrite = TRUE)

#______________________
# c. Weighted national mean in test scores 
#______________________
# Weighted mean 
national_mean <- df_merged_centered %>%
  summarise(
    mean_reading = weighted.mean(reading_proficiency_score, w = weight, na.rm = TRUE),
    mean_math    = weighted.mean(maths_proficiency_score, w = weight, na.rm = TRUE),
    n_students   = n()
  )

print(national_mean)
#______________________
# d. Check correlation between rural (binary 0/1) and numeric covariates
#______________________
numeric_covariates <- df_merged_centered %>%
  select(attendance_centered, dropout_centered100, reading_z, math_z)

# Compute correlations
cor_rural_numeric <- rcorr(as.matrix(cbind(rural = df_merged_centered$rural, numeric_covariates)))
cor_rural_numeric$r      # correlation coefficients

#______________________
# e. Check association with categorical covariates
#______________________
categorical_covariates <- c("household_income_quintile", "female", "disability_status", "parent_education_level", "year","household_agric_activity")

for (cat_var in categorical_covariates) {
  print(cat_var)
  print(table(df_merged_centered[[cat_var]], df_merged_centered$rural))
  print(prop.table(table(df_merged_centered[[cat_var]], df_merged_centered$rural), 2))  # column percentages
}

#______________________
# f. visualize distribution of numeric covariates by rural
#______________________
df_merged_centered %>%
  select(rural, attendance_centered, dropout_centered100, reading_z, math_z) %>%
  pivot_longer(cols = -rural, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = factor(rural), y = value, fill = factor(rural))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Rural (0=Urban, 1=Rural)", y = "Value in SD", title = "Distribution of numeric covariates and test scores by rural/urban") +
  theme_minimal()

# Drop out rate (rural vs urban)
df_merged_centered <- df_merged_centered %>%
  mutate(dropout_binary = if_else(dropout_risk_score > 0.1, 1, 0))

dropout_summary <- df_merged_centered %>%
  group_by(rural) %>%
  summarise(
    dropout_rate = mean(dropout_binary, na.rm = TRUE) * 100,
    n_students = n()
  )

print(dropout_summary)

# disability
df_merged_centered %>%
  select(disability, reading_z, math_z) %>%
  pivot_longer(cols = -disability, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = factor(disability), y = value, fill = factor(disability))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Disability (0=No, 1=Yes)", y = "Value in SD", title = "Distribution of test scores by disability status") +
  theme_minimal()
  
# gender
df_merged_centered %>%
  select(female, reading_z, math_z) %>%
  pivot_longer(cols = -female, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = factor(female), y = value, fill = factor(female))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Gender (0=Male, 1=Female)", y = "Value in SD", title = "Distribution of test scores by female") +
  theme_minimal()

# agriculture
df_merged_centered %>%
  select(hh_agriculture, reading_z, math_z) %>%
  pivot_longer(cols = -hh_agriculture, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = factor(hh_agriculture), y = value, fill = factor(hh_agriculture))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Agriculture (0=No, 1=Yes)", y = "Value in SD", title = "Distribution of test scores by economic activity (agriculture)") +
  theme_minimal()

# water
df_merged_centered %>%
  select(water, reading_z, math_z) %>%
  pivot_longer(cols = -water, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = factor(water), y = value, fill = factor(water))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Water (0=No, 1=Yes)", y = "Value in SD", title = "Distribution of test scores school facilities (water)") +
  theme_minimal()

# electricity
df_merged_centered %>%
  select(electricity, reading_z, math_z) %>%
  pivot_longer(cols = -electricity, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = factor(electricity), y = value, fill = factor(electricity))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Electricity (0=No, 1=Yes)", y = "Value in SD", title = "Distribution of test scores school facilities (electricity)") +
  theme_minimal()
# internet
df_merged_centered %>%
  select(internet, reading_z, math_z) %>%
  pivot_longer(cols = -internet, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = factor(internet), y = value, fill = factor(internet))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Internet (0=No, 1=Yes)", y = "Value in SD", title = "Distribution of test scores school facilities (internet)") +
  theme_minimal()

#______________________
# g. balance check for specific covariates
#______________________
# balance check for specific covariates
# Prepare data: count by rural status
df_plot <- df_merged_centered %>%
  mutate(
    rural = factor(rural, labels = c("Urban", "Rural")),
    female = factor(female, labels = c("Male", "Female")),
    disability_status = factor(disability_status, labels = c("No", "Yes"))
  ) %>%
  pivot_longer(cols = c(female, disability_status), 
               names_to = "variable", values_to = "value") %>%
  group_by(rural, variable, value) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n))

# Plot counts (grouped bar chart)
ggplot(df_plot, aes(x = rural, y = n, fill = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~variable) +
  labs(
    x = "Residence",
    y = "Number of students",
    fill = "",
    title = "Distribution of Female and Disability Status by Rural/Urban"
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_brewer(palette = "Set2")

# 4. Regression --------------------------------------------------------------
#______________________
# a. Simple OLS (student-level) with year fixed effects and cluster at school level 
#______________________

# real score - reading 
ols_reading <- lm(reading_proficiency_score ~ factor(household_income_quintile) + female + rural +avg_injob_training_last5yrs+ disability_status + parent_education_level +attendance_centered   + factor(year), data = df_merged_centered, weights = df_merged_centered$weight)
summary(ols_reading)
# real score - math
ols_math <- lm(maths_proficiency_score ~ factor(household_income_quintile) + female + rural + avg_injob_training_last5yrs+disability_status + parent_education_level + attendance_centered  + factor(year), data = df_merged_centered, weights = df_merged_centered$weight)
summary(ols_reading)

# z-scores - reading - for comparability
z_ols_reading <- lm(reading_z ~ factor(household_income_quintile) + female + rural + disability_status + avg_injob_training_last5yrs+parent_education_level + attendance_centered  + factor(year), data = df_merged_centered, weights = df_merged_centered$weight)
summary(z_ols_reading)
# z-scores - math - for comparability
z_ols_math <- lm(math_z ~ factor(household_income_quintile) + female + rural + disability_status + parent_education_level + attendance_centered  + factor(year), data = df_merged_centered, weights = df_merged_centered$weight)
summary(z_ols_math)

#______________________
# b. Multilevel model with random intercept for school, district, and student-level variance
#______________________
# Running this multilevel model to account for:
# 1. school variance - differences between schools
# 2. district variance - differences between districts 
# 3. student-level variance - differences between students withing schools

summary(df_merged_centered$attendance_centered)

mixed_reading <- lmer(
  reading_z ~ factor(household_income_quintile) + female + rural +
    disability_status + parent_education_level + attendance_centered  +
    (1 | school_id) + (1 | district_id) + factor(year),
  data = df_merged_centered,
  weights = weight
)

summary(mixed_reading)

mixed_math <- lmer(
  math_z ~ factor(household_income_quintile) + female + rural +
    disability_status + parent_education_level + attendance_centered  +
    (1 | school_id) + (1 | district_id) + factor(year),
  data = df_merged_centered,
  weights = weight
)
summary(mixed_math)


#______________________
# c. Save regression results
#______________________
# Save regression results -------------------------------------------------

# List all models
models <- list(
  "OLS - Reading (z)" = z_ols_reading,
  "OLS - Math (z)" = z_ols_math,
  "Multilevel - Reading" = mixed_reading,
  "Multilevel - Math" = mixed_math
)

# Create and save summary table to Word
modelsummary(models,
             output = file.path(out_greg_fld,"model_results.docx"),
             title = "OLS and Multilevel Models for Reading and Math Scores",
             stars = TRUE,
             gof_map = c("nobs", "r.squared", "AIC", "BIC"))
#______________________
# d. Prediction math and reading score given an increase in attendance due to the proposed intervention
#______________________

sd_increase = 0.0
nbr_school_days = 190
# Simulate an increase of 0.3 for low-income students
df_sim <- df_merged_centered %>%
  mutate(attendance_centered = if_else(low_income == 1 & rural == 1, # low income represents households in the first and second quantale
                                       attendance_centered + sd_increase,  
                                       attendance_centered))

# Predict outcomes using the mixed model (fixed effects only)
df_sim$reading_pred <- predict(mixed_reading, newdata = df_sim, re.form = NA)
df_sim$math_pred <- predict(mixed_math, newdata = df_sim, re.form = NA)

# Aggregate predicted outcomes by income quintile
sim_summary <- df_sim %>%
  group_by(household_income_quintile) %>%
  summarise(mean_reading_pred = mean(reading_pred),
            mean_math_pred = mean(math_pred))

print(sim_summary)
# mean and standard deviation of attendance
mean(df_merged_centered$attendance_rate, na.rm = TRUE)
sd(df_merged_centered$attendance_rate, na.rm = TRUE)

cat("\n--------------------------------------\n")
cat("% increase in attendance :\n")
sd(df_merged_centered$attendance_rate, na.rm = TRUE)*sd_increase
cat("Increase in attendance days thanks to school feeding. Assuming 190 days of school:\n")
sd(df_merged_centered$attendance_rate, na.rm = TRUE)*sd_increase*nbr_school_days

cat("--------------------------------------\n")
