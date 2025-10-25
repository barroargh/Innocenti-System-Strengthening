# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: Prep data script               
# Author:
# Date: 23rd October 2025

#  
#      
# Content
#     1. IMPORT THE DATA
#     2. INSPECT AND CLEAN THE DATA
#       a. Inspect
#       b. quality check
#       c. general cleaning
#     3. RESHAPE DATA TO LONG 
#       a. reshape student data
#       b. reshape schools data
#       c. reshape teachers data
#       d. reshape district data from wide to long
#     4. AGGREGATE TEACHER DATASET
#       a. Aggregate teacher-level to school-year level data
#     5. MERGE DATASETS
#       a. merge student -> school-year -> district

# 1. IMPORT THE DATA ------------------------------------------------------

dta_district = file.path(input_fld,"Districts.xlsx")
dta_schools = file.path(input_fld,"Schools.xlsx")
dta_std_sample = file.path(input_fld,"StudentsSample.xlsx")
dta_teachers = file.path(input_fld,"Teachers.xlsx")

df_district = read_excel(dta_district) 
df_schools = read_excel(dta_schools) 
df_std_sample = read_excel(dta_std_sample) 
df_teachers = read_excel(dta_teachers) 

# 2. INSPECT AND CLEAN THE DATA -------------------------------------------
#______________________
# a. Inspect
#______________________
# Create a named list of your datasets
dataset_list <- list(
  df_district = df_district,
  df_schools = df_schools,
  df_std_sample = df_std_sample,
  df_teachers = df_teachers
)

# Loop over the list
for (dataset_name in names(dataset_list)) {
  
  cat("\n-------------------------------\n")
  cat("📊 General overview of dataset:", dataset_name, "\n")
  cat("-------------------------------\n")
  
  dataset <- dataset_list[[dataset_name]]
  
  # Structure and summary
  # Glimpse for compact view
  cat("-------------------------------\n")
  cat("Glimpse of the dataset\n")
  cat("-------------------------------\n")
  #glimpse(dataset)
  print(names(dataset))
  # Count missing values
  cat("-------------------------------\n")
  cat("Missing values\n")
  cat("-------------------------------\n")
  print(colSums(is.na(dataset)))
  
}

#______________________
# b. quality check
#______________________
# Quality checks
# check for duplicates for each dataset
  # district data
data_list <- list(
  df_district = list(data = df_district, id = "district_id"),
  df_schools  = list(data = df_schools,  id = "school_id"),
  df_std_sample = list(data = df_std_sample, id = "student_id"),
  df_teachers = list(data = df_teachers, id = "teacher_id")
)


for (name in names(data_list)) {
  df <- data_list[[name]]$data
  id_col <- data_list[[name]]$id
  
  cat("\n--------------------------------------\n")
  cat("Checking duplicates in:", name, "\n")
  cat("--------------------------------------\n")
  
  dupes <- df %>% get_dupes(all_of(id_col))
  
  if (nrow(dupes) != 0) {
    
    cat("Duplicates detected:", nrow(dupes), "rows\n")
    print(head(dupes, 5))  # show a few examples
  }
}

#______________________
# c. general cleaning
#______________________
# define numeric variables  
df_std_numeric = c("dropout_risk_score_2023","dropout_risk_score_2024","dropout_risk_score_2025",
                   "weight_2023",
                   "maths_proficiency_score_2025", "maths_proficiency_score_2024","maths_proficiency_score_2023",
                   "reading_proficiency_score_2023","reading_proficiency_score_2024","reading_proficiency_score_2025",
                   "attendance_rate_2025","attendance_rate_2024","attendance_rate_2023",
                   "distance_to_school_km_2025","distance_to_school_km_2024","distance_to_school_km_2023",
                   "dropout_risk_score_2023","dropout_risk_score_2024","dropout_risk_score_2025"
)

colnames(df_std_sample)

df_std_sample = df_std_sample %>%
  mutate(across(all_of(df_std_numeric), as.numeric))

df_std_sample %>%
  summarise(
    mean_weight = mean(weight_2023),
    sd_weight = sd(weight_2023),
    min_weight = min(weight_2023),
    max_weight = max(weight_2023)
  )


#______________________
# d. Assumptions and understanding of the data
#______________________

# Inspect and analyse Weight variable to understand how to treat it.
# Correlations with key characteristics: gender, rural, income
df_std_sample %>%
  mutate(
    gender_num = as.numeric(as.factor(gender)),
    rural_num = as.numeric(as.factor(rural_residence_2023)),
    income_num = as.numeric(as.factor(household_income_quintile_2023)),
  ) %>%
  summarise(
    cor_gender = cor(weight_2023, gender_num, use = "complete.obs"),
    cor_rural = cor(weight_2023, rural_num, use = "complete.obs"),
    cor_income = cor(weight_2023, income_num, use = "complete.obs")
  )

# average weight by income quantile
df_std_sample %>%
  group_by(household_income_quintile_2023) %>%
  summarise(mean_weight = mean(weight_2023))

# average weight by residence
df_std_sample %>%
  group_by(rural_residence_2023) %>%
  summarise(mean_weight = mean(weight_2023))

# average weight by gender
df_std_sample %>%
  group_by(gender) %>%
  summarise(mean_weight = mean(weight_2023))

# sample based on income quantile
table(df_std_sample$household_income_quintile_2023)

# assumption for the income quantile:
# 1 = poorest
# 5 = richest

# From the information received, it is unclear to what the weights refer to. From an analysis of the weights with key characteristics of the population (gender, rural, income), the weights are strongly negatively correlated with income levels but insignificant correlation with rural and gender
# This could mean that despite the sample is higher for the lower income quantile still the students in the lower income quantile are under represented. Therefore, below I check for the following:

# 1. attrition
# 2. Sample replacement
# 3. consistent cohort sample


# 3. RESHAPE DATA TO LONG --------------------------------------------------------------
#______________________
# a. reshape student data
#______________________
std_long <- df_std_sample %>%
  clean_names() %>%
  pivot_longer(
    cols = ends_with(c("_2023","_2024","_2025")),
    names_to = c(".value", "year"),
    names_pattern = "(.+)_(2023|2024|2025)"
  ) %>%
  mutate(year = as.integer(year))
glimpse(std_long)

# Check if weight can be used across years in the analysis as per comment above
# a. check if the pannel data is balanced across years
std_long %>%
  group_by(year) %>%
  summarise(n_students = n_distinct(student_id))
# b. check if every single student in 2023 is still in the data in 2025 (and viceversa).

stopifnot(length(intersect(
  std_long$student_id[std_long$year == 2023],
  std_long$student_id[std_long$year == 2025]
)) == nrow(df_std_sample))

# N.B: given that the checks are positive then I will use the same weight across all three years 
# Apply same weight across years
std_long <- std_long %>%
  group_by(student_id) %>%
  mutate(weight = first(weight)) %>%
  ungroup()

#______________________
# b. reshape schools data
#______________________
schools_long <- df_schools %>%
  clean_names() %>%
  pivot_longer(
    cols = ends_with(c("_2023","_2024","_2025")),
    names_to = c(".value", "year"),
    names_pattern = "(.+)_(2023|2024|2025)"
  ) %>%
  mutate(year = as.integer(year))

glimpse(schools_long)

#______________________
# c. reshape teachers data
#______________________
teachers_long <- df_teachers %>%
  clean_names() %>%
  pivot_longer(
    cols = ends_with(c("_2023","_2024","_2025")),
    names_to = c(".value","year"),
    names_pattern = "(.+)_(2023|2024|2025)"
  ) %>%
  mutate(year = as.integer(year))

glimpse(teachers_long)

#______________________
# d. reshape district data from wide to long
#______________________
district_long <- df_district %>%
  clean_names() %>%
  pivot_longer(
    cols = ends_with(c("_2023","_2024","_2025")),
    names_to = c(".value","year"),
    names_pattern = "(.+)_(2023|2024|2025)"
  )%>%
  mutate(year = as.integer(year))
glimpse(district_long)


# 4. AGGREGATE TEACHER DATASET--------------------------------------------------
#______________________
# a. Aggregate teacher-level to school-year level data
#______________________
teacher_by_schoolyr <- teachers_long %>%
  group_by(school_id, year) %>%
  summarise(
    n_teachers = n(),
    pct_certified = mean(certification == "Yes", na.rm = TRUE),
    avg_teacher_age = mean(age, na.rm = TRUE),
    avg_teacher_nbr_chl = mean(number_of_children, na.rm = TRUE),
    avg_teacher_years_exp = mean(years_experience, na.rm = TRUE),
    avg_years_experience = mean(years_experience, na.rm = TRUE),
    avg_teacher_salary = mean(salary_band_localcurrency, na.rm = TRUE),
    avg_teacher_math_prof = mean(proficiency_in_maths_score, na.rm = TRUE),
    avg_injob_training_last5yrs = mean(if_else(injob_training_last5yrs == "Yes",1,0),na.rm = TRUE),
    .groups = "drop"
  )

# 5. MERGE DATASETS -------------------------------------------------------
#______________________
# a. merge student -> school-year -> district
#______________________

df_merged <- std_long %>%
  left_join(schools_long %>% select(school_id, year, matches("electricity|water|internet|toilet|single_sex_toilet|library|laboratory|ict|school_funding|school_management_committee_functional")), by = c("school_id","year")) %>%
  left_join(teacher_by_schoolyr, by = c("school_id","year")) %>%
  left_join(df_district %>% clean_names() %>% select(district_id, poverty_rate, literacy_rate_adult, gini_index), by = "district_id")



#______________________
# b. save merged adataset
#______________________
wb_new <- createWorkbook()

# Add worksheet and write data
addWorksheet(wb_new, sheetName = "merged")
writeData(wb_new, sheet = "merged", df_merged)

# Save the Excel file
saveWorkbook(wb_new, file.path(out_data_fld,"df_merged.xlsx"), overwrite = TRUE)




