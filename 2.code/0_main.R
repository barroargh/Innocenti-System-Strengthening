# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: master script               
# Author: 
# Date: 23rd October 2025

#  
#      
# Content
#     1. GENERAL SET-UP
#     2. PACKAGES
#     3. SET-UP FOLDER STRUCTURE
#     4. MASTER SET-UP
#     5. RUN DO FILES 


# 1.  GENERAL SET UP ---------------------------------------------------------------
options(scipen = 999)
options(max.print = 5000)
options(tibble.width = Inf)
memory.limit(size = 8000)




# 2. PACKAGES -------------------------------------------------------------


# List of required packages
required_packages <- c(
  "here",
  "readr",
  "dplyr",
  "tidyr",
  "skimr",
  "tidyselect",
  "readxl",
  "writexl",
  "openxlsx",
  "ggplot2",
  "officer",
  "janitor",
  "psych",
  "DataExplorer",
  "lubridate",
  "broom",
  "lme4",
  "scales",
  "Hmisc",
  "lmerTest",
  "scales",
  "modelsummary",
  "broom.mixed"
)

# Function to check and install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }
}

# Install missing packages
install_if_missing(required_packages)

library(here)
library(readr) 
library(dplyr) 
library(tidyr)        # for data wrangling and visualization
library(skimr)        # for quick overview
library(tidyselect)
library(readxl)
library(writexl)
library(openxlsx)
library(ggplot2)
library(officer)
library(janitor)      # for cleaning variable names
library(psych)        # for descriptive stats
library(DataExplorer) # for quick automated EDA
library(tidyverse)
library(janitor)
library(lubridate)
library(broom)
library(lme4)     
library(scales)   
library(Hmisc)
library(lmerTest)
library(scales)
library(modelsummary)
library(broom.mixed)
library(rmarkdown)
# 3. SET-UP FOLDER STRUCTURE ----------------------------------------------
# a. Code folder
here::here()
code_fld = here::here("2.code")

# b. Input folder and file
input_fld = here::here("3.input")

# c. Output folder
output_fld = here::here("4.output")
out_data_fld = file.path(output_fld,"data")
out_graph_fld =  file.path(output_fld,"graph")
out_greg_fld =  file.path(output_fld,"regression")


# 4. MASTER SET-UP --------------------------------------------------------

    prep_data       = 1
    
    analysis        = 1
    
    prediction      = 1
    
# 5. RUN DO FILES  --------------------------------------------------------


    # Prep education data 
    if(prep_data == 1) {
      
      source(file.path(code_fld, "1.prep_data.R"))
      
      print("Script PREP DATA Done.")
      
    }

    # Analyze education data
    if(analysis == 1) {
      
      source(file.path(code_fld, "2.analysis1.R"))
      
      print("Script ANALYSIS Done.")
      
    }
  
    # prediciton  
    if (prediction == 1) { 
    rmarkdown::render(
      input = file.path(code_fld,"prediction.Rmd"),
      output_file = "HGSF_Attendance_Increase.docx",
      output_dir  = here("4.output/predictions")
    )
   } 