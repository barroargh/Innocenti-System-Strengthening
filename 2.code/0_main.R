# SCRIPT DETAILS ----------------------------------------------------------

# PURPOSE: master script               
# Author: Daniele Barro
# Date: 23rd October 2025
# Editied by: DB
# Last edit: 
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

# Run the install.packages() lines if you haven't installed there packages yet

# REQUIRED PACKAGE -- install.packages("anthroplus")
# List of required packages
required_packages <- c(
  "here",
  "readr",
  "dplyr",
  "tidyr",
  "tidyselect",
  "data.table",
  "readxl",
  "writexl",
  "openxlsx",
  "ggplot2",
  "rmarkdown",
  "rlang",
  "purrr",
  "gt",
  "webr",
  "ggthemes",
  "ggrepel"
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
library(tidyr) 
library(tidyselect)
library(data.table)
library(readxl)
library(writexl)
library(openxlsx)
library(ggplot2)
library(officer)  # For PowerPoint export
library(rvg) 
library(scales)  # For formatting as currency
library(rmarkdown)
library(scales)  # For formatting
library(plyr)    # For rounding to nearest multiple
library(lubridate)
library(rlang)
library(purrr)
library(gt)
library(flextable)
library(webr)
library(ggthemes)
library(ggrepel)
library(rlang)
library(stringr)
