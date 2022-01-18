# Load Necessary Packages
if (!require("pacman")) install.packages("pacman")
p_load(
    "tidyverse",
    "haven",
    # For working with date-times
    "lubridate",
    # Weighted Functions
    "Hmisc")

# Import Data
data <- 
    read_dta(file ="../input/cps_wages_LFP.dta")
           # Override Incorrect Column Types
           col_types = cols(age = col_number()))