# Load Necessary Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
tidyverse
)

# Import Data
data = read_csv(file = "input/scp-1205.csv",
                # Relabel Columns
                col_names = c('countyname',
                              'state',
                              'contract',
                              'healthplanname',
                              'typeofplan',
                              'countyssa',
                              'eligibles',
                              'enrollees',
                              'penetration',
                              'ABrate'),
                # Override Incorrect Column Types
                col_types = cols(countyssa = col_number(),
                                 ABrate = col_number())
                )

# Replace NAs with zeroes.
data$eligibles <- replace_na(data$eligibles, 0)
data$enrollees <- replace_na(data$enrollees, 0)
data$penetration <- replace_na(data$penetration, 0)

# Construct new dataframe from old dataframe
data <- 
    data %>% 
    # Set countyname as the 'Key' Variable
    # and retain old variables that are consistent with county.
    group_by(countyname, state, countyssa, eligibles) %>% 
    summarise(
        # Add New Variables:
        # Number of health plans with more than 10 enrollees.
        numberofplans1 = length(contract[enrollees > 10]),
        # Number of health plans with penetration > 0.5.
        numberofplans2 = length(contract[penetration > 0.5]),
        # Number of individuals in the county with a MA health plan.
        totalenrollees = sum(enrollees)
        ) %>%
    # New Variable:
    # Number of individuals in the county enrolled in a MA plan.
    mutate(totalpenetration = 100 * totalenrollees / eligibles) 

# Replace 'totalpenetration' NaN's with zeroes.
data$totalpenetration <- replace_na(data$totalpenetration, 0)

# Sort by state then county.
data <- data[order(data$state, data$countyname),]

# Filter for counties with 'state' codes NOT in:
  # U.S. territories:
    # (American Samoa, Puerto Rico, Guam, Virgin Islands)
  # Unusual SCounty Codes (Unmarked State)
  # NA State Codes
data <- data %>% filter(!(state %in% c("AS","PR","GU", "VI", "99", NA)))

# Trimming Outliers:
  # Two counties, 'Manassas Park City' and 'Broomfield', are outliers in terms of 
  # 'totalpenetration.' Their values of Inf and 162 mean that more enrolled individuals
  # are enrolled in MA plans than are eligible for them. I opt to cap their penetration
  # at 100 to reflect that they must be eligible if they are enrolled.
data$totalpenetration[data$countyname %in% c('MANASSAS PARK CITY', 'BROOMFIELD')] <- 100

# Export data
write_csv(data, file = 'output/scp-1205_clean.csv')