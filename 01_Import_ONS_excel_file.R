# 01 Import_excel_data

# We use readxl library to import Excel file into R and janitor to tidy up variable names

library(readxl)
library(here)
library(dplyr)
library(janitor)


# Step 01 03
# Check files available in the data folder
list.files("./data")
list.files (path = "./data" ,pattern = "xls$")

# [1] "Home Geography Table 8.7a   Annual pay - Gross 2022.xls"

# Step 02
# List tabs from above Excel file to know which tab to import
excel_sheets("./data/Home Geography Table 8.7a   Annual pay - Gross 2022.xls")

# List of tabs
[1] "Notes"            "All"              "Male"             "Female"           "Full-Time"       
[6] "Part-Time"        "Male Full-Time"   "Male Part-Time"   "Female Full-Time" "Female Part-Time"

# We want to import in the tab labelled ALL. This is sheet 2 

# Step 03: Read in data 
# Tab named "All" corresponds to sheet 2, and we use clean_names from janitor to obtain column names
datan <- read_excel(here("data", "Home Geography Table 8.7a   Annual pay - Gross 2022.xls"), sheet = 2) %>% clean_names()
datan

# Skip first 3 lines
datan <- read_excel(here("data", "Home Geography Table 8.7a   Annual pay - Gross 2022.xls"), sheet = 2, skip =4) %>% 
  clean_names()
datan

# Step 04: Get column names
names(datan)

[1] "description" "code"        "thousand"    "median"      "change_5"    "mean"        "change_7"    "x10"        
[9] "x20"         "x25"         "x30"         "x40"         "x60"         "x70"         "x75"         "x80"        
[17] "x90"         "x18"         "x19"         "x20_2"    

# We now can subset the columns we are interested in [description,code,median,mean]
# Also apply numeric format to median and mean values
data_raw <- datan %>% select(description,code,median,mean) %>% 
                      mutate(median_pay = as.numeric(median),
                             mean_pay = as.numeric(mean))
data_raw
  
# We want to have three data sets, one for United kingdom, another for Regions and another for Unitary Authorities Below Regions
# 1. National level (United Kingdom, Great Britain, England and Wales, England)
# 2. Regions
# 3. Unitary Authorities below Regions (UA)

# Filtering in DPLYR using %in%
# library(dplyr)
# target <- c("Tom", "Lynn")
# filter(dat, name %in% target) 

# 1. National level 
# "United Kingdom", "Great Britain", "England and Wales", "England"
national <- c("United Kingdom", "Great Britain", "England and Wales", "England", "Wales", "Scotland")

National <- datan %>% select(description,code,median,mean) %>% 
                      mutate(median_pay = as.numeric(median), mean_pay = as.numeric(mean)) %>% 
                      filter(description %in% national)
National  

# 2. Regions
# The regions, formerly known as the government office regions, are the highest tier of sub-national division in England. 
# There are Nine regions in England.
# North East, North West, Yorkshire and The Humber, East Midlands, West Midlands, East, London, South East, South West 
regions <- c("North East", "North West", "Yorkshire and The Humber", 
             "East Midlands", "West Midlands", "East", "London", "South East", "South West")
  
  
Regions <- datan %>% select(description,code,median,mean) %>% 
            mutate(median_pay = as.numeric(median), mean_pay = as.numeric(mean)) %>% 
            filter(description %in% regions)
Regions   


 
 
 