# 02_Build_median_annual_pay_plots.

library(readxl)
library(here)
library(dplyr)
library(janitor)
library(purrr)
library(forcats)
library(tidyverse)

# Step 01 03
# Check files available in the data folder
list.files("./data")
list.files (path = "./data" ,pattern = "xls$")

# [1] "Home Geography Table 8.7a   Annual pay - Gross 2022.xls"

# Step 02
# List tabs from above Excel file to know which tab to import
excel_sheets("./data/Home Geography Table 8.7a   Annual pay - Gross 2022.xls")

# 1. Read in data
datan <- read_excel(here("data", "Home Geography Table 8.7a   Annual pay - Gross 2022.xls"), sheet = 2, skip =4) %>% 
  clean_names()
datan


# 2. Subset regions data
regions <- c("North East", "North West", "Yorkshire and The Humber", 
             "East Midlands", "West Midlands", "East", "London", "South East", "South West")


Regions <- datan %>% select(description,code,median,mean) %>% 
  mutate(median_pay = as.numeric(median), mean_pay = as.numeric(mean)) %>% 
  filter(description %in% regions)
Regions   

# 3. Keep relevant variables for median income
# mutate(median_pay = as.numeric(median), mean_pay = as.numeric(mean)) %>% 
Regions_data <- Regions %>% select(region = description,code, median_pay)
Regions_data

# using fct_inorder() function from {forcats} pacakge
# https://www.rdocumentation.org/packages/forcats/versions/0.2.0/topics/fct_inorder


# This will turn region character into factor variables

Regions_factors <- Regions %>% 
                        select(region = description,code, median_pay) %>% 
                        mutate(region = fct_inorder(region))
Regions_factors

Distinct_regions <- Regions_factors %>%  distinct(region)
Distinct_regions

#   region                  
# <fct>                   
#   1 North East              
# 2 North West              
# 3 Yorkshire and The Humber
# 4 East Midlands           
# 5 West Midlands           
# 6 East                    
# 7 London                  
# 8 South East              
# 9 South West  

# SECTION 01: Produce minimal ggplot2 chart 

# 4. Produce minimal chart: 
# Choose just two regions for the first plot
# East Midlands, South East

Median_pay_subset <- Regions_factors %>% filter(region %in% c("East Midlands",
                                                              "South East"))
Median_pay_subset



# 4.1 Minimal plot
# This is a clean and minimial plot for two regions (East Midlands and south East)
subset_chart <- ggplot(data = Median_pay_subset,
                       aes(x = median_pay,
                           y = region, 
                           fill = region)) +
                geom_col() +
  scale_fill_manual(values = c("cornflowerblue","darkseagreen1")) +
  theme_void() +
  theme(axis.text.y = element_text(),
        legend.position = "none")
subset_chart

# Save this plot to a file: 
ggsave (plot = last_plot(),
        filename = str_glue("median_pay_plot_region.png"), 
        width = 4, height = 1, bg = "white")

# SECTION 02: Turn ggplot template plot into a function

# 4.2 Turn this into a function
# replace hard coded regions by function parameter
# Regions: South East, East Midlands

median_pay_plot <- function(region_comp){
  
  Median_pay_subset <- Regions_factors %>% filter(region %in% c("North West",region_comp))
  
  ggplot(data = Median_pay_subset,
                         aes(x = median_pay,
                             y = region, 
                             fill = region)) +
    geom_col() +
    scale_fill_manual(values = c("cornflowerblue","darkseagreen1")) +
    theme_void() +
    theme(axis.text.y = element_text(),
          legend.position = "none")
  subset_chart
  
  # Save this plot to a file: 
  # the str_glue() functions allows to include the region name: 
  ggsave (plot = last_plot(),
          filename = str_glue("median_pay_plot-{region_comp}.png"), 
          width = 4, height = 1, bg = "white")
  
}

# Run function
# For two regions: Comparing South East with East Midlands 
median_pay_plot(region_comp = "East Midlands")

# SECTION 03: Make multiple plots using walk() function

# MAKE multiple plots
# We will use the walk() function to make one plot for each regions
# Walk() function has TWO arguments: 1. First a vector with the list of regions, 2. Then the name
# of the function (you don't add parenthesis in the function)
# By using walk() function from {purr}
# That means we can compare any other region against "South East" region, by just adding
# them to the concatenate function:

# The code below will create two extra plots for East and London regions.
walk(c("East","London"),median_pay_plot)

# Continue this video in Minute 4:09
# https://www.youtube.com/watch?v=AFhFkgXxTdk

# To demonstrate this walk() function works, I am going to create three different plots for three regions.
# Make multiple plots
# THIS FUNCTION BELOW PRODUCES A CHART WITH "SOUTH WEST" AND ANY OTHER COUNTY WE INCLUDE IN THE LIST 
# BELOW (East Midlands","South West","Yorkshire and The Humber")
walk(c("East Midlands","South West","Yorkshire and The Humber"),median_pay_plot)

# THIS WORKS FINE ( we get South East against each of the other elements we include in the function:
# As in the code above: "East Midlands","South West","Yorkshire and The Humber"))

# Walk are two argument: 
# 1. First argument is the list of regions I want make plots from
# 2. Second argument, after the comma, is the name of the function you want to apply to elements included
# in the list.
# We don't need to add the parenthesis to the function we want to apply to the elements in our vector


# SECTION 04: IMPROVE PREVIOUS FUNCTION

# using pull() function: 
# pull {dplyr}: Extract a single column. 

# We are going to use North West region as an example:

# 3. Unitary Authorities below Regions (UA), Counties (Greater Manchester Met County, Maersyde Med County),
# and cities.
# Subset data for just one region "North West"
north_wregion <- c("North West" ,"Blackburn with Darwen UA","Blackpool UA","Halton UA","Warrington UA","Cheshire East UA"
                   ,"Cheshire West and Chester","Cumberland UA","Westmorland and Furness UA","Greater Manchester Met County","Bolton"
                   ,"Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan","Lancashire","Burnley"
                   ,"Chorley","Fylde","Hyndburn","Lancaster","Pendle","Preston","Ribble Valley","Rossendale","South Ribble","West Lancashire"
                   ,"Wyre","Merseyside Met County","Knowsley","Liverpool","St. Helens","Sefton","Wirral")

# Subset data for north west region
nwr_data <- datan %>% 
  select(geography = description,code,median) %>% 
  mutate(median_pay = as.numeric(median)) %>% 
  mutate(geography = fct_inorder(geography))
nwr_data

# Subset data for North West region only
northw_region <- nwr_data %>%  
                 select(geography,code,median_pay) %>% 
                 filter(geography %in% north_wregion) 
                 
northw_region

## Tweak previous function to use North West data

# This function will display two bar charts, one for North West region and another town or county defined
# by the region_comp function parameter:

median_pay_plot_geography <- function(region_comp){
  
  Median_pay_subset <- northw_region %>% filter(geography %in% c("North West",region_comp))
  
  ggplot(data = Median_pay_subset,
         aes(x = median_pay,
             y = geography, 
             fill = geography)) +
    geom_col() +
    scale_fill_manual(values = c("cornflowerblue","darkseagreen1")) +
    theme_void() +
    theme(axis.text.y = element_text(),
          legend.position = "none")
  subset_chart
  
  # Save this plot to a file: 
  # the str_glue() functions allows to include the region name: 
  ggsave (plot = last_plot(),
          filename = str_glue("median_pay_plot-{region_comp}.png"), 
          width = 4, height = 1, bg = "white")
  
}
 

# Just to show of previous function works with this new data set, I am going to plot data just for  a previous example, we can see 
# Create three plots for "Oldham","Bury","Salford", using the walk function
median_pay_plot_geography(region_comp = "Salford")

# Using the Walk function:
# Walk() function has TWO arguments: 1. First a vector with the list of regions, 2. Then the name
# of the function (you don't add parenthesis in the function)
walk(c("Oldham","Bury","Salford"),median_pay_plot_geography)

# SECTION 05: CREATE INDIVIDUAL CHARTS FOR EACH CITY USING pull() and walk() functions 

## MAKE MULTIPLE PLOTS USING the pull() function:
# pull()  from {dplyr} function: Extract a single column from a data frame.

northw_region_cities <- northw_region %>% 
                        filter(geography != "North West") %>% 
                        pull(geography)
northw_region_cities

length(northw_region_cities)

# Re-define function to create multiple individual plots for each city
median_pay_plot_cities <- function(region_comp){
  
  Median_pay_subset <- northw_region %>% filter(geography %in% c(region_comp))
  
  ggplot(data = Median_pay_subset,
         aes(x = median_pay,
             y = geography, 
             fill = geography)) +
    geom_col() +
    scale_fill_manual(values = c("cornflowerblue")) +
    theme_void() +
    theme(axis.text.y = element_text(),
          legend.position = "none")
  subset_chart
  
  # Save this plot to a file: 
  # the str_glue() functions allows to include the region name: 
  ggsave (plot = last_plot(),
          filename = str_glue("median_pay_plot-{region_comp}.png"), 
          width = 4, height = 1, bg = "white")
  
}

# 5.1 Create individual plots for cities and UA within North West region (excluding North West Region)
# This works fine
walk(northw_region_cities,median_pay_plot_cities)


# SECTION 06: CREATE BAR CHART PLOTS COMPARING NORTH REGION AGAINST EACH INDIVIDUAL CITY 
# Input data set: northw_region
region_city_comparison <- function(region_comp){
    Median_pay_subset <- northw_region %>% filter(geography %in% c("North West",region_comp))
    ggplot(data = Median_pay_subset,
         aes(x = median_pay,
             y = geography, 
             fill = geography)) +
    geom_col() +
    scale_fill_manual(values = c("cornflowerblue","darkseagreen1")) +
    theme_void() +
    theme(axis.text.y = element_text(),
          legend.position = "none")
  subset_chart
    # Save this plot to a file: 
  # the str_glue() functions allows to include the region name: 
  ggsave (plot = last_plot(),
          filename = str_glue("median_pay_plot-{region_comp}.png"), 
          width = 4, height = 1, bg = "white")
  
}

# 6.1 Create plots comparing North West region against individual cities and UA.
northw_region_comparison <- northw_region %>% pull(geography)
northw_region_comparison

# 6.2 Create Comparison plots for North West region and individual cities
# walk() function, first argument (vector of individual cities and Region, 2. )
walk(c("Oldham","Bury","Salford"),region_city_comparison)

# Now we only hacve to replace "Oldham","Bury","Salford" by the output from the pull() function
northw_region_comparison_char <- as.character(northw_region_comparison)
northw_region_comparison_char

# Now we try to apply the function again (IT WOKRS!)
walk(northw_region_comparison_char,region_city_comparison)
