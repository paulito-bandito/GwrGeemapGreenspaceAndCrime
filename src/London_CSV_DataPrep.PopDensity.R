# =====================================
# AUTHOR: Paul Walter
# PURPOSE: 633 & 647 preparing the data.
#          It needs to be filtered, 
#          renamed, and pivoted so only
#          the common crime categories
#          between the two years are 
#          present.
# =====================================
  
  library(dplyr)
  library(tidyverse)
  library(tidyr)
  library("data.table")
  library(stringr)
  
  #disable scientific notation
  options(scipen = 999)
  
# ===========================
#         YEAR 2010
# ===========================
  
  SELECTED_YEARS = c("2011", "2012", "2013", "2014", "2015", 
    "2016", "2017", "2018", "2019", "2020", "2021", "2022" )
  
  # File is HUGE, make sure you only select columns you needd (3 Gigs!)
  popDensity <- read_csv("data/csv/london/raw/housing-density-ward.csv") %>%
    rename(WardCode = `Code`) %>%
    select(-Borough, -Ward_Name, -Hectares, -Population_per_hectare ) %>%
    filter(Year %in% SELECTED_YEARS)
  
  # filter out future projections
 
# ============================
# Pivot the categorical columns
# ============================
  
  # NOTE: make sure your remove columns (MajorCategories)
  popDensityPivoted = popDensity %>%
    group_by(WardCode, Year) %>%
    mutate(row = row_number()) %>%  # Make a unique ID, and then remove it later
    pivot_wider(
      names_from = Year, 
      # NOTE: check out the fancy multiple colum pivot!
      values_from = c(Population, Square_Kilometres, Population_per_square_kilometre)) %>%
    select(-row) # remove the unique identifier
  
  # remove 
  rm(popDensity)
  
# ============================
# Write Out the data
# ============================
  
  # write the converted data file
  write_csv(popDensityPivoted, "data/csv/london/refined/LondonPopDensityByWard_2011-2022.csv")
