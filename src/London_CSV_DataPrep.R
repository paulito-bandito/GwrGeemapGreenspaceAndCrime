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
  
  # found this by doing a intersect action lower in the file.
  COMMON_MAJOR_CATEGORIES = c("Burglary", "Robbery", "Sexual Offences", "Violence Against the Person")

# ===========================
#         YEAR 2001
# ===========================
  
  # File is HUGE, make sure you only select columns you needd (3 Gigs!)
  data2001 <- read_csv("data/csv/london/raw/MPS_Ward_Level_Crime_2001-2010.csv") %>%
    select('Ward Code',
      'Ward Name',
      'Borough',
      'Major Category',
      'Minor Category',
      '200101',
      '200102',
      '200103',
      '200104',
      '200105',
      '200106',
      '200107',
      '200108',
      '200109',
      '200110',
      '200111',
      '200112') %>%
    rename(WardCode = `Ward Code`) %>%
    rename(WardName = `Ward Name`) %>%
    rename(MajorCategory = `Major Category`) %>%
    rename(MinorCategory = `Minor Category`) %>%
    filter( MajorCategory %in% COMMON_MAJOR_CATEGORIES) 
  
    # Column summation
    data2001$YearSum = rowSums(data2001[ , c(6:17)], na.rm=TRUE)

    # select less columns
    data2001 = select(data2001, WardCode, MajorCategory, YearSum)
  
    # Row Summary (We need to do this because of MinorCategorie are different
    # in 2020 and 2001.)
    data2001 = aggregate(YearSum ~ WardCode + MajorCategory, 
      data = data2001, FUN = sum, na.rm = TRUE)
  
  
  # Categories
  unique(data2001$MajorCategory)
 
# ===========================
#         YEAR 2020
# ===========================
  
  # File is HUGE, make sure you only select columns you needd (3 Gigs!)
  data2020 <- read_csv("data/csv/london/raw/MPS Ward Level_Crime_2020-2021.csv") %>%
    select('WardCode',
      'WardName',
      'LookUp_BoroughName',
      'MajorText',
      'MinorText',
      '202001',
      '202002',
      '202003',
      '202004',
      '202005',
      '202006',
      '202007',
      '202008',
      '202009',
      '202010',
      '202011',
      '202012')%>%
    rename(Borough = LookUp_BoroughName) %>%
    rename(MajorCategory = MajorText) %>%
    rename(MinorCategory = MinorText) %>%
    filter( MajorCategory %in% COMMON_MAJOR_CATEGORIES)
  
    # Column summation
    data2020$YearSum <- rowSums(data2020[ , c(6:17)], na.rm=TRUE)

    # select less columns
    data2020 = select(data2020, WardCode, MajorCategory, YearSum)
    
    # Row Summary (We need to do this because of MinorCategorie are different
    # in 2020 and 2001.)
    data2020Test = aggregate(YearSum ~ WardCode + MajorCategory, 
      data = data2020, FUN = sum, na.rm = TRUE)
  
  
  # Categories
  unique(data2020$MajorCategory)
  unique(data2020$MinorCategory)

# ============================
# See what is common
# ============================
  # MAJOR INTERSECTION
  intersect(data2020$MajorCategory, data2001$MajorCategory)
  
  # MINOR INTERSECTION
  intersect(data2020$MinorCategory, data2001$MinorCategory)

# ============================
# Pivot the categorical columns
# ============================
  
  data2001Pivoted = data2001 %>%
    group_by(WardCode, MajorCategory) %>%
    mutate(row = row_number()) %>%  # Make a unique ID, and then remove it later
    pivot_wider(names_from = MajorCategory, values_from = YearSum) %>%
    select(-row) # remove the unique identifier
  
  data2020Pivoted = data2020 %>%
    group_by(WardCode, MajorCategory) %>%
    mutate(row = row_number()) %>%  # Make a unique ID, and then remove it later
    pivot_wider(names_from = MajorCategory, values_from = YearSum) %>%
    select(-row) # remove the unique identifier
  
  rm(data2001)
  rm(data2020)

# ============================
# Totals
# ============================
  # 
  RANGE_CRIME_TOTAL = c(2:5)
  
  data2001Pivoted$AllCrime <- rowSums(data2001Pivoted[ , RANGE_CRIME_TOTAL], na.rm=TRUE)
  data2020Pivoted$AllCrime <- rowSums(data2020Pivoted[ , RANGE_CRIME_TOTAL], na.rm=TRUE)

# ============================
# Find intersection of Wards and filter
# ============================

  wardIntersection = intersect(data2020Pivoted$WardCode, data2001Pivoted$WardCode)
  
  data2001Pivoted = filter(data2001Pivoted, WardCode %in% wardIntersection)
  data2020Pivoted = filter(data2020Pivoted, WardCode %in% wardIntersection)
    
# ============================
# Write Out the data
# ============================
  
  # write the converted data file
  write_csv(data2001Pivoted, "data/csv/london/refined/LondonCrimeByWard_2001.csv")
  write_csv(data2020Pivoted, "data/csv/london/refined/LondonCrimeByWard_2020.csv")
