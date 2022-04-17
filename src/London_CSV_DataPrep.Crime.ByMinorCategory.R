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
  
  # File is HUGE, make sure you only select columns you needd (3 Gigs!)
  data2010 <- read_csv("data/csv/london/raw/MPS Ward Level Crime 2010-2020-03.csv") %>%
    rename(MajorCategory = `MajorText`) %>%
    rename(MinorCategory = `MinorText`) 
  
    # Column summation
    #data2010$Year2010 = rowSums(data2010[c('201004','201005','201006','201007','201008','201009','201010','201011','201012')], na.rm=TRUE)
    data2010$Year2011 = rowSums(data2010[ c('201101','201102','201103','201104','201105','201106','201107','201108','201109','201110','201111','201112')], na.rm=TRUE)
    data2010$Year2012 = rowSums(data2010[ c('201201','201202','201203','201204','201205','201206','201207','201208','201209','201210','201211','201212')], na.rm=TRUE)
    data2010$Year2013 = rowSums(data2010[ c('201301','201302','201303','201304','201305','201306','201307','201308','201309','201310','201311','201312')], na.rm=TRUE)
    data2010$Year2014 = rowSums(data2010[ c('201401','201402','201403','201404','201405','201406','201407','201408','201409','201410','201411','201412')], na.rm=TRUE)
    data2010$Year2015 = rowSums(data2010[ c('201501','201502','201503','201504','201505','201506','201507','201508','201509','201510','201511','201512')], na.rm=TRUE)
    data2010$Year2016 = rowSums(data2010[ c('201601','201602','201603','201604','201605','201606','201607','201608','201609','201610','201611','201612')], na.rm=TRUE)
    data2010$Year2017 = rowSums(data2010[ c('201701','201702','201703','201704','201705','201706','201707','201708','201709','201710','201711','201712')], na.rm=TRUE)
    data2010$Year2018 = rowSums(data2010[ c('201801','201802','201803','201804','201805','201806','201807','201808','201809','201810','201811','201812')], na.rm=TRUE)
    data2010$Year2019 = rowSums(data2010[ c('201901','201902','201903','201904','201905','201906','201907','201908','201909','201910','201911','201912')], na.rm=TRUE)
    
    # partial row, mark it with a `_A` so we can paste it to it's brother later.
    data2010$Year2020_A = rowSums(data2010[ c('201901','201902','201903')], na.rm=TRUE)
   
    # select less columns
    data2010 = select(data2010, WardCode, MinorCategory, Year2011, Year2012, Year2013, Year2014, Year2015,
      Year2016, Year2017, Year2018, Year2019, Year2020_A)
  
  # Categories
  #unique(data2010$MajorCategory)
  unique(data2010$MinorCategory)
  
# ===========================
#         YEAR 2020
# ===========================
  
  # File is HUGE, make sure you only select columns you needd (3 Gigs!)
  data2020 <- read_csv("data/csv/london/raw/MPS Ward Level Crime (most recent 24 months).csv") %>%
    rename(MajorCategory = `MajorText`) %>%
    rename(MinorCategory = `MinorText`) 
    
    # Column summation
  
    # partial row, mark it with a `_B` so we can paste it to it's brother later.
    data2020$Year2020_B = rowSums(data2020[ c('202004','202005','202006','202007','202008','202009','202010','202011','202012')], na.rm=TRUE)
    
    data2020$Year2021 = rowSums(data2020[ c('202101','202102','202103','202104','202105','202106','202107','202108','202109','202110','202111','202112')], na.rm=TRUE)
    data2020$Year2022 = rowSums(data2020[ c('202201','202202','202203')], na.rm=TRUE)
    
    # select less columns
    data2020 = select(data2020, WardCode, MinorCategory, Year2020_B, Year2021, Year2022)
  
  # Categories
  #unique(data2020$MajorCategory)
  unique(data2020$MinorCategory)
  
# ===========================
# Combine the two collections
# ===========================

  data2010_2020  = rbind( data2010Pivoted, data2020Pivoted)
  names(data2010_2020)
 

# ============================
# Find Overlap
# ============================ 
  
  #intersect(data2010$MajorCategory, data2020$MajorCategory)
  OVERLAPPING_CATEGORIES = intersect(data2010$MinorCategory, data2020$MinorCategory)
  
  data2010 = filter( data2010, MinorCategory %in% OVERLAPPING_CATEGORIES) 
  data2020 = filter( data2020, MinorCategory %in% OVERLAPPING_CATEGORIES) 

# ============================
# Pivot the categorical columns
# ============================
  
  # NOTE: make sure your remove columns (MajorCategories)
  data2010Pivoted = data2010 %>%
    group_by(WardCode, MinorCategory) %>%
    mutate(row = row_number()) %>%  # Make a unique ID, and then remove it later
    pivot_wider(
      names_from = MinorCategory, 
      # NOTE: check out the fancy multiple colum pivot!
      values_from = c(Year2011, Year2012, Year2013, Year2014, Year2015, Year2016, 
                      Year2017, Year2018, Year2019, Year2020_A)) %>%
    select(-row) # remove the unique identifier
  
  # NOTE: make sure your remove columns (MajorCategories)
  data2020Pivoted = data2020 %>%
    group_by(WardCode, MinorCategory) %>%
    mutate(row = row_number()) %>%  # Make a unique ID, and then remove it later
    pivot_wider(
      names_from = MinorCategory, 
      values_from = c(Year2020_B, Year2021, Year2022)) %>%
    select(-row) # remove the unique identifier
  
  # remove 
  rm(data2010)
  rm(data2020)
  
# ===========================
# Combine the two collections
# ===========================

  # Merge the two collections
  data2010_2020  = merge(data2010Pivoted, data2020Pivoted, by=c("WardCode","WardCode"))
  
  #Combine two columns 
   data2010_2020$"Year2020_Absconding from Lawful Custody" = rowSums(data2010_2020[ c("Year2020_A_Absconding from Lawful Custody", "Year2020_B_Absconding from Lawful Custody" )], na.rm=TRUE)                    
   data2010_2020$"Year2020_Aggravated Vehicle Taking" = rowSums(data2010_2020[ c("Year2020_A_Aggravated Vehicle Taking", "Year2020_B_Aggravated Vehicle Taking" )], na.rm=TRUE)                         
   data2010_2020$"Year2020_Arson" = rowSums(data2010_2020[ c("Year2020_A_Arson", "Year2020_B_Arson" )], na.rm=TRUE)                                             
   data2010_2020$"Year2020_Bail Offences" = rowSums(data2010_2020[ c("Year2020_A_Bail Offences", "Year2020_B_Bail Offences" )], na.rm=TRUE)                                     
   data2010_2020$"Year2020_Bicycle Theft" = rowSums(data2010_2020[ c("Year2020_A_Bicycle Theft", "Year2020_B_Bicycle Theft" )], na.rm=TRUE)                                     
   data2010_2020$"Year2020_Bigamy" = rowSums(data2010_2020[ c("Year2020_A_Bigamy", "Year2020_B_Bigamy" )], na.rm=TRUE)                                            
   data2010_2020$"Year2020_Burglary Business and Community" = rowSums(data2010_2020[ c("Year2020_A_Burglary Business and Community", "Year2020_B_Burglary Business and Community" )], na.rm=TRUE)                   
   data2010_2020$"Year2020_Criminal Damage" = rowSums(data2010_2020[ c("Year2020_A_Criminal Damage", "Year2020_B_Criminal Damage" )], na.rm=TRUE)                                   
   data2010_2020$"Year2020_Dangerous Driving" = rowSums(data2010_2020[ c("Year2020_A_Dangerous Driving", "Year2020_B_Dangerous Driving" )], na.rm=TRUE)                                 
   data2010_2020$"Year2020_Disclosure, Obstruction, False or Misleading State" = rowSums(data2010_2020[ c("Year2020_A_Disclosure, Obstruction, False or Misleading State", "Year2020_B_Disclosure, Obstruction, False or Misleading State" )], na.rm=TRUE)
   data2010_2020$"Year2020_Domestic Burglary" = rowSums(data2010_2020[ c("Year2020_A_Domestic Burglary", "Year2020_B_Domestic Burglary" )], na.rm=TRUE)                                 
   data2010_2020$"Year2020_Drug Trafficking" = rowSums(data2010_2020[ c("Year2020_A_Drug Trafficking", "Year2020_B_Drug Trafficking" )], na.rm=TRUE)                                  
   data2010_2020$"Year2020_Exploitation of Prostitution" = rowSums(data2010_2020[ c("Year2020_A_Exploitation of Prostitution", "Year2020_B_Exploitation of Prostitution" )], na.rm=TRUE)                      
   data2010_2020$"Year2020_Forgery or Use of Drug Prescription" = rowSums(data2010_2020[ c("Year2020_A_Forgery or Use of Drug Prescription", "Year2020_B_Forgery or Use of Drug Prescription" )], na.rm=TRUE)               
   data2010_2020$"Year2020_Fraud or Forgery Associated with Driver Records" = rowSums(data2010_2020[ c("Year2020_A_Fraud or Forgery Associated with Driver Records", "Year2020_B_Fraud or Forgery Associated with Driver Records" )], na.rm=TRUE)   
   data2010_2020$"Year2020_Going Equipped for Stealing" = rowSums(data2010_2020[ c("Year2020_A_Going Equipped for Stealing", "Year2020_B_Going Equipped for Stealing" )], na.rm=TRUE)                       
   data2010_2020$"Year2020_Handling Stolen Goods" = rowSums(data2010_2020[ c("Year2020_A_Handling Stolen Goods", "Year2020_B_Handling Stolen Goods" )], na.rm=TRUE)                             
   data2010_2020$"Year2020_Homicide" = rowSums(data2010_2020[ c("Year2020_A_Homicide", "Year2020_B_Homicide" )], na.rm=TRUE)                                          
   data2010_2020$"Year2020_Interfering with a Motor Vehicle" = rowSums(data2010_2020[ c("Year2020_A_Interfering with a Motor Vehicle", "Year2020_B_Interfering with a Motor Vehicle" )], na.rm=TRUE)                  
   data2010_2020$"Year2020_Making, Supplying or Possessing Articles for use i" = rowSums(data2010_2020[ c("Year2020_A_Making, Supplying or Possessing Articles for use i", "Year2020_B_Making, Supplying or Possessing Articles for use i" )], na.rm=TRUE)
   data2010_2020$"Year2020_Obscene Publications" = rowSums(data2010_2020[ c("Year2020_A_Obscene Publications", "Year2020_B_Obscene Publications" )], na.rm=TRUE)                              
   data2010_2020$"Year2020_Offender Management Act" = rowSums(data2010_2020[ c("Year2020_A_Offender Management Act", "Year2020_B_Offender Management Act" )], na.rm=TRUE)                           
   data2010_2020$"Year2020_Other Firearm Offences" = rowSums(data2010_2020[ c("Year2020_A_Other Firearm Offences", "Year2020_B_Other Firearm Offences" )], na.rm=TRUE)                            
   data2010_2020$"Year2020_Other Forgery" = rowSums(data2010_2020[ c("Year2020_A_Other Forgery", "Year2020_B_Other Forgery" )], na.rm=TRUE)                                     
   data2010_2020$"Year2020_Other Knife Offences" = rowSums(data2010_2020[ c("Year2020_A_Other Knife Offences", "Year2020_B_Other Knife Offences" )], na.rm=TRUE)  
   data2010_2020$"Year2020_Other Notifiable Offences" = rowSums(data2010_2020[ c("Year2020_A_Other Notifiable Offences", "Year2020_B_Other Notifiable Offences" )], na.rm=TRUE)                         
   data2010_2020$"Year2020_Other Offences Against the State, or Public Order" = rowSums(data2010_2020[ c("Year2020_A_Other Offences Against the State, or Public Order", "Year2020_B_Other Offences Against the State, or Public Order" )], na.rm=TRUE) 
   data2010_2020$"Year2020_Other Sexual Offences" = rowSums(data2010_2020[ c("Year2020_A_Other Sexual Offences", "Year2020_B_Other Sexual Offences" )], na.rm=TRUE)                             
   data2010_2020$"Year2020_Other Theft" = rowSums(data2010_2020[ c("Year2020_A_Other Theft", "Year2020_B_Other Theft" )], na.rm=TRUE)                                       
   data2010_2020$"Year2020_Perjury" = rowSums(data2010_2020[ c("Year2020_A_Perjury", "Year2020_B_Perjury" )], na.rm=TRUE)                                           
   data2010_2020$"Year2020_Perverting Course of Justice" = rowSums(data2010_2020[ c("Year2020_A_Perverting Course of Justice", "Year2020_B_Perverting Course of Justice" )], na.rm=TRUE)                      
   data2010_2020$"Year2020_Possession of Article with Blade or Point" = rowSums(data2010_2020[ c("Year2020_A_Possession of Article with Blade or Point", "Year2020_B_Possession of Article with Blade or Point" )], na.rm=TRUE)         
   data2010_2020$"Year2020_Possession of Drugs" = rowSums(data2010_2020[ c("Year2020_A_Possession of Drugs", "Year2020_B_Possession of Drugs" )], na.rm=TRUE)                               
   data2010_2020$"Year2020_Possession of False Documents" = rowSums(data2010_2020[ c("Year2020_A_Possession of False Documents", "Year2020_B_Possession of False Documents" )], na.rm=TRUE)                     
   data2010_2020$"Year2020_Possession of Firearm with Intent" = rowSums(data2010_2020[ c("Year2020_A_Possession of Firearm with Intent", "Year2020_B_Possession of Firearm with Intent" )], na.rm=TRUE)                 
   data2010_2020$"Year2020_Possession of Firearms Offences" = rowSums(data2010_2020[ c("Year2020_A_Possession of Firearms Offences", "Year2020_B_Possession of Firearms Offences" )], na.rm=TRUE)                   
   data2010_2020$"Year2020_Possession of Other Weapon" = rowSums(data2010_2020[ c("Year2020_A_Possession of Other Weapon", "Year2020_B_Possession of Other Weapon" )], na.rm=TRUE)                        
   data2010_2020$"Year2020_Profitting From or Concealing Proceeds of Crime" = rowSums(data2010_2020[ c("Year2020_A_Profitting From or Concealing Proceeds of Crime", "Year2020_B_Profitting From or Concealing Proceeds of Crime" )], na.rm=TRUE)   
   data2010_2020$"Year2020_Public Fear Alarm or Distress" = rowSums(data2010_2020[ c("Year2020_A_Public Fear Alarm or Distress", "Year2020_B_Public Fear Alarm or Distress" )], na.rm=TRUE)                     
   data2010_2020$"Year2020_Racially or Religiously Aggravated Public Fear, Al" = rowSums(data2010_2020[ c("Year2020_A_Racially or Religiously Aggravated Public Fear, Al", "Year2020_B_Racially or Religiously Aggravated Public Fear, Al" )], na.rm=TRUE)
   data2010_2020$"Year2020_Rape" = rowSums(data2010_2020[ c("Year2020_A_Rape", "Year2020_B_Rape" )], na.rm=TRUE)                                              
   data2010_2020$"Year2020_Robbery of Business Property" = rowSums(data2010_2020[ c("Year2020_A_Robbery of Business Property", "Year2020_B_Robbery of Business Property" )], na.rm=TRUE)                      
   data2010_2020$"Year2020_Robbery of Personal Property" = rowSums(data2010_2020[ c("Year2020_A_Robbery of Personal Property", "Year2020_B_Robbery of Personal Property" )], na.rm=TRUE)                      
   data2010_2020$"Year2020_Shoplifting" = rowSums(data2010_2020[ c("Year2020_A_Shoplifting", "Year2020_B_Shoplifting" )], na.rm=TRUE)                                       
   data2010_2020$"Year2020_Soliciting for Prostitution" = rowSums(data2010_2020[ c("Year2020_A_Soliciting for Prostitution", "Year2020_B_Soliciting for Prostitution" )], na.rm=TRUE)                       
   data2010_2020$"Year2020_Theft from a Motor Vehicle" = rowSums(data2010_2020[ c("Year2020_A_Theft from a Motor Vehicle", "Year2020_B_Theft from a Motor Vehicle" )], na.rm=TRUE)                        
   data2010_2020$"Year2020_Theft from Person" = rowSums(data2010_2020[ c("Year2020_A_Theft from Person", "Year2020_B_Theft from Person" )], na.rm=TRUE)                                 
   data2010_2020$"Year2020_Theft or Taking of a Motor Vehicle" = rowSums(data2010_2020[ c("Year2020_A_Theft or Taking of a Motor Vehicle", "Year2020_B_Theft or Taking of a Motor Vehicle" )], na.rm=TRUE)                
   data2010_2020$"Year2020_Threat or Possession With Intent to Commit Crimina" = rowSums(data2010_2020[ c("Year2020_A_Threat or Possession With Intent to Commit Crimina", "Year2020_B_Threat or Possession With Intent to Commit Crimina" )], na.rm=TRUE)
   data2010_2020$"Year2020_Violence with Injury" = rowSums(data2010_2020[ c("Year2020_A_Violence with Injury", "Year2020_B_Violence with Injury" )], na.rm=TRUE)                              
   data2010_2020$"Year2020_Violence without Injury" = rowSums(data2010_2020[ c("Year2020_A_Violence without Injury", "Year2020_B_Violence without Injury" )], na.rm=TRUE)                           
   data2010_2020$"Year2020_Violent Disorder" = rowSums(data2010_2020[ c("Year2020_A_Violent Disorder", "Year2020_B_Violent Disorder" )], na.rm=TRUE)                                  
   data2010_2020$"Year2020_Wildlife Crime" = rowSums(data2010_2020[ c("Year2020_A_Wildlife Crime", "Year2020_B_Wildlife Crime" )], na.rm=TRUE)                                    
   
   # Remove the temp columns (we have merged them, it's ok to throw it away)
   data2010_2020 = select(data2010_2020, -contains('Year2020_A_'))
   data2010_2020 = select(data2010_2020, -contains('Year2020_B_'))
   
# ============================
# Sum Yearly totals
# ============================    
     
   data2010_2020$Total2011 = rowSums(select(data2010_2020, contains('Year2011'))[,-1], na.rm=TRUE)
   data2010_2020$Total2012 = rowSums(select(data2010_2020, contains('Year2012'))[,-1], na.rm=TRUE)
   data2010_2020$Total2013 = rowSums(select(data2010_2020, contains('Year2013'))[,-1], na.rm=TRUE)
   data2010_2020$Total2014 = rowSums(select(data2010_2020, contains('Year2014'))[,-1], na.rm=TRUE)
   data2010_2020$Total2015 = rowSums(select(data2010_2020, contains('Year2015'))[,-1], na.rm=TRUE)
   data2010_2020$Total2016 = rowSums(select(data2010_2020, contains('Year2016'))[,-1], na.rm=TRUE)
   data2010_2020$Total2017 = rowSums(select(data2010_2020, contains('Year2017'))[,-1], na.rm=TRUE)
   data2010_2020$Total2018 = rowSums(select(data2010_2020, contains('Year2018'))[,-1], na.rm=TRUE)
   data2010_2020$Total2019 = rowSums(select(data2010_2020, contains('Year2019'))[,-1], na.rm=TRUE)
   data2010_2020$Total2020 = rowSums(select(data2010_2020, contains('Year2020'))[,-1], na.rm=TRUE)
   data2010_2020$Total2021 = rowSums(select(data2010_2020, contains('Year2021'))[,-1], na.rm=TRUE)
   data2010_2020$Total2022 = rowSums(select(data2010_2020, contains('Year2022'))[,-1], na.rm=TRUE)
   
# ============================
# Write Out the data
# ============================
  
  # write the converted data file
  write_csv(data2010_2020, "data/csv/london/refined/London.Crime.ByWard_2011-2022.csv")
