# ======================================
# AUTHOR:       Paul Walter
# SUBJECT:      Create a single shape file out of many that will have the mean 
#               NDVI index. 
#               
#               Be sure to run the Jupyter Notebook first to produce the NDVI 
#               shape files in your ~Downloads folder. This script will be looking
#               for these intermediate files.
#
# REFERENCES:
#               GWR in R: https://rpubs.com/quarcs-lab/tutorial-gwr1
#               MGWR:     https://sgsup.asu.edu/sites/default/files/SparcFiles/mgwr_2.2_manual_final.pdf
# =====================================

# ===============================================================
# 1 INCLUDE LIBRRARY & COMMON METHODS
# ===============================================================

source("src/CreateMaps/639.FinalProject.02.2.Methods.R")
library(dplyr)
library(stats)
library(RColorBrewer)


# Change the presentation of decimal numbers to 4 and avoid scientific
# notation AND so R doesn't make your Census Tract Codes with only one
# decimal point.
options(prompt = "R> ",
        digits = 4,
        scipen = 999)



options(digits = 9) #Set the number of numeric digits to work with, so R doesn't truncate your census tract codes!

EPSG_CRC_NUM = 32629

# FIND THE CRC using this site: @see https://spatialreference.org/ref/epsg/3772/
London3TM_PROJ_4_CRC = st_crs(EPSG_CRC_NUM) #crs("+proj=tmerc +lat_0=0 +lon_0=-114 +k=0.9999 +x_0=0 +y_0=0 +ellps=clrk66 +datum=NAD27 +units=m +no_defs ")

# ===============================================================
#  2 SPATIAL DATA
# ===============================================================

Output.Areas.2011 = read_sf("data/shp/london_by_ward/raw_shp_file/LondonWard.shp")

Output.Areas.2011 = read_sf("C:/Users/paulw/Downloads/London_MeanGreenessByWard_2011.shp") %>% 
  select(GSS_CODE, BOROUGH, NAME, mean)  %>%
  rename(WardCode = GSS_CODE) %>%
  rename(WardName = NAME) %>%
  rename(BoroughName = BOROUGH) %>%
  rename(MeanNdvi2011 = mean )

Output.Areas.2012 = read_sf("C:/Users/paulw/Downloads/London_MeanGreenessByWard_2012.shp") %>% 
  select(GSS_CODE, mean)  %>%
  rename(WardCode = GSS_CODE) %>%
  rename(MeanNdvi2012 = mean )

Output.Areas.2013 = read_sf("C:/Users/paulw/Downloads/London_MeanGreenessByWard_2013.shp") %>% 
  select(GSS_CODE, mean)  %>%
  rename(WardCode = GSS_CODE) %>%
  rename(MeanNdvi2013 = mean )

Output.Areas.2014 = read_sf("C:/Users/paulw/Downloads/London_MeanGreenessByWard_2014.shp") %>% 
  select(GSS_CODE, mean)  %>%
  rename(WardCode = GSS_CODE) %>%
  rename(MeanNdvi2014 = mean )

Output.Areas.2015 = read_sf("C:/Users/paulw/Downloads/London_MeanGreenessByWard_2015.shp") %>% 
  select(GSS_CODE, mean)  %>%
  rename(WardCode = GSS_CODE) %>%
  rename(MeanNdvi2015 = mean )

Output.Areas.2016 = read_sf("C:/Users/paulw/Downloads/London_MeanGreenessByWard_2016.shp") %>% 
  select(GSS_CODE, mean)  %>%
  rename(WardCode = GSS_CODE) %>%
  rename(MeanNdvi2016 = mean )

Output.Areas.2017 = read_sf("C:/Users/paulw/Downloads/London_MeanGreenessByWard_2017.shp") %>% 
  select(GSS_CODE, mean)  %>%
  rename(WardCode = GSS_CODE) %>%
  rename(MeanNdvi2017 = mean )

Output.Areas.2018 = read_sf("C:/Users/paulw/Downloads/London_MeanGreenessByWard_2018.shp") %>% 
  select(GSS_CODE, mean)  %>%
  rename(WardCode = GSS_CODE) %>%
  rename(MeanNdvi2018 = mean )

Output.Areas.2019 = read_sf("C:/Users/paulw/Downloads/London_MeanGreenessByWard_2019.shp") %>% 
  select(GSS_CODE, mean)  %>%
  rename(WardCode = GSS_CODE) %>%
  rename(MeanNdvi2019 = mean )

Output.Areas.2020 = read_sf("C:/Users/paulw/Downloads/London_MeanGreenessByWard_2020.shp") %>% 
  select(GSS_CODE, mean)  %>%
  rename(WardCode = GSS_CODE) %>%
  rename(MeanNdvi2020 = mean )

Output.Areas.2021 = read_sf("C:/Users/paulw/Downloads/London_MeanGreenessByWard_2021.shp") %>% 
  select(GSS_CODE, mean)  %>%
  rename(WardCode = GSS_CODE) %>%
  rename(MeanNdvi2021 = mean )

# paste the means of the other shape files into one
Output.Areas.2011$MeanNdvi2012 = Output.Areas.2012$MeanNdvi2012
Output.Areas.2011$MeanNdvi2013 = Output.Areas.2013$MeanNdvi2013
Output.Areas.2011$MeanNdvi2014 = Output.Areas.2014$MeanNdvi2014
Output.Areas.2011$MeanNdvi2015 = Output.Areas.2015$MeanNdvi2015
Output.Areas.2011$MeanNdvi2016 = Output.Areas.2016$MeanNdvi2016
Output.Areas.2011$MeanNdvi2017 = Output.Areas.2017$MeanNdvi2017
Output.Areas.2011$MeanNdvi2018 = Output.Areas.2018$MeanNdvi2018
Output.Areas.2011$MeanNdvi2019 = Output.Areas.2019$MeanNdvi2019
Output.Areas.2011$MeanNdvi2020 = Output.Areas.2020$MeanNdvi2020
Output.Areas.2011$MeanNdvi2021 = Output.Areas.2021$MeanNdvi2021

# remove old variables
rm(Output.Areas.2012)
rm(Output.Areas.2013)
rm(Output.Areas.2014)
rm(Output.Areas.2015)
rm(Output.Areas.2016)
rm(Output.Areas.2017)
rm(Output.Areas.2018)
rm(Output.Areas.2019)
rm(Output.Areas.2020)
rm(Output.Areas.2021)

write_sf(Output.Areas.2011, "data/shp/london_by_ward/refined/MeanNDVI/LondonMeanNdvi.shp" )
