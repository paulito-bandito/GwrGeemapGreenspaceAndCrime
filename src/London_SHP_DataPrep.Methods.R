# ======================================
# AUTHOR:       Paul Walter
# SUBJECT:      Shared mapping functions.
#               
# REFERENCES: 
#               GWR in R: https://rpubs.com/quarcs-lab/tutorial-gwr1
#               MGWR:     https://sgsup.asu.edu/sites/default/files/SparcFiles/mgwr_2.2_manual_final.pdf
# =====================================

# ===============================================================
#  1 Libraries
# ===============================================================

knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)  # Modern data science workflow
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)
library(AICcmodavg)
library(stats)
library(dplyr)
library(RColorBrewer)



EPSG_CRC_NUM = 4326
# ===============================================================
# 2 METHODS
# ===============================================================
# 

createMap = function(spatialObject, property_name, n=20, legendTitle="", 
                     style="quantile", palette = "Greens", breaks = c(), 
                     bubbleVar=NA, bubblePalette="Reds", midpoint = NA,
                     pointLayer = NA, pointLayerPalette = "Greens",
                     cityBoundary = NA, cityBoundaryVar = "city",
                     scalePositionHorz = "left", scalePositionVert = "bottom",
                     compassPositionHorz = "left", compassPositionVert = "bottom",
                     creditsPosHorizontal = "right", creditsPosVertical = "bottom",
                     creditsPrefix = "", creditsText = "NAD83 / Alberta 3TM\n ref merid 114 W"){
  
  boundaryLayer = NA
  textForCredits = paste(creditsPrefix, creditsText)
  
  
  if(!is.na(cityBoundary)){
    
    boundaryLayer = 
      tm_shape( cityBoundary,
               # @see projection: https://spatialreference.org/ref/epsg/nad83-alberta-3tm-ref-merid-114-w/
               projection = EPSG_CRC_NUM, # EPSG Number. Project data using 3TM: https://map-rfun.library.duke.edu/031_thematic_mapping.html
      ) +
      
      # NOTE:   this is the city boundary that will lay under the data being displayed 
      #         to show how much area is missing.
      tm_polygons(col = "lightgray", 
              border.col = "black", lwd = 0.5, lty = "dashed")
  }
  
  #
  # Create first layer
  # @see: http://zevross.com/blog/2018/10/02/creating-beautiful-demographic-maps-in-r-with-the-tidycensus-and-tmap-packages/
  #
  firstLayer <- tm_shape(spatialObject, 
                         # @see projection: https://spatialreference.org/ref/epsg/nad83-alberta-3tm-ref-merid-114-w/
                         projection = 3776 # EPSG Number. Project data using 3TM: https://map-rfun.library.duke.edu/031_thematic_mapping.html
  ) +
    
    #tm_text("CTUID", size = 1) +
    #tm_polygons(col = "#E2E2E2", border.alpha = 0.5, lwd = 3)
    tm_fill(property_name, 
            
            # NOTE: This auto.palatte is depricated, but important so that negative numbers don't get displayed as the same value
            #       @see https://github.com/r-tmap/tmap/issues/130
            #
            #auto.palette.mapping = TRUE,
            palette = palette,
            midpoint = midpoint,
            n = n,
            style = style,
            breaks = breaks, #c(0, 10000, 20000, 30000, 40000, Inf),
            title = legendTitle,
            legend.hist = TRUE # show a histogram!
    ) +
    
    # @ see https://bookdown.org/nicohahn/making_maps_with_r5/docs/tmap.html
    #   tm_logo("data/functionalFoxInc.png", height = 2) +
    
    tm_dots() + # show centroids https://r-tmap.github.io/tmap-book/layers.html
    tm_layout(frame = FALSE,
              # main.title = "The quick brown fox jumps over the lazy dog", 
              # main.title.position = "center",
              
              legend.text.size = 0.6,
              legend.title.size = 0.8, 
              legend.outside = T, 
              legend.hist.size = 0.99) +
    
    
    tm_credits(textForCredits, 
               align = "left",
               size = 0.3,
               position=c(creditsPosHorizontal,creditsPosVertical) ) +
    
    tm_compass(position = c(compassPositionHorz, compassPositionVert), size = .25) +
    
    tm_scale_bar(position = c(scalePositionHorz, scalePositionVert), width = 0.15) 
  
  # ===============================
  # SEE IF SECOND LAYER IS NEEDED
  # =============================
  #
  #   In the second layer, we are going to overlay "bubbles" so we can signify 
  #   two datasets in one map.
  # 
  if(!is.na(bubbleVar)){
    firstLayer = firstLayer +
      
      tm_shape(spatialObject,
               # @see projection: https://spatialreference.org/ref/epsg/nad83-alberta-3tm-ref-merid-114-w/
               projection = EPSG_CRC_NUM, # EPSG Number. Project data using 3TM: https://map-rfun.library.duke.edu/031_thematic_mapping.html
      ) +
      
      # these are the bubbles that will overlay
      tm_bubbles(
        col = bubbleVar, 
        breaks = c(0,0.05, Inf),
        palette =bubblePalette,
        size=0.1)
  }
  
  # See if there are any point layers to add
  if(!is.na(pointLayer)){
    
    firstLayer = firstLayer +
      tm_shape( pointLayer,
               # @see projection: https://spatialreference.org/ref/epsg/nad83-alberta-3tm-ref-merid-114-w/
               projection = EPSG_CRC_NUM, # EPSG Number. Project data using 3TM: https://map-rfun.library.duke.edu/031_thematic_mapping.html
      ) +
      
      # these are the bubbles that will overlay
      tm_bubbles(
        col = bubbleVar, 
        breaks = c(0,0.05, Inf),
        palette =pointLayerPalette,
        size=0.1)
  }
  
  
  
  if( !is.na(boundaryLayer)){
    
    boundaryLayer + firstLayer # return the boundary layer
  
  }else{
    
    firstLayer # return the first layer
  }
  
}

PrepareCensusData2016 = function ( CtsToFilterBy ){
  
  #This reads your csv file into R and names it "data"
  Census.Alberta.Data <-
    read_csv("data/csv/2016_CT_Alberta_PopData.csv", col_types = "c") %>%
    dplyr::select(
      CTUID,
      GEO_LEVEL,
      `0001-Population__2016`,
      `0006-Population_density_per_square_kilometre`,
      `0008-GENDER_RATIO`,
      `0008-Total___Age_groups_and_average_age_of_the_population_-_100%_data`,
      `0009-0_to_14_years`,	`0010-0_to_4_years`,	`0011-5_to_9_years`,	`0012-10_to_14_years`,	`0013-15_to_64_years`,
      `0014-15_to_19_years`,	`0015-20_to_24_years`,	`0016-25_to_29_years`,	`0017-30_to_34_years`,	`0018-35_to_39_years`,
      `0019-40_to_44_years`,	`0020-45_to_49_years`,	`0021-50_to_54_years`,	`0022-55_to_59_years`,	`0023-60_to_64_years`,
      `0024-65_years_and_over`,
      `0040-Median_age_of_the_population`,
      `0061-Married`,
      `0064-Never_married`,
      `0087-Total___Lone-parent_census_families_in_private_households_-_100%_data`,
      `0803-Average_family_size_of_economic_families`,
      `0847-Total___Low-income_status_in_2015_for_the_population_in_private_households_to_whom_low-income_concepts_are_applicable_-_100%_data`,
      `1618-Owner`,
      `1619-Renter`,
      `1679-__of_tenant_households_in_subsidized_housing`,
      `1680-__of_tenant_households_spending_30%_or_more_of_its_income_on_shelter_costs`,
      `1681-Median_monthly_shelter_costs_for_rented_dwellings__$)`,
      `1867-Employed`,
      `1868-Unemployed`,
      `0801-Median_total_income_of_economic_families_in_2015__$)`,

      # PCA COMPONENTS =============================================

      # PCA: Material Component:These indicators are the proportion of people aged 15 years and older with no high school diploma (p_education)
      `1683-Total___Highest_certificate,_diploma_or_degree_for_the_population_aged_15_years_and_over_in_private_households_-_25%_sample_data`,
	    `1684-No_certificate__diploma_or_degree`,

      # PCA: Material Component: the population/employment ratio of people aged 15 years and older (p_employed),
      `1865-Total___Population_aged_15_years_and_over_by_Labour_force_status_-_25%_sample_data`,
	    `1867-Employed`,

      # PCA Material Component: and the average income of people aged 15 years and older (income).

	    `0670-Number_of_employment_income_recipients_aged_15_years_and_over_in_private_households___100%_data`,
	    `0682-Average_employment_income_in_2015_among_recipients__$)`,

      # PCA Social Component: the proportion of individuals aged 15 years and older living alone (p_alone),

      # How do I find this?

      # PCA Social Component: the proportion of individuals aged 15 years and older whose marital status is either separated, divorced or widowed (p_s_d_w)

	    `0065-Separated`,
	    `0066-Divorced`,
	    `0067-Widowed`,

      # PCA Social Component: and the proportion of single-parent families (p_mono)


	    `0078-Total_lone_parent_families_by_sex_of_parent`,
      `1136-Canadian_citizens`,
      `0106-English`,

    ) %>%
    
    mutate(Age0To14 = `0009-0_to_14_years` / `0001-Population__2016`) %>% 
    mutate(Age15To19 = `0014-15_to_19_years` / `0001-Population__2016`) %>%
    mutate(Age20To24 = 	`0015-20_to_24_years` /`0001-Population__2016`) %>% 
    mutate(Age24To29 = 	`0016-25_to_29_years`/`0001-Population__2016`) %>% 
    mutate(Age30To34 = 	`0017-30_to_34_years`/`0001-Population__2016`) %>% 
    mutate(Age35To39 = 	`0018-35_to_39_years`/`0001-Population__2016`) %>% 
    mutate(Age40To44 = 	`0019-40_to_44_years`/`0001-Population__2016`) %>% 
    mutate(Age45To49 = 	`0020-45_to_49_years`/`0001-Population__2016`) %>% 
    mutate(Age50To54 = 	`0021-50_to_54_years`/`0001-Population__2016`) %>%
    mutate(Age55To59 = 	`0022-55_to_59_years`/`0001-Population__2016`) %>%
    mutate(Age65AndOver = 	`0024-65_years_and_over`/`0001-Population__2016`) %>%
    mutate(Age24To65 = 	`0024-65_years_and_over`/`0001-Population__2016`) %>% 
    
    mutate(MedianAge = 	`0040-Median_age_of_the_population`) %>% 
    
    
    
    mutate(AvgFamilySize = `0803-Average_family_size_of_economic_families`) %>% 
    mutate(Employed = `1867-Employed`/`0001-Population__2016`) %>%  
    mutate(GeoLevel = GEO_LEVEL) %>% 
    mutate(LoneParent = `0087-Total___Lone-parent_census_families_in_private_households_-_100%_data` / `0001-Population__2016`) %>% 
    mutate(Married = `0061-Married` / `0001-Population__2016`) %>% 
    mutate(MedianHouseHoldIncome = `0801-Median_total_income_of_economic_families_in_2015__$)` ) %>% 
    mutate(MedianRentalCost =  `1681-Median_monthly_shelter_costs_for_rented_dwellings__$)`) %>% 
    mutate(Owner = `1618-Owner`/`0001-Population__2016`) %>%  
    mutate(PctMale = `0008-GENDER_RATIO`) %>% 
    mutate(PctRentersInSubsidizedHousing = `1679-__of_tenant_households_in_subsidized_housing`) %>% 
    mutate(PctRentersSpendingOver30PctOnHousing = `1680-__of_tenant_households_spending_30%_or_more_of_its_income_on_shelter_costs`) %>% 
    mutate(Pop2016 = `0001-Population__2016`) %>% 
    mutate(PopDensityPerKm = `0006-Population_density_per_square_kilometre` ) %>% 
    mutate(Renter = `1619-Renter`/`0001-Population__2016`) %>% 
    mutate(Renter = `1619-Renter`/`0001-Population__2016`) %>% 
    mutate(Unemployed = `1868-Unemployed`/`0001-Population__2016`) %>%  
    
    # PCA
    mutate(PCA_MATERIAL_NO_HIGHSCHOOL_PROP =  `1684-No_certificate__diploma_or_degree` / `1683-Total___Highest_certificate,_diploma_or_degree_for_the_population_aged_15_years_and_over_in_private_households_-_25%_sample_data`) %>%
	  mutate(PCA_MATERIAL_EMPLOYED_PROP = `1867-Employed` / `1865-Total___Population_aged_15_years_and_over_by_Labour_force_status_-_25%_sample_data`,) %>%  
    mutate(PCA_MATERIAL_AVG_INCOME = `0682-Average_employment_income_in_2015_among_recipients__$)`) %>% # `670-Number_of_employment_income_recipients_aged_15_years_and_over_in_private_households___100%_data`,
	  
    # mutate(PCA_MATERIAL_LIVING_ALONE = NA )  %>% # TODO: Figure out what this is in the Census
    mutate(PCA_SOCIAL_SEPARATED_DIVORCED_WIDOWED = `0065-Separated` + `0066-Divorced` + `0067-Widowed` ) %>%   
    mutate(PCA_SOCIAL_LONE_PARENT_FAMILIES =  `0078-Total_lone_parent_families_by_sex_of_parent`)%>%  
   
    # Set elements as numeric
    mutate(MedianHouseHoldIncome = as.numeric(MedianHouseHoldIncome))%>%  
    mutate(PctRentersSpendingOver30PctOnHousing = as.numeric(PctRentersSpendingOver30PctOnHousing))%>%
    mutate(PCA_MATERIAL_AVG_INCOME = as.numeric(PCA_MATERIAL_AVG_INCOME))%>%
    
    mutate(`1680-__of_tenant_households_spending_30%_or_more_of_its_income_on_shelter_costs` = as.numeric(`1680-__of_tenant_households_spending_30%_or_more_of_its_income_on_shelter_costs`))%>%
    mutate(`0847-Total___Low-income_status_in_2015_for_the_population_in_private_households_to_whom_low-income_concepts_are_applicable_-_100%_data` = as.numeric(`0847-Total___Low-income_status_in_2015_for_the_population_in_private_households_to_whom_low-income_concepts_are_applicable_-_100%_data`))%>%
    mutate(`0801-Median_total_income_of_economic_families_in_2015__$)` = as.numeric(`0801-Median_total_income_of_economic_families_in_2015__$)`))%>%
    mutate(`0670-Number_of_employment_income_recipients_aged_15_years_and_over_in_private_households___100%_data` = as.numeric(`0670-Number_of_employment_income_recipients_aged_15_years_and_over_in_private_households___100%_data`))%>%
    mutate(`0682-Average_employment_income_in_2015_among_recipients__$)` = as.numeric(`0682-Average_employment_income_in_2015_among_recipients__$)`))%>%
  
    
    # add new property after converting to numeric
    mutate(MedRentalCostDivByMedInc = MedianRentalCost / MedianHouseHoldIncome)
     
    #na.omit() #%>%
    
    # Reduce Number of variables
    #dplyr::select( CTUID, Age0To14:MedRentalCostDivByMedInc)
  
  #change to numeric
  
  
  
  Census.Alberta.Data
}

PrepareSpatialData = function(city, extentShpFile=NA, EPSG_CRC_NUM = 4326){
   
  Calgary3TM_PROJ_4_CRC = st_crs(EPSG_CRC_NUM) 
  
  query = paste("SELECT * FROM \"CensusTractsPlusData.v5\" WHERE CMANAME = '", city, "'", sep="")
  
  # Next you will load spreadsheet into data frame, you can name it anything you like, doesn't have to be 'data'
  # OR use SF library (use a query to reduce the number of CTS to join on)
  Output.Areas <- read_sf("data/shp/CensusTractsPlusData5/CensusTractsPlusData.v5.shp",
                          query= query
  ) # the shape file has all of canada, reduce it to only Calgary
  
  # FIND THE CRC using this site: @see https://spatialreference.org/ref/epsg/3772/
  Calgary3TM_PROJ_4_CRC = st_crs(EPSG_CRC_NUM) #crs("+proj=tmerc +lat_0=0 +lon_0=-114 +k=0.9999 +x_0=0 +y_0=0 +ellps=clrk66 +datum=NAD27 +units=m +no_defs ")
  # 
  Output.Areas = st_transform(Output.Areas, Calgary3TM_PROJ_4_CRC)
  
  #Remove periods from Census Tract so we can merge it propertly with data
  Output.Areas$CTUID<-gsub("\\.","",as.character(Output.Areas$CTUID))
  
  # ===============================================================
  #  CROP city boundary
  # ===============================================================
  if(!is.na(extentShpFile)){
    Output.Areas = CropSpatialData( Output.Areas, extentShpFile )
  }
  
  Output.Areas # return output
  
}

CropSpatialData = function (Output.Areas, extentShpFile, EPSG_CRC_NUM = 4326){
   
  
  Calgary3TM_PROJ_4_CRC = st_crs(EPSG_CRC_NUM) 

  # ===============================================================
  #  CROP city boundary
  # ===============================================================
  Output.Areas.Boundary <- read_sf( extentShpFile ) %>%
    st_transform(Calgary3TM_PROJ_4_CRC)
 
  Output.Areas = crop_shape( Output.Areas, Output.Areas.Boundary)

  Output.Areas # return output
  
}

PrepareData = function (city, extentShpFile, EPSG_CRC_NUM = 4326){
  
  # prepare spatial data
  spatialData = PrepareSpatialData(city, extentShpFile, EPSG_CRC_NUM);
  
  # Prepare Cenus Data
  censusData = PrepareCensusData2016( spatialData$CTUID )  
  
  #censusData = na.omit(censusData)  
  
  # Merge data and divide couts by the populations
  data = merge(spatialData, censusData, by.x="CTUID", by.y="CTUID") %>%
    mutate(CannabisPerCapita = (Cannabis_C / Pop2016) * 1000 ) %>%
    mutate(SchoolPerCapita = (Schoo_Coun / Pop2016) * 1000 ) %>%
    mutate(DayCarePerCapita = (Daycare_Co / Pop2016) * 1000 ) %>%
    mutate(Majority_m = factor(Majority_m)) %>%
    mutate(Minority_m = factor(Minority_m)) %>%
    
    # EDMONTON ZONING =========================================================
    #
    #      Majority_dGeneral Business Zone                                       0.00055948858  0.00025404107  2.20235              0.0295044 *  
    #      Majority_dIndustrial Business Zone                                    0.00255676150  0.00023840632 10.72439 < 0.000000000000000222 ***
    #      Majority_dSite Specific Development Control Provision                 0.00039298045  0.00014714794  2.67065              0.0085943 ** 
    #      Majority_dSmall Scale Infill Development Zone                         0.00022835468  0.00010533834  2.16782              0.0320982 *  
    #      Majority_dUrban Warehouse Zone                                        0.00066140985  0.00025916730  2.55206              0.0119331 *   
    #      Minority_dUrban Character Row Housing                                 0.00048834966  0.00017972720  2.71717              0.0075342 **
    #      Minority_dUrban Services Zone                                         0.00034486461  0.00017384509  1.98375              0.0495112 *  
    
    mutate(IsMajorityGeneralBizZone = as.integer(
      as.logical(Majority_d %in% c('General Business Zone')))) %>%
    
    mutate(IsMajorityIndustrialBizZone = as.integer(
      as.logical(Majority_d %in% c('Industrial Business Zone')))) %>%
    
    mutate(IsMajoritySpecificDevCtrlProvision = as.integer(
      as.logical(Majority_d %in% c('Specific Development Control Provision')))) %>%
    
    mutate(IsMajorityScaleInfillDevZone = as.integer(
      as.logical(Majority_d %in% c('Scale Infill Development Zone')))) %>%
    
    mutate(IsMajorityUrbanWarehouseZone = as.integer(
      as.logical(Majority_d %in% c('Urban Warehouse Zone')))) %>%
    
    mutate(IsMinorityUrbanCharacterRowHousing  = as.integer(
      as.logical(Minority_d %in% c('Urban Character Row Housing')))) %>%
    
    mutate(IsMinorityUrbanServicesZone  = as.integer(
      as.logical(Minority_d %in% c('Urban Services Zone')))) %>%
    
    mutate(IsEdmontonSigZone  = IsMajorityGeneralBizZone | IsMajorityIndustrialBizZone | IsMajoritySpecificDevCtrlProvision |
       IsMajorityScaleInfillDevZone | IsMajorityUrbanWarehouseZone | IsMinorityUrbanCharacterRowHousing | 
        IsMinorityUrbanServicesZone ) %>%
    
     mutate(IsEdmontonIndustrial  = as.integer(
      as.logical(Minority_m %in% c('Industrial')))) %>%
    
     mutate(IsCalgaryMediumIndustrial  = as.integer(
      as.logical(Majority_d %in% c('Medium Industrial Zone')))) %>%
    
     mutate(IsCalgarySingleDetachedMinority  = as.integer(
      as.logical(Minority_d %in% c('Single Detached Residential Zone')))) %>%
    
      mutate(IsEdmonCalgaryIndustrialOrResidential  = IsCalgarySingleDetachedMinority | 
          IsCalgaryMediumIndustrial | IsEdmontonIndustrial ) %>%
    
     
    

  
    # CALGARY ZONING ==========================================================
    #      Combine the spatial data and the Census:
      
    #      These are sigificant
    #      Majority_mCommercial - Core                       -1.25758043085   0.61630611148  -2.04051             0.04253885 *  
    #      Majority_mMajor Infrastructure                     1.16150499059   0.52885703189   2.19626             0.02915789 *  
    #      Majority_mResidential - Low Density               -0.80270442070   0.36083542370  -2.22457             0.02716421 * 
    
    mutate(IsCommercial = as.integer(
      as.logical(Majority_m %in% c('Commercial - Core')))) %>%
  
    mutate(IsResidential = as.integer(
      as.logical(Majority_m %in% c('Residential - Low Density', 'Residential - Medium Density')))) %>%
    
    mutate(IsLowResidential = as.integer(
      as.logical(Majority_m %in% c('Residential - Low Density')))) %>%
    
    mutate(IsMediumResidential = as.integer(
      as.logical(Majority_m %in% c('Residential - Medium Density')))) %>%
    
    mutate(IsDirectControl = as.integer(
      as.logical(Majority_m %in% c('Direct Control')))) %>%
    
    mutate(IsIndustrial = as.integer(
      as.logical(Majority_m %in% c('Industrial')))) %>%
    
    mutate(IsParksEducationOrRecreation = as.integer(
      as.logical(Majority_m %in% c('Parks, Recreation and Public Education')))) %>%
    
    mutate(IsTransportationCorridor = as.integer(
      as.logical(Majority_m %in% c('Transportation and Utility Corridor')))) %>%
    
    mutate(IsCommercialInfraOrLowDenResidential = as.integer(
      as.logical(Majority_m %in% c('Commercial - Core', 'Residential - Low Density', 'Direct Control',
        'Industrial', 'Parks, Recreation and Public Education', 'Residential - Medium Density',
        'Transportation and Utility Corridor')))) %>%
    
    mutate(MinorityLandCover = as.integer(
      as.logical(Minority_m %in% c('Industrial', 'Future Urban Development')))) %>%
    
    
    
    # Pair down the data
    dplyr::select(-one_of("CMAPUID",  "CMATYPE", "CTNAME", "PRUID", "PRNAME", 
      "CMAUID", "CMANAME", "Shape_Leng", "sum_Area_S", "Polygon_Co", "Join_ID")) 
      # "Minority_m", "Majority_m", "Minority_1", "Majority_1"
      #))
    
    # Turn the percentages into Z-Scores by scaling them
    # @see https://statisticsglobe.com/standardize-data-frame-columns-in-r-scale-function
    #mutate_at(c("CannabisPerCapita", "SchoolPerCapita", "DayCarePerCapita"), ~(scale(.) %>% as.vector))
  
  # plot centroids
  centroids = st_centroid(data$geometry)
  # plot(st_geometry(data))
  # plot(centroids, add = T, col = 'red', pch = 19)
  # 
  data$X = sapply(centroids, function(x) x[1])
  data$Y = sapply(centroids, function(x) x[2])
    
  rm(spatialData)
  rm(censusData)
  
  data
}

writeDataToFile = function(){
  csv_write(PrepareDataCalgary(), "data/CalgaryData.csv")
}


PrepareDataCalgary = function(EPSG_CRC_NUM = 4326){
  PrepareData("Calgary", NA, EPSG_CRC_NUM)%>%
    filter( CTUID %in% URBAN_CTS_CALGARY)
}



PrepareDataEdmonton = function(EPSG_CRC_NUM = 4326){
  PrepareData("Edmonton", NA, EPSG_CRC_NUM) %>%
    filter( CTUID %in% URBAN_CTS_EDMONTON)
}
