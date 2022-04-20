# ================================================================
# AUTHOR: Paul Walter
# DATE:   10/5/2021 	 
# ASSIGNMENT: Lab 2: Linear Regression and Inference
# LEARNING OBJECTIVES:
#   -	Review the concepts of regression.
#   -	Review the concept of classic hypothesis testing.
#   -	Develop a statistical model using a linear regression to describe the relationship between dependent and independent variables.
#   -	Critically analyze the statistical model you developed. 
# ================================================================

# source
source("src/London_SHP_DataPrep.Methods.R")

#Load software packages 

	library(dplyr)
	library(ggplot2)
	library(corrplot)
	library(psych)
	library(car)
	library(broom)
	library(foreign)

# SF libraries
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


EPSG_CRC_NUM = 32629 # London

# ================================================================
# Next, load your data:
# ================================================================

	#Load spreadsheet into data frame 
	
  #Set your working directory, so R knows where to look for files. Choose the LAB2 folder you just made.
	# setwd(choose.dir())
  # setwd("d:\\rfiles\\myfiles")

  # refined
  Data.Crime = read_csv("data/csv/london/refined/London.Crime.ByWard_2011-2022.csv")
  Data.PopDensity =   read_csv("data/csv/london/refined/London.PopDensity.ByWard_2011-2022.csv")

  # raw
  Data.Crime.Raw.1 = read_csv("data/csv/london/raw/MPS Ward Level Crime 2010-2020-03.csv")
  Data.Crime.Raw.2 = read_csv("data/csv/london/raw/MPS Ward Level Crime (most recent 24 months).csv")
  
  Data.PopDensity.Raw =   read_csv("data/csv/london/raw/housing-density-ward.csv")

	
  #Set the number of numeric digits to work with
	options(digits = 9)
	#This reads your csv file into R and names it "data"
	attach(data) 

# ================================================================
#	Load the Shapefile that has the NDVI
# ================================================================

  London.Wards <- read_sf("data/shp/london_by_ward/raw_shp_file/LondonWard.shp") # the shape file has all of canada, reduce it to only Calgary
  
  London.Wards.NDVI <- read_sf("data/shp/london_by_ward/refined/MeanNDVI/LondonMeanNdvi.shp") # the shape file has all of canada, reduce it to only Calgary
  
  createMap(
    London.Wards,
    "GSS_CODE",
    20,
    "Raw London",
    "pretty",
    "GnBu"
  )
	
	createMap(
    London.Wards.NDVI,
    "WardCod",
    20,
    "Refined NDVI",
    "pretty",
    "GnBu"
  )

# ================================================================
#	Find Intersect 
# 
# (There's something funky with the WardCodes, some don't match up)
# ================================================================
	intersection = unique(intersect(London.Wards.NDVI$WardCod, Data.Crime$WardCode )) 
	intersection2 = intersect(Data.PopDensity$WardCode, intersection)
	
	setdiff(London.Wards.NDVI$WardCod, Data.Crime$WardCode)
	length(setdiff(London.Wards.NDVI$WardCod, Data.Crime.Raw.1$WardCode))
	length(setdiff(London.Wards.NDVI$WardCod, Data.Crime.Raw.2$WardCode))
	length(setdiff(London.Wards.NDVI$WardCod, Data.PopDensity.Raw$Code))
	
	# Filter the data sets so they have the same length
	Data.Crime = filter(Data.Crime, WardCode %in% intersection2)
	London.Wards.NDVI = filter(London.Wards.NDVI, WardCod %in% intersection2)
	Data.PopDensity = filter(Data.PopDensity, WardCode %in% intersection2)
	
	AllData = left_join( London.Wards.NDVI, Data.Crime, by = c("WardCod" = "WardCode") )
	AllData = left_join( AllData, Data.PopDensity, by = c("WardCod" = "WardCode") )
	
	VARS_WE_NEED = c("WardCod", "Total2021", "Population_per_square_kilometre_2021", "MnN2021")
	AllData = AllData[,VARS_WE_NEED]
	
	createMap(
    AllData,
    "WardCod",
    20,
    "Refined NDVI",
    "pretty",
    "GnBu"
  )
	
# ================================================================
# We will start our analysis in R at Step 2: Identify explanatory 
# (i.e., independent) variables [x].
# ================================================================

	#Calculate correlation between variables 
	VARS_WE_NEED = c("Total2021", "Population_per_square_kilometre_2021", "MnN2021")
	AllDataFilteredColumns = AllData[,VARS_WE_NEED]%>%
    st_drop_geometry() # cast it as non spatial so we can ignore the geometery

	correlation_P <- cor(AllDataFilteredColumns, use="complete.obs", method="pearson")
	write.csv(correlation_P, "output/correlation_P.csv")
	corrplot(correlation_P, type = "upper", order = "hclust", method = "color",
	tl.cex = 0.7, tl.col = "black", tl.srt = 45)

# # ================================================================
# # Open up the spreadsheet "correlation_P.csv" and save is as an 
# # Excel Workbook (.xlsx) use the Conditional Formatting tip 
# # (see Step 2 above) to explore the correlation between variables.
# # ================================================================
# 
 	  # Section blank on purpose: read comment above. 

# # ================================================================
# # Next, run your initial linear regression model with all the 
# # independent variables left after Step 2. 
# # ================================================================
# 
# 	#Initial Regression model, name "modela"
# 	#First, insert all non-corrrelated variables
  	
	# holy f*ck! It doesn't work with the same variable from different colletions!
  	model0 <- lm( Data.Crime$Total2020 ~  London.Wards.NDVI$MnN2020  + 
  	    Data.PopDensity$Population_per_square_kilometre_2020 )
	
  	summary(model0) 
	
	
  	# Total
		modela <- lm( AllData$Total2021 ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	#Bicycle Theft, 
  	modela <- lm( AllData$`Year2021_Bicycle Theft` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	#drug trafficking,
  	modela <- lm( AllData$`Year2021_Drug Trafficking` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	#Exploitation of Prostitution, 
  	modela <- lm( AllData$`Year2021_Exploitation of Prostitution` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	# Soliciting for Pros
  	modela <- lm( AllData$`Year2021_Soliciting for Prostitution` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	#Going Equipped for Stealing, 
  	modela <- lm( AllData$`Year2021_Going Equipped for Stealing` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	#Homicide, 
  	modela <- lm( AllData$Year2021_Homicide ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	#Other Sexual Offences, 
  	modela <- lm( AllData$`Year2021_Other Sexual Offences` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	# Possession of Article with Blade or Point, 
  	modela <- lm( AllData$`Year2021_Possession of Article with Blade or Point` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	# Possession of Firearm with Intent, 
  	modela <- lm( AllData$`Year2021_Possession of Firearm with Intent` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	# Possession of Firearms Offences, 
  	modela <- lm( AllData$`Year2021_Possession of Firearms Offences` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	# Possession of Other Weapon, 
  	modela <- lm( AllData$`Year2021_Possession of Other Weapon` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	# Rape, 
  	modela <- lm( AllData$Year2021_Rape ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	# Theft from Person, 
  	modela <- lm( AllData$`Year2021_Theft from Person` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	#Violence with Injury
  	modela <- lm( AllData$`Year2021_Violence with Injury` ~  AllData$MnN2021  + 
  	    AllData$Population_per_square_kilometre_2021 )
	
  	summary(modela) 
  	
  	
  
  	# Residuals:
  	#   Min        1Q    Median        3Q       Max 
  	# -39816.00  -8406.44  -2193.69   5551.42 104202.30 
  	# 
  	#   Coefficients:
  	#   Estimate   Std. Error  t value   Pr(>|t|)    
  	#   (Intercept)          -4.24675e+05  8.36624e+04 -5.07605 7.6873e-07 ***
  	#   HH_Size               3.41586e+04  5.11976e+03  6.67192 1.7046e-10 ***
  	#   pop_density          -1.16610e+00  7.70144e-01 -1.51414  0.1312964    
  	#   married_prop         -7.35012e+03  3.61905e+04 -0.20310  0.8392310    
  	#   owner_prop            5.83681e+04  1.34370e+04  4.34384 2.0609e-05 ***
  	#   english_prop          2.63224e+05  8.25575e+04  3.18837  0.0016195 ** 
  	#   unemployed_prop      -1.53487e+04  1.07495e+05 -0.14278  0.8865792    
  	#   post_sec_prop         1.56157e+05  1.58294e+04  9.86497 < 2.22e-16 ***
  	#   citizen_non_ratio     4.04069e+02  1.63545e+02  2.47069  0.0141753 *  
  	#   aboriginal_non_ratio -6.65225e+04  3.09037e+04 -2.15258  0.0323411 *  
  	#   gender_ratio          3.47665e+04  1.52782e+04  2.27556  0.0237473 *  
  	#   age_15_64_prop        2.09042e+04  2.93626e+04  0.71193  0.4771930    
  	# ---
  	#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  	# 
  	# Residual standard error: 16973.8 on 242 degrees of freedom
  	# Multiple R-squared:  0.764977,	Adjusted R-squared:  0.754294 
  	# F-statistic: 71.6077 on 11 and 242 DF,  p-value: < 2.22e-16
  	
# 
# # ================================================================
# # Look at the summary table and identify the least significant 
# # variable. Remove that variable from the model and re-run the model. 
# # Repeat, until all the variables remaining are significant.
# # ================================================================
# 

#   # 1. State the hypotheses.
# 
#     	H0: There won't be a difference between a model with variable 'unemployed_prop' and a model without variable 'unemployed_prop'
#     	Ha: There will be a difference between a model with variable 'unemployed_prop' and a model without variable 'unemployed_prop'
# 
# 	# 2. State the appropriate statistical test.
# 
# 	    T-tests which R will be doing under the hood of me invoking the "summary" method on a LinearModel.
# 
# 	# 3. Choose the appropriate level of significance.
# 
# 	    I'm choosing 0.05 as my level of significance. (a significance level of 0.05 indicates a 5% risk of concluding that a difference exists when there is no actual difference)
# 
#   # 4. Determine regions of rejection.
# 
#   		If any of the P-values are higher than what they were, H0 will be rejected.
# 
#   # 5. Calculate test statistic.
# 
#          BEFORE:
# 
# 
# Residuals:
#   Min        1Q    Median        3Q       Max
# -39816.00  -8406.44  -2193.69   5551.42 104202.30
# 
# Coefficients:
#   Estimate   Std. Error  t value   Pr(>|t|)
# (Intercept)          -4.24675e+05  8.36624e+04 -5.07605 7.6873e-07 ***
#   HH_Size               3.41586e+04  5.11976e+03  6.67192 1.7046e-10 ***
#   pop_density          -1.16610e+00  7.70144e-01 -1.51414  0.1312964
# married_prop         -7.35012e+03  3.61905e+04 -0.20310  0.8392310
# owner_prop            5.83681e+04  1.34370e+04  4.34384 2.0609e-05 ***
#   english_prop          2.63224e+05  8.25575e+04  3.18837  0.0016195 **
#   unemployed_prop      -1.53487e+04  1.07495e+05 -0.14278  0.8865792
# post_sec_prop         1.56157e+05  1.58294e+04  9.86497 < 2.22e-16 ***
#   citizen_non_ratio     4.04069e+02  1.63545e+02  2.47069  0.0141753 *
#   aboriginal_non_ratio -6.65225e+04  3.09037e+04 -2.15258  0.0323411 *
#   gender_ratio          3.47665e+04  1.52782e+04  2.27556  0.0237473 *
#   age_15_64_prop        2.09042e+04  2.93626e+04  0.71193  0.4771930
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 16973.8 on 242 degrees of freedom
# Multiple R-squared:  0.764977,	Adjusted R-squared:  0.754294
# F-statistic: 71.6077 on 11 and 242 DF,  p-value: < 2.22e-16
# 
# 
#          AFTER REMOVING unemployed_prop:
# 
# 
# Residuals:
#   Min        1Q    Median        3Q       Max
# -38737.00  -9776.83  -1593.76   5732.71 112685.45
# 
# Coefficients:
#   Estimate  Std. Error   t value   Pr(>|t|)
# (Intercept)       -125047.391   10883.900 -11.48921 < 2.22e-16 ***
#   owner_prop          70480.242   10400.576   6.77657 8.8433e-11 ***
#   HH_Size             26141.079    3622.369   7.21657 6.4213e-12 ***
#   post_sec_prop      168821.048   11998.982  14.06961 < 2.22e-16 ***
#   citizen_non_ratio     544.421     133.756   4.07026 6.3103e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 17460.1 on 249 degrees of freedom
# Multiple R-squared:  0.744123,	Adjusted R-squared:  0.740012
# F-statistic: 181.031 on 4 and 249 DF,  p-value: < 2.22e-16
# 
# 
# # 6. Accept or reject null, explain why, and make conclusions.
# 
#       I'm rejecting H0 because, it's clear there is a difference in the Model excluding that var.
#       All except for 1, all P-values were lower and therefore more significant.
# 
#     	pop_density:                            0.3117592 -> 0.3096534                          (LOWER)
#     	married_prop                            < 2.22e-16 -> < 2.22e-16                        (NO CHANGE)
#     	english_prop                            0.3970337 -> 0.3901485                          (LOWER)
#     	post_sec_prop                           1.4296e-07-> 7.811e-08                          (LOWER)
#     	citizen_non_ratio                       0.1555930 -> 0.1329300                          (LOWER)
#     	aboriginal_non_ratio                    0.0765039 -> 0.0756628                          (LOWER)
#     	gender_ratio                            0.0593575 -> 0.0586374                          (LOWER)
#     	age_15_64_prop                          0.4985903 -> 0.4912208                          (LOWER)
# 
#     	My conclusion is that my Regression Model (and drawing a line with less R2 errors in between the other
#   	  Independent Variables) is more reliable without unemployed_prop.

  	# #Second regression model, name "modelb"
  	modelb <- lm(med_hh_income ~ HH_Size + pop_density + married_prop + owner_prop + english_prop +
  	               post_sec_prop + citizen_non_ratio + aboriginal_non_ratio + gender_ratio + age_15_64_prop, 
  	             data = data)

  	summary(modelb)
  	# 
  	# #Third regression model, removed Age 64
  	# modelc <- lm(med_hh_income ~  HH_Size + pop_density + married_prop + english_prop +
  	#                post_sec_prop + citizen_non_ratio + aboriginal_non_ratio + gender_ratio,
  	#              data = data)
  	# 
  	# summary(modelc)
  	# 
  	# #Fourth regression model, removed pop density
  	# modeld <- lm(med_hh_income ~ HH_Size + married_prop + english_prop +
  	#                post_sec_prop + citizen_non_ratio + aboriginal_non_ratio + gender_ratio,
  	#              data = data)
  	# 
  	# summary(modeld)
  	# 
  	# #Fifth regression model, removed english_prop
  	# modele <- lm(med_hh_income ~ HH_Size + married_prop +
  	#                post_sec_prop + citizen_non_ratio + aboriginal_non_ratio + gender_ratio,
  	#              data = data)
  	# 
  	# summary(modele)
  	# 
  	# #Sixth regression model, removed aboriginal_non_ratio
  	# modelf <- lm(med_hh_income ~ HH_Size + married_prop + post_sec_prop + citizen_non_ratio + gender_ratio, 
  	#              data = data)
  	# 
  	# summary(modelf)
  	
  	
  	# NOTE: Checking the initial variable for normality, my modelf's normality is W = 0.8218975, p-value = 2.25438e-16, which means it's not normal
  	# NOTE: None of them are normal? :-O!!!!!
  	print( shapiro.test(HH_Size) )            # W = 0.9902531, p-value = 0.0870689
  	print( shapiro.test(married_prop) )       # W = 0.9803572, p-value = 0.00139174
  	print( shapiro.test(post_sec_prop) )      # W = 0.9645721, p-value = 6.38605e-06
  	print( shapiro.test(citizen_non_ratio) )  # W = 0.7285085, p-value < 2.22e-16
  	print( shapiro.test(gender_ratio) )       # W = 0.7133164, p-value < 2.22e-16
  	
  	
  	#Sixth regression model, removed gender_ratio
  	#
  	#   NOTE: I'm keeping HH_Size because it's just a TINY bit correlated, 
  	#         and I think semantically it gives a valuable perspective.
  	#
  	modelg <- lm(med_hh_income ~ owner_prop + HH_Size + post_sec_prop + citizen_non_ratio, 
  	             data = data)
  	
  	summary(modelg)$coef
  	
  	# look at the variables
  	describe(med_hh_income)
  	describe(owner_prop)
  	describe(renter_prop)
  	describe(HH_Size)
  	describe(post_sec_prop)
  	describe(citizen_non_ratio)
  	
# # ================================================================
# # You can use some of the methods below to analyze model performance. 
# # Make sure you replace "model" in the script with the name of your 
# # final regression model (e.g., modelc).
# # ================================================================

	#Save fitted values and residuals into dataset
	data$residuals <- residuals(modelg)
	data$fitted <- fitted(modelg)
	detach(data)
	attach(data)

	# Test normality of residuals with Shapiro-Wilk's W test
	shapiro.test(data$residuals)
	
	# Shapiro-Wilk normality test
	# 
	# data:  data$residuals
	# W = 0.8172581, p-value < 2.22e-16
	
	shapiro.test(modela$residuals)
	shapiro.test(modelg$residuals)
	
	
	
	modelaSummary = summary(modela)
	modelgSummary = summary(modelg)
	
	# 2)  Determine the goodness-of-fit of your Model 1 to the data (R2), discuss the fit and 
	#     whether or not, based on this R2 value, your Model 1 is sufficient in describing your 
	#     dependent variable.
	
	modelaSummary$r.squared 
	modelaSummary$adj.r.squared
	
	modelgSummary$r.squared 
	modelgSummary$adj.r.squared
	
	# 3)  Investigate the overall significance of your Model 1 using the F-statistic reported with 
	#     your model summary output. Perform the 6 steps of classical hypothesis testing. Compare 
	#     and contrast the R2, adjusted R2, and F-statistic between your initial model from Lab 2 
	#     and your final model from Lab 2 (i.e., Model 1). Discuss how these measures changed, 
	#     and your reasoning for why these changes occurred.
	
	
	modelaSummary$fstatistic
	modelgSummary$fstatistic 
	
	# find critital value for f
	qf(0.05, 11, 242, lower.tail=FALSE) # model a
	qf(0.05, 4, 249, lower.tail=FALSE) # model b

	# pdf("output/histogram_residuals.pdf") #It will be saved into PDF file in your WD
	hist(data$residuals, breaks=15, col= "grey",
	     main="residuals",xlab="residuals")
	# dev.off()
	
	
write_sf(AllData, "data/shp/london_by_ward/refined/AllData/London.AllData.shp" )
