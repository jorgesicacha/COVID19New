###########################
## COVID19 Data Analysis ##
###########################

## First dataset: coronastatus.no ##
## Reading from API ##

library(httr)
library(jsonlite)
library(raster)
library(pbapply)
library(tidyverse)
library(ggspatial)
library(plyr)
library(mapview)
library(tmap)
library(sf)

options(stringsAsFactors = FALSE)

# Jorge's code -> Get the data from the API ----
## Aggregated data @ postcode level ##

## List of postcodes ##
postcode <- read_sf("/Users/jorgespa/Documents/Research/CORONA/PostCodeArea_Clipped.shp")
table(duplicated(postcode$POSTNUMMER)) 
# 70 duplicated postcode and same kommune nb ... Is it normal???
postnummer <- unique(postcode$POSTNUMMER)

## Retrieving data 
aggdata <- function(postnummer){
  raw0 <- GET(url="https://coronastatus.no",path=paste0("api/aggregated/",postnummer))
  raw1 <- rawToChar(raw0$content)
  raw2 <- fromJSON(raw1)
  info <- do.call("cbind",raw2)
  return(info)
}

data0 <- list()
for(i in 1:length(postnummer)){
  data0[[i]] <- aggdata(postnummer[i])
}

# Create the combined dataset CoronaData + PosteCodeArea
idxs <- which(lapply(data0, length)==6)
data <- as.data.frame(do.call("rbind",data0[idxs]))
data <- data[,-1]
names(data)[1] <- "POSTNUMMER"
data <- as.data.frame(apply(data,2,as.numeric))
cases_post <- full_join(postcode,data,by="POSTNUMMER")

# Check if the join was successful
sum(data$numberOfPeopleShowingSymptoms) #2696 # 2701
sum(cases_post$numberOfPeopleShowingSymptoms, na.rm = TRUE) #2717 # 2722

# The full join add 21 cases // 
# I think the duplicated in the postcode data influence this -> Ask Jan Ketil
# If they are real duplicates or if they really represent something!

# Ben's code
# Ben's code -> Interactive map and gathering data to get nb of cases per 1000 habitants ----
# Make the interactive map ----

# Quickly viz data from the API
tmap_mode('view')
for_map <- cases_post %>% dplyr::select(numberOfPeopleShowingSymptoms)
map <- tm_shape(for_map) + 
  tm_polygons(col = "numberOfPeopleShowingSymptoms") +
  tm_borders()
map

# Get population density per municipality (from Jan's shapefile)
ward_pop <- read_sf('/Users/jorgespa/Documents/Research/CORONA/PopByWard_Clipped.shp')

ward_pop_mun <- ward_pop %>% 
  dplyr::group_by(kommunenum) %>% 
  dplyr::summarise(totalbefol = sum(totalbefol),
                   antallmenn = sum(antallmenn),
                   antallkvin = sum(antallkvin),
                   age_0_4 = sum(befolkning),
                   age_5_9 = sum(befolkni_1 ),
                   age_10_14 = sum(befolkni_2),
                   age_15_19 = sum(befolkni_3),
                   age_20_24 = sum(befolkni_4),
                   age_25_29 = sum(befolkni_5),
                   age_30_34 = sum(befolkni_6),
                   age_35_39 = sum(befolkni_7),
                   age_40_44 = sum(befolkni_8),
                   age_45_49 = sum(befolkni_9),
                   age_50_54 = sum(befolkn_10),
                   age_55_59 = sum(befolkn_11),
                   age_60_64 = sum(befolkn_12),
                   age_65_69 = sum(befolkn_13),
                   age_70_74 = sum(befolkn_14),
                   age_75_79 = sum(befolkn_15),
                   age_80_84 = sum(befolkn_16),
                   age_85_89 = sum(befolkn_17),
                   age_90_plus = sum(befolkn_18))

sum(ward_pop_mun$totalbefol) 
# See if I didn't screw, 5 million people in Norway is coherent

# Quick viz of total nb of people per municipality
ward_pop_mun_map <- ward_pop_mun %>% dplyr::select(totalbefol)
tm_shape(ward_pop_mun_map) +
  tm_polygons(col = "totalbefol")

cases_post_centr <- st_centroid(cases_post)
sum(cases_post_centr$numberOfPeopleShowingSymptoms, na.rm = TRUE)

# For intersection
kommun <- ward_pop_mun %>% dplyr::select(kommunenum)
df_intersect <- st_intersection(kommun, cases_post_centr) ##Some information is lost here? Example: kommnumm <- 5040
sum(df_intersect$numberOfPeopleShowingSymptoms, na.rm=TRUE) # 2684

df_intersect <- df_intersect %>%  
  dplyr::group_by(kommunenum) %>% 
  dplyr::summarise(Reports = sum(numberOfReports, na.rm = TRUE),
                   Symptoms = sum(numberOfPeopleShowingSymptoms, na.rm = TRUE),
                   Cinfected = sum(numberOfConfirmedInfected, na.rm = TRUE),
                   Tested = sum(numberOfTested, na.rm = TRUE)) %>% 
  st_drop_geometry()
sum(df_intersect$Symptoms, na.rm=TRUE)


# Gather 
coronadf_pop <- full_join(ward_pop_mun, df_intersect, by = 'kommunenum')

# Check if the population number is the same as previously
sum(coronadf_pop$Symptoms, na.rm=TRUE)
sum(coronadf_pop$totalbefol) # all good too, dataset seems good!

# Create a column for the population in thousand and number of cases per 1000 people
coronadf_pop <- coronadf_pop %>% 
  mutate(pop_1000 = totalbefol / 1000) %>% 
  mutate(symptoms_1000 = Symptoms / pop_1000)

# Map of symptoms per 1000
coronadf_pop_map <- coronadf_pop %>% dplyr::select(symptoms_1000,kommunenum,totalbefol)
tm_shape(coronadf_pop_map) +
  tm_polygons(col = "symptoms_1000")
