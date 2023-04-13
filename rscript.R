
########### Required Packages ###########
packages = c("stars","starsExtra", "abind","tigris","tidycensus","dyplyr",
             "bayesplot", "lme4","RcppEigen","knitr","yardstick",
             "tidyverse", "tidyr", "broom", "caret", "dials", "doParallel", "e1071", "earth",
             "ggrepel", "glmnet", "ipred", "klaR", "kknn", "pROC", "rpart", "randomForest",
             "sessioninfo", "tidymodels","ranger", "recipes", "workflows", "themis","xgboost",
             "sf", "nngeo", "mapview","raster")


########### Define Functions ###########

Load.fun <- function(x) { 
  x <- as.character(x) 
  if(isTRUE(x %in% .packages(all.available=TRUE))) { 
    eval(parse(text=paste("require(", x, ")", sep=""))) 
    print(paste(c(x, " : already installed; requiring"), collapse=''))
  } else { 
    #update.packages()
    print(paste(c(x, " : not installed; installing"), collapse=''))
    eval(parse(text=paste("install.packages('", x, "')", sep=""))) 
    print(paste(c(x, " : installed and requiring"), collapse=''))
    eval(parse(text=paste("require(", x, ")", sep=""))) 
  } 
} 

for(i in seq_along(packages)){
  packge <- as.character(packages[i])
  Load.fun(packge)
}

plotTheme <- function(base_size = 12, title_size = 12) {
  theme(
   text = element_text( color = "black"),
   plot.title = element_text(size = title_size, colour = "black"), 
        plot.subtitle = element_text(face="italic"),
       plot.caption = element_text(hjust=0),
         axis.ticks = element_blank(),
         panel.background = element_blank(),
         panel.grid.minor = element_line("grey90", size = 0.01),
         panel.grid.major = element_line("grey90", size = 0.01),
         panel.border = element_rect(colour = "white" , fill=NA, size=0),
         strip.background = element_rect(color = "white",fill='white'),
         strip.text = element_text(size=12),
         axis.title = element_text(size=12),
         axis.text = element_text(size=10),
         plot.background = element_blank(),
         legend.background = element_blank(),
         legend.title = element_text(colour = "black", face = "italic"),
         legend.text = element_text(colour = "black", face = "italic"),
         strip.text.x = element_text(size = 8)
       )
   }
mapTheme <- theme(plot.title =element_text(size=12),
                  plot.subtitle = element_text(size=8),
                  plot.caption = element_text(size = 6),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(colour = 'transparent'),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "right",
                  plot.margin = margin(1, 1, 1, 1, 'cm'),
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))

palette2 <- c("#41b6c4","#253494")
palette4 <- c("#a1dab4","#41b6c4","#2c7fb8","#253494")
palette5 <- c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494")
palette10 <- c("#f7fcf0","#e0f3db","#ccebc5","#a8ddb5","#7bccc4",
                        "#4eb3d3","#2b8cbe","#0868ac","#084081","#f7fcf0")

#########################################################################################################################################
###################################################  --------------------  ##############################################################
################################################### |                    | ##############################################################
################################################### |  DATA PREPARATION  | ##############################################################
################################################### |                    | ##############################################################
###################################################  --------------------  ##############################################################
#########################################################################################################################################

# load all the land cover data
port_14 <- read_stars("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/lc/port_51740_lc_2014/port_51740_landcover_2014.tif") 
port_18 <- read_stars("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/lc/port_51740_lc_2018/port_51740_landcover_2018.tif")
james_14 <- read_stars("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/lc/jame_51095_lc_2014/jame_51095_landcover_2014.tif")
james_18 <- read_stars("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/lc/jame_51095_lc_2018/jame_51095_landcover_2018.tif")

isle_14_10 <- read_stars("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/lc/islelc_14_10x10.tif")
isle_18_10 <- read_stars("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/lc/islelc_18_10x10.tif")

# load all the boundary data 
port_area <- st_read("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/boundary/portsmouth.shp")
james_area <- st_read("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/boundary/James.shp")
isle_area <- st_read("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/boundary/Isle_of_Wight.shp")

#############################################################################################################################
#############################################  --------------------  ########################################################
############################################# | LANDCOVER DATASET  | ########################################################
#############################################  --------------------  ########################################################
#############################################################################################################################


# Resample the raster image to 10x10 cells
port_14_10 <- st_warp(port_14, cellsize = 10, crs = st_crs(port_14))
port_18_10 <- st_warp(port_18, cellsize = 10, crs = st_crs(port_18))
jame_14_10 <- st_warp(jame_14, cellsize = 10, crs = st_crs(jame_14))
jame_18_10 <- st_warp(jame_18, cellsize = 10, crs = st_crs(jame_18))


# Crop the resampled raster image to the test area
port_14_10_crop <- st_crop(port_14_10, port_area)
port_18_10_crop <- st_crop(port_18_10, port_area)
jame_14_10_crop <- st_crop(jame_14_10, jame_area)
jame_18_10_crop <- st_crop(jame_18_10, jame_area)
isle_14_10_crop <- st_crop(isle_14, isle_area)
isle_18_10_crop <- st_crop(isle_18, isle_area)


# Create land cover change feature
port_change <- c(port_18_10_crop - port_14_10_crop)%>%
  mutate(lcchange = case_when(port_51740_landcover_2018.tif != 0 ~ 1,
                              port_51740_landcover_2018.tif != 0 ~ 0))
isle_change <- c(isle_18_10_crop - isle_14_10_crop)%>%
  mutate(lcchange = case_when(islelc_18_10x10.tif != 0 ~ 1,
                              islelc_18_10x10.tif != 0 ~ 0))

# Reclassify the cropped raster image
port_14_10_rc <- port_14_10_crop %>% 
  mutate(
    originallc =  port_51740_landcover_2014.tif,
    lc = case_when(
    port_51740_landcover_2014.tif < 6  ~ 0, # impervious
    port_51740_landcover_2014.tif >= 6 ~ 1 # previous
  ))
port_18_10_rc <- port_18_10_crop %>% 
  mutate(
    originallc =  port_51740_landcover_2018.tif,
    lc = case_when(
    port_51740_landcover_2018.tif < 6  ~ 0,
    port_51740_landcover_2018.tif >= 6 ~ 1
  ))
jame_14_10_rc <- jame_14_10_crop %>% 
  mutate(originallc = jame_51095_landcover_2014.tif,
         lc = case_when(
         jame_51095_landcover_2014.tif < 6  ~ 0, # permeable
         jame_51095_landcover_2014.tif >= 6 ~ 1 # impermeable
         ))
jame_18_10_rc <- jame_18_10_crop %>% 
  mutate(originallc = jame_51095_landcover_2018.tif,
         lc = case_when(
           jame_51095_landcover_2018.tif < 6  ~ 0,
           jame_51095_landcover_2018.tif >= 6 ~ 1
         ))

isle_14_10_rc <- isle_14_10_crop %>% 
  mutate(originallc = case_when(
    islelc_14_10x10.tif == 1  ~ 1,
    islelc_14_10x10.tif == 2 ~ 2,
    islelc_14_10x10.tif == 3 ~ 3,
    islelc_14_10x10.tif == 4 ~ 4,
    islelc_14_10x10.tif == 5 ~ 5,
    islelc_14_10x10.tif == 6 ~ 6,
    islelc_14_10x10.tif == 7 ~ 7,
    islelc_14_10x10.tif == 8 ~ 8,
    islelc_14_10x10.tif == 9 ~ 9,
    islelc_14_10x10.tif == 10 ~ 10,
    islelc_14_10x10.tif == 11  ~ 11,
    islelc_14_10x10.tif == 12 ~ 12
  ),
  lc = case_when(
    islelc_14_10x10.tif < 6  ~ 0, # permeable
    islelc_14_10x10.tif >= 6 ~ 1 # impermeable
  ))
isle_18_10_rc <- isle_18_10_crop %>% 
  mutate(
    originallc =case_when(
      islelc_18_10x10.tif == 1  ~ 1,
      islelc_18_10x10.tif == 2 ~ 2,
      islelc_18_10x10.tif == 3 ~ 3,
      islelc_18_10x10.tif == 4 ~ 4,
      islelc_18_10x10.tif == 5 ~ 5,
      islelc_18_10x10.tif == 6 ~ 6,
      islelc_18_10x10.tif == 7 ~ 7,
      islelc_18_10x10.tif == 8 ~ 8,
      islelc_18_10x10.tif == 9 ~ 9,
      islelc_18_10x10.tif == 10 ~ 10,
      islelc_18_10x10.tif == 11  ~ 11,
      islelc_18_10x10.tif == 12 ~ 12
    ),
    lc = case_when(
      islelc_18_10x10.tif < 6  ~ 0,
      islelc_18_10x10.tif >= 6 ~ 1
    ))
# Create reclassed land cover change feature
port_change_rc <- (port_18_10_rc - port_14_10_rc)
jame_change_rc <- (jame_18_10_rc - jame_14_10_rc)
isle_change_rc <- (isle_18_10_rc - isle_14_10_rc)

#remove
rm(port_14_10,port_18_10,port_14,port_18)
rm(jame_14_10,jame_18_10,jame_14,jame_18)
rm(isle_14_10,isle_18_10,isle_14,isle_18)



#############################################################################################################################
#############################################  --------------------  ########################################################
############################################# |  CENSCUS DATASET   | ########################################################
#############################################  --------------------  ########################################################
#############################################################################################################################


## load census

options(tigris_use_cache = TRUE)

census_api_key("bd4ce20561125a8480632db6acb29869e040ed08",install = TRUE)

# Define function to retrieve census data for a given county and year
get_county_data <- function(state, county, year) {
  get_acs(
    geography = "block group",
    variables = c("B01003_001E","B02001_002E","B19013_001E","B25002_001E","B06012_002E","B27011_008E"),
    year = year,
    state = state,
    county = county,
    geometry = TRUE,
    output = "wide"
  ) %>%
    st_transform(st_crs(port_18_10_rc)) %>%
    rename(
      TotalPop = B01003_001E,
      Whites = B02001_002E,
      MedHHInc = B19013_001E,
      TotalUnit = B25002_001E
    ) %>%
    mutate(
      pctWhite = ifelse(TotalPop > 0, Whites / TotalPop * 100, 0),
      area = st_area(geometry)
    ) %>%
    dplyr::select(-NAME, -starts_with("B"),-Whites)%>%
    fill(everything())
}

# Define function to calculate changes in census variables between two years
calculate_changes <- function(data1, data2) {
  data2 %>%
    st_join(data1,left = TRUE) %>%
    mutate(
      popchange = (TotalPop.y - TotalPop.x) / (TotalPop.x * area.x),
      pctwhitechange = (pctWhite.y - pctWhite.x),
      Unitchange = (TotalUnit.y - TotalUnit.x) / area.x,
      MedHHIncchange = (MedHHInc.y - MedHHInc.x) / area.x,
      Area = area.x,
      GEOID = GEOID.x
    )%>%
    dplyr::select(-ends_with(".x"),-ends_with(".y"))
}

# Retrieve census data for Portsmouth in 2014 and 
portsmouth_14 <- get_county_data(state = "51", county = "Portsmouth", year = 2014)
portsmouth_18 <- get_county_data(state = "51", county = "Portsmouth", year = 2018)
portsmouth_21 <- get_county_data(state = "51", county = "Portsmouth", year = 2021)
# Calculate changes in census variables between 2014 and 2018
port_tract <- calculate_changes(data1 = portsmouth_14, data2 = portsmouth_18)
port_tract2 <- calculate_changes(data1 = portsmouth_18, data2 = portsmouth_21)

# Repeat for James City and Isle of Wight counties
james_city_14 <- get_county_data(state = "51", county = "James City", year = 2014)
james_city_18 <- get_county_data(state = "51", county = "James City", year = 2018)
james_city_21 <- get_county_data(state = "51", county = "James City", year = 2021)
jame_tract <- calculate_changes(data1 = james_city_14, data2 = james_city_18)
jame_tract2 <- calculate_changes(data1 = james_city_18, data2 = james_city_21)

isle_of_wight_14 <- get_county_data(state = "51", county = "Isle of Wight", year = 2014)
isle_of_wight_18 <- get_county_data(state = "51", county = "Isle of Wight", year = 2018)
isle_of_wight_21 <- get_county_data(state = "51", county = "Isle of Wight", year = 2021)
isle_tract <- calculate_changes(data1 = isle_of_wight_14, data2 = isle_of_wight_18)
isle_tract2 <- calculate_changes(data1 = isle_of_wight_18, data2 = isle_of_wight_21)

#############################################################################################################################
#############################################  --------------------  ########################################################
############################################# |    DEM DATASET     | ########################################################
#############################################  --------------------  ########################################################
#############################################################################################################################


dem1 <- read_stars("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/DEM/USGS_1_n38w077_20170509.tif")
dem2 <- read_stars("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/DEM/USGS_1_n37w077_20160315.tif")
dem <- st_mosaic(dem1, dem2)
dem2 <- st_warp(dem2, crs=st_crs(port_14))
dem_port <- st_crop (dem2, port_area)
dem_port <- st_warp(dem_port, port_14)
port_slope<- slope(dem_port)

dem1 <- st_warp(dem1, crs=st_crs(jame_area))
dem_james <- st_crop (dem1, jame_area)
dem_james <- st_warp(dem_james, jame_18_10_crop)
james_slope<- slope(dem_james)

dem2 <- st_warp(dem, crs=st_crs(isle_14_10_rc))
dem_isle <- st_crop (dem2, isle_area)
dem_isle <- st_warp(dem_isle, isle_14_10_crop)
isle_slope<- slope(dem_isle)
rm(dem1,dem2,dem)

#############################################################################################################################
#############################################  --------------------  ########################################################
############################################# |    SOIL DATASET    | ########################################################
#############################################  --------------------  ########################################################
#############################################################################################################################

# Portmouth
soil <- st_read('~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/Soil/Port/port_soil.shp', crs= 'EPSG:4326')
soilclass <- soil%>%
  mutate(soil = case_when(
    Port__Rati == 'D' ~ 6,
    Port__Rati == 'C' ~ 5,
    Port__Rati == 'B/D' ~ 4,
    Port__Rati == 'B' ~ 3,
    Port__Rati == 'A/D' ~ 2,
    Port__Rati == 'A' ~ 1,
    is.na(Port__Rati) ~ 0
  )) %>%
  dplyr::select(-colnames(soil),geometry)%>%
  st_transform(st_crs(port_14_10_rc))

soil_port <- st_crop(soilclass, port_area)

# James City
soil <- st_read('~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/Soil/James/James_soil.shp', crs= 'EPSG:4326')
soilclass <- soil%>%
  mutate(soil = case_when(
    Report___8 == 'D' ~ 6,
    Report___8 == 'C' ~ 5,
    Report___8 == 'B/D' ~ 4,
    Report___8 == 'B' ~ 3,
    Report___8 == 'A/D' ~ 2,
    Report___8 == 'A' ~ 1,
    is.na(Report___8) ~ 0
  )) %>%
  dplyr::select(-colnames(soil),geometry)%>%
  st_transform(st_crs(jame_14_10_rc))

soil_jame <- st_crop(soilclass, jame_area)

# Isle of Wight
soil <- st_read('~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/Soil/isle/isle_soil.shp', crs= 'EPSG:4326')
soilclass <- soil%>%
  mutate(soil = case_when(
    isle__Ra_1 == 'D' ~ 6,
    isle__Ra_1 == 'C' ~ 5,
    isle__Ra_1 == 'B/D' ~ 4,
    isle__Ra_1 == 'B' ~ 3,
    isle__Ra_1 == 'A/D' ~ 2,
    isle__Ra_1 == 'A' ~ 1,
    is.na(isle__Ra_1) ~ 0
  )) %>%
  dplyr::select(-colnames(soil),geometry)%>%
  st_transform(st_crs(isle_14_10_rc))

soil_isle <- st_crop(soilclass, isle_area)
#############################################################################################################################
#############################################  --------------------  ########################################################
############################################# | SPATIAL CALCULATION | #######################################################
#############################################  --------------------  ########################################################
#############################################################################################################################

port_14_pcnt_imperv = focal2(port_14_10_rc["lc"], matrix(1, 3, 3), "mean")
port_18_pcnt_imperv = focal2(port_18_10_rc["lc"], matrix(1, 3, 3), "mean")
jame_14_pcnt_imperv = focal2(jame_14_10_rc["lc"], matrix(1, 3, 3), "mean")
jame_18_pcnt_imperv = focal2(jame_18_10_rc["lc"], matrix(1, 3, 3), "mean")
isle_14_pcnt_imperv = focal2(isle_14_10_rc["lc"], matrix(1, 3, 3), "mean")
isle_18_pcnt_imperv = focal2(isle_18_10_rc["lc"], matrix(1, 3, 3), "mean")


# Function to calculate focal mean
calc_focal_mean <- function(df, field, ksize) {
  focal2(df[field], matrix(1, ksize, ksize), "mean") %>%
    focal2(., matrix(1, ksize, ksize), "mean") %>%
    focal2(., matrix(1, ksize, ksize), "mean")
}


#############################################################################################################################
#############################################  --------------------  ########################################################
############################################# |     PORTMOUTH      | #######################################################
#############################################  --------------------  ########################################################
#############################################################################################################################


# Road
port_14_road <- port_14_10_crop %>%
  mutate(road = as.numeric(port_51740_landcover_2014.tif == 9))

port_18_road <- port_18_10_crop %>%
  mutate(road = as.numeric(port_51740_landcover_2018.tif == 9))

road_14 <- calc_focal_mean(port_14_road, "road", 3)
road_18 <- calc_focal_mean(port_18_road, "road", 3)

# Canopy
port_14_canopy <- port_14_10_crop %>%
  mutate(canopy = as.numeric(port_51740_landcover_2014.tif == 3))

port_18_canopy <- port_18_10_crop %>%
  mutate(canopy = as.numeric(port_51740_landcover_2018.tif == 3))

canopy_14_mean <- calc_focal_mean(port_14_canopy, "canopy", 3)
canopy_18_mean <- calc_focal_mean(port_18_canopy, "canopy", 3)

# Shrub
port_14_shrub <- port_14_10_crop %>%
  mutate(shrub = as.numeric(port_51740_landcover_2014.tif == 2))

port_18_shrub <- port_18_10_crop %>%
  mutate(shrub = as.numeric(port_51740_landcover_2018.tif == 2))

shrub_14_mean <- calc_focal_mean(port_14_shrub, "shrub", 3)
shrub_18_mean <- calc_focal_mean(port_18_shrub, "shrub", 3)

# Water
port_14_water <- port_14_10_crop %>%
  mutate(water = as.numeric(port_51740_landcover_2014.tif == 1))

port_18_water <- port_18_10_crop %>%
  mutate(water = as.numeric(port_51740_landcover_2018.tif == 1))

water_14_mean <- calc_focal_mean(port_14_water, "water", 25)
water_18_mean <- calc_focal_mean(port_18_water, "water", 25)

# Other
port_14_other <- port_14_10_crop %>%
  mutate(other = as.numeric(port_51740_landcover_2014.tif == 8))

port_18_other <- port_18_10_crop %>%
  mutate(other = as.numeric(port_51740_landcover_2018.tif == 8))

other_14_mean <- focal2(port_14_other["other"], matrix(1, 3, 3), "mean")
other_14_mean <- focal2(other_14_mean["other"], matrix(1, 3, 3), "mean")
other_18_mean <- focal2(port_18_other["other"], matrix(1, 3, 3), "mean")
other_18_mean<- focal2(other_18_mean["other"], matrix(1, 3, 3), "mean")

## join all the data

port14 <- cbind(
  as.data.frame(canopy_14_mean)['canopy'],
  as.data.frame(road_14)['road'],
  as.data.frame(other_14_mean)['other'],
  as.data.frame(shrub_14_mean)['shrub'],
  as.data.frame(water_14_mean)['water'],
  as.data.frame(port_14_pcnt_imperv)['lc'] %>%
    rename(imperv = lc ),
  as.data.frame(port_change_rc)[4]%>%
    rename(lcchange = lc )
)

port18 <- cbind(
  as.data.frame(canopy_18_mean)['canopy'],
  as.data.frame(road_18)['road'],
  as.data.frame(other_18_mean)['other'],
  as.data.frame(shrub_18_mean)['shrub'],
  as.data.frame(water_18_mean)['water'],
  as.data.frame(port_18_pcnt_imperv)['lc'] %>%
    rename(imperv = lc )
)


port_14 <- 
  st_join(port_14_10_rc,port_tract) %>%
  st_join(.,dem_port)%>% # not changing
  st_join(.,port_slope) %>%
  st_join(., soil_port)


port_18 <- 
  st_join(port_18_10_rc,port_tract2) %>%
  st_join(.,dem_port)%>% # not changing
  st_join(.,port_slope) %>%
  st_join(., soil_port)

rm(dem_port,port_slope, port_14_10_crop, port_14_10_rc, port_14_canopy, port_14_other, port_14_road, port_14_water, port_18_10_crop, port_18_10_rc, port_18_canopy, port_18_other, port_18_road, port_18_water)


port14_df <- 
  as.data.frame(port_14) %>% 
  rename(
    terrain = USGS_1_n37w077_20160315.tif )%>%
  cbind(., port14)%>% na.omit()
port18_df <- 
  as.data.frame(port_18) %>% 
  rename(
    terrain = USGS_1_n37w077_20160315.tif) %>%
  cbind(., port18)%>%na.omit()

saveRDS(port14_df, "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/port14_df.rds")
saveRDS(port18_df, "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/port18_df.rds")


#############################################################################################################################
#############################################  --------------------  ########################################################
############################################# |      JAME CITY     | ########################################################
#############################################  --------------------  ########################################################
#############################################################################################################################

# Road
jame_14_road <- jame_14_10_crop %>%
  mutate(road = as.numeric(jame_51095_landcover_2014.tif == 9))

jame_18_road <- jame_18_10_crop %>%
  mutate(road = as.numeric(jame_51095_landcover_2018.tif == 9))

road_14 <- calc_focal_mean(jame_14_road, "road", 3)
road_18 <- calc_focal_mean(jame_18_road, "road", 3)

# Canopy
jame_14_canopy <- jame_14_10_crop %>%
  mutate(canopy = as.numeric(jame_51095_landcover_2014.tif == 3))

jame_18_canopy <- jame_18_10_crop %>%
  mutate(canopy = as.numeric(jame_51095_landcover_2018.tif == 3))

canopy_14_mean <- calc_focal_mean(jame_14_canopy, "canopy", 3)
canopy_18_mean <- calc_focal_mean(jame_18_canopy, "canopy", 3)

# Shrub
jame_14_shrub <- jame_14_10_crop %>%
  mutate(shrub = as.numeric(jame_51095_landcover_2014.tif == 2))

jame_18_shrub <- jame_18_10_crop %>%
  mutate(shrub = as.numeric(jame_51095_landcover_2018.tif == 2))

shrub_14_mean <- calc_focal_mean(jame_14_shrub, "shrub", 3)
shrub_18_mean <- calc_focal_mean(jame_18_shrub, "shrub", 3)

# Water
jame_14_water <- jame_14_10_crop %>%
  mutate(water = as.numeric(jame_51095_landcover_2014.tif == 1))

jame_18_water <- jame_18_10_crop %>%
  mutate(water = as.numeric(jame_51095_landcover_2018.tif == 1))

water_14_mean <- calc_focal_mean(jame_14_water, "water", 25)
water_18_mean <- calc_focal_mean(jame_18_water, "water", 25)

# Other
jame_14_other <- jame_14_10_crop %>%
  mutate(other = as.numeric(jame_51095_landcover_2014.tif == 8))

jame_18_other <- jame_18_10_crop %>%
  mutate(other = as.numeric(jame_51095_landcover_2018.tif == 8))

other_14_mean <- focal2(jame_14_other["other"], matrix(1, 3, 3), "mean")
other_14_mean <- focal2(other_14_mean["other"], matrix(1, 3, 3), "mean")
other_18_mean <- focal2(jame_18_other["other"], matrix(1, 3, 3), "mean")
other_18_mean<- focal2(other_18_mean["other"], matrix(1, 3, 3), "mean")

## join all the data

jame14 <- cbind(
  as.data.frame(canopy_14_mean)['canopy'],
  as.data.frame(road_14)['road'],
  as.data.frame(other_14_mean)['other'],
  as.data.frame(shrub_14_mean)['shrub'],
  as.data.frame(water_14_mean)['water'],
  as.data.frame(jame_14_pcnt_imperv)['lc'] %>%
    rename(imperv = lc ),
  as.data.frame(jame_change_rc)['lc']%>%
    rename(lcchange = lc )
)

jame18 <- cbind(
  as.data.frame(canopy_18_mean)['canopy'],
  as.data.frame(road_18)['road'],
  as.data.frame(other_18_mean)['other'],
  as.data.frame(shrub_18_mean)['shrub'],
  as.data.frame(water_18_mean)['water'],
  as.data.frame(jame_18_pcnt_imperv)['lc'] %>%
    rename(imperv = lc )
)
jame_14 <- 
  st_join(jame_14_10_rc,jame_tract) %>%
  st_join(.,dem_james)%>% # not changing
  st_join(.,james_slope) %>%
  st_join(., soil_jame)


jame_18 <- 
  st_join(jame_18_10_rc,jame_tract2) %>%
  st_join(.,dem_james)%>% # not changing
  st_join(.,james_slope) %>%
  st_join(., soil_jame)

rm(dem_jame,jame_slope, jame_14_10_crop, jame_14_10_rc, jame_14_canopy, jame_14_other, jame_14_road, jame_14_water, jame_18_10_crop, jame_18_10_rc, jame_18_canopy, jame_18_other, jame_18_road, jame_18_water)

jame14_df <- 
  as.data.frame(jame_14) %>% 
  rename(
    terrain = USGS_1_n38w077_20170509.tif )%>%
  cbind(., jame14)  %>%
  na.omit() 
jame18_df <- 
  as.data.frame(jame_18) %>% 
  rename(
    terrain = USGS_1_n38w077_20170509.tif) %>%
  cbind(., jame18)%>%
  na.omit()

saveRDS(jame14_df, "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/jame14_df.rds")
saveRDS(jame18_df, "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/jame18_df.rds")

#############################################################################################################################
#############################################  --------------------  ########################################################
############################################# |   ISLE of WIGHT    | ########################################################
#############################################  --------------------  ########################################################
#############################################################################################################################

# Road
isle_14_road <- isle_14_10_crop %>%
  mutate(road = as.numeric(islelc_14_10x10.tif == 9))

isle_18_road <- isle_18_10_crop %>%
  mutate(road = as.numeric(islelc_18_10x10.tif == 9))

road_14 <- calc_focal_mean(isle_14_road, "road", 3)
road_18 <- calc_focal_mean(isle_18_road, "road", 3)

# Canopy
isle_14_canopy <- isle_14_10_crop %>%
  mutate(canopy = as.numeric(islelc_14_10x10.tif == 3))

isle_18_canopy <- isle_18_10_crop %>%
  mutate(canopy = as.numeric(islelc_18_10x10.tif == 3))

canopy_14_mean <- calc_focal_mean(isle_14_canopy, "canopy", 3)
canopy_18_mean <- calc_focal_mean(isle_18_canopy, "canopy", 3)

# Shrub
isle_14_shrub <- isle_14_10_crop %>%
  mutate(shrub = as.numeric(islelc_14_10x10.tif == 2))

isle_18_shrub <- isle_18_10_crop %>%
  mutate(shrub = as.numeric(islelc_18_10x10.tif == 2))

shrub_14_mean <- calc_focal_mean(isle_14_shrub, "shrub", 3)
shrub_18_mean <- calc_focal_mean(isle_18_shrub, "shrub", 3)

# Water
isle_14_water <- isle_14_10_crop %>%
  mutate(water = as.numeric(islelc_14_10x10.tif == 1))

isle_18_water <- isle_18_10_crop %>%
  mutate(water = as.numeric(islelc_18_10x10.tif == 1))

water_14_mean <- calc_focal_mean(isle_14_water, "water", 25)
water_18_mean <- calc_focal_mean(isle_18_water, "water", 25)

# Other
isle_14_other <- isle_14_10_crop %>%
  mutate(other = as.numeric(islelc_14_10x10.tif == 8))

isle_18_other <- isle_18_10_crop %>%
  mutate(other = as.numeric(islelc_18_10x10.tif == 8))

other_14_mean <- focal2(isle_14_other["other"], matrix(1, 3, 3), "mean")
other_14_mean <- focal2(other_14_mean["other"], matrix(1, 3, 3), "mean")
other_18_mean <- focal2(isle_18_other["other"], matrix(1, 3, 3), "mean")
other_18_mean<- focal2(other_18_mean["other"], matrix(1, 3, 3), "mean")

## join all the data

isle14 <- cbind(
  as.data.frame(canopy_14_mean)['canopy'],
  as.data.frame(road_14)['road'],
  as.data.frame(other_14_mean)['other'],
  as.data.frame(shrub_14_mean)['shrub'],
  as.data.frame(water_14_mean)['water'],
  as.data.frame(isle_14_pcnt_imperv)['lc'] %>%
    rename(imperv = lc ),
  as.data.frame(isle_change_rc)[c(5)]%>%
    rename(lcchange = lc )
)

isle18 <- cbind(
  as.data.frame(canopy_18_mean)['canopy'],
  as.data.frame(road_18)['road'],
  as.data.frame(other_18_mean)['other'],
  as.data.frame(shrub_18_mean)['shrub'],
  as.data.frame(water_18_mean)['water'],
  as.data.frame(isle_18_pcnt_imperv)['lc'] %>%
    rename(imperv = lc )
)

isle14 <-  cbind(isle_14_10_crop,canopy_14_mean, road_14, other_14_mean, shrub_14_mean, water_14_mean, isle_14_pcnt_imperv, isle_change_rc,isle_change) # change impervious(0, 1, -1) -1 is targeted
rm(canopy_14_mean, road_14, other_14_mean, shrub_14_mean, water_14_mean, isle_14_pcnt_imperv, isle_change_rc)

isle18 <- cbind(isle_18_10_crop, canopy_18_mean, road_18,other_18_mean, shrub_18_mean, water_18_mean,isle_18_pcnt_imperv)
rm(canopy_18_mean, road_18, other_18_mean, shrub_18_mean, water_18_mean, isle_18_pcnt_imperv)

isle_14 <- 
  st_join(isle_14_10_rc,isle_tract) %>%
  st_join(.,dem_isle)%>% # not changing
  st_join(.,isle_slope) %>%
  st_join(., soil_isle)


isle_18 <- 
  st_join(isle_18_10_rc,isle_tract2) %>%
  st_join(.,dem_isle)%>% # not changing
  st_join(.,isle_slope) %>%
  st_join(., soil_isle)

rm(dem_isle,isle_slope, isle_14_10_crop, isle_14_10_rc, isle_14_canopy, isle_14_other, isle_14_road, isle_14_shrub, isle_18_10_crop, isle_18_10_rc, isle_18_canopy, isle_18_other, isle_18_road, isle_18_shrub)

isle14_df <- 
  as.data.frame(isle_14)  %>%
  rename(terrain = USGS_1_n38w077_20170509.tif )%>%
  cbind(., isle14) %>% 
  na.omit()
isle18_df <- 
  as.data.frame(isle_18) %>% 
  rename(terrain = USGS_1_n38w077_20170509.tif) %>%
  cbind(., isle18)%>%
  na.omit()

saveRDS(isle14_df, "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/isle14_df.rds")
saveRDS(isle18_df, "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/isle18_df.rds")

#########################################################################################################################################
###################################################  --------------------  ##############################################################
################################################### |                    | ##############################################################
################################################### |      MODELING      | ##############################################################
################################################### |                    | ##############################################################
###################################################  --------------------  ##############################################################
#########################################################################################################################################



set.seed(717)


"%!in%" <- Negate("%in%")

isle14_df <- readRDS( "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/isle14_df.rds")
jame14_df <- readRDS( "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/jame14_df.rds")
port14_df <- readRDS( "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/port14_df.rds")

# Define function to apply modeling process to a dataset
apply_model <- function(data) {
  # Data preparation
  data <- data %>% 
    mutate(
      lcre = case_when(lcchange == -1 ~ 1,
                       lcchange == 1 ~ 0,
                       lcchange == 0 ~ 0)
    ) %>% 
    mutate(
      lcchange = as.factor(lcchange),
      lcre = as.factor(lcre)
    )
  
  # Sample data
  data_sample <- sample_n(data, 500000)
  
  # Initial split for training and test
  data_split <- initial_split(data_sample, strata = "lcchange", prop = 0.75)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  
  # Cross-validation
  cv_splits_geo <- group_vfold_cv(data_train, group = "GEOID")
  
  # Create recipe
  model_rec <- recipe(lcre ~ ., data = data_train) %>%
    update_role(GEOID, new_role = "GEOID") %>%
    update_role(lcchange, new_role = "lcchange") %>%
    step_ns(x, y, options = list(df = 2))
  
  # Model specifications
  glm_plan <- logistic_reg() %>%
    set_engine("glmnet") %>%
    set_mode("classification") %>%
    set_args(
      penalty = tune(), 
      mixture =as.numeric(tune()))
  
  
  rf_plan <- rand_forest() %>%
    set_args(mtry  = tune()) %>%
    set_args(min_n = tune()) %>%
    set_args(trees = 1000) %>% 
    set_engine("ranger", importance = "impurity") %>% 
    set_mode("classification")
  
  
  xgb_plan <- boost_tree() %>%
    set_args(mtry  = tune()) %>%
    set_args(min_n = tune()) %>%
    set_args(trees = 100) %>% 
    set_engine("xgboost") %>% 
    set_mode("classification")
  
  # Modify the hyperparameter grid for each model
  
  glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                             mixture = seq(0,1,0.25))
  rf_grid <- expand.grid(mtry = c(2,5), 
                         min_n = c(1,5))
  xgb_grid <- expand.grid(mtry = c(3,5), 
                          min_n = c(1,5))
  
  # Create the workflow
  glm_wf <- workflow() %>% 
    add_recipe(model_rec) %>% 
    add_model(glm_plan)
  
  
  rf_wf <-
    workflow() %>% 
    add_recipe(model_rec) %>% 
    add_model(rf_plan)
  
  xgb_wf <-
    workflow() %>% 
    add_recipe(model_rec) %>% 
    add_model(xgb_plan)
  
  # Tune hyperparameters
  control <- control_resamples(save_pred = TRUE, verbose = TRUE)
  metrics <- metric_set(f_meas)
  
  glm_tuned <- glm_wf %>%
    tune_grid(resamples = cv_splits_geo,
              grid      = glmnet_grid,
              control = control,
              metrics = metrics)
  
  
  rf_tuned <- rf_wf %>%
    tune::tune_grid(
      resamples = cv_splits_geo,
      grid      = rf_grid,
      control   = control,
      metrics   = metrics)
  
  
  xgb_tuned <- xgb_wf %>%
    tune::tune_grid(resamples = cv_splits_geo,
                    grid      = rf_grid,
                    control   = control,
                    metrics   = metrics)
  
  # Select best model
  glm_best_params <- select_best(glm_tuned, metric = "accuracy")
  rf_best_params <- select_best(rf_tuned, metric = "accuracy")
  xgb_best_params <- select_best(rf_tuned, metric = "accuracy")
  glm_best_wf <- finalize_workflow(glm_wf, glm_best_params)
  rf_best_wf <- finalize_workflow(rf_wf, rf_best_params)
  xgb_best_wf <- finalize_workflow(xgb_wf, xgb_best_params)
  
  # Evaluate on test set
  glm_val_fit_geo <- glm_best_wf %>% 
    last_fit(split = data_split,
             control = control,
             metrics = metrics)
  
  rf_val_fit_geo <- rf_best_wf %>% 
    last_fit(split = data_split,
             control = control,
             metrics = metrics)
  
  xgb_val_fit_geo <- xgb_best_wf %>% 
    last_fit(split = data_split,
             control = control,
             metrics = metrics)
  
  # Show best model and its parameters
  show_best(glm_tuned, metric = "accuracy")
  show_best(rf_tuned, metric = "accuracy")
  show_best(xgb_tuned, metric = "accuracy")
  
  # Return results
  return (list(glm_best_wf,rf_best_wf,xgb_best_wf), list(glm_val_fit_geo, rf_val_fit_geo, xgb_val_fit_geo))
}


# Apply model to three different datasets
results_isle<- apply_model(isle14_df)
results_port<- apply_model(port14_df)
results_jame <- apply_model(jame14_df)

#########################################################################################################################################
###################################################  --------------------  ##############################################################
################################################### |                    | ##############################################################
################################################### |     PREDICTING     | ##############################################################
################################################### |                    | ##############################################################
###################################################  --------------------  ##############################################################
#########################################################################################################################################

isle18_df <- readRDS("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/isle18_df.rds")
port18_df <- readRDS("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/port18_df.rds")
jame18_df <- readRDS("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/jame18_df.rds")

#############################################################################################################################
#############################################  --------------------  ########################################################
############################################# |      JAME CITY     | ########################################################
#############################################  --------------------  ########################################################
#############################################################################################################################
# Fit the best model to the whole dataset
full_fit_rf <- results_jame[1][2]%>%
  fit(data = jame14_df)

# See the prediction for the full dataset
predict_df <- predict(full_fit_rf, new_data = jame14_df)
predict_df2 <- cbind(jame14_df,predict_df) %>%
  mutate(error = as.numeric(.pred_class) - as.numeric(lcre))

# Use the best model to predict for the future
predict_df3 <- predict(full_fit_rf, new_data = jame18_df %>% mutate(lcchange = as.factor(lc)))
predict_df3 <- cbind(jame18_df,predict_df3) 
saveRDS(predict_df3,'~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/predict_DF3_jame.rds')

# extract final model object
rf_full_mod <- extract_fit_parsnip(full_fit_rf)
saveRDS(rf_full_mod,'~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/best_rf1_jame.rds')


#############################################################################################################################
#############################################  --------------------  ########################################################
############################################# |      PORTMOUTH     | ########################################################
#############################################  --------------------  ########################################################
#############################################################################################################################

# Fit the best model to the whole dataset
full_fit_rf <- results_port[1][2]%>%
  fit(data = port14_df)

# See the prediction for the full dataset
predict_df <- predict(full_fit_rf, new_data = port14_df)
predict_df2 <- cbind(port14_df,predict_df) %>%
  mutate(error = as.numeric(.pred_class) - as.numeric(lcre))

# Use the best model to predict for the future
predict_df3 <- predict(full_fit_rf, new_data = port18_df %>% mutate(lcchange = as.factor(lc)))
predict_df3 <- cbind(port18_df,predict_df3) 
saveRDS(predict_df3,'~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/predict_DF3_port.rds')

# extract final model object
rf_full_mod <- extract_fit_parsnip(full_fit_rf)
saveRDS(rf_full_mod,'~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/best_rf1_port.rds')

#############################################################################################################################
#############################################  --------------------  ########################################################
############################################# |   ISLE OF WIGHT    | ########################################################
#############################################  --------------------  ########################################################
#############################################################################################################################
# Fit the best model to the whole dataset
full_fit_rf <- results_jame[1][2]%>%
  fit(data = port14_df)

# See the prediction for the full dataset
predict_df <- predict(full_fit_rf, new_data = isle14_df)
predict_df2 <- cbind(isle14_df,predict_df) %>%
  mutate(error = as.numeric(.pred_class) - as.numeric(lcre))

# Use the best model to predict for the future
predict_df3 <- predict(full_fit_rf, new_data = isle18_df %>% mutate(lcchange = as.factor(lc)))
predict_df3 <- cbind(isle18_df,predict_df3) 
saveRDS(predict_df3,'~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/predict_DF3_isle.rds')

# extract final model object
rf_full_mod <- extract_fit_parsnip(full_fit_rf)
saveRDS(rf_full_mod,'~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/best_rf1_isle.rds')

#########################################################################################################################################
###################################################  --------------------  ##############################################################
################################################### |                    | ##############################################################
################################################### |      MAPPING       | ##############################################################
################################################### |                    | ##############################################################
###################################################  --------------------  ##############################################################
#########################################################################################################################################


predict_df2<- readRDS('~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/predict_DF3_port.rds')
s <- st_as_stars(predict_df2,
                 dimensions=st_dimensions(
                   x=sort(unique(df$x)),
                   y=sort(unique(df$y)), 
                   point=TRUE),
                 dims=c('x','y'))
st_crs(s) <- st_crs(port_14)
s <- s%>%
  st_transform(4326)
x <- s['.pred_class']
x.or <-s['lcre']
error <- s['error']
thresh_value <- 1
x[x != thresh_value] <- NA
x_sf <- st_union(st_as_sf(x))
# write the sf object to a GeoJSON file
geojson_path <- "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/port_predict_xgb.geojson"
st_write(x_sf, geojson_path)



predict_df2<- readRDS('~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/predict_DF3_port.rds')
s <- st_as_stars(predict_df2,
                 dimensions=st_dimensions(
                   x=sort(unique(df$x)),
                   y=sort(unique(df$y)), 
                   point=TRUE),
                 dims=c('x','y'))
st_crs(s) <- st_crs(port_18)
s <- s%>%
  st_transform(4326)
x <- s['.pred_class']
x.or <-s['lcre']
error <- s['error']
thresh_value <- 1
x[x != thresh_value] <- NA
x_sf <- st_union(st_as_sf(x))
# write the sf object to a GeoJSON file
geojson_path <- "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/port_test_4326.geojson"
st_write(x_sf, geojson_path)

