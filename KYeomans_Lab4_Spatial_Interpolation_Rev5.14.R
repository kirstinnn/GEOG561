################################################################################
rm(list = ls()) # REMOVE ALL OBJECTS 
options(scipen = 999) # NO SCI NOTATION 
# LAB 4 - SPATIAL INTERPOLATION
# UPDATED MAY 15, 2024

source_data <- "R:/GEOG561/Students/yeomansk/Labs/Lab4_Interpolation_R/Tute3_Interpol/Data/Fiji_Data"
setwd(source_data) # SET THE WORKING DIRECTORY 


# GET PACKAGES (INSTALL IF NOT ALREADY ON YOUR MACHINE)
require(tidyverse); require(dplyr); require(tidyr)# DATA MANAGEMENT
require(sf); require(tmap); require(terra); require(raster) # SPATIAL DATA MANAGEMENT
require(gstat) # INTERPOLATION
require(patchwork) # PLOTTING

################################################################################

# READ IN DATA

temperature <- sf::read_sf("MaxTemp_VL.shp")
rainfall <- sf::read_sf("Fiji_Rain_VL.shp")
towns <-  sf::read_sf("Towns_VL.shp")
coastline <- sf::read_sf("Viti_Levu.shp")

# EXPLORE THE DATA
head(temperature); summary(temperature); str(temperature)
head(rainfall); summary(rainfall); str(rainfall)
head(towns)
head(coastline)

ggplot(data = coastline) +
  geom_sf()

ggplot(data = towns) +
  geom_sf()


# LOOK AT OVERALL AVG TEMPERATURE AND RAINFALL BY MONTH
# MAKE DATA LONG TO GET MEANS ACROSS ALL STATIONS 


temperature_long <- pivot_longer(temperature, names_to = "Month", 
                                 cols = c("JAN", "FEB", "MAR",
                                          "APR", "MAY", "JUN",
                                          "JUL", "AUG", "SEP",
                                          "OCT", "NOV", "DEC"),
                                 values_to = "temp") 


temperature_long %>%
  group_by(Month) %>%
  summarise (mean_temp = mean(temp))


# NB FACTOR THE MONTHS SO THEY GO IN A LOGICAL ORDER
temperature_long$Month <- factor(temperature_long$Month, 
                                 levels = c("JAN", "FEB", 
                                            "MAR", "APR", 
                                            "MAY", "JUN",
                                            "JUL", "AUG",
                                            "SEP", "OCT", 
                                            "NOV", "DEC"), 
                                 labels = c("January", "February",
                                            "March", "April",
                                             "May", "June",
                                            "July", "August",
                                            "September", "October",
                                            "November", "December"))


fig1a <- ggplot(data = temperature_long, aes(x = Month, y = temp)) +
  geom_boxplot() +
  labs(x = "", y = "Distribution of mean monthly temperature across all stations (C)") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # ANGLE THE TEXT FOR READABILITY
  

temperature_month_station <- temperature_long %>%
  group_by(Month, Station_Nu) %>%
  summarise (mean_temp = mean(temp))

summary(temperature_month_station$mean_temp)

# ggplot(data = temperature_month_station, aes(x = Month, y = mean_temp)) +
#  geom_bar(stat = "identity") + 
#  facet_wrap(~Station_Nu)  # NOT INCLUDED IN REPORT 

rainfall_long <- pivot_longer(rainfall, names_to = "Month", 
                                 cols = c("Jan", "Feb", "Mar",
                                          "Apr", "May", "Jun",
                                          "Jul", "Aug", "Sep",
                                          "Oct", "Nov", "Dec"),
                                 values_to = "rainfall")


# NB FACTOR THE MONTHS SO THEY GO IN A LOGICAL ORDER
rainfall_long$Month <- factor(rainfall_long$Month, 
                              levels = c("Jan", "Feb", 
                                         "Mar", "Apr", 
                                         "May", "Jun",
                                         "Jul", "Aug",
                                         "Sep", "Oct", 
                                         "Nov", "Dec"), 
                              labels = c("January", "February",
                                         "March", "April",
                                         "May", "June",
                                         "July", "August",
                                         "September", "October",
                                         "November", "December"))

rainfall_long %>%
  group_by(Month) %>%
  summarise (mean_rainfall = mean(rainfall))




fig1b <- ggplot(data = rainfall_long, aes(x = Month, y = rainfall)) +
  geom_boxplot() +
  labs(x = "", y = "Distribution of average monthly rainfall across all stations (mm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


fig1a + fig1b  # PLOT TOGETHER 
# EXPORT IMAGE USING EXPORT IN THE GRAPHICS DEVICE 

rainfall_month_station <- rainfall_long %>%
  group_by(Month, Station_Nu) %>%
  summarise (mean_rainfall = mean(rainfall))


summary(rainfall_month_station) 

# ggplot(data = rainfall_month_station, aes(x = Month, y = mean_rainfall)) +
# geom_bar(stat = "identity") + 
#  facet_wrap(~Station_Nu) # NOT USED IN REPORT 

################################################################################

# NEED TO DETERMINE IF THERE IS SPATIAL AUTOCORRELATION IN THE DATA 

# VISUAL INSPECTION 
  
    # PLOTS OF TEMP AND RAINFALL STATIONS 

ggplot() + 
  geom_sf(data = coastline) +
  geom_sf(data = temperature_long, aes(col = temp)) +
  facet_wrap(~Month)


ggplot() + 
  geom_sf(data = coastline) +
  geom_sf(data = rainfall_long, aes(col = rainfall)) +
  facet_wrap(~Month)


################################################################################

# CREATE SEMIVARIOGRAMS TO ASSESS SPATIAL AUTOCOR

# CREATE A VECTOR OF MONTHS TO LOOP THROUGH (IN SAME FORMAT AS DATA (ALL CAPS))
months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", 
            "AUG", "SEP", "OCT", "NOV", "DEC")

semivar_temp <- list() # LIST TO STORE THE SEMIVARIOGRAM OBJECTS FOR EACH MONTH
for (month in months) {   # FOR EACH MONTH IN MONTHS 
  temperature_data <- temperature[[month]] # USING THE TEMPERATURE DATA FOR EACH MONTH
  formula <- as.formula(paste0(month, " ~ 1")) # CREATE THE FORMULA OUTSIDE OF FX SO THAT YOU CAN INDEX BY MONTH
  v <- variogram(   # GET THE SEMIVARIOGRAM
    formula,
    locations=temperature,
    cutoff=35000, # TRIAL AND ERROR, PROBABLY NOT THE BEST FOR EVERY MONTH
    width=100,
    cloud = F 
  )
  semivar_temp[[month]] <- v # STORE THE RESULTS IN THE SEMIVAR LIST RESPECTIVE TO EACH MONTH 
}


# PLOT THE SEMIVARIOGRAMS (SEE NOTE)
for (month in months) {  
  print(plot(semivar_temp[[month]],  # NB THE SEMIVAR SEEM TO OVERRIDE THE PAR
             main = month               # FX, SO PRINT THEM OUT AND THEN USE BACK ARROW
  ))                                    # IN GRAPHICS DEVICE TO SEE THE PLOTS 
}



################################################################################

# SAME PROCESS FOR RAINFALL
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
            "Aug", "Sep", "Oct", "Nov", "Dec")

semivar_rainfall <- list() # CREATE LIST TO STORE OBJECTS 
for (month in months) {   # FOR EACH MONTH IN MONTHS 
  rainfall_data <- rainfall[[month]]  # USING THE RAINFALL DATA FOR EACH MONTH
  formula <- as.formula(paste0(month, " ~ 1")) # CREATE FORMULA
  v <- variogram( # GET SEMIVARIOGRAMS 
    formula,
    locations=rainfall,
    cutoff=35000, # TRIAL AND ERROR, PROBABLY NOT THE BEST FOR EVERY MONTH
    width=100,
    cloud = F 
  )
  semivar_rainfall[[month]] <- v # STORE THE RESULTS IN THE SEMIVAR LIST RESPECTIVE TO EACH MONTH
}


# PLOT THE RESULTS
for (month in months) {  
 print(plot(semivar_rainfall[[month]],  # NB THE SEMIVAR SEEM TO OVERRIDE THE PAR
 main = month                           # FX, SO PRINT THEM OUT AND THEN USE BACK ARROW
  ))                                    # IN GRAPHICS DEVICE TO SEE THE PLOTS 
}

# USE IDW - BECAUSE THE SEMIVARIOGRAMS ARE NOT SHOWING THAT MUCH OF A PATTERN
# AND FOR SIMPLICITY

################################################################################

# IDW FOR TEMPERATURE

# FIRST CREATE THE EXTENT FOR THE SAMPLING SURFACE (AS IN CLASS EXAMPLE)
# TEMPERATURE 

interp_ext <- coastline
st_crs(interp_ext)
st_crs(temperature)

# CREATE A PREDICTION GRID FOR TEMPERATURE

predict_sf_temp <- interp_ext %>%                      # INTERPOLATION EXTENT
  st_make_grid(cellsize = 2000, what = "centers") %>%  # RESOLUTION (TRIAL AND ERROR)
  st_sf() %>%                                           # MAKE AN SF OBJECT
  st_set_crs(st_crs(interp_ext))%>%                     # SPECIFY CRS
  st_filter(interp_ext)                                 # CLIP GRID TO EXTENT 


st_crs(predict_sf_temp)

# from the predict_sf sf object, create a dataframe of x,y coords with z=0
predict_xyz <- predict_sf_temp %>%
  bind_cols(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  mutate(Z = 0)

# create a raster from predict_xyz
predict_ras_temp <- rast(predict_xyz, type= "xyz")

# give predict_ras object the same CRS as temperature
crs(predict_ras_temp) <- st_crs(temperature)$wkt
st_crs(predict_ras_temp)


map1 <- qtm(temperature)
map2 <- qtm(predict_sf_temp)
tmap_arrange(map1, map2)

# NOW DO A FOR LOOP TO DO IDW FOR TEMP FOR EACH MONTH QUICKLY 
# CREATE A VECTOR OF MONTHS FOR THE FOR LOOP 

months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", 
            "AUG", "SEP", "OCT", "NOV", "DEC")

IDW_results_temp <- list() # STORE THE IDW MODELS 
Applied_SF <- list() # STORE THE APPLIED MODELS
predicted_rasters_temp <- list() # STORE THE RASTERIZED DATA 
for (month in months) {   # FOR EACH MONTH IN MONTHS 
  temperature_data <- temperature[[month]] # GET THE TEMPERATURE DATA
  formula <- as.formula(paste0(month, " ~ 1")) # SPECIFIY FORMULA STRUCTURE OUTSIDE OF MODEL
    stat_model <- gstat(              # APPLY THE G-STAT MODEL 
    formula = formula,                # ESTIMATE BASED ON TEMPERATURE
    locations = temperature,          # LOCATIONS WITH KNOWN TEMPERATURE VALUES 
    set = list(idp = 3),              # INVERSE POWER FUNCTION FOR IDW
    nmax = nrow(temperature)          # NUMBER OF NEAREST OBSERVATIONS TO BE USED (SET MAX TO NROW)
  )
    IDW_results_temp[[month]] <- stat_model  # STORE THE RESULTS OF IDW MODELS 
    applied_sf <- predict(stat_model, predict_sf_temp) # PREDICT THE SURFACE WITH THE STAT MODEL 
    applied_sf$pred_temp <- applied_sf$var1.pred # RENAME THE PREDICTED SURFACE VAR
    Applied_SF[[month]] <- applied_sf # SAVE THE APPLIED SF MODEL FOR EACH MONTH 
    applied_ras <- terra::rasterize(applied_sf, predict_ras_temp, field = "pred_temp", fun = "mean") # RASTERIZE THE DATA USING THE MEAN 
    predicted_rasters_temp[[month]] <- applied_ras # SAVE THE RASTERS FOR EACH MONTH
} # END OF LOOP 

# SET UP FOR PLOTTING AND PLOT RASTERS FOR EACH MONTH
# PLOT IN THE LOOP AND THEN JUST NEED TO LABEL WITH THE [[month]]
# SO THAT IT KNOWS WHICH ONE TO APPLY TO 
par(mfrow = c(3,4))
for (month in months) {
 plot(predicted_rasters_temp[[month]],
 main = month)

}
# EXPORT IMAGE USING EXPORT IN THE GRAPHICS DEVICE 


# SUMMARISE THE TEMPERATURE FOR EACH MONTH
for (month in months) {
  cat("Summary for", month, ":\n")
  print(summary(predicted_rasters_temp[[month]]))
}


# RECLASSIFYING RASTRES
require(raster)
raster_recl_temp <- list()
for (month in months) {
 raster_data <- as(predicted_rasters_temp[[month]], "Raster") # NEED IT TO BE RASTER TO WORK IN RASTER PKG (NOT SPAT RASTER)
 m <- c(15, 25.99999999, 1, 26, 28.999999999, 0, 29, 35, 0) # SAYING I WOULD ONLY CHOOOSE AREA WHERE IT IS B/W 15-26 C ESSENTIALLY
 
 rclmat <- matrix(m, ncol=3, byrow=TRUE)
 rc <- raster::reclassify(raster_data, rclmat, include.lowest = T)
 raster_recl_temp[[month]] <- rc
}


# HAVE TO DO TO THE .999999S TO CAPTURE THE DCIMALS 

par(mfrow = c(3,4))
for (month in months) {
  plot(raster_recl_temp[[month]])
  
}
# EXPORT IMAGE USING EXPORT IN THE GRAPHICS DEVICE 

# IF MAX EXCEEDS 1 THEN ITS INCORRECTLY CLASSIFIED 
for (month in months) {
  cat("Summary for", month, ":\n")
  print(summary(raster_recl_temp[[month]]))
}


# ADD FOR OVERALL SUITABILITY SURFACE
temperature_final <- raster_recl_temp$JAN +  raster_recl_temp$FEB + 
  raster_recl_temp$MAR +  raster_recl_temp$APR + 
  raster_recl_temp$MAY +  raster_recl_temp$JUN +
  raster_recl_temp$JUL +  raster_recl_temp$AUG +
  raster_recl_temp$SEP +  raster_recl_temp$OCT +
  raster_recl_temp$NOV  +  raster_recl_temp$DEC
# 12 WOULD BE THE MOST OPTIMAL YEAR ROUND FOR RAINFALL
dev.off()
plot(temperature_final)


################################################################################


# FOLLOW THE SAME PROCESS FOR RAINFALL

# RAINFALL 

interp_ext <- coastline
st_crs(interp_ext)
st_crs(rainfall)

# CREATE A PREDICTION GRID

predict_sf_rainfall <- interp_ext %>%                      # INTERPOLATION EXTENT
  st_make_grid(cellsize = 2000, what = "centers") %>%  # RESOLUTION (TRIAL AND ERROR)
  st_sf() %>%                                           # MAKE AN SF OBJECT
  st_set_crs(st_crs(interp_ext))%>%                     # SPECIFY CRS
  st_filter(interp_ext)                                 # CLIP GRID TO EXTENT 


st_crs(predict_sf_rainfall)

# from the predict_sf sf object, create a dataframe of x,y coords with z=0
predict_xyz <- predict_sf_rainfall %>%
  bind_cols(st_coordinates(.)) %>%
  st_drop_geometry() %>%
  mutate(Z = 0)

# create a raster from predict_xyz
predict_ras_rainfall <- rast(predict_xyz, type= "xyz")

# give predict_ras object the same CRS as rainfall
crs(predict_ras_rainfall) <- st_crs(rainfall)$wkt
st_crs(predict_ras_rainfall)


map1 <- qtm(rainfall)
map2 <- qtm(predict_sf_rainfall)
tmap_arrange(map1, map2)

################################################################################

# CREATE A VECTOR OF MONTHS FOR THE FOR LOOP TO DO IDW QUICKLY 

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
            "Aug", "Sep", "Oct", "Nov", "Dec")

IDW_results_rainfall <- list() # STORE THE IDW MODELS 
Applied_SF_rainfall <- list() # STORE THE APPLIED MODELS
predicted_rasters_rainfall <- list() # STORE THE RASTERIZED DATA 
for (month in months) {   # FOR EACH MONTH IN MONTHS 
  rainfall_data <- rainfall[[month]] # GET THE RAINFALL DATA
  formula <- as.formula(paste0(month, " ~ 1"))
  stat_model <- gstat(              # APPLY THE G-STAT MODEL 
    formula = formula,                # ESTIMATE BASED ON TEMPERATURE
    locations = rainfall,          # LOCATIONS WITH KNOWN TEMPERATURE VALUES 
    set = list(idp = 3),              # INVERSE POWER FUNCTION FOR IDW
    nmax = nrow(rainfall)          # NUMBER OF NEAREST OBSERVATIONS TO BE USED (SET MAX TO NROW)
  )
  IDW_results_rainfall[[month]] <- stat_model  # STORE THE RESULTS OF IDW MODELS 
  applied_sf <- predict(stat_model, predict_sf_rainfall) # PREDICT THE SURFACE WITH THE STAT MODEL 
  applied_sf$pred_rainfall <- applied_sf$var1.pred # RENAME THE PREDICTED SURFACE VAR
  Applied_SF_rainfall[[month]] <- applied_sf # SAVE THE APPLIED SF MODEL FOR EACH MONTH 
  applied_ras <- terra::rasterize(applied_sf, predict_ras_rainfall, field = "pred_rainfall", fun = "mean") # RASTERIZE THE DATA USING THE MEAN 
  predicted_rasters_rainfall[[month]] <- applied_ras # SAVE THE RASTERS FOR EACH MONTH
} # END OF LOOP 

# SET UP FOR PLOTTING AND PLOT RASTERS FOR EACH MONTH
# PLOT IN THE LOOP AND THEN JUST NEED TO LABEL WITH THE [[month]]
# SO THAT IT KNOWS WHICH ONE TO APPLY TO 
par(mfrow = c(3,4))
for (month in months) {
  plot(predicted_rasters_rainfall[[month]],
       main = month)
  
}
# EXPORT IMAGE USING EXPORT IN THE GRAPHICS DEVICE 


# SUMMARISE THE TEMPERATURE FOR EACH MONTH
for (month in months) {
  cat("Summary for", month, ":\n")
  print(summary(predicted_rasters_rainfall[[month]]))
}


# RECLASSIFYING RASTRES
require(raster)
raster_recl_rainfall <- list()
for (month in months) {
  raster_data <- as(predicted_rasters_rainfall[[month]], "Raster") # NEED IT TO BE RASTER TO WORK IN RASTER PKG (NOT SPAT RASTER)
  m <- c(0, 499.9999999, 1, 500, 10000, 0) # SAYING I WOULD ONLY CHOOOSE AREA WHERE IT IS B/W 10-500 mm RAIN ESSENTIALLY
  
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  rc <- raster::reclassify(raster_data, rclmat, include.lowest = T)
  raster_recl_rainfall[[month]] <- rc
}


# HAVE TO DO TO THE .999999S TO CAPTURE THE DCIMALS 

par(mfrow = c(3,4))
for (month in months) {
  plot(raster_recl_rainfall[[month]])
  
}
# EXPORT IMAGE USING EXPORT IN THE GRAPHICS DEVICE 

# IF MAX EXCEEDS 1 THEN ITS INCORRECTLY CLASSIFIED 
for (month in months) {
  cat("Summary for", month, ":\n")
  print(summary(raster_recl_rainfall[[month]]))
}
# EXPORT IMAGE USING EXPORT IN THE GRAPHICS DEVICE 

rainfall_final <- raster_recl_rainfall$Jan + raster_recl_rainfall$Feb +
  + raster_recl_rainfall$Mar + raster_recl_rainfall$Apr +
  raster_recl_rainfall$May + raster_recl_rainfall$Jun +
  raster_recl_rainfall$Jul + raster_recl_rainfall$Aug + 
  raster_recl_rainfall$Sep + raster_recl_rainfall$Oct + 
  raster_recl_rainfall$Nov + raster_recl_rainfall$Dec
# 12 WOULD BE THE MOST OPTIMAL YEAR ROUND FOR RAINFALL
plot(rainfall_final)


# PLOT OPTIMAL TEMP AND RAINFALL TOGETHER 
par(mfrow = c(1,2))
plot(temperature_final, main = "Temperature suitability surface")
plot(rainfall_final, main = "Rainfall suitability surface")
# EXPORT FIGURE

# OPTIMAL - ADD TOGETHER FOR OVERALL SUITABILITY SURFACE
dev.off()
optimal_surface <- rainfall_final + temperature_final
plot(optimal_surface) # HIGHEST SCORE MEANS MOST OPTIMAL YEAR ROUND 

optimal_surface_df <- as.data.frame(optimal_surface, xy = T)
ggplot() + 
  geom_raster(data = optimal_surface_df, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(name = "Optimal Surface", colors = c("red", "orange", "green")) +
  geom_sf(data = towns, col = "blue") +
  geom_sf_label(data = towns, aes(label = NAME)) # LABEL THE TOWNS +
  theme_bw()
