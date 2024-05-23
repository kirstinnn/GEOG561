################################################################################
rm(list = ls())

# LAB 5 - POINT PATTERN ANALYSIS 
# UPDATED MAY 22, 2024

source_data <- "R:/GEOG561/Students/yeomansk/Labs/Lab5_PPA/Data"
setwd(source_data)

# install.packages("tidyverse"); install.packages("dplyr"); install.packages("tidyr");
# install.packages("sf"); install.packages("tmap"); install.packages("terra"); install.packages("spatstat");
# install.packages("aspace"); install.packages("pals")

# GET PACKAGES (INSTALL IF NOT ALREADY)
require(tidyverse); require(dplyr); require(tidyr)# DATA MANAGEMENT
require(sf); require(tmap); require(terra); require(spatstat);
require(aspace) # SPATIAL DATA MANAGEMENT
require(pals); # COLORS FOR GRAPHICS

################################################################################

# READ IN DATA / SUMMARISATION / EXPLORATORY

cdths_sf <-read_sf("CholeraDeaths.shp") # HOUSEHOLD DEATHS 
cdths_sf
summary(cdths_sf) # SEE SUMMARY
# hist(cdths_sf$Count) # DISTRIBUTION OF DEATHS - RIGHT SKEWED
st_crs(cdths_sf) # CHECK CRS (NB PROJECTION IS UTM SO UNITS ARE METERS)


# MIN DEATHS PER HH = 1
# MAX DEATHS PER HH = 15 
# ON AVERAGE, ABOUT 2 DEATHS PER HOUSEHOLD


pumps_sf <-read_sf("WaterPumps.shp") # PUMPS 

# CREATE MAP SHOWING DISTRIBUTION OF HOUSEHOLDS BY NUMBER OF DEATHS 
pumps_sf$Pump[pumps_sf$Pump == "BROAD STREET"] <- "Broad Street" # RENAME SO NOT ALL CAPS 
# FOR BROAD STREET


figure1 <- ggplot() +
  geom_sf(data = cdths_sf, fill = "transparent", alpha = 0.2, aes(size = Count)) + # SIZE CORR WITH NUMBER OF DEATHS IN HOUSEHOLD 
# TRANSPARENCY ALLOWS YOU TO SEE OVERLAP OF HOUSEHOLDS 
  geom_sf(data = pumps_sf, shape =18, size = 4, aes(col= Pump)) +
  labs(size = "Number of deaths") +
  scale_colour_manual(values=unname(cols25())) + # SET COLOR FOR PUMP USING DISTINCTIVE COLORS  
  theme(text = element_text(colour= "black", size =11)) +
  theme_bw() 


figure1
ggsave("figure1.png", plot = figure1)

################################################################################
# FIRST ORDER PROCESS - KERNEL DENSITY ESTIMATION 
  
# CONVERT SF TO PPP OBJECT
cdths_ppp <-as.ppp(cdths_sf)
cdths_ppp


# EXPLORATORY DATA ANALYIS
# KERNEL DENSITY ESTIMATION   

# Figure 2 - see patterns based on point density of deaths
# WITHOUT HAVING TO CHOOSE AREAL UNIT SIZE
# CHOOSE OPTIMAL SIGMA USING CROSS-VALIDATION W/ BW.DIGGLE 
d_diggle <-density(cdths_ppp, bw.diggle(cdths_ppp))
plot(d_diggle, main=paste("Sigma =", as.character(round(bw.diggle(cdths_ppp),2))))
contour(d_diggle, add=T) # EXPORT FROM GRAPHICS PANEL
# SHOWS POTENTIALLY SECOND ORDER PROCESS; CHARACTERISTICS OTHER 
# THAN DISTANCE TO WELLS MAY DRIVE THE PATTERN 



###############################################################################
# CENTROGRAPHY 
# GET SOME OF THE METRICS USED IN THE KERNEL DENSITY ESTIMTION
# (CENTER MEAN; CENTER MEDIAN; STD DISTANCE)
# CODE ADAPTED FROM PENN STATE GEOG 586 COURSE
# https://www.e-education.psu.edu/geog586/node/857

# CALCULATE MEAN CENTER OF CHOLERA CASES BY HOUSEHOLD 
# USE THE X AND Y COORD 
xmean <- mean(cdths_sf$POINT_X)
ymean <-mean(cdths_sf$POINT_Y)

# CALCULATE THE MEDIAN CENTER OF CHOLERA CASES BY HOUSEHOLD 
xmed <- median(cdths_sf$POINT_X)
ymed <- median(cdths_sf$POINT_Y)

# SET CHOLERA DEATHS TO A D.F.
newcdths_df<-data.frame(cdths_sf)

# CHECK 
str(newcdths_df)

# MAKE SURE COUNT AND ID ARE INTEGERS
newcdths_df$Count <- as.integer(newcdths_df$Count)
newcdths_df$Id <- as.integer(newcdths_df$Id)

# CREATE LIST OF X COORDS FROM OG SF
a=list(cdths_sf$POINT_X)

# CALCULATE THE WEIGHTED MEAN
d=0
sumcount = 0
sumxbar = 0
sumybar = 0
for(i in 1:length(a[[1]])){ # FOR EACH COORDINATE IN LENGTH OF A OBJECT
  xbar <- (cdths_sf$POINT_X[i] * newcdths_df$Count[i]) # GET THE NUMBER OF POINTS * NUMBER OF DEATHS (X-COORD)
  ybar <- (cdths_sf$POINT_Y[i] * newcdths_df$Count[i]) # GET THE NUMBER OF POINTS * NUMBER OF DEATHS (Y-COORD)
  sumxbar = xbar + sumxbar # SUM 
  sumybar = ybar + sumybar # SUM
  sumcount <- newcdths_df$Count[i]+ sumcount # SUM NUMBER OF WEIGHTS 
}
xbarw <- sumxbar/sumcount # WEIGHTED X MEAN
ybarw <- sumybar/sumcount # WEIGHTED Y MEAN

# GET THE STANDARD DISTANCE OF CASES 
#Std_Dist <- sqrt(sum((cdths_sf$x - xmean)^2 + (cdths_sf$y - ymean)^2) / nrow(cdths_sf$n))


d=0
for(i in 1:length(a[[1]])){ # FOR OBSERVATION IN LENGTH OF A 
  c<-((cdths_sf$POINT_X[i] - xmean)^2 + (cdths_sf$POINT_Y[i] - ymean)^2) # GET THE SQUARED TERMS (DISTANCE)
  d <-(d+c) # ADD THEM TO TOGETHER
}
Std_Dist <- sqrt(d /length(a[[1]])) # TAKE THE SQRT OF ADDED TERMS DIVIDED BY NROWS (GET STD DIST)

# MAKE A CIRCLE FOR ONE STD DIST AWAY FROM THE CENTER
bearing <- 1:360 * pi/180
cx <- xmean + Std_Dist * cos(bearing)
cy <- ymean + Std_Dist * sin(bearing)
circle <- cbind(cx, cy)


# ID THE MOST CENTRAL POINT (POINT WITH SHORTEST DISTANCE TO ALL POINTS)
# IN THIS, EACH POINTS XY IS EXTRACTED AND COMPARED TO OTHER POINTS TO FIND 
# SHORTEST DISTANCE USING THE FOLLOWING FORMULA
#sqrt((x2-x1)^2 + (y2-y1)^2

sumdist2 = 1000000000 # ARBITRARY SUM (?)
for(i in 1:length(a[[1]])){ # FOR EACH OBS IN LENGTH A 
  x1 = cdths_sf$POINT_X[i] # FIND THE X COORDINATE
  y1= cdths_sf$POINT_Y[i] # AND THE Y COORDINATE
  recno = newcdths_df$Id[i] # ASSIGN THE ID NUMBER 
  #print(recno)
  #check against all other points
  sumdist1 = 0 # OBJECT FOR SAVING  (THIS IS HOW YOU COMPARE EACH DIST BETWEEN EACH POINT)
  for(j in 1:length(a[[1]])){ # FOR EACH OBS IN LENGTH A 
    recno2 = newcdths_df$Id[j] # ASSIGN ID 
    x2 = cdths_sf$POINT_X[j] # GET THE X2
    y2= cdths_sf$POINT_Y[j] # GET THE Y2  
    if(recno==recno2){  # IF TWO POINTS HAVE THE SAME ID DONT COMPARE 
    }else { # OTHERWISE
      dist1 <-(sqrt((x2-x1)^2 + (y2-y1)^2)) # GET THE DISTANCE USING FORMULA 
      sumdist1 = sumdist1 + dist1 # STORE IT IN THE SUMDIST OBJ
      #print(sumdist1)
    }
  }
  #print("test")
  if (sumdist1 < sumdist2){ # IF THE DISTANCE LESS THAN THRESHOLD (ARBITRARILY SET)
    dist3<-list(recno, sumdist1, x1,y1) # STORE A NEW DIST OBJ WITH THE ID, X COORD, Y COORD OF CENTRAL POINT
    sumdist2 = sumdist1 # UPDATE 
    xdistmin <- x1 # EXTRACT THE X COORD 
    ydistmin <- y1 # EXTRACT THE Y COORD 
  } # THIS POINT HAS THE SHORTEST DISTANCE BETWEEN IT AND ANY OTHER POINTS 
}



# NB ALSO POSSIBLE IN THE ASPACE PACKAGE (SEE LECTURE 15 CENTOGRAPHY)
xy_mat <- cbind( cdths_sf$POINT_X, cdths_sf$POINT_Y) # CREATE A MATRIX OF XY POINTS
a <- calc_mnc(id = 1, points= xy_mat, verbose=FALSE) # GET THE MEAN CENTER 

# MDC - median centre
b <- calc_mdc(id=1, points=xy_mat, verbose=FALSE)
# put a and b into a list of r objects (needed for plot_centres)
robjects <- list(a,b)
# Plot centres
plot_centres(datin=robjects, plotnew=T, plotcentre=TRUE, plotmedian=TRUE,
             plotcentral=TRUE, points.col="Blue", centre.col="Hot Pink",
             centre.pch=19, median.col="Green", TITLE="Mean & Median Example")


# AND GET GET STDEV ELLIPSE FOR PLOT
stddev_ellipse <- calc_sde(id=1, centre.xy=NULL, points= xy_mat)
print(stddev_ellipse)
plot_sde(datin=stddev_ellipse, plotnew=TRUE, plotSDEaxes=FALSE, plotweightedpts=FALSE,
         plotpoints=TRUE, title = NULL)

# MAP THE RESULTS 
# FIGURE 3 

# NB - GET PUMP_COORD FOR POINTS USING ST_COORDINATES FX
pumps_coords <- st_coordinates(pumps_sf)

par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE) # SET UP MARGINS FOR LEGEND (SO IT DOESNT COVER POINTS)
plot(cdths_sf$geometry)  # PLOT THE CASES
points(pumps_coords, col = "violetred", cex = 1.5, pch = 18) # ADD PUMPS
points(xmean,ymean,col="red", cex = 1.5, pch = 19) # MEAN CENTER 
points(xmed,ymed,col="green", cex = 1.5, pch = 19) # MEDIAN CENTER 
points(xbarw,ybarw,col="blue", cex = 1.5, pch = 19) # WEIGHTED MEAN CENTER 
points(dist3[[3]][1],dist3[[4]][1],col="orange", cex = 1.5, pch = 19) # CENTRAL POINT
lines(circle, col='purple', lwd=2) # CIRCLE OF 1 STD DIST 
lines(stddev_ellipse$FORPLOTTING$coordsSDE, col = "turquoise", lwd = 2) # ELLIPSE OF 1 STD DIST NB HAVE TO EXTRACT THE COORDS 
legend("topleft", inset = c(-0.1, -0.15), legend = c("Household cholera case locations", "Pumps", "Mean center", "Median center", 
                             "Weighted mean center", "Central point", 
                             "One standard distance circle", "Standard deviational ellipse"),
       col = c("black", "violetred", "red", "green", "blue", "orange", "purple", "turquoise"), 
       pch = c(1, 18, 19, 19, 19, 19, NA, NA), bg = "white", cex = 0.8,
       lty = c(0, 0, 0, 0, 0, 0,  1, 1))# ADD A LEGEND


################################################################################

# DISTANCE-BASED ANALYSIS - RIPLEY'S K-FUNCTION AND PAIRWISE CORRELATION FUNCTION

# K FUNCTION (NO CONFIDENCE ENVELOPE)
k_est_cdths <-Kest(cdths_ppp, correction='border')

# Figure 4a-c

k_est_cdths_env <- envelope(cdths_ppp,Kest,correction='border', nsim = 1000, nrank = 5) # 1000 MONTE CARLO
# SIMULATIONS WITH ENVELOPE PLACED AT FIFTH HIGHEST AND FIFTH LOWEST VALUES 

par(mfrow = c(1,3)) # PLOT ON SAME PLOT
plot(k_est_cdths_env, main = "a. K-function")

k_est_cdths_env_L <- envelope(cdths_ppp,Lest,correction='border') # TRANSFORM K FX TO L   
plot(k_est_cdths_env_L, main = "b. L-function")

# PCF 
pcf_env <-envelope(cdths_ppp, pcf, nsim=1000, nrank=5) # SIMULATIONS 
plot(pcf_env, main = "c. PCF")
# EXPORT 
################################################################################
