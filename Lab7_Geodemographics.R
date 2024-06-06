################################################################################
rm(list = ls()) # REMOVE ALL OBJECTS 
options(scipen = 999) # NO SCI NOTATION 
# LAB 7 - GEODEMOGRAPHICS
# UPDATED JUNE 5, 2024

source_data <- "R:/GEOG561/Students/yeomansk/Labs/Lab7_geodemo"
setwd(source_data) # SET THE WORKING DIRECTORY 

install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidycensus")
install.packages("tidyr")
install.packages("sf")
install.packages("classInt")
install.packages("spdep")
install.packages("tmap")
install.packages("caret")
install.packages("Hmisc")


# GET PACKAGES (INSTALL IF NOT ALREADY ON YOUR MACHINE)
require(tidyverse); require(dplyr); require(tidycensus); require(tidyr)# DATA MANAGEMENT
require(sf); require(classInt); require(spdep); require(tmap) # SPATIAL DATA
require(caret); require(Hmisc)

# USE CENSUS API KEY 
# census_api_key("b5bfe31eadc1a0f34e3403b397618173d7a1d4b5", install = T)  # INSTALL = T TO USE IN FUTURE SESSIONS


################################################################################

# GET THE CENSUS DATA (NB YOU CAN GET MANY AT ONCE)
# BUT ONLY ONE SUMMARY VAR ALLOWED 

# FIRST GET SOME THAT DONT NEED SUMMARY VARS 
# MEDIAN AGE AND INCOME
acs20_age_inc <- get_acs(
  state = "OR",
  county = "Multnomah",
  geography = "tract",
  variables = c(median_age = "B23013_001", median_income =
                  "B06011_001"), # RENAME VARIABLES HERE 
  geometry = F, # SET GEOMETRY TO FALSE FOR KMEANS CLUSTERING  
  output = "wide", # NEED TO BE WIDE TO COMBINE WITH OTHER DATA
  year = 2020
) 

acs20_age_inc2 <- 
  acs20_age_inc %>% 
  select(GEOID, median_ageE, median_incomeE)

rm(acs20_age_inc) # REMOVE OLD ONE 

# NOW GET THE ONES THAT NEED SUMMARY VARS 
# EDUCATION (PERCENT BACHELORS DEGREE)
acs20_edu <- get_acs(
    state = "OR",
    county = "Multnomah",
    geography = "tract",
    variables = c(edu = "B06009_005"),# RENAME VARIABLES HERE 
    summary_var = "B06009_001",
    geometry = F, # SET GEOMETRY TO FALSE FOR KMEANS CLUSTERING  
    year = 2020
  ) 

# NOW GET THE PERCENTAGES 
# EDUCATION (PERCENT BACHELORS DEGREE)
acs20_edu2 <- 
  acs20_edu %>% group_by(GEOID) %>% # KEEP GEOID
  summarise(PC_bach = estimate / summary_est * 100) %>% # GET PERCENTAGE 
  select(GEOID, PC_bach) # GET ONLY REQUIRED
  
rm(acs20_edu)

# NOW DO TRANSPORTATION 
acs20_transpo <- get_acs(
  state = "OR",
  county = "Multnomah",
  geography = "tract",
  variables = c(transpo = "B06009_005"),
  summary_var = "B06009_001",
  year = 2020
)

# PERCENT 
acs20_transpo2 <- 
  acs20_transpo %>% group_by(GEOID) %>%  # KEEP GEOID
  summarise(PC_pubtran = estimate / summary_est * 100) %>%  # GET PERCENTAGE 
  select(GEOID, PC_pubtran) # GET ONLY REQUIRED

rm(acs20_transpo)

# HOUSING 
acs20_housing <- get_acs(
  state = "OR",
  county = "Multnomah",
  geography = "tract",
  variables = c(housing = "B25026_002"),
  summary_var = "B25026_001",
  year = 2020
)


# PERCENT 
acs20_housing2 <- 
  acs20_housing %>% group_by(GEOID) %>%  # KEEP GEOID
  summarise(PC_own_occ = estimate / summary_est * 100) %>% # GET PERCENTAGE 
  select(GEOID, PC_own_occ) # GET ONLY REQUIRED

rm(acs20_housing)

# MARITAL SATUS
acs20_marital_stat <- get_acs(
  state = "OR",
  county = "Multnomah",
  geography = "tract",
  variables = c(marital_stat = "B07008_003"),
  summary_var = "B07008_001",
  year = 2020
)

# PERCENT 
acs20_marital_stat2 <- 
  acs20_marital_stat %>% group_by(GEOID) %>%  # KEEP GEOID
  summarise(PC_married = estimate / summary_est * 100) %>%# GET PERCENTAGE 
  select(GEOID, PC_married) # GET ONLY REQUIRED 

rm(acs20_marital_stat)


# EMPLOYMENT SATUS
acs20_employment_stat <- get_acs(
  state = "OR",
  county = "Multnomah",
  geography = "tract",
  variables = c(employment_stat = "C18120_006"),
  summary_var = "C18120_001",
  year = 2020
)

# PERCENT
acs20_employment_stat2 <- 
  acs20_employment_stat %>% group_by(GEOID) %>% 
  mutate(PC_employment_stat = estimate / summary_est * 100) %>% 
  select(GEOID, PC_employment_stat)

rm(acs20_employment_stat)

# COMBINED THE DATAFRAME WITH CHAINED LEFT JOINS 
combined_df <- 
  acs20_age_inc2 %>% 
  left_join(acs20_edu2, by = "GEOID") %>%
  left_join(acs20_transpo2, by = "GEOID") %>% 
  left_join(acs20_housing2, by = "GEOID") %>% 
  left_join(acs20_marital_stat2, by = "GEOID") %>% 
  left_join(acs20_employment_stat2, by = "GEOID")

glimpse(combined_df) # LOOK AT IT 

############################################################################

# GET DECENNIAL CENSUS ETHNICIY DATA 

dec20_ethnicity <- get_decennial(
  state = "OR",
  county = "Multnomah",
  geography = "tract",
  variables = c(white = "P2_005N", 
                black = "P2_006N",
                asian = "P2_008N",
                hispanic = "P2_002N"),
  summary_var = "P2_001N",
  year = 2020,
  sumfile = "pl"
) 

# CALCULATE PERCENT 
dec20_ethnicity_percent <- 
  dec20_ethnicity %>% group_by(GEOID, variable) %>% 
  summarise(PC = value / summary_value * 100)


# REFORMAT FROM LONG TO WIDE 
dec20_ethnicity_percent_wide <- 
  dec20_ethnicity_percent %>%
  pivot_wider(names_from = variable, values_from = PC) %>%  # NAMES ARE FROM 
  # ETHNICITY VARIABLE, VALUES FROM PC VARIABLE
  rename_with(~ paste0("PC_", .), c(asian, black, hispanic, white)) # ADD PC TO VAR NAMES 


################################################################################

# JOIN THE ACS DATA WITH DECENNIAL DATA 
geodemo.d <- left_join(combined_df, dec20_ethnicity_percent_wide) # JOIN BY GEOID


# GET GEOGRAPHIC FILE
acs20.g <- get_acs(
  state = "OR",
  county = "Multnomah",
  geography = "tract",
  variables = "B06011_001", # NB DOESNT MATTER WHAT VAR, JUST NEED IT TO GET THE GEOMETRY
  geometry = TRUE,
  year = 2020
) 


# REMOVE UNWANTED FIELDS 
geodemo_sf <- acs20.g %>%
  select (-estimate, -NAME, -variable, -estimate, -moe)

# JOIN DATA WITH SF 
geodemo_sf <- left_join(geodemo_sf, geodemo.d)

# CHECK THE CRS
st_crs(geodemo_sf)

# PROJECT TO OR LAMBERT 
geodemo_sf.p <-st_transform(geodemo_sf, crs = "EPSG:2992")

# CHECK 
st_crs(geodemo_sf.p)

# FILTER OUT 2 RURAL TRACTS AND ONE WITH NO DATA
summary(geodemo_sf.p)
geodemo_sf.p <- geodemo_sf.p %>%
  filter(GEOID != 41051007100) %>%
  filter(GEOID != 41051010500) %>%
  filter(GEOID != 41051980000)
summary(geodemo_sf.p)

# map PC_white just to check it looks the same as in previous lab
map1 <- tm_shape(geodemo_sf.p) +
  tm_polygons(col = 'PC_white', palette = "Reds",style = 'quantile') +
  tm_legend(legend.outside = FALSE)
map1

# write out the sf as geopackage called 'Portland.gpkg' (IF YOU WANT TO WORK IN ARCPRO)
# write_sf(geodemo_sf.p, "./Data_Download/Portland.gpkg", delete_dsn=TRUE)

# REMOVE EVERYTHING EXCEPT GEODEMO FULL DATA
rm(list = setdiff(ls(), "geodemo_sf.p"))
################################################################################

# START CLUSTER ANALYSIS
gd_sf <- geodemo_sf.p

names(gd_sf)
head(gd_sf)
plot(gd_sf)

# REMOVE GEOMETRY FOR KMEANS CLUSTERING 
gd_df <- gd_sf %>%
  st_drop_geometry()
head(gd_df)


# STANDARDIZE MEDIAN INCOME AND AGE SO THAT THEY ARE IN THE SAME 0-100 RANGE AS ALL OTHER VAR
process <- preProcess(as.data.frame(gd_df[,-1]), method=c("range"))

# APPLY PREDICT TO GET IT FROM 0-1
gd_df_std <- predict(process, as.data.frame(gd_df))

# CHECK THE STANDARDIZED DATA
head(gd_df_std)
summary(gd_df_std$median_ageE)
################################################################################

# ASSESS MULTICOLLINEARITY USING SCATETERPLOTS 
gd_df_std %>% 
  as.data.frame() %>%
  select(2:12) %>%  
  plot()

# CREATE CORRELATION MATRIX WITH HMISC
# [,-1] means ignore the first field (GEOID)
gd_cor <- rcorr(as.matrix(gd_df_std[,-1]))
gd_cor # FIRST MATRIX IS CORR MATRIX, SECOND IS P-VALUES OF CORRS 

# FIND ANYTHING ABOVE 80 THAT IS STAT SIG
high_correlation <- which(abs(gd_cor$r) > 0.80 & gd_cor$P < 0.05, arr.ind = TRUE)
print(high_correlation)

# FIND OUT WHICH VARIABLES 
variable_names <- colnames(gd_df_std)

# GET THE NAMES OF THE PAIRS FROM THE HIGH COR MATRIX 
correlated_pairs <- data.frame(Variable1 = variable_names[high_correlation[, 1]],
                               Variable2 = variable_names[high_correlation[, 2]])

correlated_pairs # SHOWS WHICH VARIABLES ARE HIGHLY CORRELATED
# FOR THIS ANALYSIS, NOT GOING TO WORRY ABOUT IT BUT SHOULD BE CONSIDERED
################################################################################

# PERFORM CLUSTER ANALYSIS
head(gd_df_std)

# SUBSET THE DATA FOR VAR OF INTEREST (PC ASIAN, BLACK, HISPANIC, WHITE)
gd_df_std_sel <- select(gd_df_std, c(1,9,10,11,12))
head(gd_df_std_sel)

# SET SEED  (to get reproducible results with kmeans())
set.seed(123)

# create an empty vector that will hold 10 within-cluster-sum-of-squares
wcss <- numeric(10)

# run kmeans 10 times producing results for 1 to 10 possible clusters
for (i in 1:10) {
  # Fit the model: km.out
  km.out <- kmeans(gd_df_std_sel[,-1], centers = i)
  # Save the within cluster sum of squares
  wcss[i] <- km.out$tot.withinss
}



# PUT DATA IN TIBBLE AND PLOT A SCREE PLOT 
wcss_df <- tibble(clusters = 1:10, wcss = wcss)

scree_plot <- ggplot(wcss_df, aes(x = clusters, y = wcss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  labs(x = 'Number of clusters', y = 'Within cluster-sum-of-squares') +
  theme_bw()
scree_plot
ggsave("figure1.png", plot = scree_plot)


# EXPORT 

# USE THE SCREE PLOT ELBOW TO DETERMINE NUMBER OF CLUSTERS
# FOR EXAMPLE, INCREASING CLUSTERS FROM 1-2 RESUCES THE WCSS BY 15 
# BUT INCREASING FROM 2-3 ONLY REDUCES IT 5, SO NOT MUCH HELP FOR DATA 
# PARTITIONING - 2 IS OPTIMAL; USE FOUR FOR ASSIGNMENT


## perform cluster analysis specifying clusters
# create a variable for number of clusters (so it can be passed to other functions below)
n_clust = 4 # CHANGE TO 4 FOR TASK 1 
km <- kmeans(gd_df_std_sel[,-1], n_clust)


# look at the cluster means 
km$centers

# join the km cluster numbers to the dataframe, while naming the added field 'Cluster'
gd_df_std_k <- mutate(gd_df_std_sel, "Cluster" = as.factor(km$cluster))


# GET THE BOXPLOT LIKE IN ARCPRO
gd_sf_std_k_long <- gd_df_std_k %>% 
  pivot_longer(cols = (PC_asian:PC_white))
cluster_centers <- km$centers
cluster_centers_df <- as.data.frame(cluster_centers)
cluster_centers_df$Cluster <- c(1, 2, 3, 4) # ADD CLUSTER VAR
cntr_pvt <- pivot_longer(cluster_centers_df, PC_asian:PC_white)



g_comb <- ggplot()+
  geom_boxplot(data = gd_sf_std_k_long, aes(x=name, y = value))+
  geom_line(data = cntr_pvt, aes(x=name, y = value, group = as.factor(Cluster),color = as.factor(Cluster)), size=1)+
  scale_color_manual(values=c("#e84c25", "#25e890", "#56B4E9", "purple"),
                     labels=c("1", "2", "3", "4"), name = "Cluster Mean")+
  scale_x_discrete(labels = c("% Asian", "% Black", "% Hispanic", "% White")) +
  theme(axis.text.x = element_text(vjust = 1, angle = 45))+
  labs(x="", y="Standardized Value") + 
  theme_bw()

g_comb
ggsave("figure2.png", plot = g_comb)


# look at the table with the cluster number joined to it
head(gd_df_std_k)

# create the point plot
gd_df_std_k %>%
  select(-GEOID) %>%
  pivot_longer(-Cluster) %>%
  ggplot() +
  geom_point(aes(x = value, y = name, colour = Cluster)) +
  facet_wrap(~ Cluster, nrow = 1)


# join the km cluster numbers to the sf
gd_sf_k <- mutate(gd_sf, "Cluster" = as.factor(km$cluster))

# look at it
head(gd_sf_k)

# map just the clusters (NOT INCLUDED IN REPORT)
tm_shape(gd_sf_k) +
  tm_polygons (col = 'Cluster') +
  tm_legend(legend.outside = FALSE)

# map four together 
# compare mapped variables to clusters
map1 <- tm_shape(gd_sf_k) + # (NOT INCLUDED IN REPORT)
  tm_fill ('PC_white', palette = "Reds", style = "quantile")+
  tm_legend(legend.outside = FALSE, bg.color="white")+
  tm_layout(frame=FALSE)

map2 <- tm_shape(gd_sf_k) +# (NOT INCLUDED IN REPORT)
  tm_fill ('PC_black', palette = "Reds", style='quantile', n=5)+
  tm_legend(legend.outside = FALSE, bg.color="white")+
  tm_layout(frame=FALSE)

map3 <- tm_shape(gd_sf_k) +# (NOT INCLUDED IN REPORT)
  tm_fill('PC_hispanic', palette = "Reds", style='quantile', n=5)+
  tm_legend(legend.outside = FALSE, bg.color="white")+
  tm_layout(frame=FALSE)

map4 <- tm_shape(gd_sf_k) +# (NOT INCLUDED IN REPORT)
  tm_fill('PC_asian', palette = "Reds", style='quantile', n=5) +
  tm_legend(legend.outside = FALSE, bg.color="white")+
  tm_layout(frame=FALSE)

tmap_arrange(map1, map2, map3, map4, ncol=2)# (NOT INCLUDED IN REPORT)

par(mfrow = c(2,2), mar = c(3,6,1,1)) # sets 2x2 display; sets margins for (bottom, left, top, right)

boxplot(PC_white ~ Cluster, data = gd_sf_k, ylim=c(0,90))# (NOT INCLUDED IN REPORT)
boxplot(PC_black ~ Cluster, data = gd_sf_k, ylim=c(0,90))# (NOT INCLUDED IN REPORT)
boxplot(PC_hispanic ~ Cluster, data = gd_sf_k, ylim=c(0,90))# (NOT INCLUDED IN REPORT)
boxplot(PC_asian ~ Cluster, data = gd_sf_k, ylim=c(0,90))# (NOT INCLUDED IN REPORT)
par(mfrow = c(1,1), mar = c(1,1,1,1)) # returns display to default

################################################################################

# ASSIGNMENT TASK 2 - CHOOSE 4-5 VARIABLES TO DO A GEODEMOGRAPHIC CLASSIFICATION WITH 
# LET'S LOOK AT MED AGE, INCOME, EMPLOYMENT STAT, MARRIED 

# PERFORM CLUSTER ANALYSIS
head(gd_df_std)

# SUBSET THE DATA FOR VAR OF INTEREST (MEDAGE,MEDINCOME,PCMARRIED, PCEMPLOYED)
gd_df_std_sel2 <- select(gd_df_std, c(1,2,3,7,8))
head(gd_df_std_sel2)

# SET SEED  (to get reproducible results with kmeans())
set.seed(123)

# create an empty vector that will hold 10 within-cluster-sum-of-squares
wcss2 <- numeric(10)

# run kmeans 10 times producing results for 1 to 10 possible clusters
for (i in 1:10) {
  # Fit the model: km.out
  km.out2 <- kmeans(gd_df_std_sel2[,-1], centers = i)
  # Save the within cluster sum of squares
  wcss2[i] <- km.out2$tot.withinss
}





# Put the data in a tibble and plot a scree plot
wcss_df2 <- tibble(clusters = 1:10, wcss = wcss2)

scree_plot2 <- ggplot(wcss_df2, aes(x = clusters, y = wcss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  labs(x = 'Number of clusters', y = 'Within cluster-sum-of-squares') +
  theme_bw()
scree_plot2
ggsave("figure3.png", plot=scree_plot2)

# USE THE SCREE PLOT ELBOW TO DETERMINE NUMBER OF CLUSTERS
# 3 CLUSTERS APPEARS OPTIMAL


## perform cluster analysis specifying clusters
# create a variable for number of clusters (so it can be passed to other functions below)
n_clust = 3
km <- kmeans(gd_df_std_sel2[,-1], n_clust)


# look at the cluster means 
km$centers

# join the km cluster numbers to the dataframe, while naming the added field 'Cluster'
gd_df_std_k2 <- mutate(gd_df_std_sel2, "Cluster" = as.factor(km$cluster))


# GET THE BOXPLOT LIKE IN ARCPRO
gd_sf_std_k_long2 <- gd_df_std_k2 %>% 
  pivot_longer(cols = (c(median_ageE, median_incomeE, PC_married, PC_employment_stat)))
cluster_centers2 <- km$centers
cluster_centers_df2 <- as.data.frame(cluster_centers2)
cluster_centers_df2$Cluster <- c(1, 2, 3) # ADD CLUSTER VAR
cntr_pvt2 <- pivot_longer(cluster_centers_df2, c(median_ageE, median_incomeE, PC_married, PC_employment_stat))


g_comb2 <- ggplot()+
  geom_boxplot(data = gd_sf_std_k_long2, aes(x=name, y = value))+
  geom_line(data = cntr_pvt2, aes(x=name, y = value, group = as.factor(Cluster),color = as.factor(Cluster)), size=1)+
  scale_color_manual(values=c("#e84c25", "#25e890", "#56B4E9"),
                     labels=c("1", "2", "3"), name = "Cluster Mean")+
  scale_x_discrete(labels = c("Median age", "Median income", "% unemployed", "% married")) +
  labs(x="", y="Standardized Value") +
  theme_bw()

g_comb2
ggsave("figure4.png", plot=g_comb2)

# look at the table with the cluster number joined to it
head(gd_df_std_k2)

# create the point plot
gd_df_std_k2 %>%
  select(-GEOID) %>%
  pivot_longer(-Cluster) %>%
  ggplot() +
  geom_point(aes(x = value, y = name, colour = Cluster)) +
  facet_wrap(~ Cluster, nrow = 1)


# join the km cluster numbers to the sf
gd_sf_k2 <- mutate(gd_sf, "Cluster" = as.factor(km$cluster))

# look at it
head(gd_sf_k2)



# map four together 
# compare mapped variables to clusters
map1 <- tm_shape(gd_sf_k2) +
  tm_fill ('median_ageE', palette = "Blues", title = "Median age",
           style = "quantile", n = 5)+
  tm_legend(legend.outside = FALSE, bg.color="white")+
  tm_layout(frame=FALSE)


map2 <- tm_shape(gd_sf_k2) +
  tm_fill ('median_incomeE', palette = "Blues", title = "Median income",
           style='quantile', n=5)+
  tm_legend(legend.outside = FALSE, bg.color="white")+
  tm_layout(frame=FALSE)

map3 <- tm_shape(gd_sf_k2) +
  tm_fill('PC_married', style='quantile', 
          palette = "Blues", title = "% married", n=5)+
  tm_legend(legend.outside = FALSE, bg.color="white")+
  tm_layout(frame=FALSE)

map4 <- tm_shape(gd_sf_k2) +
  tm_fill('PC_employment_stat',palette = "Blues",
          title = "% unemployed",
          style='quantile', n=5) +
  tm_legend(legend.outside = FALSE, bg.color="white")+
  tm_layout(frame=FALSE)

map_comb <- tmap_arrange(map1, map2, map3, map4, ncol=2)
tmap_save(map_comb, "figure5.png") # USE TMAP SAVE TO SAVE THEMATIC MAPS 

# MAP THE CLUSTERS 
map5 <- tm_shape(gd_sf_k2) +
  tm_fill('Cluster') +
  tm_legend(legend.outside = FALSE, bg.color="white")+
  tm_layout(frame=FALSE)
tmap_save(map5, "figure6.png") # USE TMAP SAVE TO SAVE THEMATIC MAPS 

map5
tmap_save(map5, "figure6.png") 

################################################################################
