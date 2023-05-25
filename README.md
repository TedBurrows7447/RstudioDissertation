# RstudioDissertation
My code for my dissertation


# install.packages

library(dplyr)
library(sf)
library(tmap)
library(ggplot2)
library(tidyverse)
install.packages("writexl")

#london shapefile to clip the data
london <- st_read("england_rgn_2022.shp")
qtm(london)

## 1. Load CSV data of health facilities
gp <- read.csv("GPPractices.csv")
hs <- read.csv("Hospital.csv")
ph <- read.csv("Pharmacy.csv")
de <- read.csv("Dentists.csv")



hs_orig = hs

# the dimensions of hs/ filtering out the private sector
dim(hs)
hs <- filter(hs, Sector == "NHS Sector")
# the new dimensions of hs
dim(hs)

## Make spatial data

# gp - GP surgeries
summary(gp[,c("Latitude", "Longitude")])
gp <- filter(gp, !is.na(Latitude))
gp_sf <- st_as_sf(gp, coords = c("Longitude", "Latitude"), crs = 4326)

# hs - hospitals
hs <- filter(hs, !is.na(Latitude))
hs_sf <- st_as_sf(hs, coords = c("Longitude", "Latitude"), crs = 4326)
# ph - pharmacies
ph <- filter(ph, !is.na(Latitude))
ph_sf <- st_as_sf(ph, coords = c("Longitude", "Latitude"), crs = 4326)
# de - dentists
de <- filter(de, !is.na(Latitude))
de_sf <- st_as_sf(de, coords = c("Longitude", "Latitude"), crs = 4326)



## Clip spatial to London area 

london <- st_read("england_rgn_2022.shp")
qtm(london)

tmap_options(check.and.fix = TRUE)

# Initial plots of each of the health centres 

  tm_shape(london) + tm_polygons() + 
    tm_shape(hs_sf) +tm_dots(col = "purple", size = 0.01, alpha = 0.5)

tm_shape(london) + tm_polygons() + 
  tm_shape(gp_sf) +tm_dots(col = "green", size = 0.01, alpha = 0.5)

tm_shape(london) + tm_polygons() + 
  tm_shape(ph_sf) +tm_dots(col = "lightblue", size = 0.01, alpha = 0.5)

tm_shape(london) + tm_polygons() + 
  tm_shape(de_sf) +tm_dots(col = "red", size = 0.01, alpha = 0.5)


tmap_mode("view")
# transform projections
gp_sf <- st_transform(gp_sf, 27700)
hs_sf <- st_transform(hs_sf, 27700)
ph_sf <- st_transform(ph_sf, 27700)
de_sf <- st_transform(de_sf, 27700)
# clip to the london layer
gp_sf <- gp_sf[london,]
hs_sf <- hs_sf[london,]
ph_sf <- ph_sf[london,]
de_sf <- de_sf[london,]


 ## Demand- creating the population weighted centroids
lsoapls <- st_read ("LSOA_2011_London_gen_MHW.shp")
oa.pc <- st_read("LSOAREAL.csv")

#map the pop centroids
tmap_options(check.and.fix = TRUE)
tmap_mode("view")
tm_shape(london) + tm_polygons(alpha = 0.6) +
  tm_shape(oa.pc_sf) + tm_dots(col = "red") +
  tm_basemap("OpenStreetMap")
# reset tmap mode
tmap_mode("plot")


## Supply and Demand distances- finding out each population centroids distance to health centres
# gp
nearest <- st_nearest_feature(oa.pc_sf, gp_sf)
# check
length(nearest)
nrow(oa.pc_sf)
gp_dist <- st_distance(oa.pc_sf, gp_sf[nearest,], by_element=TRUE)
head(gp_dist)
summary(gp_dist)
# hospitals
nearest <- st_nearest_feature(oa.pc_sf, hs_sf)
hosp_dist <- st_distance(oa.pc_sf, hs_sf[nearest,], by_element=TRUE)
head(hosp_dist)
# pharmacy
nearest <- st_nearest_feature(oa.pc_sf, ph_sf)
pharm_dist <- st_distance(oa.pc_sf, ph_sf[nearest,], by_element=TRUE)
# dentists
nearest <- st_nearest_feature(oa.pc_sf, de_sf)
dent_dist <- st_distance(oa.pc_sf, de_sf[nearest,], by_element=TRUE)


#Using the same format for the health data for the rest of the domains

#Primary school
#read in the primary school data
ps <- read.csv("First, Primary and Infant schools.csv") 
head(ps)
#create a subset to change easting and northing to long/lat
ps.sub <- subset(ps, select = c("name", "feature_easting", "feature_northing"))
head(ps.sub)
# Create a unique ID for each primary school
ps.sub$ps.sub_ID <- 1:nrow(ps.sub)
# Create coordinates variable
coords.ps <- cbind(easting = as.numeric(as.character(ps.sub$feature_easting)),
                   northing = as.numeric(as.character(ps.sub$feature_northing)))
# Create the SpatialPointsDataFrame
ps.sub_SP <- SpatialPointsDataFrame(coords.ps, data = data.frame(ps.sub$ps.sub_ID), proj4string = CRS("+init=epsg:27700"))
#check it worked through plotting and renaming coloumns
plot(ps.sub_SP)
ps.sub_SP_LL <- sp::spTransform(ps.sub_SP, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
colnames(ps.sub_SP_LL@coords)[colnames(ps.sub_SP_LL@coords) == "easting"] <- "Longitude" 
colnames(ps.sub_SP_LL@coords)[colnames(ps.sub_SP_LL@coords) == "northing"] <- "Latitude"
head(ps.sub_SP_LL@coords)
summary(ps.sub_SP_LL@coords[,c("Latitude", "Longitude")])
#project to CRS
PS <- st_as_sf(ps.sub_SP_LL, coords = c("Longitude", "Latitude"), crs = 4326)
#plot primary schools
tmap_options(check.and.fix = TRUE)
tm_shape(london) + tm_polygons() + 
  tm_shape(PS) +tm_dots(col = "orange", size = 0.04, alpha = 0.5)
#clip to london
PS <- st_transform(PS, 27700)
PS <- PS[london,]
## Supply and Demand distances
nearest <- st_nearest_feature(oa.pc_sf, PS)
# check
length(nearest)
nrow(oa.pc_sf)
PS_dist <- st_distance(oa.pc_sf, PS[nearest,], by_element=TRUE)
head(PS_dist)


#State Secondary
#read it in 
ss <- read.csv("Broad age range and Secondary state schools.csv") 
head(ss)
#create subset to change from easting/northing to log/lat
ss.sub <- subset(ss, select = c("name", "feature_easting", "feature_northing"))
head(ss.sub)
# Create a unique ID for each Secondary
ss.sub$ss.sub_ID <- 1:nrow(ss.sub)
# Create coordinates variable
coords.ss <- cbind(easting = as.numeric(as.character(ss.sub$feature_easting)),
                   northing = as.numeric(as.character(ss.sub$feature_northing)))
# Create the SpatialPointsDataFrame
ss.sub_SP <- SpatialPointsDataFrame(coords.ss, data = data.frame(ss.sub$ss.sub_ID), proj4string = CRS("+init=epsg:27700"))
plot(ss.sub_SP)
ss.sub_SP_LL <- sp::spTransform(ss.sub_SP, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
colnames(ss.sub_SP_LL@coords)[colnames(ss.sub_SP_LL@coords) == "easting"] <- "Longitude" 
colnames(ss.sub_SP_LL@coords)[colnames(ss.sub_SP_LL@coords) == "northing"] <- "Latitude"
head(ss.sub_SP_LL@coords)
summary(ss.sub_SP_LL@coords[,c("Latitude", "Longitude")])
SS <- st_as_sf(ss.sub_SP_LL, coords = c("Longitude", "Latitude"), crs = 4326)
plot(st_geometry(SS), cex = 0.2)
#plot the secondary scools
# set the tmap mode to view
tmap_mode("view")
tm_shape(SS) +tm_dots(col = "red") + tm_basemap("OpenStreetMap")
# set is back to plot
tmap_mode("plot")
tmap_options(check.and.fix = TRUE)
tm_shape(london) + tm_polygons() + 
  tm_shape(SS) +tm_dots(col = "orange", size = 0.04, alpha = 0.5)
SS <- st_transform(SS, 27700)
SS <- SS[london,]
## Supply and Demand distances
# Secondary school distances
nearest <- st_nearest_feature(oa.pc_sf, SS)
# check
length(nearest)
nrow(oa.pc_sf)
SS_dist <- st_distance(oa.pc_sf, SS[nearest,], by_element=TRUE)
head(SS_dist)

#Further Education establishments 
fe <- read.csv("Further education establishments.csv") 
head(fe)
#create subset
fe.sub <- subset(fe, select = c("name", "feature_easting", "feature_northing"))
head(fe.sub)
# Create a unique ID for each Further education
fe.sub$fe.sub_ID <- 1:nrow(fe.sub)
# Create coordinates variable
coords.fe <- cbind(easting = as.numeric(as.character(fe.sub$feature_easting)),
                   northing = as.numeric(as.character(fe.sub$feature_northing)))
# Create the SpatialPointsDataFrame
fe.sub_SP <- SpatialPointsDataFrame(coords.fe, data = data.frame(fe.sub$fe.sub_ID), proj4string = CRS("+init=epsg:27700"))
plot(fe.sub_SP)
fe.sub_SP_LL <- sp::spTransform(fe.sub_SP, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
colnames(fe.sub_SP_LL@coords)[colnames(fe.sub_SP_LL@coords) == "easting"] <- "Longitude" 
colnames(fe.sub_SP_LL@coords)[colnames(fe.sub_SP_LL@coords) == "northing"] <- "Latitude"
head(fe.sub_SP_LL@coords)
summary(fe.sub_SP_LL@coords[,c("Latitude", "Longitude")])
FE <- st_as_sf(fe.sub_SP_LL, coords = c("Longitude", "Latitude"), crs = 4326)
plot(st_geometry(FE), cex = 0.2)
# set the tmap mode to view and plot 
tmap_mode("view")
tm_shape(FE) +tm_dots(col = "red") + tm_basemap("OpenStreetMap")
# set is back to plot
tmap_mode("plot")
tmap_options(check.and.fix = TRUE)
tm_shape(london) + tm_polygons() + 
  tm_shape(FE) +tm_dots(col = "orange", size = 0.04, alpha = 0.5)
FE <- st_transform(FE, 27700)
FE <- FE[london,]
## Supply and Demand distances for each further education
# gp
nearest <- st_nearest_feature(oa.pc_sf, FE)
# check
length(nearest)
nrow(oa.pc_sf)
FE_dist <- st_distance(oa.pc_sf, FE[nearest,], by_element=TRUE)
head(FE_dist)

#Supermarkets
sm <- read.csv("Supermarket chains.csv") 
head(sm)
#create subset
sm.sub <- subset(sm, select = c("name", "feature_easting", "feature_northing"))
head(sm.sub)
# Create a unique ID for each supermarket
sm.sub$sm.sub_ID <- 1:nrow(sm.sub)
# Create coordinates variable
coords.sm <- cbind(easting = as.numeric(as.character(sm.sub$feature_easting)),
                   northing = as.numeric(as.character(sm.sub$feature_northing)))
# Create the SpatialPointsDataFrame
sm.sub_SP <- SpatialPointsDataFrame(coords.sm, data = data.frame(sm.sub$sm.sub_ID), proj4string = CRS("+init=epsg:27700"))
plot(sm.sub_SP)
sm.sub_SP_LL <- sp::spTransform(sm.sub_SP, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
colnames(sm.sub_SP_LL@coords)[colnames(sm.sub_SP_LL@coords) == "easting"] <- "Longitude" 
colnames(sm.sub_SP_LL@coords)[colnames(sm.sub_SP_LL@coords) == "northing"] <- "Latitude"
head(sm.sub_SP_LL@coords)
summary(sm.sub_SP_LL@coords[,c("Latitude", "Longitude")])
SM <- st_as_sf(sm.sub_SP_LL, coords = c("Longitude", "Latitude"), crs = 4326)
plot(st_geometry(SM), cex = 0.2)
#plot supermarkets
# set the tmap mode to view
tmap_mode("view")
tm_shape(SM) +tm_dots(col = "red") + tm_basemap("OpenStreetMap")
tmap_options(check.and.fix = TRUE)
tm_shape(london) + tm_polygons() + 
  tm_shape(SM) +tm_dots(col = "pink", size = 0.04, alpha = 0.5)
SM <- st_transform(SM, 27700)
SM <- SM[london,]
## Supply and Demand distances for supermarkets
nearest <- st_nearest_feature(oa.pc_sf, SM)
# check
length(nearest)
nrow(oa.pc_sf)
SM_dist <- st_distance(oa.pc_sf, SM[nearest,], by_element=TRUE)
head(SM_dist)

#Convienience stores
cn <- read.csv("Convienience stores and independent supermarkets.csv") 
head(cn)
#create subset for each convience store
cn.sub <- subset(cn, select = c("name", "feature_easting", "feature_northing"))
head(cn.sub)
# Create a unique ID for each store
cn.sub$cn.sub_ID <- 1:nrow(cn.sub)
# Create coordinates variable
coords.cn <- cbind(easting = as.numeric(as.character(cn.sub$feature_easting)),
                   northing = as.numeric(as.character(cn.sub$feature_northing)))
# Create the SpatialPointsDataFrame
cn.sub_SP <- SpatialPointsDataFrame(coords.cn, data = data.frame(cn.sub$cn.sub_ID), proj4string = CRS("+init=epsg:27700"))
plot(cn.sub_SP)
cn.sub_SP_LL <- sp::spTransform(cn.sub_SP, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
colnames(cn.sub_SP_LL@coords)[colnames(cn.sub_SP_LL@coords) == "easting"] <- "Longitude" 
colnames(cn.sub_SP_LL@coords)[colnames(cn.sub_SP_LL@coords) == "northing"] <- "Latitude"
head(cn.sub_SP_LL@coords)
summary(cn.sub_SP_LL@coords[,c("Latitude", "Longitude")])
CN <- st_as_sf(cn.sub_SP_LL, coords = c("Longitude", "Latitude"), crs = 4326)
plot(st_geometry(CN), cex = 0.2)
# set the tmap mode to view
tmap_mode("view")
tm_shape(CN) +tm_dots(col = "red") + tm_basemap("OpenStreetMap")
# set is back to plot
tmap_mode("plot")
tmap_options(check.and.fix = TRUE)
tm_shape(london) + tm_polygons() + 
  tm_shape(CN) +tm_dots(col = "darkred", size = 0.04, alpha = 0.5)
CN <- st_transform(CN, 27700)
CN <- CN[london,]
## Supply and Demand distances for each convienience store
nearest <- st_nearest_feature(oa.pc_sf, CN)
# check
length(nearest)
nrow(oa.pc_sf)
CN_dist <- st_distance(oa.pc_sf, CN[nearest,], by_element=TRUE)
head(CN_dist)


#Halls and Community centres
cc <- read.csv("Halls and community centres.csv") 
head(cc)

#Convert Easting/northing to lat and longitude
cc.sub <- subset(cc, select = c("name", "easting", "northing"))
head(cc.sub)
# Create a unique ID for each com centre
cc.sub$cc.sub_ID <- 1:nrow(cc.sub)
# Create coordinates variable
coords.cc <- cbind(easting = as.numeric(as.character(cc.sub$easting)),
                   northing = as.numeric(as.character(cc.sub$northing)))
# Create the SpatialPointsDataFrame
cc.sub_SP <- SpatialPointsDataFrame(coords, data = data.frame(cc.sub$cc.sub_ID), proj4string = CRS("+init=epsg:27700"))
plot(cc.sub_SP)
cc.sub_SP_LL <- sp::spTransform(cc.sub_SP, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
colnames(cc.sub_SP_LL@coords)[colnames(cc.sub_SP_LL@coords) == "easting"] <- "Longitude" 
colnames(cc.sub_SP_LL@coords)[colnames(cc.sub_SP_LL@coords) == "northing"] <- "Latitude"
head(cc.sub_SP_LL@coords)
summary(cc.sub_SP_LL@coords[,c("Latitude", "Longitude")])
CC <- st_as_sf(cc.sub_SP_LL, coords = c("Longitude", "Latitude"), crs = 4326)
plot(st_geometry(CC), cex = 0.2)
tm_shape(CC) +tm_dots() + tm_layout(title = "Halls and Community Centre")
#plot the com centres
# set the tmap mode to view
tmap_mode("view")
tm_shape(CC) +tm_dots(col = "red") + tm_basemap("OpenStreetMap")
# set is back to plot
tmap_mode("plot")
tmap_options(check.and.fix = TRUE)
tm_shape(london) + tm_polygons() + 
  tm_shape(CC) +tm_dots(col = "grey", size = 0.04, alpha = 0.5)
CC <- st_transform(CC, 27700)
CC <- CC[london,]
## Supply and Demand distances for com centres
nearest <- st_nearest_feature(oa.pc_sf, CC)
# check
length(nearest)
nrow(oa.pc_sf)
CC_dist <- st_distance(oa.pc_sf, CC[nearest,], by_element=TRUE)
head(CC_dist)
summary(CC_dist)


#Greenspace 
#read in greenspace access point dar
gsap = st_read("TQ_AccessPoint.shp", quiet = T)
head(gsap)
gsap.sub <- subset(gsap, select = c( "geometry"))
head(gsap.sub)
plot(gsap.sub)
plot(st_geometry(gsap.sub), cex = 0.2)
# set the tmap mode to view and initially map points 
tmap_mode("view")
tm_shape(gsap.sub) +tm_dots(col = "red") + tm_basemap("OpenStreetMap")
tmap_options(check.and.fix = TRUE)
tm_shape(london) + tm_polygons() + 
  tm_shape(gsap.sub) +tm_dots(col = "green", size = 0.04, alpha = 0.5)
GS <- st_transform(gsap.sub, 27700)
GS <- GS[london,]
## Supply and Demand distances
nearest <- st_nearest_feature(oa.pc_sf, GS)
# check
length(nearest)
nrow(oa.pc_sf)
GS_dist <- st_distance(oa.pc_sf, GS[nearest,], by_element=TRUE)
head(GS_dist)


#Accessibility index
#Groupings 

#Health Accessibility
#creating health dataframe
healthdist_df <- data.frame(gp_dist, hosp_dist, pharm_dist, dent_dist)
#adding distances
healthdist <- rowSums(healthdist_df)
#normalising 
healthdist <- 100 * (1- healthdist/max(healthdist))
summary(healthdist)
head(healthdist)
#joining index to mke spatial
MAPfinal.ss$healthdist <- healthdist[join_index]

#School Accessibility
#same process as above
schooldist_df <- data.frame(PS_dist, SS_dist, FE_dist)
schooldist <- rowSums(schooldist_df)
schooldist <- 100 * (1- schooldist/max(schooldist))
summary(schooldist)
MAPfinal.ss$schooldist <- schooldist1[join_index]

#Grocery shop accessibility
#same process
grocerydist_df <- data.frame(SM_dist, CN_dist)
grocerydist <- rowSums(grocerydist_df)
grocerydist <- 100 * (1- grocerydist/max(grocerydist))
summary(grocerydist1)
MAPfinal.ss$grocerydist <- grocerydist1[join_index]

#Halls and Community center accessibility
CC_dist1 <- rowSums(CC_dist)
CC_dist1 <- 100 * (1- CC_dist/max(CC_dist))
summary(CC_dist)
MAPfinal.ss$CC_dist1 <- CC_dist1[join_index]

#Greenspace accessibility 
#distance to access points for greenspace from pop centroid
GSfinal<- rowSums(GSCC_df[ , 1, drop = FALSE])
GSfinal <- 100 * (1- GSfinal/max(GSfinal))
MAPfinal.ss$GSfinal <- GSfinal[join_index]


#creating the borough boundaries 
boroughs = st_read("London_Borough_Excluding_MHW.shp", quiet = T)
#creating a subset with just the spatial data
boroughs.sub <- subset(boroughs, select = c( "geometry"))
#borough map
boroughs <- tm_shape(boroughs.sub) +
  tm_borders("gray") + tm_text("NAME", size = 0.5) + tm_layout( frame = FALSE) 

#MAPS for each of the domains
tmap_mode("plot")

#community centre map
tm_shape(lsoapls) +
  tm_fill("CCfinal",
          palette = "Greens", 
          title = "Community centre accessibility") + tm_shape(boroughs) +
  tm_borders("black", lwd= 0.6) + tm_layout( frame = FALSE, legend.position = c("left", "top")) 

#greenspace map
tm_shape(lsoapls) +
  tm_fill("GSfinal",
          palette = "Greens", 
          title = "Greenspace accessibility") + tm_shape(boroughs) +
  tm_borders("black", lwd= 0.6) + tm_layout( frame = FALSE, legend.position = c("left", "top")) 

#school map 
tm_shape(lsoapls) +
  tm_fill("schooldist", palette = "Greens", 
          title = "School Accessibility") + tm_shape(boroughs) +
  tm_borders("black", lwd= 0.6) + tm_layout( frame = FALSE, legend.position = c("left", "top")) 

#health map 
tm_shape(lsoapls) +
  tm_fill("healthdist", palette = "Greens", 
          title = "Health Service Accessibility") + tm_shape(boroughs) +
  tm_borders("black", lwd= 0.6) + tm_layout( frame = FALSE, legend.position = c("left", "top")) 

#grocery map
tm_shape(lsoapls) +
  tm_fill("grocerydist",
          palette = "Greens", 
          title = "Grocery shop accessibility") + tm_shape(boroughs) +
  tm_borders("black", lwd= 0.6) + tm_layout( frame = FALSE, legend.position = c("left", "top")) 




#Whole accessibility
#create dataframe of all domains
MAPfinal.ss <- data.frame( healthdist, schooldist, grocerydist, CCfinal, GSfinal)
#creating new coloumn with non-weigjted index scoresthe index 
MAPfinal.ss$newindexnoweights=(healthdist+schooldist+grocerydist+CCfinal1+GSfinal1)/5
#distributing the weights for the weighted index
MAPfinal.ss$indexfinal=(healthdist * 35 + schooldist * 20 + grocerydist * 15 + CCfinal * 10 + GSfinal* 20)/100

#creating the comprehensive index map without and with weights 
Noweight <-  tm_shape(MAPfinaldone) +
   tm_fill("indexnoweightspls",
           palette = "Greens", 
           title = "Accessibility score", breaks = c(0, 20, 40, 60, 80, 100)) + tm_shape(boroughs) +
   tm_borders("black", lwd= 0.6) + tm_layout( frame = FALSE, legend.position = c("left", "top")) 
 
weights <- tm_shape(MAPfinaldone) +
   tm_fill("indexfinalpls",
           palette = "Greens", 
           title = "Accessibility score", breaks = c(0, 20, 40, 60, 80, 100)) + tm_shape(boroughs) +
   tm_borders("black", lwd= 0.6) + tm_layout( frame = FALSE, legend.position = c("left", "top")) 
 #arrange them next to eachother
 tmap_arrange(Noweight, weights)
 
 

#create index table to write into excel
Indexscoretable <- subset(MAPfinal, select = c( "LSOA11CD", "healthdist", "schooldist", "grocerydist", "CCfinal", "GSfinal", "indexfinalnowts","indexfinal"))
 head(Indexscoretable)
 library("writexl")
 write_xlsx(Indexscoretable,"Desktop\\Final Index table.xlsx")
 

 #Population density 
 #read in population density data
PD2 <- read.csv("POPDENSSS.csv")
 #merge,clip to london 
PD2sp <- merge(MAPfinal, PD2, by.x="LSOA11CD", by.y="LSOA.Code")
PDfinal <- st_transform(PD2sp, 27700)
PDfinal <- PD2sp[london,]
#checking the class to check numeric
class(PDfinal$People.per.Sq.Km)
library(dplyr)
PDfinalpls <- PDfinal %>% mutate_at(c('People.per.Sq.Km'), as.numeric)
class(PDfinal$Area.Sq.Km)
# remove commas and convert to numeric values so able to map
unique(PDfinal$People.per.Sq.Km[is.na(PDfinalpls$People.per.Sq.Km)])
PDfinal$People.per.Sq.Km <- as.numeric(gsub(",", "", PDfinal$People.per.Sq.Km)) 

#create pop dens map with quintiles
tm_shape(PDfinal) +
  tm_fill("People.per.Sq.Km",
          palette = "Purples", 
          title = "Population density", breaks= c(0, 4898, 7471, 10760, 15517, 106716)) + tm_shape(boroughs) +
  tm_borders("black", lwd= 0.6) + 
  tm_layout(frame = FALSE, legend.position = c("left", "top"))

tmap_mode('plot')
tmap_options(check.and.fix = TRUE)
#arrange to put population density and weighted accessibility together 
tmap_arrange(PDmap, weights)


#Poverty - Townsend deprivation index 
#read in the Townsend scores
TDSscores <- read.csv("TDS scores.csv") 
head(TDSscores) 
#merging to clip to london
TDSlsoa <- merge(lsoapls, TDSscores, by.x="LSOA11CD", by.y="GEO_CODE")
 
#Map for TDS
deprivation <- tm_shape(TDSlsoa) +
  tm_fill("quintile",palette = "-RdBu", 
          title = "TDS quintiles") + tm_shape(boroughs) +
  tm_borders("black", lwd= 0.6) + tm_layout( frame = FALSE, legend.position = c("left", "top")) 

#putting TDS and accessibility together 
tmap_arrange(deprivation, weights)
