#ENM maps production
#RStudio 2022.12.0+353
#R version 4.2.2

#########################################
#   Environmental Niche Modeling Run    #
#########################################

setwd()

### Load Libraries ---- 
library(rgbif)
library(ENMeval)
library(raster)
library(sp)
library(sf)
library(pbapply)
library(tidyverse)
library(RStoolbox)
library(ggplot2)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(dismo)
library(factoextra)
library(beepr)

### Load in setup file ----
source('ENM_setup.R')

### Get occurrence data and plot them to assess occurrences from the correct regions have been downloaded  ---


# Name of the species you want to work with. Do this for every species 
species <-  "Emys_orbicularis"

# Get occurencies report
occs <- get_occ(species)

# If file was already downloaded
#occs <- read.csv("data/Emys_orbicularis/occurences.csv")

# Plot the occurrence downloaded to check their position
plot_occ(occs, species)

# Write a table with every occurrence downloaded 
write.csv(occs, paste0("data/", species, "/occurrences.csv"))

### Get information from the PCA ----

# Get covariate data for all occurences
covs <- get_covs(occs, wc_data, bufsize=10)

# Rasterize covs to save them
saveRDS(covs, paste0("data/", species, "/covs_general.Rds"))
#readRDS(paste0("data/", species, "/covs_general.Rds"))  #to load them if you already did this step

# Extract information on covariates

summary(covs$model)
get_eigenvalue(covs$model)
loadings(covs$model)

### Fit an ENM model ----

mod_list <- fit_models(occs, covs, species)

# Select best model
# Identify best model (by AIC)
bestmod <- best_model(mod_list)

# Get model info
mod_list@results

 # Export the model
saveRDS(mod_list, paste0("data/", species, "/General_model.Rds"))
#mod_list <- readRDS(paste0("data/", species, "/General_model.Rds"))   load the model if already produced

### Generate predictive maps based on best model ---

# Present

pr_map <- enm.maxnet@predict(bestmod, covs$map, other.settings = list(pred.type='logistic', doClamp=F))
plot(pr_map, main = 'Present_prediction')

saveRDS(pr_map, paste0("data2/", species, "/present_map.Rds"))  # Analysed species name


# Last Interglacial Period (LIG)
int <- c(present=covs$map, int_data)
int <- crop(int_data, get_buffer(occs, bufsize=10))
int <- get_scores(int, covs) #get information from PCA
inter_map<- enm.maxnet@predict(bestmod, int, other.settings = list(pred.type='logistic', doClamp=F))
plot(inter_map, main = 'interglacial_prediction')

saveRDS(inter_map, paste0("data2/", species, "/inter_map.Rds"))  # Analysed species name

# Last Glacial Maximum
gm <- c(present=covs$map, gm_data)
gm <- crop(gm_data, get_buffer(occs, bufsize=10))
gm <- get_scores(gm, covs)  #get information from PCA
gm_map<- enm.maxnet@predict(bestmod, gm, other.settings = list(pred.type='logistic', doClamp=F))
plot(gm_map, main = 'last_glacial_maximum_prediction')

saveRDS(gm_map, paste0("data2/", species, "/gm_map.Rds"))

# Load Glaciers shapefiles 
glaciers <- read_sf("data/digital_maps_02_all_other_files/lgm_global.shp")
alpes_lgm <- read_sf("data/digital_maps_02_all_other_files/lgm_alpen.shp")

# Remove permanent glaciers from predicted occurrence maps (only for graphical maps generation; these areas are always associated to negligible predicted occurrence probabilities)

gm_no_ice <- raster::mask(gm_map, glaciers, inverse = T)
gm_no_ice <- raster::mask(gm_no_ice, alpes_lgm, inverse = T)

saveRDS(gm_no_ice, paste0("data2/", species, "/gm_no_ice.Rds"))  # Analysed species name

# Early Holocene Period
eh <- c(present=covs$map, eh_data)
eh <- crop(eh_data, get_buffer(occs, bufsize=10))
eh <- get_scores(eh, covs)#get information from PCA
eh_map<- enm.maxnet@predict(bestmod, eh, other.settings = list(pred.type='logistic', doClamp=F))
plot(eh_map, main = 'early_holocene_prediction')

saveRDS(eh_map, paste0("data2/", species, "/eh_map.Rds"))  # Analysed species name

# Load Glaciers shapefiles 

ice_cov_EH_NA <- read_sf("data/ice_reconstruction/margins/North_America_a1/North_America_EH.shp")
ice_cov_EH_Eu <- read_sf("data/ice_reconstruction/margins/Eurasia/Eurasia_EH.shp")
ice_cov_EH_Pa <- read_sf("data/ice_reconstruction/margins/Patagonia/Patagonia_EH.shp")

# Remove glaciers from maps (as for line 109)

eh_no_ice <- raster::mask(eh_map, ice_cov_EH_NA, inverse = T)
eh_no_ice <- raster::mask(eh_no_ice, ice_cov_EH_Eu, inverse = T)
eh_no_ice <- raster::mask(eh_no_ice, ice_cov_EH_Pa, inverse = T)

plot(eh_no_ice)
  saveRDS(eh_no_ice, paste0("data2/", species, "/eh_no_ice.Rds"))  # Analysed species name
  
### MIS19 model subset ----

# Get Covariates 
covs_M19 <- get_covs(occs, subset(wc_data, c(1,4,8:19)), bufsize=10)

# Extract information on covariates
saveRDS(covs_M19, paste0("data/", species, "/MIS19_covs.Rds"))  # Analysed species name


# Get model info
summary(covs_M19$model)
get_eigenvalue(covs_M19$model)
loadings(covs_M19$model)

# Create a model 
mod_M19 <- fit_models(occs, covs_M19, species, pliest=TRUE)

# Save the model
saveRDS(mod_M19, paste0("data/", species, "/MIS19_model.Rds"))  # Analysed species name
#readRDS("data2/Emys_orbicularis/MIS19_model.Rds")   #Load model in already produced


# Select best model
bestmod_M19 <- best_model_M19(mod_M19)
# Get model info
mod_M19@results
# Identify best model (by AIC)

# Generate predictive maps based on the best model
M19 <- c(present=covs$map, pliest)
M19 <- crop(pliest, get_buffer(occs, bufsize=10))
M19 <- get_scores(M19, covs_M19)#get information from PCA
M19_map <- enm.maxnet@predict(bestmod_M19, M19, other.settings = list(pred.type='logistic', doClamp=F))
plot(M19_map, main = 'MIS-19_prediction')

saveRDS(M19_map, paste0("data2/", species, "/pl_map.Rds"))  # Analysed species name
