#ENM maps production
#RStudio 2022.12.0+353
#R version 4.2.2

#########################################
#  Environmental Niche Modeling Setup   #
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


### Bioclimatic data preparation ----

# Load present-day climatic data
wc_data <- readRDS('data/worldclim_2.5.Rds')

# Load past climatic covariate rasters
int_data <- stack(lapply(paste0('data/interglacial/bio_',1:19,'.tif'), raster))
gm_data <- stack(lapply(paste0('data/glacialmax//bio_',1:19,'.tif'), raster))
eh_data <-   stack(lapply(paste0('data/eh_hol/bio_',1:19,'.tif'),raster))

# Create a list of the maps for downstream analysis 
map_list <- lapply(list(
  interglacial=paste0('data/interglacial/bio_',1:19,'.tif'),
  glacialmax=paste0('data/glacialmax/bio_',1:19,'.tif'),
  earlyholo=paste0('data/eh_hol/bio_',1:19,'.tif')),
  function(files) stack(lapply(files, raster)))

map_list$interglacial$bio_3 <- mask(map_list$interglacial$bio_3,
                                    map_list$interglacial$bio_2)

# Load MIS-19 raster 
pliest <- stack(lapply(paste0('data/MIS_19/bio_',c(1,4,8:19),'.tif'), raster))


### Occurrence data preparation ---

# Distribution maps for tortoises and turtles are not available in the literature.
# To restrict the species occurrence predictions to their natural range (excluding invasions and traslocations) we manually defined the
 # area to download the occurrences from. This can be done by defining the latitude and
 # longitude of the desired area (lat_lim and lng_lim) or restricting the search area
 # to the desired continent

get_occ <- function(species, ignore_bl=FALSE){
  lat_lim <- '-90,90'
  lng_lim <- '-180,180'
  #see how many available occurrence records there are
  avail_points <- occ_search(scientificName=species,
                             hasCoordinate= TRUE,
                             hasGeospatialIssue= FALSE, year='1970,2022',
                             basisOfRecord = 'HUMAN_OBSERVATION',
                             continent= "Europe",   #To limit the search to the desired continent 
                             decimalLatitude=lat_lim,
                             decimalLongitude=lng_lim,
                             limit=0)$meta$count
  
  npoints <- min(10000, avail_points)
  
  
  # Download points in batches
  nbatches <- floor(npoints / 300)
  left <- npoints %% 300
  if(left > 0) nbatches <- nbatches + 1
  index <- 1
  cat("Downloading occurrence data\n")
  out <- pblapply(1:nbatches, function(i){
    lim <- ifelse(left > 0 & i == nbatches, left, 300)
    out <- occ_search(scientificName=species,
                      hasCoordinate=TRUE,
                      hasGeospatialIssue=FALSE, year='1970,2022',
                      basisOfRecord = 'HUMAN_OBSERVATION',
                      continent = "Europe",  #To limit the search to the desired continent
                      decimalLatitude=lat_lim,
                      decimalLongitude=lng_lim,
                      fields='minimal',limit=lim, start=index)$data
    index <<- index + lim
    out
  })
  

  out <- do.call("rbind", out) %>% drop_na()
  #Subsample to 3000 records if necessary
  if(nrow(out)>3000) out <- out[sample(1:nrow(out), 3000),]
  saveRDS(out, 'fname.Rds')
  cat(paste("Saved", nrow(out), "records\n"))
  
  out
}


# Convert occurrence records to spatial class
get_sp <- function(occs){
  SpatialPoints(as.data.frame(occs[,c('decimalLongitude','decimalLatitude')]))
}

# Buffer of 10 degrees around points
get_buffer <- function(occs, bufsize=10){
  data_sp <- get_sp(occs)
  bb <- bbox(data_sp)
  extent(bb[1]-bufsize, bb[3]+bufsize, bb[2]-bufsize, bb[4]+bufsize)
}


# Plot records
plot_occ <- function(occ, species){
  pts <- get_sp(occ)
  wm <- ne_coastline()
  plot(wm)
  plot(pts, add=T)
}


### Function for PCA and PC selections ----

# Get Bioclim covariates and do PCA
get_covs <- function(occs, wc, bufsize=10){
  
  bb_buf <- get_buffer(occs, bufsize)
  cat('Cropping WorldClim data\n')
  wc_clip <- crop(wc, bb_buf)
  
  #PCA
  cat('Doing PCA on climate data\n')
  out <- rasterPCA(wc_clip, spca=TRUE, maskCheck=FALSE, nSamples=10000)
  out$map <- subset(out$map, 1:6)
  out
}

### Produce the ENM model ----

fit_models <- function(occs, covs, sp, pliest=FALSE){
  occs <- occs[,c('decimalLongitude','decimalLatitude')]
  rcovs <- covs$map
  
  # Background points
  bg <- randomPoints(rcovs[[1]], n=10000)
  bg <- as.data.frame(bg)
  colnames(bg) <- c('decimalLongitude', 'decimalLatitude')
  kfold <- list(kfolds = 4)
  # Fit and evaluate models
  cat('Doing ENMevaluate\n')
  out <- ENMevaluate(occs, rcovs, bg,
                      method='randomkfold',
                      partition.settings = kfold, 
                      RMvalues = c(1,2,5),
                      fc=c('L','LQ','LQP'),
                      algorithm = 'maxnet',
                      clamp = FALSE)
  
  saveRDS(out, 'fn.Rds')
  out
}

# Identify the best model (by AIC)
best_model <- function(mod_list, AUC=FALSE){
  AUCs <- mod_list@results$avg.test.AUC
  best <- ifelse(AUC, which(AUCs==max(AUCs,na.rm=TRUE)),
                 which(mod_list@results$delta.AICc==0))
  cat(paste0('Best model is ',mod_list@results$tune.args[best],'\n'))
  mod_list@models[[best]]
}

# Identify best model for M_19

best_model_M19 <- function(mod_19, AUC=FALSE){
  AUCs <- mod_19@results$avg.test.AUC
  best <- ifelse(AUC, which(AUCs==max(AUCs,na.rm=TRUE)),
                 which(mod_19@results$delta.AICc==0))
  cat(paste0('Best model is ', mod_19@results$tune.args[best],'\n'))
  mod_19@models[[best]]
}

### Get 6 first Principal Components ----

get_scores <- function(new_stack, covs){
  vals <- getValues(new_stack)
  for (i in 1:ncol(vals)){
    vals[,i] <- (vals[,i] - covs$model$center[i]) / covs$model$scale[i]
  }
  loads <- covs$model$loadings[,1:6]
  new_vals <- vals %*% loads
  values(new_stack) <- new_vals
  names(new_stack) <- names(covs$map)
  new_stack
}
