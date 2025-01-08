# PGLS analysis
### RStudio 2022.12.0+353 
# R version 4.2.2

################################################################
#  PGLS analysis between H or Ne and Conservation status setup#
################################################################

setwd()

### Load Libraries ----

library(tidyverse)
library(readxl)
library(raster)
library(pbapply)
library(dplyr)

### Species specification ----

species <- as.character(read.table('data/Species.txt')$V1)


### Past suitable areas ----

if(!file.exists('data/enm_areas.csv')){ 
  library(raster)
  library(pbapply)

  allsp <- list.files("maps/",pattern="_july24.Rds")
  allsp <- gsub("_map_july24.Rds", "", allsp)
  allsp <- gsub("_present", "", allsp)
  allsp <- gsub("_eh_no_ice", "", allsp)
  allsp <- gsub("_gm_no_ice", "", allsp)
  allsp <- gsub("_inter", "", allsp)
  allsp <- gsub("_pl", "", allsp)
  allsp <- unique(allsp)


  calc_areas <- function(maps){
    lapply(maps, function(x){
      crs(x) <- "+proj=longlat"
      cell_areas <- getValues(area(x))
      keep <- getValues(x)>=0.90 #0.36 0.75 0.50
      sum(cell_areas[keep], na.rm=T)
    })
  }

  extract_areas <- function(species){
    maps1 <- readRDS(paste0("maps/",species,"_present_map_july24.Rds"))  
    maps2 <- readRDS(paste0("maps/",species,"_eh_no_ice_map_july24.Rds"))
    maps3 <- readRDS(paste0("maps/",species,"_gm_no_ice_map_july24.Rds"))
    maps4 <- readRDS(paste0("maps/",species,"_inter_map_july24.Rds"))
    maps5 <- readRDS(paste0("maps/",species,"_pl_map_july24.Rds"))
    
    maps <- c(maps5, maps4, maps3, maps2, maps1)
    unlist(calc_areas(maps))
  }
  
  all_areas <- pblapply(allsp, extract_areas)
  all_areas <- as.data.frame(do.call("rbind", all_areas))
  all_areas <- data.frame(species=allsp, all_areas)
  
  write.csv(all_areas, "data/enm_areas.csv", row.names=F)
}

areas <- read.csv("data/enm_areas.csv")

colnames(areas) <- c("species", "pliest", "interglacial", "glacialmax", "earlyholo", "present")

areas <- areas %>%
  mutate(past_area_mean = c(pliest+interglacial+glacialmax+earlyholo)/4)

areas_var <- apply(areas[,c("pliest","interglacial","glacialmax","earlyholo")],
                   1,var,na.rm=T)
areas$past_area_var <- areas_var

### Mean Ne ----
# Mean_Ne e Var_Ne was calculated in Excel 

Mean_Ne <- read_excel("data/Species_Ne.xlsx")

# Life history traits ----

traits <- read_excel("data/Species_TSD_HET_IUCN.xlsx")
mutate(traits, IUCN = factor(IUCN, levels=c('LC','VU','NT', 'EN','CR','EX')))
dplyr::select(traits, 'Species','IUCN')

# Heterozigosity ----

gen <- read_excel("data/Species_TSD_HET_IUCN.xlsx")
gen <- dplyr::select(gen, Species, heterozygosity, Wtheta)
names(gen) <- c('Species', 'Het', "Wtheta") #"SD", "Habitat", "Diet",

# Combine data ----
# We used only Het, Area and Mean_Ne 

dat <- data.frame(traits, Het=gen$Het) %>% as_tibble()

dat <- dat %>%
  mutate(IUCN=fct_recode(IUCN, `VU+EN+CR`='VU', `VU+EN+CR`='EN', `VU+EN+CR`="CR",  `LC+NT`="LC", `LC+NT`="NT" ))%>%
  left_join(areas %>% rename(Species=species)) %>%
  left_join(Mean_Ne)%>%
  dplyr::select(Species, Het, Wtheta, IUCN, Mean_Ne, Var_Ne, present, past_area_mean, past_area_var)
  
  
write.csv(dat, "data/dat_all_total.csv", row.names = FALSE) 

mytheme <-   theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "black", linewidth = 1),
        axis.text=element_text(size=16), axis.title=element_text(size=18))
