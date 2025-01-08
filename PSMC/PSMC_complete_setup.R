# Create PSMC and Area plot 
### RStudio 2022.12.0+353 
# R version 4.2.2

############################
# PSMC and Area plots setup#
############################

setwd()

### Load libraries ----

library(ggplot2)
library(cowplot)
library(raster)
library(sf)
library(RStoolbox)
library(gridGraphics)
library(rgdal)
library(sp)
library(ggpubr)
library(readxl)

### PSMC trajectories + bootstraps----

plot_psmc <- function(zipped, linecol, cols, xtitle=TRUE){
  sp <- gsub(".tar.gz", "", basename(zipped))
  tmp_dir <- tempdir()
  untar(zipped, exdir=tmp_dir)
  sp_dir <- paste0(tmp_dir,'/Bt_files')
  #sp_dir <- paste0(tmp_dir, '/', psmc_species)
  nboots <- length(list.files(sp_dir, pattern=".txt"))-1
  if(nboots < 30) cat("Boots less than 30\n")
  
  template <- list.files(sp_dir, pattern="\\.0\\.txt$")
  fbase <- gsub("0.txt", "", template)
  
  dat <- get_plot_data(paste0(sp_dir, "/", fbase, '0.txt'))
  
  pop_max <- max(dat$pop)
  
  bs_files <- paste0(sp_dir, "/", fbase, 1:30, '.txt')
  bs <- lapply(bs_files, get_plot_data)
  
  bs <- lapply(bs, function(x){
    x$pop <- x$pop/pop_max
    x
  })
  dat$pop <- dat$pop/pop_max

  eholo <- c(8.326e3,11.7e3)
  gmax <- 21000
  lgp <- 115000
  iglacial <- c(120000,140000)
  pliest <- 787000

  x_min <- 8.326e3
  x_max <- max(pliest, max(dat$time, na.rm=T), na.rm=T)
  
  nbmax <- max(sapply(bs, function(x) max(x$pop,na.rm=T)),na.rm=T)
  
  
  nbmin <- min(sapply(bs, function(x) min(x$pop,na.rm=T)),na.rm=T)
  ymax <- min(nbmax*1.2, max(dat$pop, na.rm=T)*1.2)
  ymin <- max(nbmin*0.9, min(dat$pop, na.rm=T)*0.9)

  out <- ggplot(data=dat, aes(x=time, y=pop)) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.title=element_text(size=12),
          axis.text=element_text(size=12),
          plot.title = element_text(hjust = 0.5, size=14)) +
    geom_rect(data=dat[1,],xmin=log10(eholo[1]),xmax=log10(eholo[2]),ymin=-Inf,ymax=Inf,
              fill=bcols[1], alpha=0.3) +
    geom_vline(xintercept=gmax, col=bcols[2], size=2, alpha=0.3) +
    geom_rect(data=dat[1,],xmin=log10(iglacial[1]),xmax=log10(iglacial[2]),ymin=-Inf,ymax=Inf,
              fill=bcols[3], alpha=0.3) +
    geom_vline(xintercept=pliest, col=bcols[4], size=2, alpha=0.3) +
    geom_vline(xintercept = 3400000, linetype = "dashed") + # Eocene/Oligocene transition from green house to icehouse (first stable antarctic ice sheet)
    scale_x_log10("Time since present (years)",
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  limits=c(x_min,x_max),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    ylab(expression(Normalised~italic(N[e]))) +
    scale_y_continuous(limits = c(0,1))+ 
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank())
          
    for (i in 1:30){
    bs_sub <- bs[[i]]
    out <- out + geom_step(data=bs_sub, color= "black", alpha=0.1)
  }
  
  out <- out + geom_step(size=1, color="black")
  out

  return(list(plot=out, x_max=x_max))
}


### Temperature plot ----

plot_Tdata <- function(Tdata){
  Tdata <- as.data.frame(Tdata)
  Tdata$`Ts(C)` <- as.numeric(substr(Tdata$`Ts(C)`, 0, 30))
  
  T_plot <- ggplot() +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.title=element_text(size=12),
          axis.text=element_text(size=12))+
    geom_line(data= Tdata, aes(x=`TimeBP`, y=`Ts(C)`), color = "orange") +
    scale_x_log10("Time since present (years)",
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  limits=c(8.326e3, ppl$x_max),
                  labels = scales::trans_format("log10", scales::math_format(10^.x)))+
    ylab("Superficial\n mean temperature °C") +
    scale_y_continuous(limits = c(8,21),
                       breaks = c(10,15,20),
                       labels = c(10,15,20))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.x=element_blank())
  
}



### General function ---- 
get_plot_data <- function(file){
  tab_cut <- read.table(file)
  names(tab_cut) <- c("time", "pop")
  tab_cut
}


### Normalized area plot ----

 area_plot <- function(inter_map, gm_map,eh_map, pl_map, x_max=787000){
  
  area_dat <- data.frame(
    names=letters[1:4],
    area=unlist(get_areas(c(eh_map, gm_map, inter_map,pl_map))),   
    time=c(mean(c(8.326e3,11.7e3)),21000,130000,787000))
  
  area_adjust <- area_dat$area/max(area_dat$area)
  
  area_dat %>%
    ggplot(aes(x=time, y=area_adjust)) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.title=element_text(size=12),
          axis.text=element_text(size=12),
          plot.title = element_text(hjust = 0.5, size=12)) +
    scale_x_log10("Time since present (years)",
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  limits=c(8.326e3,x_max),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    ylab("Amount of\n Normalized suitable area") +
    geom_line(linetype=2) +
    geom_point(aes(col=names), size=3) +
    scale_color_manual(values=bcols[1:4]) +
    theme(legend.position='none')
}

# Get suitable area

get_areas <- function(maps){
  lapply(maps, function(x){
    crs(x) <- "+proj=longlat"
    cell_areas <- getValues(area(x))
    keep <- getValues(x)>=0.36   # Define the minimum suitability as 36%. Change at will  
    sum(cell_areas[keep], na.rm = T)
  })
}


#Plot suitability maps --------
plot_map <- function(pred, title=NULL, legend="none", use_mask=NULL,
                     bcolor='white', area=FALSE, scale_cols){
  #Calculate area
  crs(pred) <- "+proj=longlat"
  if(!is.null(use_mask)){
    pred <- mask(pred, use_mask)
  }
  cell_areas <- raster::getValues(area(pred))
  keep <- raster::getValues(pred)>=0.36 #medium and high
  tot_area <- sum(cell_areas[keep], na.rm=TRUE)
  
  tot_area <- formatC(tot_area, format = "e", digits = 2)
  prts <- strsplit(tot_area, "e+")
  bnum <- prts[[1]][1]
  exnum <- as.numeric(gsub("+", "", prts[[1]][2]))
  
  out <- ggplot() +
    geom_raster(data=pred, aes(x=x,y=y,fill=layer), interpolate=F) +
    theme(legendlegend.spacing.x = unit(0, 'cm'),
          legend.title.align = 0.5)+
    scale_fill_gradientn(colors=rev(terrain.colors(300)),limits=c(0,1),
                         na.value='white',
                         guide=guide_legend(reverse=FALSE,
                                            byrow = TRUE,
                                            title.position="top",
                                            direction = "horizontal",
                                            label.position = "bottom",
                                            title.vjust=0.1,
                                            title.hjust=0.5, 
                                            keywidth = unit(0.8, 'cm'),
                                            keyheight = unit(0.8, 'cm'), 
                                            legend.spacing.x = unit(0, 'cm'),
                                            #legend.title = element_text(hjust = 0.5),
                                            label.theme = element_text(size = 10),
                                            title.theme=element_text(size=12,  
                                                                     #angle=90 
                                                                     ))) +
   
    coord_quickmap() +
    theme_void() +
    theme(legend.position=legend,
          #legend.spacing = unit(0.01, 'cm'),
          plot.subtitle=element_text(hjust=0.5),
          panel.border = element_rect(colour = bcolor, fill=NA, size=1),
          plot.title=element_text(hjust=0.5, size=12)
    )
  
  if(area){
    out <- out +
      labs(title = title,
           subtitle = bquote(.(bnum)*x*10^{.(exnum)}~km^{2}),
           fill='Pr(Habitat)')
  }else{
    out <- out + labs(title = title,
                      fill='Percent coverage \nof suitable area')
  }
  out
}


