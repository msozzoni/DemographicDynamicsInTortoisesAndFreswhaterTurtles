#Create PSMC and Area plot 
###RStudio 2022.12.0+353 
#R version 4.2.2

##############################
#  General Demographic trend #
##############################

setwd()

### Load Libraries ---

library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)


### Define colour palette ----

bcols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40")


### Get data ----

#Create a filelist from all the file in the folder, one for species e.g Emys_orbicularis.0.txt
filelist <- list.files(pattern = ".0.txt", path = "data/psmc/general/")

#create an empty data.frame 

psmc <- data.frame(Time = numeric(), Ne = numeric(), ID = character(), 
                normalized_Ne = numeric(), stringsAsFactors = F)

#Populate the data.frame with all the information from the file in the filelist
for (aaa in 1:length(filelist)){
  out <- fread(paste0(filelist[aaa]), header = T, select = c(1:2))
  names(out) <- c("Time", "Ne")
  row.names(out) <- c(1:length((row.names(out))))
  
  out.df <- as.data.frame(out)
  out.df$ID <- rep(filelist[aaa])
  out.df$ID <- substr(filelist[aaa], 1, nchar(as.character(out.df$ID))-12)
  temp_name <- substr(filelist[aaa], 1, nchar(as.character(filelist[aaa]))-6)
  write.csv(out.df, paste0(temp_name, ".csv"))
  psmc <- rbind(psmc, out.df)
}

psmc$Species <- psmc$ID

### Load life history traits Information ----

Traits <- read.csv("trait.csv")

psmc <- psmc %>%
  left_join(Traits)

#Get the plain Ne values 
psmc$Ne <- psmc$Ne*10^4

#Subset for only the time > 0 
psmc <- subset(psmc, Time > 0)

psmc$Species <- factor(psmc$Species, levels = unique(psmc$Species))

gmax <- 21000
iglacial <- c(120000,140000)
pliest <- 787000

#Discard times more recent than 10 thousand years ago

psmc_10 <- subset(psmc, Time > 10000)

psmc_10$Species <- factor(psmc_10$Species, levels = unique(psmc_10$Species))

### General demographic trend ----

pdf("General_demographic_trend.pdf")
  ggplot() +
  geom_step(aes(x=log10(psmc_10$Time), y=log10(psmc_10$Ne), group = psmc_10$Species),
            size = 0.35) +
  theme_classic() +
  geom_smooth(aes(x=log10(psmc_10$Time), y=log10(psmc_10$Ne)), size=1, colour = "firebrick", fill = "chocolate2") + #linetype ='dashed'
  xlab("Years ago (log10 scale)") +
  scale_y_continuous(name = expression(paste("Effective size (", italic(N[e]), ")", sep="")), 
                     breaks=c(1,2,3,4,5,6), labels = expression(10^1,10^2,10^3,10^4,10^5,10^6)) +
  scale_x_continuous(name = "Time since present",  breaks=c(3,4,5,6,7,8,9),
                     labels = expression(10^3,10^4,10^5,10^6,10^7,10^8,10^9)) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=16))+ 
  geom_vline(xintercept=log10(gmax), col=bcols[2], size=2, alpha=0.3) +
  geom_rect(data=psmc_10[1,],xmin=log10(iglacial[1]),xmax=log10(iglacial[2]),ymin=-Inf,ymax=Inf,
            fill=bcols[3], alpha=0.3) +
  geom_vline(xintercept=log10(pliest), col=bcols[4], size=2, alpha=0.3) +
  geom_vline(xintercept = log10(34000000), linetype = "dotted") #eocene/oligocene transition from green house to icehouse (first stable antartci ice sheet)
dev.off()


### Aquatic species demographic trend ----
psmc_10aq <- subset(psmc_10, Habitat == "Aquatic")
psmc_10aq$Species <- factor(psmc_10aq$Species, levels = unique(psmc_10aq$Species))


pdf("Acquatic_species_demographic_trend.pdf")
ggplot() +
  geom_step(aes(x=log10(psmc_10aq$Time), y=log10(psmc_10aq$Ne), group = psmc_10aq$Species),
            size = 0.35) +
  theme_classic() +
  geom_smooth(aes(x=log10(psmc_10aq$Time), y=log10(psmc_10aq$Ne)), size=1, colour = "firebrick", fill = "chocolate2") + #linetype ='dashed'
  xlab("Years ago (log10 scale)") +
  theme(legend.key.size = unit(0.5,"line"), legend.position = c(0.8,0.8), 
        legend.text=element_text(size=9),legend.title=element_text(size=11),
        legend.background = element_rect(colour = "black", size=0.4)) +
  scale_y_continuous(name = expression(paste("Effective size (", italic(N[e]), ")", sep="")), 
                     breaks=c(1,2,3,4,5,6), labels = expression(10^1,10^2,10^3,10^4,10^5,10^6)) +
  scale_x_continuous(name = "Time since present",  breaks=c(3,4,5,6,7,8,9),
                     labels = expression(10^3,10^4,10^5,10^6,10^7,10^8,10^9)) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=16),
        axis.title.x = element_blank())+ 
  geom_vline(xintercept=log10(gmax), col=bcols[2], size=2, alpha=0.3) +
  geom_rect(data=psmc_10[1,],xmin=log10(iglacial[1]),xmax=log10(iglacial[2]),ymin=-Inf,ymax=Inf,
            fill=bcols[3], alpha=0.3) +
  geom_vline(xintercept=log10(pliest), col=bcols[4], size=2, alpha=0.3) +
  geom_vline(xintercept = log10(34000000), linetype = "dotted")  #eocene/oligocene transition from green house to icehouse (first stable antartci ice sheet)
dev.off()


### Terrestrial species demographic trend ----
psmc_10ter <- subset(psmc_10, Climate == "Terrestrial")
psmc_10ter$Species <- factor(psmc_10ter$Species, levels = unique(psmc_10ter$Species))

pdf("Terrestrial_species_demographic_trend.pdf")
 ggplot() +
  geom_step(aes(x=log10(psmc_10ter$Time), y=log10(psmc_10ter$Ne), group = psmc_10ter$Species),
            size = 0.35) +
  theme_classic() +
  geom_smooth(aes(x=log10(psmc_10ter$Time), y=log10(psmc_10ter$Ne)), size=1, colour = "firebrick", fill = "chocolate2") + #linetype ='dashed'
  xlab("Years ago (log10 scale)") +
  theme(legend.key.size = unit(0.5,"line"), legend.position = c(0.8,0.8), 
        legend.text=element_text(size=9),legend.title=element_text(size=11),
        legend.background = element_rect(colour = "black", size=0.4)) +
  scale_y_continuous(name = expression(paste("Effective size (", italic(N[e]), ")", sep="")), 
                     breaks=c(1,2,3,4,5,6), labels = expression(10^1,10^2,10^3,10^4,10^5,10^6)) +
  scale_x_continuous(name = "Time since present",  breaks=c(3,4,5,6,7,8,9),
                     labels = expression(10^3,10^4,10^5,10^6,10^7,10^8,10^9)) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=16),
        axis.title.y = element_blank())+ 
  geom_vline(xintercept=log10(gmax), col=bcols[2], size=2, alpha=0.3) +
  geom_rect(data=psmc_10[1,],xmin=log10(iglacial[1]),xmax=log10(iglacial[2]),ymin=-Inf,ymax=Inf,
            fill=bcols[3], alpha=0.3) +
  geom_vline(xintercept=log10(pliest), col=bcols[4], size=2, alpha=0.3) +
  geom_vline(xintercept = log10(34000000), linetype = "dotted")  #eocene/oligocene transition from green house to icehouse (first stable antartci ice sheet)
dev.off()

### Temperate species demographic trend ----
psmc_10tem <- subset(psmc_10, Climate == "Temperate")
psmc_10tem$Species <- factor(psmc_10tem$Species, levels = unique(psmc_10tem$Species))

pdf("Temperate_species_demographic_trend.pdf")
 ggplot() +
  geom_step(aes(x=log10(psmc_10tem$Time), y=log10(psmc_10tem$Ne), group = psmc_10tem$Species),
            size = 0.35) +
  theme_classic() +
  geom_smooth(aes(x=log10(psmc_10tem$Time), y=log10(psmc_10tem$Ne)), size=1, colour = "firebrick", fill = "chocolate2") + #linetype ='dashed'
  xlab("Years ago (log10 scale)") +
  theme(legend.key.size = unit(0.5,"line"), legend.position = c(0.8,0.8), 
        legend.text=element_text(size=9),legend.title=element_text(size=11),
        legend.background = element_rect(colour = "black", size=0.4)) +
  scale_y_continuous(name = expression(paste("Effective size (", italic(N[e]), ")", sep="")), 
                     breaks=c(1,2,3,4,5,6), labels = expression(10^1,10^2,10^3,10^4,10^5,10^6)) +
  scale_x_continuous(name = "Time since present",  breaks=c(3,4,5,6,7,8,9),
                     labels = expression(10^3,10^4,10^5,10^6,10^7,10^8,10^9)) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=16))+ 
  geom_vline(xintercept=log10(gmax), col=bcols[2], size=2, alpha=0.3) +
  geom_rect(data=psmc_10[1,],xmin=log10(iglacial[1]),xmax=log10(iglacial[2]),ymin=-Inf,ymax=Inf,
            fill=bcols[3], alpha=0.3) +
  geom_vline(xintercept=log10(pliest), col=bcols[4], size=2, alpha=0.3) +
  geom_vline(xintercept = log10(34000000), linetype = "dotted")  #eocene/oligocene transition from green house to icehouse (first stable antartci ice sheet)
dev.off()


### Tropical species demographic trend ----
psmc_10tro <- subset(psmc_10, Habitat == "Tropical")
psmc_10tro$Species <- factor(psmc_10tro$Species, levels = unique(psmc_10tro$Species))

pdf("Tropical_species_demographic_trend.pdf")
ggplot() +
  geom_step(aes(x=log10(psmc_10tro$Time), y=log10(psmc_10tro$Ne), group = psmc_10tro$Species),
            size = 0.35) +
  theme_classic() +
  geom_smooth(aes(x=log10(psmc_10tro$Time), y=log10(psmc_10tro$Ne)), size=1, colour = "firebrick", fill = "chocolate2") + #linetype ='dashed'
  xlab("Years ago (log10 scale)") +
  theme(legend.key.size = unit(0.5,"line"), legend.position = c(0.8,0.8), 
        legend.text=element_text(size=9),legend.title=element_text(size=11),
        legend.background = element_rect(colour = "black", size=0.4)) +
  scale_y_continuous(name = expression(paste("Effective size (", italic(N[e]), ")", sep="")), 
                     breaks=c(1,2,3,4,5,6), labels = expression(10^1,10^2,10^3,10^4,10^5,10^6)) +
  scale_x_continuous(name = "Time since present",  breaks=c(3,4,5,6,7,8,9),
                     labels = expression(10^3,10^4,10^5,10^6,10^7,10^8,10^9)) +
  theme(axis.text = element_text(size=13),
        axis.title = element_text(size=16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+ 
  geom_vline(xintercept=log10(gmax), col=bcols[2], size=2, alpha=0.3) +
  geom_rect(data=psmc_10[1,],xmin=log10(iglacial[1]),xmax=log10(iglacial[2]),ymin=-Inf,ymax=Inf,
            fill=bcols[3], alpha=0.3) +
  geom_vline(xintercept=log10(pliest), col=bcols[4], size=2, alpha=0.3) +
  geom_vline(xintercept = log10(34000000), linetype = "dotted") + #eocene/oligocene transition from green house to icehouse (first stable antartci ice sheet)
dev.off()

### Save final images ----
supp <- ggarrange(out1,out2,out3,out4, 
          labels = c("a", "b", "c", "d"))
ggsave("Supp_figure.png", width = 8, height = 8, dpi= 600)
ggsave("Supp_figure.svg", width = 8, height = 8, dpi = 600)
ggsave("Supp_figure.pdf", width = 8, height = 8, dpi = 600)



