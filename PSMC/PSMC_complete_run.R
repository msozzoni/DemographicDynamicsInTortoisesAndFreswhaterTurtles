# Create PSMC and Area plot 
### RStudio 2022.12.0+353 
# R version 4.2.2

############################
#  PSMC and Area plots run #
############################

setwd()

### Define species ----

psmc_species <- "Emys_orbicularis"  #Species of interest. Change for every species analysed 

### Define colour palette ----

bcols <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40")

### Load produced ENM maps ----

inter_map <- readRDS(paste0('data/habitat prediction/', psmc_species, '/inter_map.Rds'))
gm_map <- readRDS(paste0('data/habitat prediction/', psmc_species, '/gm_no_ice.Rds'))
eh_map <- readRDS(paste0('data/habitat prediction//', psmc_species, '/eh_no_ice.Rds'))
pl_map <- readRDS(paste0('data/habitat prediction/', psmc_species, '/pl_map.Rds'))

### Function for complete figure ----

make_plot <- function(psmc_species){
 
ppl <- plot_psmc(zipped=paste0('data/psmc/', psmc_species, '.tar.gz'),
                 'chocolate2',
                 cols, FALSE)

pp <- ppl$plot

# Plot temperature graph
Tdata <- read_excel("data/Temperature/T_data.xlsx")

T_plot <- plot_Tdata(Tdata)


# Plot prediction maps

eh <- plot_map(eh_map, title = NULL, bcolor=bcols[1])
gm <- plot_map(gm_map, title=NULL, bcolor=bcols[2])
ig <- plot_map(inter_map, title=NULL, bcolor=bcols[3])
pli <- plot_map(pl_map, title=NULL, bcolor=bcols[4])

# Get suitable area from the prediction maps

apl <- area_plot(inter_map, gm_map,eh_map, pl_map, ppl$x_max)

# Create legends
grobs <- ggplotGrob(
  plot_map(eh_map, 'test', "left"))$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name == "guide-box"))]]

# Create grid for prediction maps

bl_plot <- ggplot() + ylim(0,1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y= element_blank(),
        axis.text.x =element_text(size = 12)) +
  scale_x_continuous(NULL,
                     breaks = c(1,3,5,7),
                     labels = c("Early\nholocene", "Last\nglacial max",
                                "last\ninterglacial", "MIS19"), limits = c(0,8))

# Plot grid 
dist_grid <- plot_grid(eh, gm, ig, pli, nrow = 1)
dg2 <- bl_plot + draw_plot(dist_grid, x=-0, y=0, width = 8, height=1)

frm <- plot_grid(NULL, dg2, NULL, legend, nrow = 2, ncol = 2, rel_widths = c(0.16, 1), rel_heights=c(1, 0.8), labels = "d" )

# Plot all the part of the image

top_rows <- plot_grid(T_plot, pp, apl, nrow=3, align='v', rel_heights=c(1,1,1), labels = c("a", "b", "c"))

out <- plot_grid(top_rows, frm, nrow = 2, rel_heights = c(1,0.4))

out

}

pl1 <- make_plot(paste0(psmc_species)) + 
  theme(plot.margin=unit(c(0,0.1,0,1),"cm"))

pl1

# Save in different format

ggsave("data2/Emys_orbicularis/PSMC_area_graph_w_temperature.tiff", width = 5.78, height = 5, unit= "in", limitsize = FALSE)
ggsave("data2/Emys_orbicularis/PSMC_area_graph_w_temperature.png", width = 5.78, height = 10, unit= "in", limitsize = FALSE)
ggsave("data2/Emys_orbicularis/PSMC_area_graph_w_temperature.svg", width = 5.78, height = 5, unit= "in", limitsize = FALSE)
ggsave("data2/Emys_orbicularis/PSMC_area_graph_w_temperature.pdf", width = 5.78, height = 10)


### Function for only PSMC and Temperature graph ----

make_plot2 <- functtion(psmc_species){

plot_psmc(zipped=paste0('data/psmc/', psmc_species, '.tar.gz'),
                 'chocolate2',
                 cols, FALSE)

pp <- ppl$plot

# Plot temperature graph
Tdata <- read_excel("data/Temperature/T_data.xlsx")

T_plot <- plot_Tdata(Tdata)


out <- plot_grid(T_plot, pp, nrow=2, align='v', rel_heights=c(1,1), labels = c("a", "b"))

out
 
 
 }

pl2 <- make_plot2(paste0(psmc_species))+
	theme(plot.margin=unit(c(0,0.1,0.1),"cm"))
	
pl2

