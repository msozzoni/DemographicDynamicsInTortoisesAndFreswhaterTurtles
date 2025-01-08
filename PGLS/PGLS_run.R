#PGLS analysis
###RStudio 2022.12.0+353 
#R version 4.2.2

##########################################################
#  PGLS analysis between H or Ne and Conservation status #
##########################################################

setwd()

### Load Library ----

library(multcomp)
library(tidyverse)
library(readxl)
library(ggplot2)
library(cowplot)
library(ape)
library(nlme)
library(rr2)
library(sjPlot)
library(AICcmodavg)
library(ggpubr)


### Cutstom method for  PGLS from Bruniche-Olsen 2021 ----

model.matrix.gls <- function(object, ...) {
  model.matrix(terms(object), data = getData(object), ...)
}

model.frame.gls <- function(object, ...) {
  model.frame(formula(object), data = getData(object), ...)
}

terms.gls <- function(object, ...) {
  terms(model.frame(object), ...)
}

### Load and polish data ----

dat_all <- read.csv("data/dat_all_total.csv")


# Change name of species if different in the tree

dat_all$species_adjust <- dat_all$Species

dat_all$species_adjust[dat_all$Species=="Pla_meg"] <-
  "Plat_meg"

dat_all$species_adjust[dat_all$Species=="Che_ser"] <-
  "Che_serp"

# Load tree
tree.short <- read.tree("turtle_tree.nwk") 

## Run H analysis ----

dat_all <- dat_all %>%
  mutate(IUCN = fct_recode(IUCN, VUENCR="VU+EN+CR", LCNT="LC+NT"))

dat_all$IUCN <- factor(dat_all$IUCN, levels=c("LCNT","VUENCR"))

dat_het <- dat_all %>%
  rename(area=present)

dat_het <- dat_het[dat_het$area!=0,]   # Run if some areas equal 0

dat_het$sc_log_area <- scale(log(dat_het$area))
dat_het$log_het <- log(dat_het$Het)

mod <- gls(log_het ~ IUCN + sc_log_area,
           data=dat_het,
           correlation=corPagel(1,tree.short, form= ~ species_adjust))

summary(mod)
R2(mod)

## Run Ne analysis -----

dat_ne <- dat_all %>%
  rename(area=past_area_mean)
  
dat_ne <- dat_het[dat_ne$area!=0,]   #Run if you have some areas that are 0


dat_ne$sc_log_area <- scale(log(dat_ne$area))

mod_ne <- gls(log(Mean_Ne) ~ IUCN + sc_log_area,
              data=dat_ne,
              correlation=corPagel(1,tree.short, form=~species_adjust))

summary(mod_ne)
R2(mod_ne)

### IUCN figure -----
# H
df_temp <- data.frame(sc_log_area=0,
                      IUCN=factor('LCNT',levels=c("LCNT","VUENCR")))

levs <- levels(dat_het$IUCN)

mc <- glht(mod, linfct = mcp(IUCN ="Tukey"))

summary(mc)

nd <- df_temp[rep(1,length(levels(dat_het$IUCN))),]

nd$IUCN <- factor(levels(dat_het$IUCN),levels=levels(dat_het$IUCN))

pr <- AICcmodavg::predictSE.gls(mod, newdata=nd, se.fit=T)

p_df <- data.frame(y=levels(dat_het$IUCN), x=0.9*max(log(dat_het$Het)),
                   text=c("A","A")) %>%
  mutate(y=fct_recode(y, `At Risk`='VUENCR',
                      `Not at Risk`="LCNT"))
dat_raw <- dat_het %>%
  mutate(IUCN=fct_recode(IUCN, `At Risk`='VUENCR',
                         `Not at Risk`="LCNT"))

pl_het <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
                     upper=pr$fit + 1.96*pr$se.fit,
                     IUCN=nd$IUCN) %>%
  mutate(IUCN=fct_recode(IUCN, `At Risk`='VUENCR',
                         `Not at Risk`="LCNT"))  %>%
  ggplot(aes(y=IUCN, x=pr)) +
  geom_jitter(data=dat_raw,
              aes(y=IUCN, x=log(Het)), col='grey35',
              height=0.1) +
  geom_errorbarh(aes(xmin=lower,xmax=upper), height=0.4,col='chocolate2', linewidth=0.8) +
  geom_point(size=3,col='chocolate2') +
  labs(x=expression('log('*italic("H")*")"), y="Threath status") +
  mytheme +
  theme(axis.text=element_text(size=12, colour = "black"),
        axis.text.y=element_text(margin=margin(r=10), size = 12), 
        axis.ticks.y=element_blank(), 
        axis.text.x=element_text(vjust = -0),
        axis.title.x = element_text(vjust = -1, size = 12),
        axis.title.y= element_text(hjust = 0.5, vjust = 2, size=12),
        )  
pl_het

# Ne

df_temp <- data.frame(sc_log_area=0,
                      IUCN=factor('LCNT',levels=c("LCNT","VUENCR")))


mc <- glht(mod_ne, linfct = mcp(IUCN="Tukey"))

summary(mc)

nd <- df_temp[rep(1,length(levels(dat_ne$IUCN))),]
nd$IUCN <- factor(levels(dat_ne$IUCN),levels=levels(dat_ne$IUCN))


pr <- AICcmodavg::predictSE.gls(mod_ne, newdata=nd, se.fit=T)

p_df <- data.frame(y=levels(dat_het$IUCN), x=1.08*max(log(dat_ne$Mean_Ne)),
                   text=c("A","A")) 

pl_ne <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
                    upper=pr$fit + 1.96*pr$se.fit,
                    IUCN=nd$IUCN) %>%
  ggplot(aes(y=IUCN, x=pr)) +
  geom_jitter(data=dat_ne,
              aes(y=IUCN, x=log(Mean_Ne)), col='grey35',
              height=0.1) +
  geom_errorbarh(aes(xmin=lower,xmax=upper), height=0.4,col='chartreuse3',size=0.8) +
  geom_point(size=3,col='chartreuse3') +
  labs(x=expression(log(italic(N[e]))), y="IUCN status") +
  mytheme +
  xlim(min(log(dat_ne$Mean_Ne)) -0.1, 3.5) +
  theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.y=element_blank(), axis.text.x=element_text(size=12, vjust = -0, margin = margin(r=15)),
        axis.title.x = element_text(vjust = -1, size = 12))
pl_ne


cowplot::plot_grid(pl_het, pl_ne, rel_widths = c(1.4,1), rel_heights = 0.5)


ggsave("figures/fig_IUCN_LCNT_VUENCR.tiff", compression='lzw', dpi=600, height=3,width=6)
ggsave("figures/fig_IUCN_LCNT_VUENCR.png", dpi=600, height=3, width=6)
ggsave("figures/fig_IUCN_LCNT_VUENCR.svg", dpi=600, height=3, width=6)
ggsave("figures/fig_IUCN_LCNT_VUENCR.pdf", dpi = 600, height = 3, width = 6)



# Area figure ------
# Het
df_temp <- data.frame(sc_log_area=0,
                      IUCN=factor('LCNT',levels=c("LCNT","VUENCR")))


s <- summary(mod)$tTable
p <- s[,4]

area_seq <- seq(range(dat_het$area,na.rm=T)[1],
                range(dat_het$area,na.rm=T)[2], length.out=1000)

area_seq_sc <- (log(area_seq) - attr(dat_het$sc_log_area, "scaled:center"))/
  attr(dat_het$sc_log_area, "scaled:scale")

nd <- df_temp
nd <- nd[rep(1,1000),]

nd$sc_log_area <- area_seq_sc
pr <- AICcmodavg::predictSE.gls(mod, newdata=nd, se.fit=T)

pval <- p["sc_log_area"]
if(pval < 0.01){
  pval <- paste0("P < 0.01")
} else {
  pval <- paste0("P = ", sprintf('%.2f', pval))
}
p_df <- data.frame(x=mean(range(log(area_seq))), y=0.9*max(log(dat_het$Het)),text=pval)


pl_het <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
                     upper=pr$fit + 1.96*pr$se.fit) %>%
  ggplot(aes(x=log(area_seq), y=pr)) +
  geom_point(data=dat_het, aes(x=log(area), y=log(Het)), col='grey35') +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.5, fill='chocolate2') +
  geom_line() +
  geom_text(data=p_df, aes(x=x,y=y,label=text), cex=4, col="black") +
  labs(x='log(suitable area)', y=expression('log('*italic("H")*")")) +
  mytheme +
  theme(plot.margin=margin(0.3,0.3,1,0.3, "cm"),
        axis.text.x = element_text(colour = "black", vjust = 0, size = 12),
        axis.title.x = element_text(vjust = -1, size = 12),
        axis.text.y = element_text(colour = "black", margin = margin(r=5), size = 12),
        axis.title.y= element_text(hjust = 0.5, vjust = 2, size = 12))  
pl_het

# Ne

df_temp <- data.frame(sc_log_area=0,
                      IUCN=factor('LCNT',levels=c("LCNT","VUENCR")))


s <- summary(mod_ne)$tTable
p <- s[,4]

area_seq2 <- seq(range(dat_ne$area,na.rm=T)[1],range(dat_ne$area,na.rm=T)[2], length.out=1000)

area_seq_sc <- (log(area_seq2) - attr(dat_ne$sc_log_area, "scaled:center"))/
  attr(dat_ne$sc_log_area, "scaled:scale")

nd <- df_temp
nd <- nd[rep(1,1000),]
nd$sc_log_area <- area_seq_sc
pr <- AICcmodavg::predictSE.gls(mod_ne, newdata=nd, se.fit=T)

pval <- p["sc_log_area"]
if(pval < 0.01){
  pval <- paste0("P < 0.01")
} else {
  pval <- paste0("P = ", sprintf('%.2f', pval))
}
p_df <- data.frame(x=mean(range(log(area_seq2))), y=1.1*max(log(dat_ne$Mean_Ne)),text=pval)


pl_ne <- data.frame(pr=pr$fit, lower=pr$fit - 1.96*pr$se.fit,
                    upper=pr$fit + 1.96*pr$se.fit) %>%
  ggplot(aes(x=log(area_seq2), y=pr)) +
  geom_point(data=dat_ne, aes(x=log(area), y=log(Mean_Ne)), col='grey35') +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.5, fill='chartreuse3') +
  geom_line() +
  geom_text(data=p_df, aes(x=x,y=y,label=text), cex=4, col = "black") +
  labs(x='log(suitable past area)', y=expression(log(italic(N[e])))) +
  mytheme +
  theme(plot.margin=margin(0.3,0.5,1,0.3, "cm"),
        axis.text.x = element_text(colour = "black", vjust = 0, size = 12),
        axis.title.x = element_text(vjust = -1, size = 12),
        axis.text.y = element_text(colour = "black", margin = margin(r=5), size = 12),
        axis.title.y= element_text(hjust = 0.5, vjust = 2, size = 12))  

pl_ne

cowplot::plot_grid(pl_het, pl_ne, rel_widths=c(1,1))

ggsave("figures/fig_area_LCNT_VUENCR.tiff", compression='lzw', dpi=600, height=4,width=7)
ggsave("figures/fig_area_LCNT_VUENCR.png", height=4, width=7, dpi=600)
ggsave("figures/fig_area_LCNT_VUENCR.svg", height=4, width=7, dpi=600)
ggsave("figures/fig_area_LCNT_VUENCR.pdf", height=4, width=7, dpi=600)
