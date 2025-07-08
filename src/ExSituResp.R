#Ex Situ Resp Code from LI 7000
library(dplyr)
library(tidyr)
library(cowplot)
library(patchwork)
library(here)
library(lubridate)

ExSituResp=read.csv(here("data","AllExSituResp.csv"))
#need volume of soil - mass is in g and bulk density is g/cm^3 so I need mass/bulk density in liters
ExSituResp$SoilVolume <- ExSituResp$soilmass_g / ExSituResp$BulkDensity / 1000
  

#convert CO2 from a concentration (ppm) to mass basis (ug CO2-C in a container of known volume)
# NOTE: headspace volume is estimated as 0.473 L -- this should be adjusted as needed (need to subtract volume of 1mm glass beads)
#40 g of glass beads is approximately 0.01891 L so headspace in each jar is 0.473 - 0.01891, which is 0.45409 L
#calculate ug CO2-C at time T1 (ideal gas law, correcting for T)
ExSituResp$CO2.C_ug<- ((ExSituResp$CO2.ppmv.)*(0.45409-ExSituResp$SoilVolume) *(44/22.4)*(12/44)*(273/(273+ExSituResp$temperature))) 
#convert date.time to units understandable to R
ExSituResp <- ExSituResp |>
  mutate(Datetime = mdy_hm(Date.time))

CO2rates <- ExSituResp |> 
  group_by(Sample) |> 
  arrange(Sample, Datetime) |> 
  mutate(Diff_time = c(0,as.numeric(diff(Datetime), units="hours")),  #difference in time in hours
         Diff_CO2C = CO2.C_ug - lag(CO2.C_ug), #, #difference in CO2 C
         CO2flux = Diff_CO2C/(soilmass_g * (soil_pctC/100))/Diff_time)  |>   #(Change in CO2C/ (soil mass * (soil %C/100)))/elapsed time
  ungroup()
View(CO2rates)
#write.csv(CO2rates, "~/Documents/Masters/Research/Respiration Data/ex situ resp/ExSituRespRates.csv")

#test plot - add cumulative elapsed time, site, treatment, and microsite
ExSituRespRates <- read.csv("~/Documents/Masters/Research/Respiration Data/ex situ resp/AllExSituRespRates.csv")
ExSituRespRates <- drop_na(ExSituRespRates)
ExSituRespRates$Site = factor(ExSituRespRates$Site, levels=c("Black Point","Antelope","Blue Chute","Arboretum","Camp Colton"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
ExSituRespRates$Treatment = factor(ExSituRespRates$Treatment, levels=c("Exclusion","Ambient","Addition"), labels=c("Exclusion","Ambient","Addition")) 
Canexsituresp <- subset(ExSituRespRates, ExSituRespRates[ , 6] == 'Canopy')  
Canexsituresp$ElapsedDays <-Canexsituresp$Cumulative.Elapsed.time..hours./24
# remove any negative flux
Canexsituresp <- subset(Canexsituresp, Canexsituresp[ , 17] > 0)  

# outliers
ggqqplot(Canexsituresp$CO2flux)
hist(Canexsituresp$CO2flux)
# subest addition
addresp = subset(Canexsituresp,Treatment %in% "Addition")
hist(addresp$CO2flux)
ggplot(data = Canexsituresp, aes(x=CO2flux, fill=Treatment)) + 
  geom_histogram(position = "jitter", bins=30)+ 
  facet_grid(Treatment~Site, scales = "free_y",labeller = labeller(Site = label_wrap_gen(width = 15))) +
  xlim(0, 100) 
  
# remove outliers for resp < 100 and time < 60 days
CanexsiturespNoOutliers <- subset(Canexsituresp, CO2flux<100 & ElapsedDays<60) 
CanexsiturespNoOutliers$SiteOrder = factor(CanexsiturespNoOutliers$Site, levels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 


# Graph for all resp can 0-5 individual jars paper figure-------------------------
ExSitufacet_labels <- data.frame(
  SiteOrder = rep(c("Desert Scrub", "Desert Grassland", "Juniper Savanna", "Ponderosa Pine Meadow", "Mixed Conifer Meadow"), 3),
  Treatment = rep(c("Exclusion", "Ambient", "Addition"), each = 5),
  label = c("a.", "b.", "c.", "d.", "e.",
            "f.", "g.", "h.", "i.", "j.",
            "k.", "l.", "m.", "n.", "o."),
  x = -Inf,
  y = Inf
)

ExSitufacet_labels$SiteOrder <- factor(
  ExSitufacet_labels$SiteOrder,
  levels = levels(CanexsiturespNoOutliers$SiteOrder)
)

# ExSitufacet_labels$Treatment <- factor(
#   ExSitufacet_labels$Treatment,
#   levels = levels(CanexsiturespNoOutliers$Treatment)
# )

exsituresp <- ggplot(data = CanexsiturespNoOutliers, aes(x= ElapsedDays, y=CO2flux, color=Treatment)) + 
  geom_point(position = "identity", size = 2)+ # aes(shape=Site)) +
  #geom_text(aes(label=Sample), hjust = 0, nudge_x = 0.1, size =3, color = 'black') +
  stat_smooth(method="loess", span = 1)+
  facet_grid(Treatment~SiteOrder, scales = "free_x",labeller = labeller(SiteOrder = label_wrap_gen(width = 15))) +
  ylim(0, 100) +
  xlim(0,60)+  
  ylab(expression(paste(Soil~CO[2]~Efflux~Rate~(mu*g*CO[2]*C~g^{-1}*Soil*C~h^{-1}))))+
  xlab(expression(paste(Elapsed~Time~(Days))))+
  theme_grey(base_size = 19) +
  #scale_shape_manual(values=c(15,16,17,0,4),
  scale_color_manual(values=c("#de2d26", "#bdbdbd", "#08519c")) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    strip.background = element_rect(fill = NA,
                                    colour = "black"),
    #axis.text=element_text(size=20),
    panel.border = element_rect(color = "black", fill = NA))+
  geom_text(data = ExSitufacet_labels,
            aes(x = -Inf, y = Inf, label = label),
            hjust = -0.5, vjust = 1.5, size = 5, inherit.aes = FALSE) # add labels within panels that are in upper left
exsituresp
# ggsave("ExSituResp.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/Writing/Paper/Figures for Paper/",
#        height=10,width=12,units="in",dpi=1000)


# AUC ----
#testing different spans. used span of 1 to make graph
loess_flux_10 <- loess(CanexsiturespNoOutliers$CO2flux ~ CanexsiturespNoOutliers$ElapsedDays, span=1)
flux_smoothed10 <- predict(loess_flux_10)
# parameters 3.47, residual standard error 16.88 
loess_flux_20 <- loess(CanexsiturespNoOutliers$CO2flux ~ CanexsiturespNoOutliers$ElapsedDays, span=2)
flux_smoothed20 <- predict(loess_flux_20)
# parameters 3.06, residual standard error 16.88
loess_flux_05 <- loess(CanexsiturespNoOutliers$CO2flux ~ CanexsiturespNoOutliers$ElapsedDays, span=0.5)
flux_smoothed05 <- predict(loess_flux_05)
# parameters 6.26, residual standard error 16.92

# this is integrating everything
flux_all <- function(x) predict(loess_flux_10,newdata=x)
integrate(flux_all,0,57.93) #integrate from day 0 to day 57.93 to avoid error

# need to separate out each thing that needs to get integrated i.e. each site by treatment for 15 total subsets
# all names ExSituSiteTreatment sites: ds, dg, js, pp, mc; treatments:E,A,C for exclusion, addition, and control
ExSituDSE <- subset(CanexsiturespNoOutliers, Site == "Desert Scrub" & Treatment == "Exclusion")
ExSituDSA <- subset(CanexsiturespNoOutliers, Site == "Desert Scrub" & Treatment == "Addition")
ExSituDSC <- subset(CanexsiturespNoOutliers, Site == "Desert Scrub" & Treatment == "Ambient")
ExSituDGE <- subset(CanexsiturespNoOutliers, Site == "Desert Grassland" & Treatment == "Exclusion")
ExSituDGA <- subset(CanexsiturespNoOutliers, Site == "Desert Grassland" & Treatment == "Addition")
ExSituDGC <- subset(CanexsiturespNoOutliers, Site == "Desert Grassland" & Treatment == "Ambient")
ExSituJSE <- subset(CanexsiturespNoOutliers, Site == "Juniper Savanna" & Treatment == "Exclusion")
ExSituJSA <- subset(CanexsiturespNoOutliers, Site == "Juniper Savanna" & Treatment == "Addition")
ExSituJSC <- subset(CanexsiturespNoOutliers, Site == "Juniper Savanna" & Treatment == "Ambient")
ExSituPPE <- subset(CanexsiturespNoOutliers, Site == "Ponderosa Pine Meadow" & Treatment == "Exclusion")
ExSituPPA <- subset(CanexsiturespNoOutliers, Site == "Ponderosa Pine Meadow" & Treatment == "Addition")
ExSituPPC <- subset(CanexsiturespNoOutliers, Site == "Ponderosa Pine Meadow" & Treatment == "Ambient")
ExSituMCE <- subset(CanexsiturespNoOutliers, Site == "Mixed Conifer Meadow" & Treatment == "Exclusion")
ExSituMCA <- subset(CanexsiturespNoOutliers, Site == "Mixed Conifer Meadow" & Treatment == "Addition")
ExSituMCC <- subset(CanexsiturespNoOutliers, Site == "Mixed Conifer Meadow" & Treatment == "Ambient")

# loess regression and integration for each site/treatment combo, put results into csv file
# Desert Scrub
loess_DSE <- loess(ExSituDSE$CO2flux ~ ExSituDSE$ElapsedDays, span=1)
flux_smoothedDSE <- predict(loess_DSE)
flux_DSE <- function(x) predict(loess_DSE,newdata=x)
integrate(flux_DSE,0,53.17) #1389.427

loess_DSA <- loess(ExSituDSA$CO2flux ~ ExSituDSA$ElapsedDays, span=1)
flux_smoothedDSA <- predict(loess_DSA)
flux_DSA <- function(x) predict(loess_DSA,newdata=x)
integrate(flux_DSA,0,57.06) #360.6464

loess_DSC <- loess(ExSituDSC$CO2flux ~ ExSituDSC$ElapsedDays, span=1)
flux_smoothedDSC <- predict(loess_DSC)
flux_DSC <- function(x) predict(loess_DSC,newdata=x)
integrate(flux_DSC,0,57.93) #930.5255

# Desert Grassland
loess_DGE <- loess(ExSituDGE$CO2flux ~ ExSituDGE$ElapsedDays, span=1)
flux_smoothedDGE <- predict(loess_DGE)
flux_DGE <- function(x) predict(loess_DGE,newdata=x)
integrate(flux_DGE,0,57.11) #757.0358

loess_DGA <- loess(ExSituDGA$CO2flux ~ ExSituDGA$ElapsedDays, span=1)
flux_smoothedDGA <- predict(loess_DGA)
flux_DGA <- function(x) predict(loess_DGA,newdata=x)
integrate(flux_DGA,1,57.14) #958.8923

loess_DGC <- loess(ExSituDGC$CO2flux ~ ExSituDGC$ElapsedDays, span=1)
flux_smoothedDGC <- predict(loess_DGC)
flux_DGC <- function(x) predict(loess_DGC,newdata=x)
integrate(flux_DGC,0,57.13) #857.3408

# Juniper Savanna
loess_JSE <- loess(ExSituJSE$CO2flux ~ ExSituJSE$ElapsedDays, span=1)
flux_smoothedJSE <- predict(loess_JSE)
flux_JSE <- function(x) predict(loess_JSE,newdata=x)
integrate(flux_JSE,0,56.99) #423.3769

loess_JSA <- loess(ExSituJSA$CO2flux ~ ExSituJSA$ElapsedDays, span=1)
flux_smoothedJSA <- predict(loess_JSA)
flux_JSA <- function(x) predict(loess_JSA,newdata=x)
integrate(flux_JSA,0,57.08) #641.7962

loess_JSC <- loess(ExSituJSC$CO2flux ~ ExSituJSC$ElapsedDays, span=1)
flux_smoothedJSC <- predict(loess_JSC)
flux_JSC <- function(x) predict(loess_JSC,newdata=x)
integrate(flux_JSC,0,57.00) #518.8154

# Ponderosa Pine
loess_PPE <- loess(ExSituPPE$CO2flux ~ ExSituPPE$ElapsedDays, span=1)
flux_smoothedPPE <- predict(loess_PPE)
flux_PPE <- function(x) predict(loess_PPE,newdata=x)
integrate(flux_PPE,0.04,54.90) #1144.236

loess_PPA <- loess(ExSituPPA$CO2flux ~ ExSituPPA$ElapsedDays, span=1)
flux_smoothedPPA <- predict(loess_PPA)
flux_PPA <- function(x) predict(loess_PPA,newdata=x)
integrate(flux_PPA,0,56.98) #1178.416

loess_PPC <- loess(ExSituPPC$CO2flux ~ ExSituPPC$ElapsedDays, span=1)
flux_smoothedPPC <- predict(loess_PPC)
flux_PPC <- function(x) predict(loess_PPC,newdata=x)
integrate(flux_PPC,0,54.90) #782.6268

# Mixed Conifer
loess_MCE <- loess(ExSituMCE$CO2flux ~ ExSituMCE$ElapsedDays, span=1)
flux_smoothedMCE <- predict(loess_MCE)
flux_MCE <- function(x) predict(loess_MCE,newdata=x)
integrate(flux_MCE,0,56.92) #950.6724

loess_MCA <- loess(ExSituMCA$CO2flux ~ ExSituMCA$ElapsedDays, span=1)
flux_smoothedMCA <- predict(loess_MCA)
flux_MCA <- function(x) predict(loess_MCA,newdata=x)
integrate(flux_MCA,0,56.95) #829.4

loess_MCC <- loess(ExSituMCC$CO2flux ~ ExSituMCC$ElapsedDays, span=1)
flux_smoothedMCC <- predict(loess_MCC)
flux_MCC <- function(x) predict(loess_MCC,newdata=x)
integrate(flux_MCC,0,56.99) #1131.843

# AUC graph ----
ExSituAUC=read.csv("~/Documents/Masters/Research/Respiration Data/ex situ resp/ExSituRespAUC.csv")
ExSituAUC$SiteOrder = factor(ExSituAUC$Site, levels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
ExSituAUC$TreatmentOrder = factor(ExSituAUC$Treatment, levels=c("Exclusion","Ambient","Addition"), labels=c("Exclusion","Ambient","Addition")) 

ggplot(data = ExSituAUC, aes(x=SiteOrder,y=AUC, fill=Treatment))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Treatment", values = c("#de2d26", "#bdbdbd", "#08519c"))+
  scale_x_discrete(labels = label_wrap(10))+
  xlab(bquote('Site')) +
  ylab(expression(paste(Cumulative~CO[2]~Efflux~(mu*g*CO[2]*C~g^{-1}*Soil~C~over~60~days))))+
  ylim(0,1500)+
  theme_grey(base_size = 20) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x=element_blank(),
    legend.position = "none",   
    axis.line = element_line(color = "black"))

# ggsave("ExSituRespAUC.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/Writing/Paper/Figures for Paper/",
#        height=8,width=14,units="in",dpi=1000)

#AUC stats ----
ExSituAUC$SiteTrt <- paste(ExSituAUC$Site,ExSituAUC$Treatment)
shapiro.test(ExSituAUC$AUC)
# data are normal p-value = 0.9434
AUCaov <- aov(AUC ~ Site + Treatment, data = ExSituAUC)
summary(AUCaov)
TukeyHSD(AUCaov)
#AUC not significant

#Resp rates Stats----------------------------------------------------------
#check normality
shapiro.test(Canexsituresp$CO2flux)
#not normal p-value = 2.2e-16
qqp(Canexsituresp$CO2flux, "lnorm") #not within range
qqnorm(Canexsituresp$CO2flux)
var(Canexsituresp$CO2flux)
#log transformation
qqnorm(log(Canexsituresp$CO2flux))
shapiro.test(log(Canexsituresp$CO2flux))
#still not normal p-value = 3.318e-13
resmodel <- lm(Canexsituresp$CO2flux~Canexsituresp$Sample)
Canexsituresp$Resmodel <- residuals(resmodel)
#normality on residuals 
shapiro.test(Canexsituresp$Resmodel)
#non normal p-value < 2.2e-16
#residuals on log model
logmodel <- lm(log(Canexsituresp$CO2flux)~Canexsituresp$Sample)
Canexsituresp$res <- residuals(logmodel)
#normality on residuals of log
shapiro.test(Canexsituresp$res)
#non normal p-value = 1.897e-13

summary(aov(CO2flux ~ Site + Treatment, data=Canexsituresp))
#site significant, treatment not significant
summary(aov(CO2flux ~ Site * Treatment, data=Canexsituresp))
#site significant and significant interaciton between site and treatment
TukeyHSD(aov(CO2flux ~ Site * Treatment, data=Canexsituresp))
#only significant change is BC and BP with BP rates higher than BC

#individual site graphs------------------------------------------------
#just BP
BPcanexsituresp <- subset(Canexsituresp, Canexsituresp[ , 4] == 'Black Point')  
BPcanexsituresp <- subset(BPcanexsituresp, BPcanexsituresp[ ,17] > 0 )  
ggplot(data = BPcanexsituresp, aes(x= ElapsedDays, y=CO2flux, color = Sample)) + 
  geom_point(position = "identity", size = 4)+ # aes(shape=Site)) +
  geom_path()+
  #geom_text(aes(label=Sample), hjust = 0, nudge_x = 0.1, size =3, color = 'black') +
  facet_wrap(~Treatment, scales = "free_x") +
  ylim(0, 40000) +
  #ylim(0, 10000) +
  xlim(0,75)+  
  ylab(expression(paste(Soil~CO[2]~Efflux~Rate~(mu*g*CO[2]*C~g^{-1}*Soil*C~h^{-1}))))+
  xlab(expression(paste(Elapsed~Time~(Days))))+
  theme_grey(base_size = 22) +
  #scale_shape_manual(values=c(15,16,17,0,4),
  #scale_color_manual(values=c("#de2d26", "#bdbdbd", "#08519c")) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(),
    #legend.position = c(0.8,0.8),
    panel.background = element_blank(),
    #axis.text=element_text(size=20),
    axis.line = element_line(color = "black"))

#just ANT
ANTcanexsituresp <- subset(Canexsituresp, Canexsituresp[ , 4] == 'Antelope')  
ANTcanexsituresp <- subset(ANTcanexsituresp, ANTcanexsituresp[ ,17] > 0 )  
ggplot(data = ANTcanexsituresp, aes(x= ElapsedDays, y=CO2flux, color = Sample)) + 
  geom_point(position = "identity", size = 4)+ # aes(shape=Site)) +
  geom_path()+
  facet_wrap(~Treatment, scales = "free_x") +
  ylim(0, 10000) +
  xlim(0,75)+  
  ylab(expression(paste(Soil~CO[2]~Efflux~Rate~(mu*g*CO[2]*C~g^{-1}*Soil*C~h^{-1}))))+
  xlab(expression(paste(Elapsed~Time~(Days))))+
  theme_grey(base_size = 22) +
  #scale_shape_manual(values=c(15,16,17,0,4),
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(),
    #legend.position = c(0.8,0.9),
    panel.background = element_blank(),
    #axis.text=element_text(size=20),
    axis.line = element_line(color = "black"))


#just BC
BCcanexsituresp <- subset(Canexsituresp, Canexsituresp[ , 4] == 'Blue Chute')  
BCcanexsituresp <- subset(BCcanexsituresp, BCcanexsituresp[ ,17] > 0 )  
ggplot(data = BCcanexsituresp, aes(x= ElapsedDays, y=CO2flux, color = Sample)) + 
  geom_point(position = "identity", size = 4)+ # aes(shape=Site)) +
  geom_path()+
  facet_wrap(~Treatment, scales = "free_x") +
  #ylim(0, 1000) +
  xlim(0,75)+  
  ylab(expression(paste(Soil~CO[2]~Efflux~Rate~(mu*g*CO[2]*C~g^{-1}*Soil*C~h^{-1}))))+
  xlab(expression(paste(Elapsed~Time~(Days))))+
  theme_grey(base_size = 22) +
  #scale_shape_manual(values=c(15,16,17,0,4),
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(),
    #legend.position = c(0.9,0.9),
    panel.background = element_blank(),
    #axis.text=element_text(size=20),
    axis.line = element_line(color = "black"))

#just ARB
ARBcanexsituresp <- subset(Canexsituresp, Canexsituresp[ , 4] == 'Arboretum')  
ARBcanexsituresp <- subset(ARBcanexsituresp, ARBcanexsituresp[ ,17] > 0 )  
ggplot(data = ARBcanexsituresp, aes(x= ElapsedDays, y=CO2flux, color = Sample)) + 
  geom_point(position = "identity", size = 4)+ # aes(shape=Site)) +
  geom_path()+
  facet_wrap(~Treatment, scales = "free_x") +
  #ylim(0, 1000) +
  xlim(0,75)+  
  ylab(expression(paste(Soil~CO[2]~Efflux~Rate~(mu*g*CO[2]*C~g^{-1}*Soil*C~h^{-1}))))+
  xlab(expression(paste(Elapsed~Time~(Days))))+
  theme_grey(base_size = 22) +
  #scale_shape_manual(values=c(15,16,17,0,4),
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(),
    #legend.position = c(0.8,0.9),
    panel.background = element_blank(),
    #axis.text=element_text(size=20),
    axis.line = element_line(color = "black"))

#just CC

CCcanexsituresp <- subset(Canexsituresp, Canexsituresp[ , 4] == 'Camp Colton')  
CCcanexsituresp <- subset(CCcanexsituresp, CCcanexsituresp[ ,17] > 0 )  
ggplot(data = CCcanexsituresp, aes(x= ElapsedDays, y=CO2flux, color = Sample)) + 
  geom_point(position = "identity", size = 4)+ # aes(shape=Site)) +
  geom_path()+
  facet_wrap(~Treatment, scales = "free_x") +
  ylim(0, 50000) +
  xlim(0,61)+  
  ylab(expression(paste(Soil~CO[2]~Efflux~Rate~(mu*g*CO[2]*C~g^{-1}*Soil*C~h^{-1}))))+
  xlab(expression(paste(Elapsed~Time~(Days))))+
  theme_grey(base_size = 22) +
  #scale_shape_manual(values=c(15,16,17,0,4),
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(),
    #legend.position = c(0.9,0.9),
    panel.background = element_blank(),
    #axis.text=element_text(size=20),
    axis.line = element_line(color = "black"))

#loess regression -------------------------------------------------------
#BP exl
BPExlexsituresp <- subset(BPcanexsituresp, BPcanexsituresp[ , 5] == 'Exclusion')  
loess_flux_BPexl <- loess(BPExlexsituresp$CO2flux ~ BPExlexsituresp$Cumulative.Elapsed.time..hours., span=1)
BPexl_flux_smoothed <- predict(loess_flux_BPexl)
f_BPexl_C <- function(x) predict(loess_flux_BPexl,newdata=x)
integrate(f_BPexl_C,1,1500)
#BP ctrl
BPCtrlexsituresp <- subset(BPcanexsituresp, BPcanexsituresp[ , 5] == 'Ambient')  
loess_flux_BPctrl <- loess(BPCtrlexsituresp$CO2flux ~ BPCtrlexsituresp$Cumulative.Elapsed.time..hours., span=1)
BPctrl_flux_smoothed <- predict(loess_flux_BPctrl)
f_BPctrl_C <- function(x) predict(loess_flux_BPctrl,newdata=x)
integrate(f_BPctrl_C,1,1500)
#BP add
BPAddexsituresp <- subset(BPcanexsituresp, BPcanexsituresp[ , 5] == 'Addition')  
loess_flux_BPAdd <- loess(BPAddexsituresp$CO2flux ~ BPAddexsituresp$Cumulative.Elapsed.time..hours., span=1)
BPAdd_flux_smoothed <- predict(loess_flux_BPAdd)
f_BPadd_C <- function(x) predict(loess_flux_BPAdd,newdata=x)
integrate(f_BPadd_C,1,60)

#ANT exl
ANTExlexsituresp <- subset(ANTcanexsituresp, ANTcanexsituresp[ , 5] == 'Exclusion')  
loess_flux_ANTexl <- loess(ANTExlexsituresp$CO2flux ~ ANTExlexsituresp$Cumulative.Elapsed.time..hours., span=1)
ANTexl_flux_smoothed <- predict(loess_flux_ANTexl)
f_ANTexl_C <- function(x) predict(loess_flux_ANTexl,newdata=x)
integrate(f_ANTexl_C,1,60)
#ANT ctrl
ANTCtrlexsituresp <- subset(ANTcanexsituresp, ANTcanexsituresp[ , 5] == 'Ambient')  
loess_flux_ANTctrl <- loess(ANTCtrlexsituresp$CO2flux ~ ANTCtrlexsituresp$Cumulative.Elapsed.time..hours., span=1)
ANTctrl_flux_smoothed <- predict(loess_flux_ANTctrl)
f_ANTctrl_C <- function(x) predict(loess_flux_ANTctrl,newdata=x)
integrate(f_ANTctrl_C,1,60)
#ANT add
ANTAddexsituresp <- subset(ANTcanexsituresp, ANTcanexsituresp[ , 5] == 'Addition')  
loess_flux_ANTAdd <- loess(ANTAddexsituresp$CO2flux ~ ANTAddexsituresp$Cumulative.Elapsed.time..hours., span=1)
ANTAdd_flux_smoothed <- predict(loess_flux_ANTAdd)
f_ANTadd_C <- function(x) predict(loess_flux_ANTAdd,newdata=x)
integrate(f_ANTadd_C,1,60)

#BC exl
BCExlexsituresp <- subset(BCcanexsituresp, BCcanexsituresp[ , 5] == 'Exclusion')  
loess_flux_BCexl <- loess(BCExlexsituresp$CO2flux ~ BCExlexsituresp$Cumulative.Elapsed.time..hours., span=1)
BCexl_flux_smoothed <- predict(loess_flux_BCexl)
f_BCexl_C <- function(x) predict(loess_flux_BCexl,newdata=x)
integrate(f_BCexl_C,1,60)
#BC ctrl
BCCtrlexsituresp <- subset(BCcanexsituresp, BCcanexsituresp[ , 5] == 'Ambient')  
loess_flux_BCctrl <- loess(BCCtrlexsituresp$CO2flux ~ BCCtrlexsituresp$Cumulative.Elapsed.time..hours., span=1)
BCctrl_flux_smoothed <- predict(loess_flux_BCctrl)
f_BCctrl_C <- function(x) predict(loess_flux_BCctrl,newdata=x)
integrate(f_BCctrl_C,1,60)
#BC add
BCAddexsituresp <- subset(BCcanexsituresp, BCcanexsituresp[ , 5] == 'Addition')  
loess_flux_BCAdd <- loess(BCAddexsituresp$CO2flux ~ BCAddexsituresp$Cumulative.Elapsed.time..hours., span=1)
BCAdd_flux_smoothed <- predict(loess_flux_BCAdd)
f_BCadd_C <- function(x) predict(loess_flux_BCAdd,newdata=x)
integrate(f_BCadd_C,1,60)

#ARB exl
ARBExlexsituresp <- subset(ARBcanexsituresp, ARBcanexsituresp[ , 5] == 'Exclusion')  
loess_flux_ARBexl <- loess(ARBExlexsituresp$CO2flux ~ ARBExlexsituresp$Cumulative.Elapsed.time..hours., span=1)
ARBexl_flux_smoothed <- predict(loess_flux_ARBexl)
f_ARBexl_C <- function(x) predict(loess_flux_ARBexl,newdata=x)
integrate(f_ARBexl_C,1,60)
#ARB ctrl
ARBCtrlexsituresp <- subset(ARBcanexsituresp, ARBcanexsituresp[ , 5] == 'Ambient')  
loess_flux_ARBctrl <- loess(ARBCtrlexsituresp$CO2flux ~ ARBCtrlexsituresp$Cumulative.Elapsed.time..hours., span=1)
ARBctrl_flux_smoothed <- predict(loess_flux_ARBctrl)
f_ARBctrl_C <- function(x) predict(loess_flux_ARBctrl,newdata=x)
integrate(f_ARBctrl_C,1,60)
#ARB add
ARBAddexsituresp <- subset(ARBcanexsituresp, ARBcanexsituresp[ , 5] == 'Addition')  
loess_flux_ARBAdd <- loess(ARBAddexsituresp$CO2flux ~ ARBAddexsituresp$Cumulative.Elapsed.time..hours., span=1)
ARBAdd_flux_smoothed <- predict(loess_flux_ARBAdd)
f_ARBadd_C <- function(x) predict(loess_flux_ARBAdd,newdata=x)
integrate(f_ARBadd_C,1,60)

#CC exl
CCExlexsituresp <- subset(CCcanexsituresp, CCcanexsituresp[ , 5] == 'Exclusion')  
loess_flux_CCexl <- loess(CCExlexsituresp$CO2flux ~ CCExlexsituresp$Cumulative.Elapsed.time..hours., span=1)
CCexl_flux_smoothed <- predict(loess_flux_CCexl)
f_CCexl_C <- function(x) predict(loess_flux_CCexl,newdata=x)
integrate(f_CCexl_C,1,60)
#CC ctrl
CCCtrlexsituresp <- subset(CCcanexsituresp, CCcanexsituresp[ , 5] == 'Ambient')  
loess_flux_CCctrl <- loess(CCCtrlexsituresp$CO2flux ~ CCCtrlexsituresp$Cumulative.Elapsed.time..hours., span=1)
CCctrl_flux_smoothed <- predict(loess_flux_CCctrl)
f_CCctrl_C <- function(x) predict(loess_flux_CCctrl,newdata=x)
integrate(f_CCctrl_C,1,60)
#CC add
CCAddexsituresp <- subset(CCcanexsituresp, CCcanexsituresp[ , 5] == 'Addition')  
loess_flux_CCAdd <- loess(CCAddexsituresp$CO2flux ~ CCAddexsituresp$Cumulative.Elapsed.time..hours., span=1)
CCAdd_flux_smoothed <- predict(loess_flux_CCAdd)
f_CCadd_C <- function(x) predict(loess_flux_CCAdd,newdata=x)
integrate(f_CCadd_C,1,60)
