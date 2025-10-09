#C & N analyses for GRaMPS
library(plyr)
library(dplyr)
library(car)
library(ggplot2)
library(scales)

GrampsCN = read.csv("~/Documents/Masters/Research/CN/MonusGramps_CN_SumDat.csv", header = TRUE)

#already have mean of analytical replicates for each individual sample as well as group means
#example: I already have mean of CN for BP, addition, intercanopy, 0-5cm

#bar graph --------------------------------------------------------
GrampsCN$SiteOrder = factor(GrampsCN$Site, levels=c("BlackPoint","Antelope","BlueChute","Arboretum","CampColton"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
GrampsCN$TreatmentOrder = factor(GrampsCN$Trt, levels=c("Exl","Ctrl","Add"), labels=c("Exclusion","Ambient","Addition")) 
GrampsCN$Microsite <- factor(GrampsCN$Canopy, levels = c("Can", "IC"),
                             labels = c("Canopy", "Inter-Canopy"))
GrampsCN$DepthOrder <- factor(GrampsCN$Depth, levels = c("0-5","10-May"), # why did R think 5-10 was May 10th, probably an excel thing -_- 
                              labels = c("0-5 cm","5-10 cm"))

ggplot(data = GrampsCN, aes(x=SiteOrder, y=mean.C...., fill = TreatmentOrder)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(DepthOrder~Microsite) +
  scale_fill_manual(name = "Treatment", values = c("#de2d26", "#bdbdbd", "#08519c"))+
  scale_y_continuous(limits=c(0,6), breaks = c(0,2,4,6))+
  labs(x = "Site") +
  labs(y = "Soil Organic Carbon (mg Carbon/mg Soil)") +
  ylab(expression(paste(Soil~Organic~Carbon~(mg~Carbon~g~Soil^{-1}))))+
  scale_x_discrete(labels = label_wrap(10))+
  theme_grey(base_size = 18) +
theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x = element_text(angle=45,hjust = 1),
    legend.position = c(0.1,.88),   
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA))+
  geom_errorbar(aes(ymin=mean.C....-SE.1, ymax=mean.C....+SE.1), width=.2, position=position_dodge(.9)
  )

# ggsave("SOCmeans.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/CN/RFigures/",
#        height=10,width=12,units="in",dpi=1000)

# graph without treatments ----
SOCmeansnotrt <- ddply(GrampsCN, c("Site","Canopy","Depth"), summarise,
                      N    = sum(!is.na(mean.C....)),
                      mean_SOC = mean(mean.C...., na.rm=TRUE),
                      sd_SOC = sd(mean.C...., na.rm=TRUE),
                      se_SOC  = sd_SOC / sqrt(N))

SOCmeansnotrt$SiteOrder = factor(SOCmeansnotrt$Site, levels=c("BlackPoint","Antelope","BlueChute","Arboretum","CampColton"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
SOCmeansnotrt$Microsite <- factor(SOCmeansnotrt$Canopy, levels = c("Can", "IC"),
                             labels = c("Canopy", "Inter-Canopy"))
SOCmeansnotrt$DepthOrder <- factor(SOCmeansnotrt$Depth, levels = c("0-5","10-May"), # why did R think 5-10 was May 10th, probably an excel thing -_- 
                              labels = c("0-5 cm","5-10 cm"))

ggplot(data = SOCmeansnotrt, aes(x=SiteOrder, y=mean_SOC, fill = DepthOrder)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~Microsite) +
  scale_y_continuous(limits=c(0,6), breaks = c(0,2,4,6))+
  scale_fill_manual(name = "Depth", values = c("#525252", "#bdbdbd"))+
  labs(x = "Site") +
  labs(y = "Soil Organic Carbon (mg Carbon/mg Soil)") +
  ylab(expression(paste(Soil~Organic~Carbon~(mg~Carbon~g~Soil^{-1}))))+
  scale_x_discrete(labels = label_wrap(10))+
  theme_grey(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x = element_text(angle=45,hjust = 1),
    legend.position = "none",   
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA))+
  geom_errorbar(aes(ymin=mean_SOC-se_SOC, ymax=mean_SOC+se_SOC), width=.2, position=position_dodge(.9)
  )

# ggsave("SOCmeansNoTrt.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/Writing/Paper/Figures for Paper/",
#        height=10,width=12,units="in",dpi=1000)

#Can 0-5 graph -----------------------------------------------------------
#only want to plot 0-5 cm MBC and Canopy
Cupper <- GrampsCN[!GrampsCN$Depth == "5-10", ]
CCanUpper <- Cupper[!Cupper$Canopy == "IC", ]
CCanUpper$PlantCommunities = factor(CCanUpper$Site, levels=c("BlackPoint","Antelope","BlueChute","Arboretum","CampColton"), labels=c("Desert","Grassland","Pinyon-Juniper","Ponderosa","Mixed Conifer")) 
CCanUpper$Treatment <- factor(CCanUpper$Trt, levels=c("Exl","Ctrl","Add"), labels=c("Exclusion","Ambient","Addition")) 
CCanUpper$Site <- factor(CCanUpper$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Black Point","Antelope","Blue Chute","Arboretum","Camp Colton")) 
CCanUpper$SiteOrder <- NULL
CCanUpper$TreatmentOrder <- NULL

#can 0-5 bar graph
ggplot(data = CCanUpper, aes(x=PlantCommunities,y=mean.C....,fill=Treatment)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Treatment", values = c("#de2d26", "#bdbdbd", "#08519c"))+
  labs(x = "Plant Community") +
  ylab(expression(paste(Soil~Organic~Carbon~(mg~Carbon~g~Soil^{-1}))))+
  theme_grey(base_size = 24) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x=element_blank(),
    legend.position = c(0.1,.85),   
    axis.line = element_line(color = "black"))+
  geom_errorbar(aes(ymin=mean.C....-SE.1, ymax=mean.C....+SE.1), width=.2, position=position_dodge(.9))


#Stats -------------------------------------------------------------
#across sites-raw data
leveneTest(GrampsCN$meanCpercent~GrampsCN$Site) #unequal variance
qqnorm(GrampsCN$meanCpercent) #nonnormal
shapiro.test(GrampsCN$meanCpercent) #verify data is nonnormal
qqp(GrampsCN$meanCpercent, "lnorm") #use log normal
#data does not fit lognormal distribution - should be transformed or use residuals 

#two way anova
anovamodel = lm(meanCpercent~Site*Trt*Canopy, data=GrampsCN)
anovamodel <- aov(meanCpercent~Site*Trt*Canopy, data=GrampsCN)
summary(anovamodel)
#both are significant 

#do two way anova on raw data - Use this one!
anovamodel2 <- aov(meanCpercent~Site*Microsite, data=GrampsCN)
summary(anovamodel2)

#SOC means

SOCmeans <- ddply(GrampsCN, c("Site","Trt"), summarise,
                         N    = sum(!is.na(meanCpercent)),
                         mean_SOC = mean(meanCpercent, na.rm=TRUE),
                         sd_SOC = sd(meanCpercent, na.rm=TRUE),
                         se_SOC  = sd_SOC / sqrt(N)
)

# write.csv(SOCmeans, "~/Documents/Masters/Research/CN/SOCmeans.csv", row.names=F) 

# linear models
summary(lm(meanCpercent ~ Trt, data=GrampsCN)) 
#treatment is not significant
summary(lm(meanCpercent ~ SiteOrder + Trt, data=GrampsCN)) 
#treatment is not significant
#ANT, BC, ARB, and CC all significant 
summary(lm(meanCpercent ~ Site * Trt, data=GrampsCN)) 
#no interactions  
summary(lm(meanCpercent ~ SiteOrder + Trt + Depth, data=GrampsCN)) 
#treatment is not significant
#ANT, BC, ARB, and CC all significant 
#depth significant 
summary(lm(meanCpercent ~ SiteOrder + Trt + Depth + Canopy, data=GrampsCN)) 
#treatment is not significant
#ANT, BC, ARB, and CC all significant 
#depth significant 
#canopy marginally significant 
summary(aov(mean.C.... ~ SiteOrder + Trt + Depth + Canopy, data=GrampsCN))
summary(aov(mean.C.... ~ SiteOrder * Canopy + Trt + Depth, data=GrampsCN))
#no interaction with site and microsite - no greater differences at lower sites between microsites
summary(aov(mean.C.... ~ SiteOrder * Trt * Depth * Canopy, data=GrampsCN))

TukeyHSD(aov(lm(mean.C.... ~ SiteOrder + Trt + Depth + Canopy, data=GrampsCN)))
