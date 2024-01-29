#DOC results - last used 7/07/2020
library(ggplot2)
library(plyr)
library(car)
library(MASS)
library(lme4)
library(scales)

#read in results created from MicrobialBiomass.R 
MBC1 <- read.csv("~/Documents/Masters/Research/DOC/MBC results/CFEround1results.csv", header=T)
MBC2 <- read.csv("~/Documents/Masters/Research/DOC/MBC results/CFEround2results.csv", header=T)
MBC3 <- read.csv("~/Documents/Masters/Research/DOC/MBC results/CFEround3results.csv", header=T)
MBC4 <- read.csv("~/Documents/Masters/Research/DOC/MBC results/CFEround4results.csv", header=T)
MBC5 <- read.csv("~/Documents/Masters/Research/DOC/MBC results/CFEround5results.csv", header=T)
MBC6 <- read.csv("~/Documents/Masters/Research/DOC/MBC results/CFEround6results.csv", header=T)
MBC7 <- read.csv("~/Documents/Masters/Research/DOC/MBC results/CFEround7results.csv", header=T)
MBC8 <- read.csv("~/Documents/Masters/Research/DOC/MBC results/CFEround8results.csv", header=T)
MBC9 <- read.csv("~/Documents/Masters/Research/DOC/MBC results/CFEround9results.csv", header=T)
SiteMAP <- read.csv("~/Documents/Masters/Research/SiteData/MAP.csv") # precip is in mm
SiteMAP$Site = factor(SiteMAP$Site, levels=c("Black Point","Antelope","Blue Chute","Arboretum","Camp Colton"), labels=c("Black Point","Antelope","Blue Chute","Arboretum","Camp Colton")) 

#combine into one dataset
MBC <- rbind(MBC1, MBC2, MBC3, MBC4, MBC5, MBC6, MBC7, MBC8, MBC9)
write.csv(MBC, "~/Documents/Masters/Research/DOC/MBCresults.csv", row.names=F) 
#remove rows with zero as a result
MBC = MBC[-c(29:34, 44, 46, 53:55, 145, 155, 240, 247, 249),]
write.csv(MBC, "~/Documents/Masters/Research/DOC/MBCresultsNoZeros.csv", row.names=F) 

#means----------------------------------------------------------------------------------
MBCmeansoverall <- ddply(MBC, c("Site","Treatment","Plot", "Microsite","Depth"), summarise,
                     N    = sum(!is.na(MBC)),
                     mean_MBC = mean(MBC, na.rm=TRUE),
                     sd_MBC = sd(MBC, na.rm=TRUE),
                     se_MBC  = sd_MBC / sqrt(N)
                     )
#print(MBCmeans)
#MBCmeansoverall$se_MBC <- NULL
#MBCmeansoverall$sd_MBC <- NULL
MBCmeansoverall$N <- NULL

#need MBCoverall since it is by plot with precip
AllMBCandMAP <- MBCmeansoverall
AllMBCandMAP$MAP <- paste(AllMBCandMAP$Site,AllMBCandMAP$Treatment)
AllMBCandMAP$MAP <- factor(AllMBCandMAP$MAP, levels=c("BP Add","BP Ctrl", "BP Exl","ANT Add","ANT Ctrl", "ANT Exl","BC Add","BC Ctrl", "BC Exl","ARB Add","ARB Ctrl", "ARB Exl","CC Add","CC Ctrl", "CC Exl" ), labels=c("277.70","181.50","96.20","388.41","253.86","134.55","654.55","427.81","226.74","821.85","537.16","284.69","877.30","573.40","303.90"))  
str(AllMBCandMAP)
AllMBCandMAP$MAP <- as.character(AllMBCandMAP$MAP)
AllMBCandMAP$MAP <- as.numeric(AllMBCandMAP$MAP)
AllMBCandMAP$Site <- factor(AllMBCandMAP$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Black Point","Antelope","BlueChute","Arboretum","Camp Colton")) 
AllMBCandMAP$Treatment = factor(AllMBCandMAP$Treatment, levels=c("Exl","Ctrl","Add"), labels=c("Exclusion","Ambient","Addition")) 
AllMBCandMAP$Depth = factor(AllMBCandMAP$Depth, levels=c("upper","lower"), labels=c("0-5 cm","5-10 cm")) 

write.csv(MBCmeansoverall, "~/Documents/Masters/Research/DOC/MBCmeans.csv", row.names=F) 

MBCmeans <- ddply(MBCmeansoverall, c("Site","Treatment", "Microsite","Depth"), summarise,
                  N    = sum(!is.na(mean_MBC)),
                  mean2_MBC = mean(mean_MBC, na.rm=TRUE),
                  sd2_MBC = sd(mean_MBC, na.rm=TRUE),
                  se2_MBC  = sd2_MBC / sqrt(N))

#just means for camp colton to match with precip
#combining microsites and depths to get one biomass mean since i can only 
#compare treatment effect with precip values
MBCPlotmeans <- ddply(MBC, c("Site","Treatment"), summarise,
                      N    = sum(!is.na(MBC)),
                      mean_MBC = mean(MBC, na.rm=TRUE),
                      sd_MBC = sd(MBC, na.rm=TRUE),
                      se_MBC  = sd_MBC / sqrt(N))
write.csv(MBCPlotmeans, "~/Documents/Masters/Research/DOC/MBCPlotmeans.csv", row.names=F) 

#Means by site and treatment and plot (combining depth and microsite)
MBCtreatmentmeans <- ddply(MBCmeansoverall, c("Site","Treatment", "Plot"), summarise,
                  N    = sum(!is.na(mean_MBC)),
                  mean2_MBC = mean(mean_MBC, na.rm=TRUE),
                  sd2_MBC = sd(mean_MBC, na.rm=TRUE),
                  se2_MBC  = sd2_MBC / sqrt(N))
MBCtreatmentmeans$N <- NULL
CCtreatmentmeans <- MBCtreatmentmeans[49:60, 1:4]
ANTtreatmentmeans <- MBCtreatmentmeans[1:12, 1:4]
ARBtreatmentmeans <- MBCtreatmentmeans[13:24, 1:4]

MBCandprecip <- merge(MBCPlotmeans, SiteMAP)
#this is with combining both depths and microsites
MBCcan05precip <- merge(MBCCanUpper, SiteMAP)
#this is just can 0-5

#graph ----------------------------------------------------------------------------------
MBCmeans$SiteOrder = factor(MBCmeans$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
MBCmeans$TreatmentOrder = factor(MBCmeans$Treatment, levels=c("Exl","Ctrl","Add"), labels=c("Exclusion","Ambient","Addition")) 
MBCmeans$Microsite <- factor(MBCmeans$Microsite, levels = c("Can", "IC"),
                  labels = c("Canopy", "Inter-Canopy"))
MBCmeans$DepthOrder <- factor(MBCmeans$Depth, levels = c("upper", "lower"),
                             labels = c("0-5 cm","5-10 cm"))


ggplot(data = MBCmeans [1:60,], aes(x=SiteOrder,y=mean2_MBC,fill=TreatmentOrder)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(DepthOrder~Microsite, scales = "free_x") +
  #facet_wrap(~Microsite, scales = "free_x") +
  scale_fill_manual(values=c("#de2d26", "#bdbdbd", "#08519c")) +
  scale_x_discrete(labels = label_wrap(10))+
  labs(x = "Site") +
  labs(y = "Microbial Biomass (mg DOC/kg dry soil)") +
  ylab(expression(paste(Microbial~Biomass~(mg~DOC~kg~dry~soil^{-1}))))+
  theme_grey(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x=element_blank(),
    legend.position = c(0.1,.89),   
    axis.line = element_line(color = "black"))+
  guides(fill=guide_legend(title="Treatment"))+
  geom_errorbar(aes(ymin=mean2_MBC-se2_MBC, ymax=mean2_MBC+se2_MBC), width=.2, position=position_dodge(.9)
  )

# ggsave("MBCmeans.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/DOC/RFigures/",
#        height=8,width=14,units="in",dpi=1000)

# graph without treatments ----
MBCmeansnotrt <- ddply(MBCmeansoverall, c("Site","Microsite","Depth"), summarise,
                       N    = sum(!is.na(mean_MBC)),
                       mean_MBC2 = mean(mean_MBC, na.rm=TRUE),
                       sd_MBC2 = sd(mean_MBC, na.rm=TRUE),
                       se_MBC2  = sd_MBC2 / sqrt(N))

MBCmeansnotrt$SiteOrder = factor(MBCmeansnotrt$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
MBCmeansnotrt$MicrositeOrder <- factor(MBCmeansnotrt$Microsite, levels = c("Can", "IC"),
                                  labels = c("Canopy", "Inter-Canopy"))
MBCmeansnotrt$DepthOrder <- factor(MBCmeansnotrt$Depth, levels = c("upper","lower"), 
                                   labels = c("0-5 cm","5-10 cm"))

ggplot(data = MBCmeansnotrt, aes(x=SiteOrder, y=mean_MBC2, fill = DepthOrder)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(~MicrositeOrder) +
  #scale_y_continuous(limits=c(0,6), breaks = c(0,2,4,6))+
  scale_fill_manual(name = "Depth", values = c("#525252", "#bdbdbd"))+
  labs(x = "Site") +
  labs(y = "Microbial Biomass (mg DOC/kg dry soil)") +
  ylab(expression(paste(Microbial~Biomass~(mg~DOC~kg~dry~soil^{-1}))))+
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
  geom_errorbar(aes(ymin=mean_MBC2-se_MBC2, ymax=mean_MBC2+se_MBC2), width=.2, position=position_dodge(.9)
  )

# ggsave("MBCmeansnotrt.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/Writing/Paper/Figures for Paper",
#        height=8,width=14,units="in",dpi=1000)

#only want to plot 0-5 cm MBC and Canopy
MBCUpper <- MBCmeans[!MBCmeans$Depth == "5-10 cm", ]
MBCCanUpper <- MBCUpper[!MBCUpper$Microsite == "Inter-Canopy", ]
MBCCanUpper$PlantCommunities = factor(MBCCanUpper$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Desert","Grassland","Pinyon-Juniper","Ponderosa","Mixed Conifer")) 
MBCCanUpper$Treatment <- factor(MBCCanUpper$Treatment, levels=c("Exl","Ctrl","Add"), labels=c("Exclusion","Ambient","Addition")) 
MBCCanUpper$Site <- factor(MBCCanUpper$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Black Point","Antelope","Blue Chute","Arboretum","Camp Colton")) 
MBCCanUpper$SiteOrder <- NULL
MBCCanUpper$TreatmentOrder <- NULL

#can 0-5 bar graph
ggplot(data = MBCCanUpper, aes(x=PlantCommunities,y=mean2_MBC,fill=Treatment)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Treatment", values = c("#de2d26", "#bdbdbd", "#08519c"))+
  labs(x = "Plant Community") +
  ylab(expression(paste(Microbial~Biomass~(mg~DOC~kg~dry~soil^{-1}))))+
  theme_grey(base_size = 24) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x=element_blank(),
    legend.position = c(0.1,.85),   
    axis.line = element_line(color = "black"))+
geom_errorbar(aes(ymin=mean2_MBC-se2_MBC, ymax=mean2_MBC+se2_MBC), width=.2, position=position_dodge(.9))

#plot for Can 0-5 vs precip
ggplot(data = MBCcan05precip, aes(x=Precip,y=mean2_MBC,color=Treatment)) + 
  geom_point(position = "identity", aes(shape=Site)) +
  #stat_smooth(aes(group = 1), color="black", method = "lm", se = FALSE, formula = y ~ x) +
  scale_color_manual(name = "Treatment", values = c("#de2d26", "#bdbdbd", "#08519c"))+
  scale_shape_manual(values=c(15,16,17,0,4), name = "Site")+
  labs(x = "Mean Annual Precipitation (mm)") +
  labs(y = "Microbial Biomass (mg DOC/kg dry soil)") +
  theme_grey(base_size = 24) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x=element_blank(),
    legend.position = c(0.8,.75),   
    axis.line = element_line(color = "black"))
  #geom_errorbar(aes(ymin=mean2_MBC-se2_MBC, ymax=mean2_MBC+se2_MBC), width=.2, position=position_dodge(.9))

#plot means graph
MBCandprecip$Site = factor(MBCandprecip$Site, levels=c("Black Point","Antelope","Blue Chute","Arboretum","Camp Colton"), labels=c("Black Point","Antelope","Blue Chute","Arboretum","Camp Colton")) 
MBCPlotmeans$Treatment = factor(MBCandprecip$Treatment, levels=c("Exclusion","Ambient","Addition"), labels=c("Exclusion","Ambient","Addition")) 

ggplot(data = AllMBCandMAP, aes(x=MAP,y=mean_MBC,color=Treatment))+ 
  geom_point(position = "identity", aes(shape=Site),size=4) +
  stat_smooth(aes(group = 1), color="black", method = "lm", se = FALSE, formula = y ~ x) +
  scale_color_manual(values=c("#de2d26", "#bdbdbd", "#08519c")) +
  scale_shape_manual(values=c(15,16,17,0,4), name = "Site")+ 
  labs(x = "July 2017- July 2018 Cumulative Precipitation (mm)") +
  ylab(expression(paste(Microbial~Biomass~(mg~DOC~kg~dry~soil^{-1}))))+
  ylim(0,1000)+
  theme_grey(base_size = 22) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x=element_blank(),
    legend.box = "horizontal",
    legend.position = c(.8,.85),   
    axis.line = element_line(color = "black"))


#MBC and soil moisture
#MBCandmoisture <- merge(MBCmeans,dailymoisturemeans)


#Statistical analyses ------------------------------------------------------------------
#use MBCmeansoverall for stats because this averages analytical reps that I did before I stopped doing analytical reps

shapiro.test(MBCmeansoverall$mean_MBC)
#not normal
qqp(MBCmeansoverall$mean_MBC, "lnorm") #use log normal
ggqqplot(MBCmeansoverall$mean_MBC)
bestNormalize(MBCmeansoverall$mean_MBC)
x <- MBCmeansoverall$mean_MBC
sqrt_x_obj <- sqrt_x(x)
sqrt_x_obj
p <- predict(sqrt_x_obj)
x2 <- predict(sqrt_x_obj, newdata = p, inverse = TRUE)
all.equal(x2, x)
MBCmeansoverall$Sqrt <- p
shapiro.test(MBCmeansoverall$Sqrt)
ggqqplot(MBCmeansoverall$Sqrt)

orderNorm_obj <- orderNorm(x)
orderNorm_obj
p <- predict(orderNorm_obj)
x2 <- predict(orderNorm_obj, newdata = p, inverse = TRUE)
all.equal(x2, x)
MBCmeansoverall$OrderNorm <- p
shapiro.test(MBCmeansoverall$OrderNorm)
ggqqplot(MBCmeansoverall$OrderNorm)
summary(aov(mean_MBC ~ Site + Treatment + Microsite + Depth, data = MBCmeansoverall))

#3 way ANOVA with site,microsite,treatment
MBCanova <- aov(mean_MBC ~ Site * Treatment * Microsite, data = MBCmeansoverall)
summary(MBCanova)

#1 way ANOVA for depth
Depthanova <- aov(MBC ~ Depth, data = MBC)
summary(Depthanova)

#use LMER - see https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html for more info

#check normality
shapiro.test(MBCmeans$mean2_MBC)
#not normal p-value = 0.0008705
qqp(MBCmeans$mean2_MBC, "lnorm") #use log normal
#data fits the lognormal distribution so don't transform data

#linear mixed model
lmm <- lmer(mean_MBC ~ Site + Treatment + Microsite + Depth + (1 | Plot), data = MBCmeansoverall,
     REML = FALSE)
#everything after ~ is a response variable and in () is the random variable
summary(lmm)
Anova(lmm)
#variance due to plot is 0, so can do regular linear model 

lmMBC <- lm(mean_MBC ~ Site + Treatment + Microsite + Depth, data = MBCmeansoverall)
summary(lmMBC)
#Anova(lmMBC)
aov(lmMBC)
summary(aov(lmMBC))
#post hoc
tukey = aov(mean_MBC~Site + Depth, data=MBCmeansoverall)
TukeyHSD(tukey)
#results of Tukey: BP-ANT, BC-ARB, CC-ARB, and CC-BC are not significantly different but everyhting else is 

#LM for just Can 0-5 MBCCanUpper
lmMBCCanUpper <- lm(mean2_MBC ~ Site + Treatment, data = MBCCanUpper)
summary(lmMBCCanUpper)
aov(lmMBCCanUpper)
anova(lmMBCCanUpper)

#linear model
MBClm <- lm(mean_MBC ~ Site + Treatment + Microsite + Depth, data = MBCmeansoverall)
summary(MBClm)
aov(MBClm) #Estimated effects may be unbalanced
anova(MBClm)
TukeyHSD(aov(MBClm))

#Plot means stats
MBCplotanova <- aov(mean_MBC ~ Site * Treatment, data = MBCPlotmeans)
summary(MBCplotanova) #Estimated effects may be unbalanced
Anova(lm(mean_MBC~Site * Treatment, data = MBCPlotmeans),type=2)
summary(Anova(lm(mean_MBC~Site * Treatment, data = MBCPlotmeans),type=2))

MBCplotlm <- lm(mean_MBC ~ Site + Treatment, data = MBCPlotmeans)
summary(MBCplotlm)
aov(MBCplotlm) #Estimated effects may be unbalanced
anova(MBCplotlm)
TukeyHSD(aov(MBCplotlm))

MBCPlotmeans$posthoc[MBCPlotmeans$Site == "BP"] <- "a"
MBCPlotmeans$posthoc[MBCPlotmeans$Site == "ANT"] <- "a"
MBCPlotmeans$posthoc[MBCPlotmeans$Site == "BC"] <- "b"
MBCPlotmeans$posthoc[MBCPlotmeans$Site == "ARB"] <- "b"
MBCPlotmeans$posthoc[MBCPlotmeans$Site == "CC"] <- "b"

#ANOVA just on CC means by treatment - nothing is significantly different, EXL-CTRL p = 0.215
CCMBClm <- lm(mean2_MBC ~ Treatment, data = CCtreatmentmeans)
summary(CCMBClm)
aov(CCMBClm) #Estimated effects may be unbalanced
anova(CCMBClm)
TukeyHSD(aov(CCMBClm))

#ANOVA just on ANT means by treatment - nothing is significantly different, EXL-CTRL p = 0.147 and EXL-ADD p = 0.17
ANTMBClm <- lm(mean2_MBC ~ Treatment, data = ANTtreatmentmeans)
summary(ANTMBClm)
aov(ANTMBClm) #Estimated effects may be unbalanced
anova(ANTMBClm)
TukeyHSD(aov(ANTMBClm))

#ANOVA just on ARB means by treatment - nothing is significantly different, EXL-CTRL p = 0.147 and EXL-ADD p = 0.17
ARBMBClm <- lm(mean2_MBC ~ Treatment, data = ARBtreatmentmeans)
summary(ARBMBClm)
aov(ARBMBClm) #Estimated effects may be unbalanced
anova(ARBMBClm)
TukeyHSD(aov(ARBMBClm))

#MBCandprecip is MBCplotmeans is means by microsite and depth with MAP
#MBCcan05precip is MBCCanUpper is means of just canopy 0-5 cm with MAP 
#MBCandprecip linear models - changed to AllMBCandMAP
summary(lm(mean_MBC ~ Treatment, data=AllMBCandMAP)) 
#treatment is not significant
summary(lm(mean_MBC ~ MAP + Treatment, data=AllMBCandMAP)) 
#treatment is not significant
#precip is not significant
summary(lm(mean_MBC ~ Site * Treatment, data=AllMBCandMAP)) 
#no interactions  
summary(lm(mean_MBC ~ Site + Treatment, data=AllMBCandMAP)) 
#ANT and BC different than BP
#treatment not significant 
anova(lm(mean_MBC ~ Site + Treatment, data=MBCandprecip))
summary(lm(mean_MBC ~ Site + Treatment , data=MBCandprecip))

#MBCcan05precip linear models
summary(lm(mean2_MBC ~ Treatment, data=MBCcan05precip)) 
#treatment is not significant
summary(lm(mean2_MBC ~ Precip + Treatment, data=MBCcan05precip)) 
#treatment is not significant
#precip is not significant
summary(lm(mean2_MBC ~ Site * Treatment, data=MBCcan05precip)) 
#not enough data for interactions  
summary(lm(mean2_MBC ~ Site + Treatment, data=MBCcan05precip)) 
#BC different than BP
#Arb and CC are marginally significant
#treatment not significant

#all data
summary(lm(mean_MBC ~ Site + Treatment + Depth + Microsite, data=AllMBCandMAP))
anova(lm(mean_MBC ~ Treatment + Depth + Microsite + MAP, data=AllMBCandMAP))
#without site, MAP and depth significant but microsite is marginally sig
anova(lm(mean_MBC ~ Site + Treatment + Depth + Microsite + MAP, data=AllMBCandMAP))
#with site, site, depth, microsite,and  treatment are significant but MAP is not
anova(lm(mean_MBC ~ Site + Treatment + Depth + Microsite, data=AllMBCandMAP))
#without MAP site, treatment, depth, and microsite all sig
TukeyHSD(aov(lm(mean_MBC ~ Site + Treatment + Depth + Microsite, data=AllMBCandMAP)))
TukeyHSD(aov(lm(mean_MBC ~ MAP + Depth + Microsite, data=AllMBCandMAP)))
summary(aov(mean_MBC ~ MAP + Depth + Microsite, data=AllMBCandMAP))
#use only site and treatment, not MAP, when looking at climate variables
summary(aov(mean_MBC ~ Site + Treatment + Depth + Microsite, data=AllMBCandMAP))
TukeyHSD(aov(mean_MBC ~ Site + Treatment + Depth + Microsite, data=AllMBCandMAP))

#root biomass:MBC --------------------------------------------
rootmass<- read.csv("~/Documents/Masters/Research/Summer 2018 soil/SoilMassBM.csv", header=T)
rootmass$Initial.mass..g. <- NULL
rootmass$mass.after.sieving..g. <- NULL
rootmass$mass.of.roots..g. <- NULL
rootmass$X<-NULL
colnames(rootmass)[6] <- "DryRootMass"

RootVsMBC <- merge(rootmass,MBC)
RootVsMBC$SiteOrder = factor(RootVsMBC$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
RootVsMBC$TreatmentOrder = factor(RootVsMBC$Treatment, levels=c("Exl","Ctrl","Add"), labels=c("Exclusion","Ambient","Addition")) 

#treatment lm
ggplot(data = RootVsMBC, aes(x=DryRootMass,y=MBC,color=TreatmentOrder))+ 
  geom_point(position = "identity", aes(shape=SiteOrder),size=4) +
  facet_grid(Depth~Microsite, scales = "free_x") +
  stat_smooth(aes(group = Treatment), method = "lm", se = FALSE, formula = y ~ x) +
  scale_color_manual(values=c("#de2d26", "#bdbdbd", "#08519c")) +
  scale_shape_manual(values=c(15,16,17,0,4), name = "Site")+ 
  labs(x = "Dry Root Biomass (g)") +
  ylab(expression(paste(Microbial~Biomass~(mg~DOC~kg~dry~soil^{-1}))))+
  ylim(0,1000)+
  theme_grey(base_size = 22) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x=element_blank(),
    #legend.box = "horizontal",
    #legend.position = c(.4,.87),   
    axis.line = element_line(color = "black"))+
  guides(fill=guide_legend(title="Treatment"))

# ggsave("MBCvsRootBiomass.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/DOC/RFigures/",
#        height=8,width=14,units="in",dpi=1000)

#just root biomass ----
rootmass$SiteOrder = factor(rootmass$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
rootmass$TreatmentOrder = factor(rootmass$Treatment, levels=c("Exl","Ctrl","Add"), labels=c("Exclusion","Ambient","Addition")) 
rootmass$MicrositeOrder <- factor(rootmass$Microsite, levels = c("Can", "IC"),
                                     labels = c("Canopy", "Inter-Canopy"))
rootmass$DepthOrder <- factor(rootmass$Depth, levels = c("upper","lower"), 
                                 labels = c("0-5 cm","5-10 cm"))

Rootmeansall <- ddply(rootmass, c("Site","Treatment", "Microsite","Depth"), summarise,
                     N    = sum(!is.na(DryRootMass)),
                     mean_rootmass = mean(DryRootMass, na.rm=TRUE),
                     sd_rootmass = sd(DryRootMass, na.rm=TRUE),
                     se_rootmass  = sd_rootmass / sqrt(N))

Rootmeansall$SiteOrder = factor(Rootmeansall$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
Rootmeansall$TreatmentOrder = factor(Rootmeansall$Treatment, levels=c("Exl","Ctrl","Add"), labels=c("Exclusion","Ambient","Addition")) 
Rootmeansall$MicrositeOrder <- factor(Rootmeansall$Microsite, levels = c("Can", "IC"),
                                  labels = c("Canopy", "Inter-Canopy"))
Rootmeansall$DepthOrder <- factor(Rootmeansall$Depth, levels = c("upper","lower"), 
                              labels = c("0-5 cm","5-10 cm"))

ggplot(data = Rootmeansall, aes(x=SiteOrder,y=mean_rootmass,fill=TreatmentOrder))+ 
  geom_bar(stat = "identity", position = "dodge")  +
  facet_grid(DepthOrder~MicrositeOrder, scales = "free_x") +
  scale_fill_manual(values=c("#de2d26", "#bdbdbd", "#08519c")) +
  labs(x = "Site") +
  ylab(expression(paste(Dry~Root~Biomass~(g))))+
  scale_x_discrete(labels = label_wrap(10))+ # if label_wrap giving errors load scales package
  #ylim(0,4)+
  theme_grey(base_size = 22) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x=element_blank(),
    #legend.box = "horizontal",
    legend.position = c(.65,.8),   
    axis.line = element_line(color = "black"))+
  guides(fill=guide_legend(title="Treatment")) +
  geom_errorbar(aes(ymin=mean_rootmass-se_rootmass, ymax=mean_rootmass+se_rootmass), width=.2, position=position_dodge(.9))

# ggsave("Rootbiomass.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/DOC/RFigures/",
#        height=8,width=14,units="in",dpi=1000)

Rootmeanscombinedmicrosite <- ddply(rootmass, c("Site","Treatment","Depth"), summarise,
                      N    = sum(!is.na(DryRootMass)),
                      mean_rootmass = mean(DryRootMass, na.rm=TRUE),
                      sd_rootmass = sd(DryRootMass, na.rm=TRUE),
                      se_rootmass  = sd_rootmass / sqrt(N))

Rootmeanscombinedmicrosite$SiteOrder = factor(Rootmeanscombinedmicrosite$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
Rootmeanscombinedmicrosite$TreatmentOrder = factor(Rootmeanscombinedmicrosite$Treatment, levels=c("Exl","Ctrl","Add"), labels=c("Exclusion","Ambient","Addition")) 
Rootmeanscombinedmicrosite$DepthOrder <- factor(Rootmeanscombinedmicrosite$Depth, levels = c("upper","lower"), 
                                  labels = c("0-5 cm","5-10 cm"))
ggplot(data = Rootmeanscombinedmicrosite, aes(x=SiteOrder,y=mean_rootmass,fill=TreatmentOrder))+ 
  geom_bar(stat = "identity", position = "dodge")  +
  facet_grid(~DepthOrder, scales = "free_x") +
  scale_fill_manual(values=c("#de2d26", "#bdbdbd", "#08519c")) +
  labs(x = "Site") +
  ylab(expression(paste(Dry~Root~Biomass~(g))))+
  scale_x_discrete(labels = label_wrap(10))+
  #ylim(0,4)+
  theme_grey(base_size = 22) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x=element_blank(),
    #legend.box = "horizontal",
    legend.position = c(.15,.8),   
    axis.line = element_line(color = "black"))+
  guides(fill=guide_legend(title="Treatment")) +
  geom_errorbar(aes(ymin=mean_rootmass-se_rootmass, ymax=mean_rootmass+se_rootmass), width=.2, position=position_dodge(.9))

# ggsave("RootbiomassCombinedMicrosite.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/DOC/RFigures/",
#        height=8,width=14,units="in",dpi=1000)

#site lm
ggplot(data = RootVsMBC, aes(x=DryRootMass,y=MBC,color=SiteOrder))+ 
  geom_point(position = "identity", aes(shape=Treatment),size=4) +
  facet_grid(Microsite~Depth, scales = "free_x") +
  stat_smooth(aes(group = Site), method = "lm", se = FALSE, formula = y ~ x) +
  scale_color_manual(values=c("#f7f7f7","#cccccc","#969696","#636363","#252525"), name = "Site") +
  scale_shape_manual(values=c(15,16,17,0,4), name = "Treatment")+ 
  labs(x = "Dry Root Biomass (g)") +
  ylab(expression(paste(Microbial~Biomass~(mg~DOC~kg~dry~soil^{-1}))))+
  ylim(0,1000)+
  theme_grey(base_size = 22) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.ticks.x=element_blank(),
    #legend.box = "horizontal",
    #legend.position = c(.4,.85),   
    axis.line = element_line(color = "black"))+
  
summary(aov(lm(DryRootMass ~ MBC, data=RootVsMBC)))

#Normalize MBC by SOC ------------------------------------
#SOC is mg C / g Soil as a percent, mg -> g is the same conversion as g -> kg so percent stays the same
SOClong <- read.csv("~/Documents/Masters/Research/CN/GrampsCNlong.csv", header=T)
#combine data
MBC$SOC <- SOClong$PctC
MBC$SOC100 <- MBC$SOC /100
#multiply MBC by SOC %
MBC$NormalizedMBC <- MBC$MBC / MBC$SOC
#need means of samples that were run more than once
MBCSOCmeansoverall <- ddply(MBC, c("Site","Treatment","Plot", "Microsite","Depth"), summarise,
                         N    = sum(!is.na(NormalizedMBC)),
                         mean_MBC = mean(NormalizedMBC, na.rm=TRUE),
                         sd_MBC = sd(NormalizedMBC, na.rm=TRUE),
                         se_MBC  = sd_MBC / sqrt(N))
#now get means for graph
MBCSOCmeans <- ddply(MBCSOCmeansoverall, c("Site","Treatment", "Microsite","Depth"), summarise,
                  N    = sum(!is.na(mean_MBC)),
                  mean2_MBC = mean(mean_MBC, na.rm=TRUE),
                  sd2_MBC = sd(mean_MBC, na.rm=TRUE),
                  se2_MBC  = sd2_MBC / sqrt(N))
#now graph it

MBCSOCmeans$SiteOrder = factor(MBCSOCmeans$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
MBCSOCmeans$TreatmentOrder = factor(MBCSOCmeans$Treatment, levels=c("Exl","Ctrl","Add"), labels=c("Exclusion","Ambient","Addition")) 
MBCSOCmeans$MicrositeOrder <- factor(MBCSOCmeans$Microsite, levels = c("Can", "IC"),
                             labels = c("Canopy", "Inter-Canopy"))
MBCSOCmeans$DepthOrder <- factor(MBCSOCmeans$Depth, levels = c("upper","lower"), 
                              labels = c("0-5 cm","5-10 cm"))

ggplot(data = MBCSOCmeans [1:60,], aes(x=SiteOrder,y=mean2_MBC,fill=TreatmentOrder)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(DepthOrder~MicrositeOrder, scales = "free_x") +
  #facet_wrap(~Microsite, scales = "free_x") +
  scale_fill_manual(values=c("#de2d26", "#bdbdbd", "#08519c")) +
  labs(x = "Site") +
  ylab(expression(paste(Microbial~Biomass~Soil~Orgnic~Carbon^{-1})))+
  scale_x_discrete(labels = label_wrap(10))+
  theme_grey(base_size = 22) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    #axis.text.x = element_text(angle=45,hjust = 1),
    axis.ticks.x=element_blank(),
    legend.position = "none",   
    axis.line = element_line(color = "black"))+
  guides(fill=guide_legend(title="Treatment"))+
  geom_errorbar(aes(ymin=mean2_MBC-se2_MBC, ymax=mean2_MBC+se2_MBC), width=.2, position=position_dodge(.9)
  )
# ggsave("MBCperSOC.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/Writing/Paper/Figures for Paper/",
#        height=8,width=14,units="in",dpi=1000)
#   