#Enzyme analyses

library(ggplot2)
library(plyr)
library(tidyr)


Enzymeslong <- read.csv("~/Documents/Masters/Research/Enzymes/EnzymeResultslong.csv", header=TRUE)

EnzymeMeans <- ddply(Enzymeslong, c("Site","Treatment","Enzyme"), summarise,
                     N    = sum(!is.na(Activity)),
                     mean_activity = mean(Activity, na.rm=TRUE),
                     sd_activity = sd(Activity, na.rm=TRUE),
                     se_activity  = sd_activity / sqrt(N))

EnzymeMeansnotrt <- ddply(Enzymeslong, c("Site","Enzyme"), summarise,
                     N    = sum(!is.na(Activity)),
                     mean_activity = mean(Activity, na.rm=TRUE),
                     sd_activity = sd(Activity, na.rm=TRUE),
                     se_activity  = sd_activity / sqrt(N))

EnzymeMeans$SiteOrder = factor(EnzymeMeans$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
EnzymeMeans$EnzymeOrder = factor(EnzymeMeans$Enzyme, levels=c("BG","CBH","NAG","PHOS"), labels=c("Beta Glucosidase","Cellobiohydrolase","N-Acetyl glucosaminidase","Phosphatase")) 
EnzymeMeans$TreatmentOrder = factor(EnzymeMeans$Treatment, levels=c("Exl","Ctrl","Add"), labels=c("Exclusion","Ambient","Addition")) 

EnzymeMeansnotrt$SiteOrder = factor(EnzymeMeansnotrt$Site, levels=c("BP","ANT","BC","ARB","CC"), labels=c("Desert Scrub","Desert Grassland","Juniper Savanna","Ponderosa Pine Meadow","Mixed Conifer Meadow")) 
EnzymeMeansnotrt$EnzymeOrder = factor(EnzymeMeansnotrt$Enzyme, levels=c("BG","CBH","NAG","PHOS"), labels=c("Beta Glucosidase","Cellobiohydrolase","N-Acetyl glucosaminidase","Phosphatase")) 


ggplot(data = EnzymeMeans, aes(x=SiteOrder, y=mean_activity, fill = TreatmentOrder)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(vars(EnzymeOrder), scales = "free", labeller = labeller(EnzymeOrder = label_wrap_gen(width = 15))) +
  scale_fill_manual(name = "Treatment", values = c("#de2d26", "#bdbdbd", "#08519c"))+
  labs(x = "Site") +
  ylab(expression(paste(Enzyme~Activity(nmol~h^{-1}*g^{-1}))))+
  scale_x_discrete(labels = label_wrap(10))+
  theme_grey(base_size = 17) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle=45,hjust = 1),
    legend.position = c(0.1,.65),   
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA))+
  geom_errorbar(aes(ymin=mean_activity-se_activity, ymax=mean_activity+se_activity), width=.2, position=position_dodge(.9)
  )

# ggsave("Enzymemeans.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/Enzymes/RFigures/",
#        height=9,width=10,units="in",dpi=1000)

ggplot(data = EnzymeMeansnotrt, aes(x=SiteOrder, y=mean_activity)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(vars(EnzymeOrder), scales = "free", labeller = labeller(EnzymeOrder = label_wrap_gen(width = 15))) +
  scale_fill_manual(values = c("#525252"))+
  labs(x = "Site") +
  ylab(expression(paste(Enzyme~Activity(nmol~h^{-1}*g^{-1}))))+
  scale_x_discrete(labels = label_wrap(10))+
  theme_grey(base_size = 17) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle=45,hjust = 1),
    legend.position = c(0.1,.65),   
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA))+
  geom_errorbar(aes(ymin=mean_activity-se_activity, ymax=mean_activity+se_activity), width=.2, position=position_dodge(.9)
  )

# ggsave("Enzymemeansnotrt.png",plot = last_plot(),
#        path="~/Documents/Masters/Research/Enzymes/RFigures/",
#        height=9,width=10,units="in",dpi=1000)
