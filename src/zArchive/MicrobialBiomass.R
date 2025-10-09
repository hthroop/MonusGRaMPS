install.packages(splitstackshape)
library(splitstackshape)

# Read in DOC data - Round 10 data
infile <- read.csv("~/Documents/Masters/Research/DOC/DOCround10GELdata.csv", header=T)
# Read in soil weight data
soil.weights <- read.csv("~/Documents/Masters/Research/DOC/ChloroformFumigationRound10.csv", header=T)

# See how R is treating each of our data columns
str(infile)
# We want the Sample.Name, Sample.ID, and Results columns to be in character format, not factor format
infile$Sample.Name <- as.character(infile$Sample.Name)
infile$Sample.ID <- as.character(infile$sample.ID)
infile$Results <- as.character(infile$Result)

# We have our own blanks that account for both contamination in the machine
# AND contamination during the extraction procedure, so we don't need
# the blanks that account for contamination in the machine only (Sample.Name = Blank).
# Also we don't need the Cleaning, Standards, or QC values.
# And we only really need the Sample.ID and Results columns
#put data we want into dataframe d
d <- infile[ !(infile$Sample.Name %in% c("Blank","Cleaning","Standards","QC")), c("Sample.ID","Results")]

# Parse Results column into numerical values
d$Results <- gsub("mg/L","",d$Results)
d$Results <- gsub("NPOC:","",d$Results)
d$Results <- as.numeric(d$Results)
# And rename it so we remember the units
names(d)[2] <- "DOC.mg.per.L"


# Corrent for blanks
# First, calculate the mean blank value for each set of extractions
blank.names <- c("blank1-IN","blank1-CFE")#,"blank2-IN","blank2-CFE") #"blank3-IN","blank3-CFE","blank4-IN","blank4-CFE")
d.blanks <- d[d$Sample.ID %in% blank.names,]
blanks.IN <- mean(d.blanks$DOC.mg.per.L[d.blanks$Sample.ID %in% c("blank1-IN","blank2-IN")]) #"blank3-IN","blank4-IN")])
blanks.CFE <- mean(d.blanks$DOC.mg.per.L[d.blanks$Sample.ID %in% c("blank1-CFE","blank2-CFE")]) #"blank3-CFE","blank4-CFE")])

# Second, subtract the appropriate blank value from each sample
d.samples <- d[ !(d$Sample.ID %in% blank.names),]
#separate extraction type into new column
d.samples <- as.data.frame(cSplit(d.samples, 'Sample.ID', sep="-", type.convert=FALSE, drop=FALSE))
names(d.samples)[3:4] <- c("Sample.Name","Extraction.Type") #put actual sample name in sample id (sample id is what goldwater labels used)

d.samples$Blank <- 0
d.samples$Blank <- ifelse(d.samples$Extraction.Type == "IN", blanks.IN, 
                          ifelse(d.samples$Extraction.Type == "CFE", blanks.CFE, NA))

#subtracts blank value from measured DOC for corrected DOC
d.samples$Blank.Corrected.mg.per.L <- d.samples$DOC.mg.per.L - d.samples$Blank

# Adjust concentrations for the 1:5 dilution with nanopure water performed in Goldwater
d.samples$Actual.mg.per.L <- d.samples$Blank.Corrected.mg.per.L * 5

d.doc <- d.samples[,c("Sample.ID","Sample.Name","Extraction.Type","Actual.mg.per.L")]

# Calculate mass of dry soil used in extraction using wet and dry soil mass data
soil.weights$moist.soil <- soil.weights$TinPlusWetSoil - soil.weights$TinWeight
soil.weights$dry.soil <- soil.weights$TinPlusDrySoil - soil.weights$TinWeight

soil.weights$IN.g.dry.soil <- soil.weights$InitialSampleMass * (soil.weights$dry.soil/soil.weights$moist.soil)
soil.weights$CFE.g.dry.soil <- soil.weights$FumigatedSampleMass * (soil.weights$dry.soil/soil.weights$moist.soil)

IN.soil.dry.weights <- soil.weights[,c("Sample.Name","IN.g.dry.soil")]
CFE.soil.dry.weights <- soil.weights[,c("Sample.Name","CFE.g.dry.soil")]

IN.soil.dry.weights$Extraction.Type <- "IN"
CFE.soil.dry.weights$Extraction.Type <- "CFE"

names(IN.soil.dry.weights)[2] <- "g.dry.soil"; names(CFE.soil.dry.weights)[2] <- "g.dry.soil"
soil.dry.weights <- rbind(IN.soil.dry.weights,CFE.soil.dry.weights)
soil.dry.weights$Sample.ID <- paste0(soil.dry.weights$Sample.Name,"-",soil.dry.weights$Extraction.Type)

#troubleshoot merge
#d.doc$sample.match <- match(d.doc$Sample.ID, soil.dry.weights$Sample.ID)
#soil.dry.weights$sample.match <- match(soil.dry.weights$Sample.ID, d.doc$Sample.ID)

#d.doc$Sample.ID[is.na(d.doc$sample.match)==T]
#soil.dry.weights$Sample.ID[is.na(soil.dry.weights$sample.match)==T]

# Merge DOC and dry soil mass data
d.data <- merge(d.doc, soil.dry.weights[,c("Sample.ID","g.dry.soil")], by="Sample.ID")

# Calculate mg DOC per kg dry soil
d.data$mg.doc <- d.data$Actual.mg.per.L * (50/1000)   # mg/L DOC * L of K2SO4 used in extraction
d.data$mg.doc.per.kg.dry.soil <- (d.data$mg.doc / d.data$g.dry.soil) * 1000 # mg per kg is a common unit to report this in

# Calculate extractable microbial biomass carbon as the difference between IN and CFE for each sample
d.data.IN <- d.data[d.data$Extraction.Type=="IN",c("Sample.Name","mg.doc.per.kg.dry.soil")]
names(d.data.IN)[2] <- "mg.doc.per.kg.dry.soil.IN"

d.data.CFE <- d.data[d.data$Extraction.Type=="CFE",c("Sample.Name","mg.doc.per.kg.dry.soil")]
names(d.data.CFE)[2] <- "mg.doc.per.kg.dry.soil.CFE"

d.data.out <- merge(d.data.IN, d.data.CFE, by="Sample.Name")

d.data.out$initial.DOC <- d.data.out$mg.doc.per.kg.dry.soil.IN
d.data.out$extractable.MBC <- d.data.out$mg.doc.per.kg.dry.soil.CFE - d.data.out$mg.doc.per.kg.dry.soil.IN
d.data.out$extractable.MBC <- ifelse(d.data.out$extractable.MBC < 0, 0, d.data.out$extractable.MBC)

# MBC = extractable.MBC/0.45 per Jenkinson et al. (2004). Soil Biology and Biochemistry 36(1): 5-7.
# Some reviewers think this number is somewhat arbitrary and prefer that authors report extractable MBC
d.data.out$MBC <- d.data.out$extractable.MBC/0.45 

d.data.out <- d.data.out[,c("Sample.Name","initial.DOC","extractable.MBC","MBC")]

# Write out results 
write.csv(d.data.out, "~/Documents/Masters/Research/DOC/CFEround9results.csv", row.names=F)  #NOTE: all concentrations are in mg C per kg dry soil
DOC <- read.csv("~/Documents/Masters/Research/DOC/MBC results/CFEround7results.csv", header=T)

library(plyr)

mbc_means <- ddply(df, c("Site", "storage"), summarise,
                      N    = sum(!is.na(MBC)),
                      mean_MBC = mean(MBC, na.rm=TRUE),
                      sd_MBC   = sd(MBC, na.rm=TRUE),
                      se_MBC  = sd_MBC / sqrt(N))
print(mbc_means)
df2<- mbc_means
df2$se_MBC = as.numeric(df2$se_MBC)

ggplot(data = df2, aes(x=Site, y=mean_MBC, fill = storage)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Site") +
  labs(y = "Microbial biomass") +
  geom_text(aes(label=Site), vjust=3, colour="black",
            position=position_dodge(.9), size=3) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    legend.position = c(0.2,.75))+
  geom_errorbar(aes(ymin=mean_MBC-se_MBC, ymax=mean_MBC+se_MBC), width=0.2, position=position_dodge(0.9)
  )

#anova
anovamodel = lm(mean_MBC~storage, data=df2)
anova.model= aov(anovamodel)
summary(anova.model)

#mean and standard error
ANTdrydata <-c(169.0487,179.1299,163.4440)
ANTdrymean <- mean(ANTdrydata)
sd(ANTdrydata)
ANTdryse <- sqrt(sd(ANTdrydata)/3)
ANTfrozendata <-c(290.1241,282.6596,273.8071)
ANTfrozenmean <- mean(ANTfrozendata)
sd(ANTfrozendata)
ANTfrozense <- sqrt(sd(ANTfrozendata)/3)
ARBdrydata <-c(448.2696,494.0779,503.6671)
ARBdrymean <- mean(ARBdrydata)
sd(ARBdrydata)
ARBdryse <- sqrt(sd(ARBdrydata)/3)
ARBfrozendata <- c(514.2777,494.9443,502.5608)
ARBfrozenmean <- mean(ARBfrozendata)
sd(ARBfrozendata)
ARBfrozense <- sqrt(sd(ARBfrozendata)/3)

MBCmeans <-c(ANTdrymean,ANTfrozenmean,ARBdrymean,ARBfrozenmean)
MBCerror <-c(ANTdryse,ANTfrozense,ARBdryse,ARBfrozense)
#plot
barplot(MBCmeans)
arrows(MBCerror)

#ggplot
library(ggplot2)

ggplot(data = MBCmeans)


#sample names and MCB
MBC <- d.data.out[,c(1,4)]
MBC$site <- c("BC","BC","BC","BC","BC","BC","BP","BP","BP","BP","BP","BP","CC","CC","CC","CC","CC","CC")
MBC$treatment <- c("Add","Add","Ctrl","Ctrl","Exl","Exl","Add","Add","Ctrl","Ctrl","Exl","Exl","Add","Add","Ctrl","Ctrl","Exl","Exl")
MBC$microsite <- c("Can","IC","Can","IC","Can","IC","Can","IC","Can","IC","Can","IC","Can","IC","Can","IC","Can","IC")
write.csv(MBC, "~/Documents/Masters/Research/Preliminary Data/preliminary_soils_MBC.csv", row.names=F)  #NOTE: all concentrations are in mg C per kg dry soil
