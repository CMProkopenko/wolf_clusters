#####Models
####


###load packages
libs <- c('data.table', 'lme4', 'glmmTMB',
          'ggplot2', 'ggthemes', 'colorspace', 'viridis', 'patchwork')
lapply(libs, require, character.only = TRUE)

####Load and subset data ####

### load All Sites data after cleaning protocol
datrmnp <- fread('data/2023-02-22_RMNP_K_All_Clu.csv') 
datrmnp[,'StudySite':= as.factor('RMNP')]
###prey density for area by year



### load All Sites data after cleaning protocol
datgha26 <- fread('data/2023-08-01_GHA_K_All_Clu.csv') 
datgha26[,'StudySite':= as.factor('SEMB')]
###prey density for area by year

clustdat <- rbind(datrmnp, datgha26, fill=TRUE)

summary(clustdat)

summary(clustdat$StudySite)

### clusters matched with investigated sites
investdat <-clustdat[JoinStatus == 'Matched' & finalIgnore == 0]
investdat$Behaviour_1 <- as.factor(investdat$Behaviour_1)
summary(investdat$Behaviour_1)


###create prey categories for kill behavours
###clean up behaviours for plotting
investdat[,'Behav':= as.character('Other')]
investdat$Behav[investdat$Behaviour_1 =="Kill" & investdat$Calf == 1] <-'Calf Kill'
investdat$Behav[investdat$Behaviour_1 =="Kill" & investdat$Moose == 1 & investdat$Calf == 0] <- 'Moose Kill'
investdat$Behav[investdat$Behaviour_1 =="Kill" & investdat$Elk == 1 & investdat$Calf == 0] <- 'Elk Kill'
investdat$Behav[investdat$Behaviour_1 =="Kill" & investdat$WTD == 1& investdat$Calf == 0] <- 'Deer Kill'
investdat$Behav[investdat$Behaviour_1 =="Kill" & investdat$Beaver == 1] <- 'Beaver Kill'
investdat$Behav[investdat$Behaviour_1 =="Probable kill"] <-"Probable Kill"
investdat$Behav[investdat$Behaviour_1 =="Probable prey encounter"]<-"Probable Kill" ###check this is what we want to do
investdat$Behav[investdat$Behaviour_1  =="Revisit"]<-"Revisit"
investdat$Behav[investdat$Behaviour_1  =="Scavenge"]<-"Scavenge"
#investdat$Behav[investdat$Behaviour_1  =="Probable scavenge"]<-"Scavenge" ##check
investdat$Behav[investdat$Behaviour_1  =="Resting"]<-"Resting"
investdat$Behav[investdat$Behaviour_1  =="Probable resting"]<-"Resting"
investdat$investdat[investdat$Behaviour_1  =="Probable resting"]<-"Resting"
investdat$Behav[investdat$Behaviour_1  =="Den"]<-"Den"
investdat$Behav[investdat$Behaviour_1  =="Rendez-vous"]<-"Rendez-vous"

investdat$Behav <- as.factor(investdat$Behav)
summary(investdat$Behav)


#####cluster models



####cluster duration
####biomass estimate

#####


durmod <- lm(CluDurHours ~ biomass + preydensity + StudySite + randomeffect , data=investdat)

summary(durmod)

####site revisits

revisitmod <- lm(Site_revisit ~ Behav:StudySite, data=investdat)

summary(revisitmod)

