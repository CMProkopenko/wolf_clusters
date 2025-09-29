#####Models
####


###load packages
libs <- c('data.table', 'lme4', 'glmmTMB', 'tidyverse',
          'ggplot2', 'viridis', 'patchwork')
lapply(libs, require, character.only = TRUE)

####Load and subset data ####

### load All Sites data after cleaning protocol
datrmnp <- fread('data/2023-02-22_RMNP_K_All_Clu.csv') 
datrmnp[,'StudySite':= as.factor('RMNP')]
###prey density for area by year

### load All Sites data after cleaning protocol
datgha26 <- fread('data/2023-08-01_GHA_K_All_Clu.csv') 
datgha26[,'StudySite':= as.factor('SEMB')]

summary(as.factor(datgha26$CollarID))


####
indivdat <- fread('data/GLM_datasets/Individual_dataset_full.csv') 

####prey biomass?
biomass <- fread("data/Biomass_values_both.csv")
glimpse(biomass)
summary(as.factor(biomass$Species))

#####

clustdat <- rbind(datrmnp, datgha26, fill=TRUE)
summary(clustdat)

### clusters matched with investigated sites
investdat <-clustdat[JoinStatus == 'Matched' & finalIgnore == 0]
investdat$Behaviour_1 <- as.factor(investdat$Behaviour_1)
summary(as.factor(investdat$Prey_1))
killdat <-investdat[Behaviour_1 == "Kill"]

###clean up prey type to merge with Biomass
killdat[,'Species':= as.character('Other')]
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Calf == 1] <-'Ungulate_Calf'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Calf == 1 & killdat$Moose == 1] <-'Moose_Calf'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Calf == 1 & killdat$Elk == 1] <-'Elk_Calf'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Calf == 1 & killdat$Elk == 1] <-'Deer_Calf'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Moose == 1 & killdat$Calf == 0] <- 'Moose_Adult'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Moose == 1 & killdat$Calf == 0 & killdat$Prey_sex == "Female"] <- 'Moose_Adult_Female'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Moose == 1 & killdat$Calf == 0 & killdat$Prey_sex == "Male"] <- 'Moose_Adult_Male'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Elk == 1 & killdat$Calf == 0] <- 'Elk_Adult'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Elk == 1 & killdat$Calf == 0 & killdat$Prey_sex == "Female"] <- 'Elk_Adult_Female'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Elk == 1 & killdat$Calf == 0 & killdat$Prey_sex == "Male"] <- 'Elk_Adult_Male'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$WTD == 1 & killdat$Calf == 0] <- 'Deer_Adult'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$WTD == 1 & killdat$Calf == 0 & killdat$Prey_sex == "Female"] <- 'Deer_Adult_Female'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$WTD == 1 & killdat$Calf == 0 & killdat$Prey_sex == "Male"] <- 'Deer_Adult_Male'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Beaver == 1] <- 'Beaver'
killdat$Species[killdat$Behaviour_1 =="Kill" & killdat$Prey_1 == "Snowshoe hare" | killdat$Prey_1 == "Hare"] <- 'Hare'

summary(as.factor(killdat$Species))

killdat[,Season := ifelse(mwKmonth == '5' | mwKmonth == '6' | mwKmonth == '7'| mwKmonth == '8' | mwKmonth == '9'| mwKmonth == '10','Snow Free', 'Snow')]
killdat$Season

####add season

kill_covs <- merge(killdat, biomass, by = c("Species","Season"))
glimpse(kill_covs)

indiv.season <-indivdat[season != "full-year"]
indiv.season[,Season := ifelse(season == 'Snow', 'Snow', 'Snow Free')]
indiv.season$Season

indiv.season[,StudySite := ifelse(study_area == 'GHA', 'SEMB', 'RMNP')]
indiv.season$StudySite



glimpse(indiv.season)

summary(as.factor(kill_covs$StudySite))
summary(as.factor(indiv.season$StudySite))



kill_covs2 <- merge(kill_covs, indiv.season, by = c("CollarID", "Season", "StudySite"))

summary(as.factor(kill_covs2$StudySite))

####SAVE RDS



####EXPLORE COVARIATES

summary(kill_covs2$CluDurHours)
summary(kill_covs2$CluDurHours)
summary(kill_covs2$moose_density_averaged)

ggplot(kill_covs2) + geom_density(aes((CluDurHours), fill = StudySite), alpha = 0.5)
ggplot(kill_covs2) + geom_density(aes((moose_density_averaged), fill = study_area), alpha = 0.5)
ggplot(kill_covs2) + geom_density(aes((Avg_Weight), fill = Species), alpha = 0.5)
ggplot(kill_covs2) + geom_density(aes((lf_dens), fill = StudySite), alpha = 0.5)



####KILL CLUSTER MODELS (include cluster ID as random effect)

patchtime<- lmer(log(CluDurHours) ~ log(Avg_Weight) + log(moose_density_averaged+0.000001) + lf_dens + Season + StudySite + (1|PackID/CollarID) , data=kill_covs2)

summary(patchtime)


revisit<- lmer(log(C) ~ log(Avg_Weight) + log(moose_density_averaged+0.000001) + lf_dens + Season + StudySite + (1|PackID) , data=kill_covs2)

summary(durkill)
