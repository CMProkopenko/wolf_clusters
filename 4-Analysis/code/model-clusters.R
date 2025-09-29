#####Models
####


###load packages
libs <- c('data.table', 'effects', 'lme4', 'glmmTMB','grid', 'ggdist','gridExtra', 'tidyverse', 'sjPlot',
          'ggplot2', 'viridis', 'patchwork', 'broom.mixed')
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
indivdat <- fread('data/GLM_datasets/Individual_dataset_full_updated_2025-08-15.csv') 

####prey biomass
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



killdat$Season

####add season

kill_covs <- merge(killdat, biomass, by = c("Species","Season"))

indivdat <- indivdat[!(pack == "Great-Falls-Lone" | pack == "Black-River")]

indiv.season <-indivdat[season != "full-year"]
indiv.season[,Season := ifelse(season == 'Snow', 'Snow', 'Snow Free')]
indiv.season$Season

indiv.season[,StudySite := ifelse(study_area == 'GHA', 'SEMB', 'RMNP')]
indiv.season$StudySite

indiv.season[,wolf_density := ifelse(study_area == 'GHA', population/72, population/30)]
indiv.season$wolf_density

indiv.season[,pack_density := (pack_size/area.95)*1000]
indiv.season$pack_density

indiv.season[,decmix :=(deciduous+mixed)]

indiv.season$decmix

indiv.season$Season <- as.factor(indiv.season$Season)
indiv.season$StudySite <- as.factor(indiv.season$StudySite)

glimpse(indiv.season)




kill_covs2 <- merge(kill_covs, indiv.season, by = c("CollarID", "Season", "StudySite"))

summary(as.factor(kill_covs2$StudySite))

kill_covs2$Season <- as.factor(kill_covs2$Season)
kill_covs2$StudySite <- as.factor(kill_covs2$StudySite)

glimpse(kill_covs2)

####SAVE RDS



####EXPLORE COVARIATES

ggplot(kill_covs2) + geom_density(aes((CluDurHours), fill = StudySite), alpha = 0.5)
ggplot(kill_covs2) + geom_density(aes((Site_revisit), fill = StudySite), alpha = 0.5)
ggplot(kill_covs2) + geom_density(aes(((moose_density_averaged)), fill = study_area), alpha = 0.5)
ggplot(kill_covs2) + geom_density(aes((Avg_Weight), fill = Species), alpha = 0.5)
ggplot(kill_covs2) + geom_density(aes((lf_dens), fill = StudySite), alpha = 0.5)
ggplot(kill_covs2) + geom_density(aes(((wolf_density)), fill = StudySite), alpha = 0.5) ###should make this density
ggplot(kill_covs2) + geom_density(aes((pack_size), fill = StudySite), alpha = 0.5)
ggplot(kill_covs2) + geom_density(aes((decmix), fill = StudySite), alpha = 0.5)


cov_summary <- kill_covs2 |> dplyr::reframe(weight_min = min(Avg_Weight),
                                            weight_max = max(Avg_Weight),
                                            moose_min = min(moose_density_averaged),
                                            moose_max = max(moose_density_averaged),
                                            lf_min = min(lf_dens),
                                            lf_max = max(lf_dens),
                                            wolf_min = min(wolf_density),
                                            wolf_max = max(wolf_density),
                                            pack_min = min(pack_size),
                                            pack_max = max(pack_size),
                                            decmix_min = min(decmix),
                                            decmix_max = max(decmix),
                                            .by = StudySite) 
cov_summary

####KILL CLUSTER MODELS (include cluster ID as random effect)


#   
# patchtime <- lmer(log(CluDurHours) ~ 0+ log(Avg_Weight)+ moose_density_averaged + lf_dens + pack_size + wolf_density + decmix + StudySite+ Season  + (1|PackID/CollarID) , data=kill_covs2)
# summary(patchtime)
# logLik(patchtime)



patchtime <- lmer(log(CluDurHours) ~ 0 + log(Avg_Weight):StudySite + moose_density_averaged:StudySite + lf_dens:StudySite + wolf_density:StudySite + pack_size:StudySite  + decmix:StudySite + Season:StudySite+  StudySite + (1|PackID/CollarID) , data=kill_covs2)
summary(patchtime)
logLik(patchtime)

simple_patchtime <- lmer(log(CluDurHours) ~ 0 + log(Avg_Weight) + moose_density_averaged + lf_dens + wolf_density + pack_size  + decmix + Season +  StudySite + (1|PackID/CollarID) , data=kill_covs2)
summary(simple_patchtime)
logLik(simple_patchtime)



# rm_covs <- kill_covs2[kill_covs2$StudySite =='RMNP',]
# se_covs <- kill_covs2[kill_covs2$StudySite =='SEMB',]
# 
# rmpatchtime <- lmer(log(CluDurHours) ~ 0+ log(Avg_Weight)+ moose_density_averaged + lf_dens + pack_size + wolf_density + decmix + Season  + (1|PackID/CollarID) , data=rm_covs)
# rmtime_mod <- broom.mixed::tidy(rmpatchtime, conf.int=TRUE)
# rmtime_mod
# 
# sepatchtime <- lmer(log(CluDurHours) ~ 0+ log(Avg_Weight)+ moose_density_averaged + lf_dens + pack_size + wolf_density + decmix + Season  + (1|PackID/CollarID) , data=se_covs)
# setime_mod <- broom.mixed::tidy(sepatchtime, conf.int=TRUE)
# setime_mod


# 
# snow_covs <- kill_covs2[kill_covs2$Season =='Snow',]
# free_covs <- kill_covs2[kill_covs2$Season =='Snow Free',]
# 
# snowpatchtime <- lmer(log(CluDurHours) ~ 0+ log(Avg_Weight)+ moose_density_averaged + lf_dens + pack_size + wolf_density + decmix + StudySite+ (1|PackID/CollarID) , data=snow_covs)
# snowtime_mod <- broom.mixed::tidy(snowpatchtime, conf.int=TRUE)
# snowtime_mod
# 
# freepatchtime <- lmer(log(CluDurHours) ~ 0+ log(Avg_Weight)+ moose_density_averaged + lf_dens + pack_size + wolf_density + decmix + StudySite+ (1|PackID/CollarID) , data=free_covs)
# freetime_mod <- broom.mixed::tidy(freepatchtime, conf.int=TRUE)
# freetime_mod


# patchtime1 <- lmer(log(CluDurHours) ~ Season + StudySite + (1|PackID/CollarID) , data=kill_covs2)
# logLik(patchtime1) ###'log Lik.' -1362.16 (df=6)
# 
# patchtime2 <- lmer(log(CluDurHours) ~ log(Avg_Weight) + log(moose_density_averaged+0.0001) + lf_dens + pack_size + wolf_density + (1|PackID/CollarID) , data=kill_covs2)
# logLik(patchtime2) ###'log Lik.' -1306.849 (df=9)


time_mod <- broom.mixed::tidy(patchtime, conf.int=TRUE)
time_mod

write.csv(time_mod, "results/patchtime_results.csv")


simple_time_mod <- broom.mixed::tidy(simple_patchtime, conf.int=TRUE)
simple_time_mod

write.csv(simple_time_mod, "results/simple_patchtime_results.csv")


weight_site_effect <- Effect(c('StudySite','Avg_Weight'),xlevels=100,patchtime)
weight_site_df <- as.data.frame(weight_site_effect)

weightplot <- ggplot() +
  geom_line(data = weight_site_df[weight_site_df$StudySite =='RMNP'&  weight_site_df$Avg_Weight > 20,], aes(Avg_Weight,fit), colour = "#21918c") +
  geom_line(data = weight_site_df[weight_site_df$StudySite =='SEMB'&  weight_site_df$Avg_Weight < 401,], aes(Avg_Weight,fit), colour = "#440154") +
  geom_ribbon(data = weight_site_df[weight_site_df$StudySite =='RMNP'&  weight_site_df$Avg_Weight > 20,], aes(Avg_Weight,fit, ymin=lower,ymax=upper),fill="#21918c",alpha=0.1) +
  geom_ribbon(data = weight_site_df[weight_site_df$StudySite =='SEMB'&  weight_site_df$Avg_Weight < 401,], aes(Avg_Weight,fit, ymin=lower,ymax=upper),fill="#440154",alpha=0.1) +
  geom_rug(data=weight_site_effect$data,aes(Avg_Weight, y=NULL, colour = StudySite),sides="b") +
  scale_colour_manual(values = c("#21918c","#440154"), name = "StudySite") +
  xlab("Prey Size (kg)") +
  ylab("log Cluster Duration (hours)") + 
  ggtitle("a.")+
  theme_bw()  + 
  theme(legend.position = "none") +
  theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.05, vjust = -8),
    axis.line = element_line(colour = "black", linewidth = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20),
    legend.key = element_blank(),
    legend.background = element_blank())

weightplot

# weight_season_effect <- Effect(c('Season','Avg_Weight'),patchtime) 
# 
# ggplot(as.data.frame(weight_season_effect),
#        aes(Avg_Weight,fit, colour=Season, fill = Season))+
#   geom_line()+
#   ## colour=NA suppresses edges of the ribbon
#   geom_ribbon(colour=NA,alpha=0.1,
#               aes(ymin=lower,ymax=upper))+
#   xlab("Prey Weight (kg)") +
#   ylab("log Cluster Duration (hours)") + 
#   scale_colour_manual(values = c("#440154","#21918c"), name = "Season") +
#   scale_fill_manual(values = c("#440154","#21918c"), name = "Season") +
#   ## add rug plot based on original data
#   geom_rug(data=weight_season_effect$data,aes(y=NULL),sides="b") +
#   theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 20, hjust = .98, vjust = -8),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20),
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20),
#     legend.key = element_blank())

lf_site_effect <- Effect(c('StudySite','lf_dens'),xlevels=100, patchtime)

lf_site_df <- as.data.frame(lf_site_effect )

lfplot <- ggplot() +
  geom_line(data = lf_site_df[lf_site_df$StudySite =='RMNP' &  lf_site_df$lf_dens <= 0.6,], aes(lf_dens,fit), colour = "#21918c") +
  geom_line(data = lf_site_df[lf_site_df$StudySite =='SEMB' & lf_site_df$lf_dens >= .1,], aes(lf_dens,fit,), colour = "#440154") +
  geom_ribbon(data = lf_site_df[lf_site_df$StudySite =='RMNP' &  lf_site_df$lf_dens <= 0.6,], aes(lf_dens, fit, ymin=lower,ymax=upper),fill="#21918c",alpha=0.1) +
  geom_ribbon(data = lf_site_df[lf_site_df$StudySite =='SEMB'& lf_site_df$lf_dens >= .1,], aes(lf_dens,fit,ymin=lower,ymax=upper),fill="#440154",alpha=0.1) +
  geom_rug(data=lf_site_effect$data,aes(lf_dens, y=NULL, colour = StudySite),sides="b") +
  scale_colour_manual(values = c("#21918c","#440154"), name = "StudySite") +
  xlab("Linear Feature Density (km per squared km)") +
  ylab("log Cluster Duration (hours)") + 
  ggtitle("e.")+
  theme_bw()  + 
  theme(legend.position = "none") +
  theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.05, vjust = -8),
    axis.line = element_line(colour = "black", linewidth = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20),
    legend.key = element_blank())

lfplot

# lf_season_effect <- Effect(c('Season','lf_dens'),patchtime) 
# 
# ggplot(as.data.frame(lf_season_effect),
#        aes(lf_dens,fit, colour=Season, fill = Season))+
#   geom_line()+
#   geom_ribbon(colour=NA,alpha=0.1,
#               aes(ymin=lower,ymax=upper))+
#   xlab("Linear Feature Density (km per squared km)") +
#   ylab("log Cluster Duration (hours)") + 
#   scale_colour_manual(values = c("#440154","#21918c"), name = "Season") +
#   scale_fill_manual(values = c("#440154","#21918c"), name = "Season") +
#   ## add rug plot based on original data
#   geom_rug(data=lf_season_effect$data,aes(y=NULL),sides="b") +
#   theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 20, hjust = .98, vjust = -8),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20),
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20),
#     legend.key = element_blank())


pack_site_effect <- Effect(c('StudySite','pack_size'),xlevels=100,patchtime) 
pack_site_df <- as.data.frame(pack_site_effect)

packplot <-ggplot() +
  geom_line(data = pack_site_df[pack_site_df$StudySite =='RMNP' &  pack_site_df$pack_size <= 11,], aes(pack_size,fit), colour = "#21918c") +
  geom_line(data = pack_site_df[pack_site_df$StudySite =='SEMB' & pack_site_df$pack_size >= 4,], aes(pack_size,fit,), colour = "#440154") +
  geom_ribbon(data = pack_site_df[pack_site_df$StudySite =='RMNP' &  pack_site_df$pack_size <= 11,], aes(pack_size, fit, ymin=lower,ymax=upper),fill="#21918c",alpha=0.1) +
  geom_ribbon(data = pack_site_df[pack_site_df$StudySite =='SEMB'& pack_site_df$pack_size >= 4,], aes(pack_size,fit,ymin=lower,ymax=upper),fill="#440154",alpha=0.1) +
  geom_rug(data=pack_site_effect$data,aes(pack_size, y=NULL, colour = StudySite),sides="b") +
  scale_colour_manual(values = c("#21918c","#440154"), name = "StudySite") +
  xlab("Pack Size") +
  ylab("log Cluster Duration (hours)") + 
  ggtitle("b.")+
  theme_bw()  + 
  theme(legend.position = c(0.9, 0.9)) +
  theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.05, vjust = -8),
    axis.line = element_line(colour = "black", linewidth = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20),
    legend.key = element_blank(),
    legend.background = element_blank(),)

packplot

# pack_season_effect <- Effect(c('Season','pack_size'),patchtime) 
#  
# ggplot(as.data.frame(pack_season_effect),
#        aes(pack_size,fit, colour=Season, fill = Season))+
#   geom_line()+
#   ## colour=NA suppresses edges of the ribbon
#   geom_ribbon(colour=NA,alpha=0.1,
#               aes(ymin=lower,ymax=upper))+
#   xlab("Pack Size") +
#   ylab("log Cluster Duration (hours)") +
#   scale_colour_manual(values = c("#440154","#21918c"), name = "Season") +
#   scale_fill_manual(values = c("#440154","#21918c"), name = "Season") +
#   ## add rug plot based on original data
#   geom_rug(data=pack_season_effect$data,aes(y=NULL),sides="b") +
#   theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 20, hjust = .98, vjust = -8),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20),
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20),
#     legend.key = element_blank())
 
 

 
wolf_site_effect <- Effect(c('StudySite','wolf_density'),xlevels = 100,patchtime)
wolf_site_df <- as.data.frame(wolf_site_effect)

wolfplot <- ggplot()+
  geom_line(data = wolf_site_df[wolf_site_df$StudySite =='RMNP' & wolf_site_df$wolf_density >1.9,], aes(wolf_density,fit), colour = "#21918c") +
  geom_line(data = wolf_site_df[wolf_site_df$StudySite =='SEMB' & wolf_site_df$wolf_density <=2,], aes(wolf_density,fit,), colour = "#440154") +
  geom_ribbon(data = wolf_site_df[wolf_site_df$StudySite =='RMNP'& wolf_site_df$wolf_density >1.9,], aes(wolf_density, fit, ymin=lower,ymax=upper),fill="#21918c",alpha=0.1) +
  geom_ribbon(data = wolf_site_df[wolf_site_df$StudySite =='SEMB'& wolf_site_df$wolf_density <=2,], aes(wolf_density,fit,ymin=lower,ymax=upper),fill="#440154",alpha=0.1) +
  geom_rug(data=wolf_site_effect$data,aes(wolf_density, y=NULL, colour = StudySite),sides="b") +
  scale_colour_manual(values = c("#21918c","#440154"), name = "StudySite") +
  xlab("Wolf Density (per 10km2)") +
  ylab("log Cluster Duration (hours)") +
  ggtitle("d.")+
  ## add rug plot based on original data
  geom_rug(data=pack_site_effect$data,aes(y=NULL),sides="b") +
  theme_bw()  + 
  theme(legend.position = "none") +
  theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.05, vjust = -8),
    axis.line = element_line(colour = "black", linewidth = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20),
    legend.key = element_blank())

wolfplot

moose_site_effect <- Effect(c('StudySite','moose_density_averaged'),xlevels = 20, patchtime) 
moose_site_df <- as.data.frame(moose_site_effect)

mooseplot <- ggplot() + 
  geom_line(data = moose_site_df[moose_site_df$StudySite =='RMNP' & moose_site_df$moose_density_averaged >.5,], aes(moose_density_averaged,fit), colour = "#21918c") +
  geom_line(data = moose_site_df[moose_site_df$StudySite =='SEMB' & moose_site_df$moose_density_averaged <=.4,], aes(moose_density_averaged,fit,), colour = "#440154") +
  geom_ribbon(data = moose_site_df[moose_site_df$StudySite =='RMNP'& moose_site_df$moose_density_averaged >.5,], aes(moose_density_averaged, fit, ymin=lower,ymax=upper),fill="#21918c",alpha=0.1) +
  geom_ribbon(data = moose_site_df[moose_site_df$StudySite =='SEMB'& moose_site_df$moose_density_averaged <=.4,], aes(moose_density_averaged,fit,ymin=lower,ymax=upper),fill="#440154",alpha=0.1) +
  geom_rug(data=moose_site_effect$data,aes(moose_density_averaged, y=NULL, colour = StudySite),sides="b") +
  scale_colour_manual(values = c("#21918c","#440154"), name = "StudySite") +
  xlab("Moose Density (km^2)") +
  ylab("log Cluster Duration (hours)") + 
  ggtitle("c.")+
  theme_bw()  + 
  theme(legend.position = "none") +
  theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.05, vjust = -8),
    axis.line = element_line(colour = "black", linewidth =  = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20),
    legend.key = element_blank())

mooseplot 

lc_site_effect <- Effect(c('StudySite','decmix'),xlevels = 20, patchtime) 
lc_site_df <- as.data.frame(lc_site_effect)

lcplot <- ggplot() + 
  geom_line(data = lc_site_df[lc_site_df$StudySite =='RMNP' & lc_site_df$decmix >.5,], aes(decmix,fit), colour = "#21918c") +
  geom_line(data = lc_site_df[lc_site_df$StudySite =='SEMB' & lc_site_df$decmix >.1,], aes(decmix,fit,), colour = "#440154") +
  geom_ribbon(data = lc_site_df[lc_site_df$StudySite =='RMNP'& lc_site_df$decmix >.5,], aes(decmix, fit, ymin=lower,ymax=upper),fill="#21918c",alpha=0.1) +
  geom_ribbon(data = lc_site_df[lc_site_df$StudySite =='SEMB'& lc_site_df$decmix >.1,], aes(decmix,fit,ymin=lower,ymax=upper),fill="#440154",alpha=0.1) +
  geom_rug(data=lc_site_effect$data,aes(decmix, y=NULL, colour = StudySite),sides="b") +
  scale_colour_manual(values = c("#21918c","#440154"), name = "StudySite") +
  xlab("Proportion Deciduous/Mixed Forest") +
  ylab("log Cluster Duration (hours)") + 
  ggtitle("f.")+
  theme_bw()  + 
  theme(legend.position = "none") +
  theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.05, vjust = -8),
    axis.line = element_line(colour = "black",linewidth = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20),
    legend.key = element_blank())

lcplot 

png('results/patchtime_interactions.png', width = 15000, height = 18000, res=1000, units="px")

grid.arrange(weightplot,packplot,mooseplot,wolfplot,lfplot,lcplot, ncol = 2)


dev.off()



simple_revisit <- lmer((Site_revisit) ~ 0+ log(Avg_Weight)+ moose_density_averaged+ lf_dens + pack_size + wolf_density + decmix + Season + StudySite + (1|PackID/CollarID) , data=kill_covs2)
logLik(simple_revisit)


# revisit1<- lmer((Site_revisit) ~  Season + StudySite + (1|PackID/CollarID) , data=kill_covs2)
# logLik(revisit1) ###'log Lik.' -2045.006 (df=6)

# revisit2 <- lmer((Site_revisit) ~  log(Avg_Weight) + log(moose_density_averaged+0.0001) + lf_dens + pack_size + wolf_density  + (1|PackID/CollarID) , data=kill_covs2)
# logLik(revisit2) ###'log Lik.' -1998.464 (df=9)

revisit <- lmer((Site_revisit) ~  0 + log(Avg_Weight):StudySite + moose_density_averaged:StudySite + lf_dens:StudySite + wolf_density:StudySite + pack_size:StudySite  + decmix:StudySite + Season:StudySite+  StudySite + (1|PackID/CollarID) , data=kill_covs2)
logLik(revisit)

revisit_mod <- broom.mixed::tidy(revisit, conf.int=TRUE)
revisit_mod

write.csv(revisit_mod, "results/revisit_results.csv")


simple_revisit_mod <- broom.mixed::tidy(simple_revisit, conf.int=TRUE)
simple_revisit_mod

write.csv(simple_revisit_mod, "results/simple_revisit_results.csv")

# weight_site_reveff <- Effect(c('StudySite','Avg_Weight'),revisit) 
# 
# ggplot(as.data.frame(weight_site_reveff),
#        aes(Avg_Weight,fit, colour=StudySite, fill = StudySite))+
#   geom_line()+
#   ## colour=NA suppresses edges of the ribbon
#   geom_ribbon(colour=NA,alpha=0.1,
#               aes(ymin=lower,ymax=upper))+
#   ## add rug plot based on original data
#   geom_rug(data=weight_site_reveff$data,aes(y=NULL),sides="b") +
#   scale_colour_manual(values = c("#21918c","#440154"), name = "Study Site") +
#   scale_fill_manual(values = c("#21918c","#440154"), name = "Study Site") +
#   theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 20, hjust = .98, vjust = -8),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20),
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20),
#     legend.key = element_blank())
# 
# weight_season_reveff <- Effect(c('Season','Avg_Weight'),revisit) 
# 
# ggplot(as.data.frame(weight_season_reveff),
#        aes(Avg_Weight,fit, colour=Season, fill = Season))+
#   geom_line()+
#   ## colour=NA suppresses edges of the ribbon
#   geom_ribbon(colour=NA,alpha=0.1,
#               aes(ymin=lower,ymax=upper))+
#   ## add rug plot based on original data
#   geom_rug(data=weight_season_reveff$data,aes(y=NULL),sides="b") +
#   scale_colour_manual(values = c("#440154","#21918c"), name = "Season") +
#   scale_fill_manual(values = c("#440154","#21918c"), name = "Season") +
#   theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 20, hjust = .98, vjust = -8),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20),
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20),
#     legend.key = element_blank())
# 
# lf_site_reveff <- Effect(c('StudySite','lf_dens'),revisit) 
# 
# ggplot(as.data.frame(lf_site_reveff),
#        aes(lf_dens,fit, colour=StudySite, fill = StudySite))+
#   geom_line()+
#   ## colour=NA suppresses edges of the ribbon
#   geom_ribbon(colour=NA,alpha=0.1,
#               aes(ymin=lower,ymax=upper))+
#   ## add rug plot based on original data
#   geom_rug(data=lf_site_reveff$data,aes(y=NULL),sides="b") +
#   scale_colour_manual(values = c("#21918c","#440154"), name = "Study Site") +
#   scale_fill_manual(values = c("#21918c","#440154"), name = "Study Site") +
#   theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 20, hjust = .98, vjust = -8),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20),
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20),
#     legend.key = element_blank())
# 
# lf_season_reveff <- Effect(c('Season','lf_dens'),revisit) 
# 
# ggplot(as.data.frame(lf_season_reveff),
#        aes(lf_dens,fit, colour=Season, fill = Season))+
#   geom_line()+
#   ## colour=NA suppresses edges of the ribbon
#   geom_ribbon(colour=NA,alpha=0.1,
#               aes(ymin=lower,ymax=upper))+
#   ## add rug plot based on original data
#   geom_rug(data=lf_season_reveff$data,aes(y=NULL),sides="b") +
#   scale_colour_manual(values = c("#440154","#21918c"), name = "Season") +
#   scale_fill_manual(values = c("#440154","#21918c"), name = "Season") +
#   theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 20, hjust = .98, vjust = -8),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20),
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20),
#     legend.key = element_blank())
# 
# 
# pack_site_reveff <- Effect(c('StudySite','pack_size'),revisit) 
# 
# ggplot(as.data.frame(pack_site_reveff),
#        aes(pack_size, fit, colour=StudySite, fill = StudySite))+
#   geom_line()+
#   ## colour=NA suppresses edges of the ribbon
#   geom_ribbon(colour=NA,alpha=0.1,
#               aes(ymin=lower,ymax=upper))+
#   ## add rug plot based on original data
#   geom_rug(data=pack_site_reveff$data,aes(y=NULL),sides="b") +
#   scale_colour_manual(values = c("#21918c","#440154"), name = "Study Site") +
#   scale_fill_manual(values = c("#21918c","#440154"), name = "Study Site") +
#   theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 20, hjust = .98, vjust = -8),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20),
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20),
#     legend.key = element_blank()) 
# 
# pack_season_reveff <- Effect(c('Season','pack_size'),revisit) 
# 
# ggplot(as.data.frame(pack_season_reveff),
#        aes(pack_size,fit, colour=Season, fill = Season))+
#   geom_line()+
#   ## colour=NA suppresses edges of the ribbon
#   geom_ribbon(colour=NA,alpha=0.1,
#               aes(ymin=lower,ymax=upper))+
#   ## add rug plot based on original data
#   geom_rug(data=pack_season_reveff$data,aes(y=NULL),sides="b") + 
#   scale_colour_manual(values = c("#440154","#21918c"), name = "Season") +
#   scale_fill_manual(values = c("#440154","#21918c"), name = "Season") +
#   theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 20, hjust = .98, vjust = -8),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20),
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20),
#     legend.key = element_blank())
# 
# 
# 
# wolf_season_reveff <- Effect(c('Season','wolf_density'),revisit) 
# 
# ggplot(as.data.frame(wolf_season_reveff),
#        aes(wolf_density,fit, colour=Season, fill = Season))+
#   geom_line()+
#   ## colour=NA suppresses edges of the ribbon
#   geom_ribbon(colour=NA,alpha=0.1,
#               aes(ymin=lower,ymax=upper))+
#   ## add rug plot based on original data
#   geom_rug(data=pack_season_reveff$data,aes(y=NULL),sides="b") + 
#   scale_colour_manual(values = c("#440154","#21918c"), name = "Season") +
#   scale_fill_manual(values = c("#440154","#21918c"), name = "Season") +
#   theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(size = 20, hjust = .98, vjust = -8),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20),
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20),
#     legend.key = element_blank())
