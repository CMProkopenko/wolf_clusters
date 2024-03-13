

libs <- c('data.table', 'glmmTMB', 'lme4')
lapply(libs, require, character.only = TRUE)

dietdat <- fread('data/Diet_summary_Seasonal_2023-11-05.csv')  




killdat <- dietdat[Data == 'Site']
scatdat <- dietdat[Data == 'Scat']


dietdat2 <- killdat[scatdat, on = .(Species, Site, Season)]

#dietdat2 <- dietdat2[Perc_Diet >= 5]


dietdat2 <- killdat[scatdat, on = .(Species, Site, Season)]


datrm <- dietdat2[Site == 'RMNP']
dat26 <- dietdat2[Site == 'GHA26']


mod1 <- glm(Perc_Diet ~ Species + Species:Site + Species:Season,
                data = dietdat2)

summary(mod1)

mod2 <-glm(Perc_Diet ~ Species + Species:Season,
           data = datrm)

summary(mod2)

mod3 <-glm(Perc_Diet ~ Species + Species:Season,
           data = dat26)

summary(mod3)
