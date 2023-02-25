####
###investigated site data exploration

###packages
libs <- c('data.table', 'rcartocolor', 
          'ggplot2', 'ggridges', 'ggdist', 'ggbeeswarm','gghalves',
          'ggthemes', 'colorspace', 'viridis', 'patchwork')
lapply(libs, require, character.only = TRUE)

####Load and subset data ####

### load All Sites data after cleaning protocol
datrm <- fread('data/2021-09-17_RMNP_All_Sites.csv')  ###6547 obs
datrm <- datrm[Act_fixes <= 150] ###6433 obs. ##removes inv match only

### clusters matched with investigated sites
inv_datrm <-datrm[JoinStatus == 'Matched' & finalIgnore == 0]
inv_datrm$Behaviour_1 <- as.factor(inv_datrm$Behaviour_1)
summary(inv_datrm$Behaviour_1)

inv_datrm = inv_datrm[inv_datrm$Behaviour_1 != 'MORT' & inv_datrm$Behaviour_1 != 'Non collared wolf mort',]

###create prey categories for kill behavours
###clean up behaviours for plotting
inv_datrm[,'Behav':= as.factor('Other')]
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Kill" & inv_datrm$Calf == 1] <- 'Calf Kill'
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Kill" & inv_datrm$Moose == 1 & inv_datrm$Calf == 0] <- 'Moose Kill'
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Kill" & inv_datrm$Elk == 1 & inv_datrm$Calf == 0] <- 'Elk Kill'
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Kill" & inv_datrm$Moose == 1 & inv_datrm$WTD == 0] <- 'Deer Kill'
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Kill" & inv_datrm$Beaver == 1] <- 'Beaver Kill'
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Probable kill"]<-"Probable kill"
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Probable prey encounter"]<-"Probable kill" ###check this is what we want to do
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Revisit"]<-"Revisit"
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Scavenge"]<-"Scavenge"
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Resting"]<-"Resting"
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Probable resting"]<-"Resting"
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Den"]<-"Den"
inv_datrm$Behav[inv_datrm$Behaviour_1  =="Rendez-vous"]<-"Rendez-vous"

summary(as.factor(inv_datrm$Behav))

### clusters that are kills
kclu_datrm <- inv_datrm[Behaviour_1 == "Kill"]
### unique kill sites (remove multiple clusters and wolves) 
ksite_datrm <- kclu_datrm[mwKfirst == 1]


###calculate mean investigation time for sites
####
summary(as.factor(inv_datrm$Behaviour_1))

sum_inv <- inv_datrm[ , .(days.mean  = mean(daysEarly),
                          days.min = min(daysEarly),
                          days.max = max(daysEarly),
                          fix.mean=mean(Act_fixes),
                          fix.min = min(Act_fixes),
                          fix.max=max(Act_fixes),
                          rad.mean=mean(Clus_rad_m),
                          rad.min = min(Clus_rad_m),
                          rad.max=max(Clus_rad_m),
                          count = .N
), by = Behaviour_1]
sum_inv


####### investigated clusters #######
####plot behaviour barplot for investigated clusters

########need to order by count

p_beclu <- ggplot(sum_inv,aes( x = reorder(Behaviour_1, -count), y = count)) + geom_col()
p_beclu

####time by behaviour

p_timeinv <- ggplot(inv_datrm,aes(x = reorder(Behav, -daysEarly), y = daysEarly)) 

p_timeinv +  geom_boxplot() + coord_flip()

ridge_timeinv <- 
  ggplot(inv_datrm, aes(daysEarly, fct_rev(Behav), color = Behav, fill = Behav)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07))  

ridge_timeinv +   ggridges::stat_density_ridges(
  quantile_lines = TRUE, quantiles = 2, 
  color = "black", alpha = .8, size = 1) + xlim(-100,50)



##plot fix number by behaviour 
p_behavfix <- ggplot(inv_datrm,aes(x = reorder(Behav, -Act_fixes), y = Act_fixes)) 

p_behavfix +  geom_boxplot() + coord_flip()

# p_behavfix + geom_violin() + coord_flip()

# p_behavfix + ggdist::stat_halfeye(
#   aes(fill = Behaviour_1, fill = after_scale(colorspace::lighten(fill, .7)))
# )

ridge_behavfix <- 
  ggplot(inv_datrm, aes(Act_fixes, fct_rev(Behav), color = Behav, fill = Behav)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07))  

ridge_behavfix +   ggridges::stat_density_ridges(
  quantile_lines = TRUE, quantiles = 2, 
  color = "black", alpha = .8, size = 1) 


##plot radius size by behaviour

p_behavrad <- ggplot(inv_datrm,aes(x = reorder(Behav, -Clus_rad_m), y = Clus_rad_m)) 

p_behavrad +  geom_boxplot() + coord_flip()

ridge_behavrad <- 
  ggplot(inv_datrm, aes(Clus_rad_m, fct_rev(Behav), color = Behav, fill = Behaviour_1)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07))  

ridge_behavrad +   ggridges::stat_density_ridges(
  quantile_lines = TRUE, quantiles = 2, 
  color = "black", alpha = .8, size = 1) 

# ridge_behavrad + 
#   ggridges::stat_density_ridges(
#     aes(fill = factor(stat(quantile))),
#     geom = "density_ridges_gradient", calc_ecdf = TRUE,
#     quantiles = c(0.025, 0.975),
#     color = "black", size = 1.5
#   ) +
#   scale_fill_manual(
#     name = "Probability:", values = c("lightblue", "grey70", "#003366"),
#     labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
#   ) +
#   guides(fill = guide_legend(override.aes = list(color = "transparent")))

####### kill clusters only #######

###plot fixes for prey kills

ridge_killfix <- 
  ggplot(kclu_datrm, aes(Act_fixes, fct_rev(Prey_1), color = Prey_1, fill = Prey_1)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07))  

ridge_killfix +   ggridges::stat_density_ridges(
  quantile_lines = TRUE, quantiles = 2, 
  color = "black", alpha = .8, size = 1) 


###plot radius size for prey kills

ridge_killrad <- 
  ggplot(kclu_datrm, aes(Clus_rad_m, fct_rev(Prey_1), color = Prey_1, fill = Prey_1)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07))  

ridge_killrad +   ggridges::stat_density_ridges(
  quantile_lines = TRUE, quantiles = 2, 
  color = "black", alpha = .8, size = 1) 

