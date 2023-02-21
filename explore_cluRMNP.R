
####
###cluster data exploration

###packages
libs <- c('data.table', 'dplyr', 'tidyr', 'tidyverse', 'rcartocolor', 
          'ggplot2', 'ggridges', 'ggdist', 'ggbeeswarm','gghalves',
          'ggthemes', 'colorspace', 'viridis', 'patchwork')
lapply(libs, require, character.only = TRUE)

####Load and subset data ####

### load All Sites data after cleaning protocol
datrm <- fread('data/2021-09-17_RMNP_All_Sites.csv')


### clusters matched with investigated sites
inv_datrm <-subset(datrm, JoinStatus == 'Matched' & finalIgnore == 0)
head(inv_datrm)

### unique investigated sites (remove multi wolves)
####Do not have a column ID for this yet

### clusters that are kills

kclu_datrm <-subset(inv_datrm, Behaviour_1 == "Kill")

### unique kill sites (remove multiple clusters and wolves) 

ksite_datrm <- subset(kclu_datrm, mwKfirst == 1)

#### calculate mean actual fixes and radius for clusters

sum_clu <- summarise(group_by(datrm, JoinStatus), 
                   fix.mean=mean(Act_fixes), 
                   fix.min = min(Act_fixes),
                   fix.max=max(Act_fixes),
                   rad.mean=mean(Clus_rad_m), 
                   rad.min = min(Clus_rad_m),
                   rad.max=max(Clus_rad_m))

head(sum_clu)

####### all clusters #######
###plot size histogram for matched and not investigated clusters

p_clu <- ggplot(datrm) +
  geom_histogram(aes(x= Act_fixes, colour = JoinStatus), 
                 fill = "white", alpha = 0.2, position = "dodge") +
  geom_vline(data = sum_clu, aes(xintercept = fix.mean, colour = JoinStatus),
             linetype="dashed")
  #+ xlim(0,100)

p_clu

p_clu_rad <- ggplot(datrm) +
  geom_histogram(aes(x= Clus_rad_m, colour = JoinStatus), 
                 fill = "white", alpha = 0.2, position = "dodge") +
  geom_vline(data = sum_clu, aes(xintercept = rad.mean, colour = JoinStatus),
             linetype="dashed")
#+ xlim(0,100)

p_clu_rad



####### investigated clusters #######
####plot behaviour barplot for clusters

p_beclu <- ggplot(inv_datrm) + geom_bar(aes(x = Behaviour_1))

p_beclu

##plot fix number by behaviour 

p_behavfix <- ggplot(inv_datrm,aes(x = reorder(Behaviour_1, -Act_fixes), y = Act_fixes)) 

p_behavfix +  geom_boxplot() + coord_flip()

p_behavfix + geom_violin() + coord_flip()

p_behavfix + ggdist::stat_halfeye(
  aes(fill = Behaviour_1, fill = after_scale(colorspace::lighten(fill, .7)))
  )

ridge_behavfix <- 
  ggplot(inv_datrm, aes(Act_fixes, fct_rev(Behaviour_1), color = Behaviour_1, fill = Behaviour_1)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07))  

ridge_behavfix +   ggridges::stat_density_ridges(
  quantile_lines = TRUE, quantiles = 2, 
  color = "black", alpha = .8, size = 1) 


##plot radius size by behaviour

p_behavrad <- ggplot(inv_datrm,aes(x = reorder(Behaviour_1, -Clus_rad_m), y = Clus_rad_m)) 

p_behavrad +  geom_boxplot() + coord_flip()

p_behavrad + ggdist::stat_halfeye(aes(fill = Behaviour_1, fill = after_scale(colorspace::lighten(fill, .7))))

ridge_behavrad <- 
  ggplot(inv_datrm, aes(Clus_rad_m, fct_rev(Behaviour_1), color = Behaviour_1, fill = Behaviour_1)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07))  

ridge_behavrad +   ggridges::stat_density_ridges(
  quantile_lines = TRUE, quantiles = 2, 
  color = "black", alpha = .8, size = 1) 

ridge_behavrad + 
  ggridges::stat_density_ridges(
    aes(fill = factor(stat(quantile))),
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975),
    color = "black", size = 1.5
  ) +
  scale_fill_manual(
    name = "Probability:", values = c("lightblue", "grey70", "#003366"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  ) +
  guides(fill = guide_legend(override.aes = list(color = "transparent")))

####### kill clusters #######
###plot prey histogram for unique kills by prey

p_kclu <- ggplot(kclu_datrm) + geom_bar(aes(x = Prey_1))
p_kclu


###plot prey histogram for unique kills by prey

p_ksite <- ggplot(ksite_datrm) + geom_bar(aes(x = Prey_1))
p_ksite

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


