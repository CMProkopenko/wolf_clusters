#########################################
###Investigated Site Data Exploration
####Riding Mountain
###### Christina Prokopenko
###### Started April 2023
#########################################

###packages
libs <- c('data.table', 'rcartocolor', 
          'ggplot2', 'ggridges', 'ggdist', 'ggbeeswarm','gghalves',
          'ggthemes', 'colorspace', 'viridis', 'patchwork')
lapply(libs, require, character.only = TRUE)

####Load and subset data ####

### load All Sites data after cleaning protocol
datrmp <- fread('data/2023-02-22_RMNP_K_All_Clu.csv')  ###6547 obs
#datrmp <- datrmp[Act_fixes <= 300] ###6433 obs. ##removes inv match only

### clusters matched with investigated sites
inv_datrmnp <-datrmp[JoinStatus == 'Matched' & finalIgnore == 0]
inv_datrmnp$Behaviour_1 <- as.factor(inv_datrmnp$Behaviour_1)
summary(inv_datrmnp$Behaviour_1)

inv_datrmnp = inv_datrmnp[inv_datrmnp$Behaviour_1 != 'MORT' & inv_datrmnp$Behaviour_1 != 'Non collared wolf mort',]

###create prey categories for kill behavours
###clean up behaviours for plotting
inv_datrmnp[,'Behav':= as.character('Other')]
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1 =="Kill" & inv_datrmnp$Calf == 1] <-'Calf Kill'
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1 =="Kill" & inv_datrmnp$Moose == 1 & inv_datrmnp$Calf == 0] <- 'Moose Kill'
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1 =="Kill" & inv_datrmnp$Elk == 1 & inv_datrmnp$Calf == 0] <- 'Elk Kill'
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1 =="Kill" & inv_datrmnp$WTD == 1& inv_datrmnp$Calf == 0] <- 'Deer Kill'
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1 =="Kill" & inv_datrmnp$Beaver == 1] <- 'Beaver Kill'
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1 =="Probable kill"] <-"Probable Kill"
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1 =="Probable prey encounter"]<-"Probable Kill" ###check this is what we want to do
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1  =="Revisit"]<-"Revisit"
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1  =="Scavenge"]<-"Scavenge"
#inv_datrmnp$Behav[inv_datrmnp$Behaviour_1  =="Probable scavenge"]<-"Scavenge" ##check
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1  =="Resting"]<-"Resting"
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1  =="Probable resting"]<-"Resting"
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1  =="Den"]<-"Den"
inv_datrmnp$Behav[inv_datrmnp$Behaviour_1  =="Rendez-vous"]<-"Rendez-vous"



####specify levels 
inv_datrmnp$Behav <- as.factor(inv_datrmnp$Behav)
summary(inv_datrmnp$Behav)

inv_datrmnp$Behav <- factor(inv_datrmnp$Behav , levels = c("Other", 
                                                       "Den", "Rendez-vous",
                                                       "Resting",
                                                       "Scavenge", "Revisit",
                                                       "Probable Kill", "Beaver Kill", "Calf Kill",  
                                                       "Deer Kill", "Elk Kill","Moose Kill"))


### clusters that are kills
kclu_datrmp <- inv_datrmnp[Behaviour_1 == "Kill"]
### unique kill sites (remove multiple clusters and wolves) 
ksite_datrmp <- kclu_datrmp[mwKfirst == 1]

#####calculate stats for investigated clusters

summary(as.factor(inv_datrmnp$Behaviour_1))

sum_invrm <- inv_datrmnp[ , .(days.med = median(daysEarly),
                              days.mean  = mean(daysEarly),
                              days.min = min(daysEarly),
                              days.max = max(daysEarly),
                              fix.med = median(Act_fixes),
                              fix.mean=mean(Act_fixes),
                              fix.min = min(Act_fixes),
                              fix.max=max(Act_fixes),
                              hr.med = median(CluDurHours),
                              hr.mean = mean(CluDurHours),
                              hr.min = min(CluDurHours),
                              hr.max = max(CluDurHours),
                              rad.med = median(Clus_rad_m),
                              rad.mean=mean(Clus_rad_m),
                              rad.min = min(Clus_rad_m),
                              rad.max=max(Clus_rad_m),
                              rev.med = median(Site_revisit),
                              rev.mean = mean(Site_revisit),
                              rev.min = min(Site_revisit),
                              rev.max = max(Site_revisit),
                              away.med = median(Away_ratio),
                              away.mean = mean(Away_ratio),
                              away.min = min(Away_ratio),
                              away.max = max(Away_ratio),
                              count = .N
), by = Behaviour_1]
sum_invrm

write.csv(sum_invrm,"results/rmnp_inv.csv")


###calculate stats investigation time for clusters
####
summary(as.factor(inv_datrmnp$Behav))

sum_invrm_2 <- inv_datrmnp[ , .(days.med = median(daysEarly),
                                days.mean  = mean(daysEarly),
                                days.min = min(daysEarly),
                                days.max = max(daysEarly),
                                fix.med = median(Act_fixes),
                                fix.mean=mean(Act_fixes),
                                fix.min = min(Act_fixes),
                                fix.max=max(Act_fixes),
                                hr.med = median(CluDurHours),
                                hr.mean = mean(CluDurHours),
                                hr.min = min(CluDurHours),
                                hr.max = max(CluDurHours),
                                rad.med = median(Clus_rad_m),
                                rad.mean=mean(Clus_rad_m),
                                rad.min = min(Clus_rad_m),
                                rad.max=max(Clus_rad_m),
                                rev.med = median(Site_revisit),
                                rev.mean = mean(Site_revisit),
                                rev.min = min(Site_revisit),
                                rev.max = max(Site_revisit),
                                away.med = median(Away_ratio),
                                away.mean = mean(Away_ratio),
                                away.min = min(Away_ratio),
                                away.max = max(Away_ratio),
                                count = .N
), by = Behav]
sum_invrm_2

write.csv(sum_invrm_2,"results/rmnp_inv2.csv")

####for large clusters only

datrmnplarge <- inv_datrmnp[Act_fixes >= 6] 

largeclurm <- datrmnplarge[ , .(fix.mean=mean(Act_fixes), 
                                fix.med=median(Act_fixes),
                                fix.min = min(Act_fixes),
                                fix.max=max(Act_fixes),
                                hr.mean = mean(CluDurHours),
                                hr.med = median(CluDurHours),
                                hr.min = min(CluDurHours),
                                hr.max = max(CluDurHours),
                                rad.mean=mean(Clus_rad_m), 
                                rad.med=median(Clus_rad_m),
                                rad.min = min(Clus_rad_m),
                                rad.max=max(Clus_rad_m),
                                count = .N), by = Behaviour_1]

head(largeclurm )

write.csv(largeclurm ,"results/largeclu_rmnp_inv1.csv")

##########

datrmnplarge <- inv_datrmnp[Act_fixes >= 6] 

largeclurm <- datrmnplarge[ , .(fix.mean=mean(Act_fixes), 
                                fix.med=median(Act_fixes),
                                fix.min = min(Act_fixes),
                                fix.max=max(Act_fixes),
                                hr.mean = mean(CluDurHours),
                                hr.med = median(CluDurHours),
                                hr.min = min(CluDurHours),
                                hr.max = max(CluDurHours),
                                rad.mean=mean(Clus_rad_m), 
                                rad.med=median(Clus_rad_m),
                                rad.min = min(Clus_rad_m),
                                rad.max=max(Clus_rad_m),
                                count = .N), by = Behav]

head(largeclurm )

write.csv(largeclurm ,"results/largeclu_rmnp_inv5.csv")


####### investigated clusters #######
####plot behaviour barplot for investigated clusters

########need to order by count

p_beclurm <- ggplot(sum_invrm,aes( x = reorder(Behaviour_1, -count), y = count)) + geom_col()
p_beclurm

####time by behaviour

p_timeinvrm <- ggplot(inv_datrmnp,aes(x = reorder(Behav, -daysEarly), y = daysEarly)) 

p_timeinvrm +  geom_boxplot() + coord_flip()

# ridge_timeinv <- 
#   ggplot(inv_datrmnp, aes(daysEarly, fct_rev(Behav), color = Behav, fill = Behav)) + 
#   coord_cartesian(clip = "off") +
#   scale_y_discrete(expand = c(.07, .07))  
# 
# ridge_timeinv +   ggridges::stat_density_ridges(
#   quantile_lines = TRUE, quantiles = 2, 
#   color = "black", alpha = .8, size = 1) + xlim(-100,50)



##plot fix number by behaviour 
inv_datrmnp$title <- "D. RMNP"

p_behavfixrm <- ggplot(inv_datrmnp, aes(x = Behav, y = Act_fixes)) +
 geom_boxplot(outlier.shape = NA)  +
  geom_jitter( colour = "#5ec962", position=position_jitter(0.2), alpha = 0.2) +
  coord_flip() + #ylim(0,200) +
  geom_vline(xintercept = 1.5) + 
  geom_vline(xintercept = 3.5) +
  geom_vline(xintercept = 4.5) +
  annotate(geom="text", x=5, y=200, label="Energy Acquisition", alpha = .7, size = 6) +
  annotate(geom="text", x=4, y=200, label="Energy Conservation",alpha = .7, size = 6) +
  annotate(geom="text", x=2.5, y=200, label="Reproduction", alpha = .7, size = 6) +
  xlab("Behaviors") + ylab("Number of locations") +
 # ggtitle("D. RMNP") +
  facet_grid(. ~ title)+ 
  theme_bw() +theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
   # panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 20,hjust = 0),
    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20)) 
p_behavfixrm 

png('results/behaviourvfixes.png', width = 10000, height = 7000, res=1000, units="px")

p_behavfixrm

dev.off()




###add scatter

# p_behavfix + geom_violin() + coord_flip()

# p_behavfix + ggdist::stat_halfeye(
#   aes(fill = Behaviour_1, fill = after_scale(colorspace::lighten(fill, .7)))
# )

# ridge_behavfix <- 
#   ggplot(inv_datrmnp, aes(Act_fixes, fct_rev(Behav), color = Behav, fill = Behav)) + 
#   coord_cartesian(clip = "off") +
#   scale_y_discrete(expand = c(.07, .07))  
# 
# ridge_behavfix +   ggridges::stat_density_ridges(
#   quantile_lines = TRUE, quantiles = 2, 
#   color = "black", alpha = .8, size = 1) + 
#   scale_fill_viridis(discrete = TRUE)


##plot radius size by behaviour



p_behavradrm <- ggplot(inv_datrmnp,aes(x = Behav, y = Clus_rad_m)) +
  geom_boxplot(outlier.shape = NA)  +
  geom_jitter( position=position_jitter(0.2), alpha = 0.2) +
  coord_flip() + 
  ggtitle("RMNP") +
  xlab("Behaviours") + ylab("Cluster Radius (m)") +
    theme_bw() +theme_bw()  + theme(
      panel.background =element_rect(colour = "black", fill=NA, size=1),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black", size = .1),
      axis.text.x = element_text(size=20),
      axis.title = element_text(size=20),
      axis.text.y = element_text(size=20),
      legend.title=element_text(size=20),
      legend.text = element_text(size = 20))
p_behavradrm

png('results/behaviour_radrm.png', width = 12000, height = 10000, res=1200, units="px")

p_behavradrm

dev.off()


# ridge_behavrad <- 
#   ggplot(inv_datrmnp, aes(Clus_rad_m, fct_rev(Behav))) + 
#   coord_cartesian(clip = "off") +
#   scale_y_discrete(expand = c(.07, .07))  
# 
# ridge_behavrad +   ggridges::stat_density_ridges(
#   quantile_lines = TRUE, quantiles = 2, 
#   color = "black", alpha = .8, size = 1) + 
#   scale_fill_viridis(discrete = TRUE) + 
#   ylab("Behaviours") + xlab("Cluster Radius (m)") +
#   theme_bw() +theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(), 
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20), 
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20)) 


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

###duration
# p_behavdurrm <- ggplot(inv_datrmnp,aes(x = Behav, y = Total_hours)) +
#   geom_boxplot(outlier.shape = NA)  +
#   geom_jitter( colour = "#5ec962", position=position_jitter(0.2), alpha = 0.2) +
#   coord_flip() + 
#  #ylim(0,500) +
#   xlab("Behaviours") + ylab("Duration (hours)") +
#   theme_bw() +theme_bw()  + theme(
#     panel.background =element_rect(colour = "black", fill=NA, size=1),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black", size = .1),
#     axis.text.x = element_text(size=20),
#     axis.title = element_text(size=20),
#     axis.text.y = element_text(size=20),
#     legend.title=element_text(size=20),
#     legend.text = element_text(size = 20))
# p_behavdurrm


# p_behavs_tvr <- ggplot(inv_datrmnp,aes(x = Clus_rad_m, y = Total_hours)) +
#   geom_point(aes(col = Behav))


p_behavdurrm <- ggplot(inv_datrmnp,aes(x = Behav, y = CluDurHours)) +
  geom_boxplot(outlier.shape = NA)  +
  geom_jitter(position=position_jitter(0.2), alpha = 0.2) +
  coord_flip() + 
  #ylim(0,500) +
  ggtitle("RMNP") +
  xlab("Behaviours") + ylab("Duration (hours)") +
  theme_bw() +theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20))
p_behavdurrm



png('results/behaviour_durrm.png', width = 12000, height = 10000, res=1200, units="px")

p_behavdurrm

dev.off()

p_revisitrm <- ggplot(inv_datrmnp, aes(x = Behav, y = Site_revisit)) +
  geom_boxplot(outlier.shape = NA)  +
  geom_jitter(position=position_jitter(0.2), alpha = 0.2) +
  coord_flip() + 
  #ylim(0,500) +
  ggtitle("RMNP") +
  xlab("Behaviours") + ylab("Revisit") +
  theme_bw() +theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20))
p_revisitrm


png('results/behaviour_revisitrm.png', width = 12000, height = 10000, res=1200, units="px")

p_revisitrm

dev.off()

p_awayrm <- ggplot(inv_datrmnp, aes(x = Behav, y = Away_ratio)) +
  geom_boxplot(outlier.shape = NA)  +
  geom_jitter(position=position_jitter(0.2), alpha = 0.2) +
  coord_flip() + 
  #ylim(0,500) +
  ggtitle("RMNP") +
  xlab("Behaviours") + ylab("Away Ratio") +
  theme_bw() +theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20))
p_awayrm


png('results/behaviour_awayrm.png', width = 12000, height = 10000, res=1200, units="px")

p_awayrm

dev.off()



p_investdaysrm <- ggplot(inv_datrmnp, aes(x = Behav, y = daysEarly)) +
  geom_boxplot(outlier.shape = NA)  +
  geom_jitter(position=position_jitter(0.2), alpha = 0.2) +
  coord_flip() + 
  #ylim(0,500) +
 # ggtitle("RMNP") +
  #facet_grid(. ~ title)+ 
  xlab("Behaviours") + ylab("Investigation Time (days)") +
  theme_bw() +theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20))
p_investdaysrm


png('results/investigationtimerm.png', width = 12000, height = 10000, res=1200, units="px")

p_investdaysrm

dev.off()



