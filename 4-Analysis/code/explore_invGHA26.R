#########################################
###Investigated Site Data Exploration
####EASTERN MB (GHA 26)
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
datgha26 <- fread('data/2023-08-01_GHA_K_All_Clu.csv')  ###11237 obs

summary(datgha26$Act_fixes) ###max Actual fixes 260
##clusters less than 265 actual fixes
##removes inv match only as well because there are no clusters associated
datgha26 <- datgha26[Act_fixes <= 265] ###11804 obs




### clusters matched with investigated sites
inv_datgha26 <-datgha26[JoinStatus == 'Matched' & finalIgnore == 0]
inv_datgha26$Behaviour_1 <- as.factor(inv_datgha26$Behaviour_1)
summary(inv_datgha26$Behaviour_1)



#####Morts not included in GHA data


###create prey categories for kill behavours
###clean up behaviours for plotting
inv_datgha26[,'Behav':= as.character('Other')]
inv_datgha26$Behav[inv_datgha26$Behaviour_1 =="Kill" & inv_datgha26$Calf == 1] <-'Calf Kill'
inv_datgha26$Behav[inv_datgha26$Behaviour_1 =="Kill" & inv_datgha26$Moose == 1 & inv_datgha26$Calf == 0] <- 'Moose Kill'
inv_datgha26$Behav[inv_datgha26$Behaviour_1 =="Kill" & inv_datgha26$Elk == 1 & inv_datgha26$Calf == 0] <- 'Elk Kill'
inv_datgha26$Behav[inv_datgha26$Behaviour_1 =="Kill" & inv_datgha26$WTD == 1& inv_datgha26$Calf == 0] <- 'Deer Kill'
inv_datgha26$Behav[inv_datgha26$Behaviour_1 =="Kill" & inv_datgha26$Beaver == 1] <- 'Beaver Kill'
inv_datgha26$Behav[inv_datgha26$Behaviour_1 =="Probable kill"] <-"Probable Kill"
inv_datgha26$Behav[inv_datgha26$Behaviour_1  =="Revisit"]<-"Revisit"
inv_datgha26$Behav[inv_datgha26$Behaviour_1  =="Scavenging"]<-"Scavenge" ###is scavenging for GHA 26
inv_datgha26$Behav[inv_datgha26$Behaviour_1  =="Resting"]<-"Resting"
inv_datgha26$Behav[inv_datgha26$Behaviour_1  =="Probable resting"]<-"Resting"
inv_datgha26$Behav[inv_datgha26$Behaviour_1  =="Den"]<-"Den"
inv_datgha26$Behav[inv_datgha26$Behaviour_1  =="Rendez-vous"]<-"Rendez-vous"



####specify levels 
inv_datgha26$Behav <- as.factor(inv_datgha26$Behav)
summary(inv_datgha26$Behav)

inv_datgha26$Behav <- factor(inv_datgha26$Behav , levels = c("Other", 
                                                         "Den", "Rendez-vous",
                                                         "Resting",
                                                         "Scavenge", "Revisit",
                                                         "Probable Kill", "Beaver Kill", "Calf Kill",  
                                                         "Deer Kill", "Elk Kill","Moose Kill"))


### clusters that are kills
kclu_datgha26 <- inv_datgha26[Behaviour_1 == "Kill"]
### unique kill sites (remove multiple clusters and wolves) 
ksite_datgha26 <- kclu_datgha26[mwKfirst == 1]


###calculate mean investigation time for sites
####
summary(as.factor(inv_datgha26$Behaviour_1))

sum_inv26 <- inv_datgha26[ , .(days.med = median(daysEarly),
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
sum_inv26

write.csv(sum_inv26,"results/gha26_inv.csv")


summary(as.factor(inv_datgha26$Behav))

sum_inv26_2<- inv_datgha26[ , .(days.med = median(daysEarly),
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
sum_inv26_2

write.csv(sum_inv26_2,"results/gha26_inv2.csv")

####


dat26large <- inv_datgha26[Act_fixes >= 6] 

largeclu26 <- dat26large[ , .(fix.mean=mean(Act_fixes), 
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

head(largeclu26)

write.csv(largeclurm ,"results/largeclu_gha26_inv1.csv")

################now with behaviour

dat26large <- inv_datgha26[Act_fixes >= 6] 

largeclu26 <- dat26large[ , .(fix.mean=mean(Act_fixes), 
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

head(largeclu26)

write.csv(largeclurm ,"results/largeclu_gha26_inv5pt.csv")

##### prey kills

dat26large <- inv_datgha26[Act_fixes >= 8] 

largeclu26 <- dat26large[ , .(fix.mean=mean(Act_fixes), 
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

head(largeclu26)

write.csv(largeclurm ,"results/largeclu_gha26_inv.csv")

####### investigated clusters #######
####plot behaviour barplot for investigated clusters

########need to order by count

p_beclu26 <- ggplot(sum_inv26,aes( x = reorder(Behaviour_1, -count), y = count)) + geom_col()
p_beclu26

####time by behaviour

p_timeinv26 <- ggplot(inv_datgha26,aes(x = reorder(Behav, -daysEarly), y = daysEarly)) 

p_timeinv26 +  geom_boxplot() + coord_flip()

# ridge_timeinv <- 
#   ggplot(inv_datgha26, aes(daysEarly, fct_rev(Behav), color = Behav, fill = Behav)) + 
#   coord_cartesian(clip = "off") +
#   scale_y_discrete(expand = c(.07, .07))  
# 
# ridge_timeinv +   ggridges::stat_density_ridges(
#   quantile_lines = TRUE, quantiles = 2, 
#   color = "black", alpha = .8, size = 1) + xlim(-100,50)

 


##plot fix number by behaviour 
inv_datgha26$title <- "E. GHA 26"

p_behavfix26 <- ggplot(inv_datgha26, aes(x = Behav, y = Act_fixes)) +
  geom_boxplot(outlier.shape = NA)  +
  geom_jitter( colour = "#5ec962", position=position_jitter(0.2), alpha = 0.2) +
  coord_flip() + #ylim(0,100) +
  geom_vline(xintercept = 1.5) + 
  geom_vline(xintercept = 3.5) +
  geom_vline(xintercept = 4.5) +
  annotate(geom="text", x=5, y=200, label="Energy Acquisition", alpha = .7, size = 6) +
  annotate(geom="text", x=4, y=200, label="Energy Conservation",alpha = .7, size = 6) +
  annotate(geom="text", x=2.5, y=200, label="Reproduction", alpha = .7, size = 6) +
xlab("Behaviors") + ylab("Number of locations") +
  facet_grid(. ~ title)+ 
  theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
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
p_behavfix26

png('results/behaviourvfixes26.png', width = 10000, height = 7000, res=1000, units="px")

p_behavfix26

dev.off()


###add scatter

# p_behavfix + geom_violin() + coord_flip()

# p_behavfix + ggdist::stat_halfeye(
#   aes(fill = Behaviour_1, fill = after_scale(colorspace::lighten(fill, .7)))
# )

# ridge_behavfix <- 
#   ggplot(inv_datgha26, aes(Act_fixes, fct_rev(Behav), color = Behav, fill = Behav)) + 
#   coord_cartesian(clip = "off") +
#   scale_y_discrete(expand = c(.07, .07))  
# 
# ridge_behavfix +   ggridges::stat_density_ridges(
#   quantile_lines = TRUE, quantiles = 2, 
#   color = "black", alpha = .8, size = 1) + 
#   scale_fill_viridis(discrete = TRUE)


##plot radius size by behaviour


p_behavrad26 <- ggplot(inv_datgha26,aes(x = Behav, y = Clus_rad_m)) +
  geom_boxplot(outlier.shape = NA) + coord_flip() +  
  geom_jitter(position=position_jitter(0.2), alpha = 0.2) +
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
p_behavrad26


png('results/behaviour_rad26.png', width = 12000, height = 10000, res=1200, units="px")

p_behavrad26

dev.off()


# ridge_behavrad <- 
#   ggplot(inv_datgha26, aes(Clus_rad_m, fct_rev(Behav))) + 
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


p_behavdur26 <- ggplot(inv_datgha26,aes(x = Behav, y = CluDurHours)) +
  geom_boxplot(outlier.shape = NA ) + coord_flip() +  
  geom_jitter(position=position_jitter(0.2), alpha = 0.2) +
  ggtitle("GHA 26") +
  xlab("Behaviours") + ylab("Cluster Duration (hours)") +
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
p_behavdur26


png('results/behaviour_dur26.png', width = 12000, height = 10000, res=1200, units="px")

p_behavdur26

dev.off()


p_revisit26 <- ggplot(inv_datgha26, aes(x = Behav, y = Site_revisit)) +
  geom_boxplot(outlier.shape = NA)  +
  geom_jitter(position=position_jitter(0.2), alpha = 0.2) +
  coord_flip() + 
  #ylim(0,500) +
  ggtitle("GHA 26") +
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
p_revisit26


png('results/behaviour_revisit26.png', width = 12000, height = 10000, res=1200, units="px")

p_revisit26

dev.off()


p_away26 <- ggplot(inv_datgha26, aes(x = Behav, y = Away_ratio)) +
  geom_boxplot(outlier.shape = NA)  +
  geom_jitter(position=position_jitter(0.2), alpha = 0.2) +
  coord_flip() + 
  #ylim(0,500) +
  ggtitle("GHA 26") +
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
p_away26


png('results/behaviour_away26.png', width = 12000, height = 10000, res=1200, units="px")

p_away26

dev.off()



p_investdays26 <- ggplot(inv_datgha26, aes(x = Behav, y = daysEarly)) +
  geom_boxplot(outlier.shape = NA)  +
  geom_jitter(position=position_jitter(0.2), alpha = 0.2) +
  coord_flip() + 
  #ylim(0,500) +
  ggtitle("GHA 26") +
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
p_investdays26


png('results/investigationtime26.png', width = 12000, height = 10000, res=1200, units="px")

p_investdays26

dev.off()

