
####
###cluster data exploration

###packages
libs <- c('data.table', 'rcartocolor', 
          'ggplot2', 'ggridges', 'ggdist', 'ggbeeswarm','gghalves',
          'ggthemes', 'colorspace', 'viridis', 'patchwork')
lapply(libs, require, character.only = TRUE)

####Load and subset data ####

### load All Sites data after cleaning protocol
datrm <- fread('data/2021-09-17_RMNP_All_Sites.csv')  ###6547 obs
datrm <- datrm[Act_fixes <= 150] ###6433 obs. ##removes inv match only



### unique investigated sites (remove multi wolves)
####Do not have a column ID for this yet
### calculate mean actual fixes and radius for clusters

summary(as.factor(datrm$JoinStatus))

sum_clu <- datrm[ , .(fix.mean=mean(Act_fixes), 
                   fix.min = min(Act_fixes),
                   fix.max=max(Act_fixes),
                   rad.mean=mean(Clus_rad_m), 
                   rad.min = min(Clus_rad_m),
                   rad.max=max(Clus_rad_m),
                   count = .N), by = JoinStatus]

head(sum_clu)


### proportion of clusters investigated by size
catfix <- datrm 
catfix$Act_fixes <- as.factor(catfix$Act_fixes)
catfix$JoinStatus <- as.factor(catfix$JoinStatus)

fixcount <- catfix[,.(count = .N), by = c('Act_fixes', 'JoinStatus')]
cluct <- fixcount[JoinStatus == 'CLUonly']
invct <-fixcount[JoinStatus == 'Matched']
allct <- invct[cluct, on = 'Act_fixes']

allct[is.na(allct$count), ] <- 0   

propinv <- allct[, "propInv" := (count/(count+i.count))]
propinv <-propinv[, "total" := (count+i.count)]

propinv$Act_fixes <-  as.numeric(as.character(propinv$Act_fixes))


####### all clusters #######
###plot size histogram for matched and not investigated clusters

p_clu <- ggplot() +
  geom_histogram(data = datrm, aes(x= Act_fixes, colour = JoinStatus), 
                 fill = "white", alpha = 0.2, position = "dodge") +
  geom_vline(data = sum_clu, aes(xintercept = fix.mean, colour = JoinStatus),
             linetype="dashed") 

p_clu


####proportion investigated by fixes
# 
# p_total <- ggplot(propinv, aes(x = propinv$Act_fixes, y = total)) +
#   geom_col()+ geom_hline(yintercept = 5, linetype = "dashed") + xlim(0,50)
# p_total


a <- 4000

p_invfix <- ggplot() +
  geom_histogram(data = datrm, aes(x= Act_fixes,fill = JoinStatus, colour = JoinStatus),
                 alpha = 0.1, size = 0.9) +
  geom_vline(data = sum_clu, aes(xintercept = fix.mean, colour = JoinStatus),
             linetype="dashed", size = 0.9) +
  geom_point(data = propinv, aes(x = propinv$Act_fixes, y = propInv*a), alpha = 0.5, col = "#21918c") + 
  geom_smooth(data = propinv, aes(x = propinv$Act_fixes, y = propInv*a), se = F, size = 0.9, col = "#21918c") +
  scale_y_continuous("Frequency", 
                     sec.axis = sec_axis(~ (. - 1)/a, name = "Proportion Investigated")) +
  xlab("Number of fixes") +  
  ggtitle("a.") +
  scale_colour_manual(values = c("#3b528b","#5ec962"),labels=c('Not Investigated', 'Investigated'), name = "Cluster Status") +
  scale_fill_manual(values = c("#3b528b","#5ec962"),labels=c('Not Investigated', 'Investigated'), name = "Cluster Status") +
  theme_bw() +theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20), 
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20),
    legend.position=c(0.8, 0.9)) + xlim(0,100)
  
p_invfix 

png('results/clustersvfixes.png', width = 10000, height = 7000, res=1000, units="px")

p_invfix 

dev.off()

### by cluster radius
p_clu_rad <- ggplot(datrm) +
  geom_histogram(aes(x= Clus_rad_m, colour = JoinStatus), 
                 fill = "white", alpha = 0.2, position = "dodge") +
  geom_vline(data = sum_clu, aes(xintercept = rad.mean, colour = JoinStatus),
             linetype="dashed")

p_clu_rad




