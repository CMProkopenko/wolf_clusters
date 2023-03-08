
####
###cluster data exploration for RMNP

###packages
libs <- c('data.table', 'rcartocolor', 
          'ggplot2', 'ggridges', 'ggdist', 'ggbeeswarm','gghalves',
          'ggthemes', 'colorspace', 'viridis', 'patchwork')
lapply(libs, require, character.only = TRUE)

####Load and subset data ####

### load All Sites data after cleaning protocol
datgha26 <- fread('data/2023-02-22_GHA_All_Clu.csv')  ###11237 obs

##clusters less than 150 actual fixes
##removes inv match only as well because there are no clusters associated
datgha26 <- datgha26[Act_fixes <= 150] ###11179 obs



### unique investigated sites (remove multi wolves)
####Do not have a column ID for this yet
### calculate mean actual fixes and radius for clusters

summary(as.factor(datgha26$JoinStatus))

sum_clu26 <- datgha26[ , .(fix.mean=mean(Act_fixes), 
                          fix.min = min(Act_fixes),
                          fix.max=max(Act_fixes),
                          hr.mean = mean(CluDurHours),
                          hr.min = min(CluDurHours),
                          hr.max = max(CluDurHours),
                          rad.mean=mean(Clus_rad_m), 
                          rad.min = min(Clus_rad_m),
                          rad.max=max(Clus_rad_m),
                          count = .N), by = JoinStatus]

head(sum_clu26)


### proportion of clusters investigated by size
catfix26 <- datgha26
catfix26$Act_fixes <- as.factor(catfix26$Act_fixes)
catfix26$JoinStatus <- as.factor(catfix26$JoinStatus)

fixcount26 <- catfix26[,.(count = .N), by = c('Act_fixes', 'JoinStatus')]
cluct26 <- fixcount26[JoinStatus == 'CLUonly']
invct26  <-fixcount26[JoinStatus == 'Matched']
allct26  <- invct26 [cluct26, on = 'Act_fixes']

allct26 [is.na(allct26 $count), ] <- 0 

allct26  <- allct26 [, "propInv" := (count/(count+i.count))]
allct26  <-allct26 [, "total" := (count+i.count)]

allct26 $Act_fixes <-  as.numeric(as.character(allct26 $Act_fixes))


####### all clusters #######
###plot size histogram for matched and not investigated clusters

p_clu26 <- ggplot() +
  geom_histogram(data = datgha26, aes(x= Act_fixes, colour = JoinStatus), 
                 fill = "white", alpha = 0.2, position = "dodge") +
  geom_vline(data = sum_clu26, aes(xintercept = fix.mean, colour = JoinStatus),
             linetype="dashed") 

p_clu26


####proportion investigated by fixes
# # 
#  p_total26 <- ggplot(allct26 , aes(x = allct26 $Act_fixes, y = total)) +
#    geom_col()+ geom_hline(yintercept = 5, linetype = "dashed") + xlim(0,50)
#  p_total26

# 
# a <- 4000
# 
# p_infix26 <- ggplot() +
#   geom_histogram(data = datgha26, aes(x= Act_fixes,fill = JoinStatus, colour = JoinStatus),
#                  alpha = 0.1, size = 0.9) +
#  geom_vline(data = sum_clu26, aes(xintercept = fix.mean, colour = JoinStatus),
#             linetype="dashed", size = 0.9) +
#   geom_point(data = allct26 , aes(x = allct26 $Act_fixes, y = propInv*a), alpha = 0.5, col = "#21918c") + 
#   geom_smooth(data = allct26 , aes(x = allct26 $Act_fixes, y = propInv*a), se = F, size = 0.9, col = "#21918c") +
#   scale_y_continuous("Frequency", 
#                      sec.axis = sec_axis(~ (. - 1)/a, name = "Proportion Investigated")) +
#   xlab("Number of locations") +  
#   ggtitle("a.") +
#   scale_colour_manual(values = c("#3b528b","#5ec962"),labels=c('Not Investigated', 'Investigated'), name = "Cluster Status") +
#   scale_fill_manual(values = c("#3b528b","#5ec962"),labels=c('Not Investigated', 'Investigated'), name = "Cluster Status") +
#   theme_bw() +theme_bw()  + theme(
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
#     legend.position=c(0.8, 0.9)) + xlim(0,100)
# 
# p_invfix26
 


p_histfix26 <- ggplot() +
  geom_histogram(data = datgha26, aes(x= Act_fixes,fill = JoinStatus, colour = JoinStatus),
                   alpha = 0.1, size = 0.9, binwidth = 2) +
  geom_vline(data = sum_clu26, aes(xintercept = fix.mean, colour = JoinStatus),
               linetype="dashed", size = 0.9) +
  ylab("Frequency") + ylim(0,5000) +
  xlab("Number of locations") +   xlim(0,100) +
  ggtitle("b. GHA 26") + 
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
    legend.position="none") 

p_histfix26

p_invfix26 <- ggplot() +
  geom_point(data = allct26 , aes(x = allct26 $Act_fixes, y = propInv), alpha = 0.5, col = "#21918c") + 
  geom_smooth(data = allct26 , aes(x = allct26 $Act_fixes, y = propInv), se = F, size = 0.9, col = "#21918c") +
  ylab("Proportion Investigated") +
  xlab("Number of locations") +   xlim(0,100) +
  ggtitle("c.") + 
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
    legend.position=c(0.7, 0.7)) 
p_invfix26


png('results/clustersvfixesgha26.png', width = 10000, height = 7000, res=1000, units="px")

p_histfix26/p_invfix26 

dev.off()


### by cluster radius
p_clu_rad26 <- ggplot(datgha26) +
  geom_histogram(aes(x= Clus_rad_m, colour = JoinStatus), 
                 fill = "white", alpha = 0.2, position = "dodge") +
  geom_vline(data = sum_clu26, aes(xintercept = rad.mean, colour = JoinStatus),
             linetype="dashed")

p_clu_rad26




