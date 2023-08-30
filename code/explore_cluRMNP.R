
####
###cluster data exploration for RMNP

###packages
libs <- c('data.table', 'rcartocolor', 
          'ggplot2', 'ggridges', 'ggdist', 'ggbeeswarm','gghalves',
          'ggthemes', 'colorspace', 'viridis', 'patchwork')
lapply(libs, require, character.only = TRUE)

####Load and subset data ####

### load All Sites data after cleaning protocol
datrmnp <- fread('data/2023-02-22_RMNP_K_All_Clu.csv')  ###6547 obs

summary(datrmnp$Act_fixes) ###max actual fixes 275

##clusters less than 150 actual fixes
##removes inv match only as well because there are no clusters associated
datrmnp <- datrmnp[Act_fixes <= 300] ###6433 obs



### unique investigated sites (remove multi wolves)
####Do not have a column ID for this yet

summary (datrmnp$mwKid)


### calculate stats for actual fixes and radius for clusters

summary(as.factor(datrmnp$JoinStatus))

sum_clurm <- datrmnp[ , .(fix.mean=mean(Act_fixes), 
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
                   count = .N), by = JoinStatus]

head(sum_clurm)


### proportion of clusters investigated by actual fixes
catfixrm <- datrmnp
catfixrm$Act_fixes <- as.factor(catfixrm$Act_fixes)
catfixrm$JoinStatus <- as.factor(catfixrm$JoinStatus)


fixcountrm <- catfixrm[,.(count = .N), by = c('Act_fixes', 'JoinStatus')]
cluctrm <- fixcountrm[JoinStatus == 'CLUonly']
invctrm <-fixcountrm[JoinStatus == 'Matched']
allctrm <- invctrm[cluctrm, on = 'Act_fixes']



###if for a point number it is NA I just want it to be 0 for count,
##but if Matched is NA then it should still be matched even if the count is 0


allctrm$count <- as.numeric(allctrm$count)
allctrm$i.count <- as.numeric(allctrm$i.count)

allctrm$JoinStatus[is.na(allctrm$JoinStatus)]  <- "Matched"
allctrm$count[is.na(allctrm$count)]  <- 0



propinvrm <- allctrm[, "propInv" := (count/(count+i.count))]
propinvrm <-propinvrm[, "total" := (count+i.count)]

propinvrm$Act_fixes <-  as.numeric(as.character(propinvrm$Act_fixes))
allctrm$Act_fixes <-  as.numeric(as.character(allctrm$Act_fixes))

####### all clusters #######
###plot size histogram for matched and not investigated clusters

p_clu <- ggplot() +
  geom_histogram(data = datrmnp, aes(x= Act_fixes, colour = JoinStatus), 
                 fill = "white", alpha = 0.2, position = "dodge", binwidth = 2) +
  geom_vline(data = sum_clurm, aes(xintercept = fix.mean, colour = JoinStatus),
             linetype="dashed") 

p_clu


####proportion investigated by fixes
# 
# p_total <- ggplot(propinvrm, aes(x = propinvrm$Act_fixes, y = total)) +
#   geom_col()+ geom_hline(yintercept = 5, linetype = "dashed") + xlim(0,50)
# p_total


# a <- 4000
# 
# p_invfix <- ggplot() +
#   geom_histogram(data = datrmnp, aes(x= Act_fixes,fill = JoinStatus, colour = JoinStatus),
#                  alpha = 0.1, size = 0.9) +
#   geom_vline(data = sum_clurm, aes(xintercept = fix.mean, colour = JoinStatus),
#              linetype="dashed", size = 0.9) +
#   geom_point(data = propinvrm, aes(x = propinvrm$Act_fixes, y = propInv*a), alpha = 0.5, col = "#21918c") + 
#   geom_smooth(data = propinvrm, aes(x = propinvrm$Act_fixes, y = propInv*a), se = F, size = 0.9, col = "#21918c") +
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
# p_invfix 


p_histfixrm <- ggplot() +
  geom_histogram(data = datrmnp, aes(x= Act_fixes,fill = JoinStatus, colour = JoinStatus),
                 alpha = 0.1, size = 0.9, binwidth = 2) +
  geom_vline(data = sum_clurm, aes(xintercept = fix.med, colour = JoinStatus),
             linetype="dashed", size = 0.9) +
  ylab("Frequency") + #ylim(0,6000) +
  xlab("Number of locations") + #xlim(0,100) +
  ggtitle("a. RMNP") + 
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
    legend.position=c(0.7, 0.7)) 

p_histfixrm

p_invfixrm <- ggplot() +
  geom_point(data = allctrm, aes(x = allctrm$Act_fixes, y = propInv), alpha = 0.5, col = "#21918c") + 
  geom_smooth(data = allctrm, aes(x = allctrm$Act_fixes, y = propInv), se = F, size = 0.9, col = "#21918c") +
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
p_invfixrm

# png('results/clustersvfixes.png', width = 10000, height = 7000, res=1000, units="px")
# 
# p_invfix 
# 
# dev.off()

### proportion of clusters investigated by radiues
catradrm <- datrmnp
catradrm$Clus_rad_m <- as.factor(catradrm$Clus_rad_m)
catradrm$JoinStatus <- as.factor(catradrm$JoinStatus)

radcountrm <- catradrm[,.(count = .N), by = c('Clus_rad_m', 'JoinStatus')]
cluctradrm <- radcountrm[JoinStatus == 'CLUonly']
invctradrm <-radcountrm[JoinStatus == 'Matched']
allctradrm <- invctradrm[cluctradrm, on = 'Clus_rad_m']

allctradrm[is.na(allctradrm$count), ] <- c(0)  

propinvradrm <- allctradrm[, "propInv" := (count/(count+i.count))]
propinvradrm <-propinvradrm[, "total" := (count+i.count)]

propinvradrm$Clus_rad_m <-  as.numeric(as.character(propinvrm$Clus_rad_m))
allctradrm$Clus_rad_m <-  as.numeric(as.character(allctradrm$Clus_rad_m))


### by cluster radius
p_clu_radrm <- ggplot(datrmnp) +
  geom_histogram(aes(x= Clus_rad_m, colour = JoinStatus), 
                 fill = "white", alpha = 0.2, position = "dodge") +
  geom_vline(data = sum_clurm, aes(xintercept = rad.med, colour = JoinStatus),
             linetype="dashed") +
  xlab("Count") + ylab("Cluster Radius (meters)") +
  ggtitle("RMNP") +
  theme_bw() +theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
#    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20),
    legend.position=c(0.7, 0.7)) 
p_clu_radrm


png('results/clu_radrm.png', width = 12000, height = 10000, res=1200, units="px")

p_clu_radrm

dev.off()


#####cluster duration 

catdurrm <- datrmnp
#catdurrm$CluDurHours- as.factor(catdurrm$CluDurHours)
catdurrm$JoinStatus <- as.factor(catdurrm$JoinStatus)

durcountrm <- catdurrm[,.(count = .N), by = c('CluDurHours', 'JoinStatus')]
cluctdurrm <- durcountrm[JoinStatus == 'CLUonly']
invctdurrm <-durcountrm[JoinStatus == 'Matched']
allctdurrm <- invctdurrm[cluctdurrm, on = 'CluDurHours']

allctdurrm[is.na(allctdurrm$count), ] <- c(0)

propinvdurrm <- allctdurrm[, "propInv" := (count/(count+i.count))]
propinvdurrm <-propinvdurrm[, "total" := (count+i.count)]

propinvdurrm$CluDurHours<-  as.numeric(as.character(propinvrm$CluDurHours))
allctdurrm$CluDurHours <-  as.numeric(as.character(allctrm$CluDurHours))




###### by cluster duration
p_clu_dur<- ggplot(datrmnp) +
  geom_histogram(aes(x= CluDurHours, colour = JoinStatus), 
                 fill = "white", alpha = 0.2, position = "dodge") +
  geom_vline(data = sum_clurm, aes(xintercept = hr.med, colour = JoinStatus),
             linetype="dashed") +
  xlab("Count") + ylab("Cluster Duration (hours)") +
  ggtitle("RMNP") +
  theme_bw() +theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
#    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20),
    legend.position=c(0.7, 0.7)) 
p_clu_dur


png('results/clu_durrm.png', width = 12000, height = 10000, res=1200, units="px")

p_clu_dur

dev.off()


#### seems the same
#time <- ggplot(datrmnp) + geom_point(aes(x = CluDurHours, y = Total_hours))
