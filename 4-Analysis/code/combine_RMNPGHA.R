
#########################################
###Combining cluster data sets 
####RMNP and Eastern MB (GHA 26)
###### Christina Prokopenko
###### Started April 2023
#########################################

###run explore cluRMNP and cluGHA26
###combine hists and scatterplot 

###both study sites in one
allct26$title <- "C. Both Study Areas"

p_invfix2 <- ggplot() +
  geom_point(data = allctrm, aes(x = allctrm$Act_fixes, y = propInv,  col = "RMNP"), alpha = 0.5) + 
  geom_smooth(data = allctrm, aes(x = allctrm$Act_fixes, y = propInv,  col = "RMNP"), span = .75,  se = F, size = 0.9) +
  geom_point(data = allct26 , aes(x = allct26 $Act_fixes, y = propInv, col = "GHA 26"), alpha = 0.5) + 
  geom_smooth(data = allct26 , aes(x = allct26 $Act_fixes, y = propInv, col = "GHA 26"), span = .75, se = F, size = 0.9) +
  ylab("Proportion Investigated") +
  xlab("Number of locations") +   #xlim(0,100) +
  #ggtitle("C.") + 
  facet_grid(. ~ title) + 
  scale_colour_manual(values = c("#440154","#21918c"),labels=c('GHA 26', 'RMNP'), name = "Study Area") +
  scale_fill_manual(values = c("#440154","#21918c"),labels=c('GHA 26', 'RMNP'), name = "Study Area") +
  theme_bw() + theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, size=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 20,hjust = 0),
    plot.title = element_text(size = 20, hjust = .98, vjust = .8),
    axis.line = element_line(colour = "black", size = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20),
    legend.position=c(0.8, 0.8)) 
p_invfix2


###arranging plots 
########this is figure 2 in main text

hists <- p_histfixrm + p_histfix26 + plot_layout(nrow = 2)

png('results/clusters_invest2.png', width = 20000, height = 15000, res=1200, units="px")


#((p_histfixrm/p_histfix26)|p_invfix2)/(p_behavfixrm|p_behavfix26)

hists/p_invfix2|p_behavfixrm/p_behavfix26 

dev.off()





#####radius


p_invrad2 <- ggplot() +
  geom_point(data = allctradrm, aes(x = allctradrm$Clus_rad_m, y = propInv,  col = "RMNP"), alpha = 0.5) +
  geom_point(data = allctrad26, aes(x = allctrad26$Clus_rad_m, y = propInv,  col = "GHA 26"), alpha = 0.5) +
  geom_smooth(data = allctradrm, aes(x = allctradrm$Clus_rad_m, y = propInv,  col = "RMNP"), alpha = 0.5, se = F) +
  geom_smooth(data = allctrad26, aes(x = allctrad26$Clus_rad_m, y = propInv,  col = "GHA 26"), alpha = 0.5, se = F) +
  ylab("Proportion Investigated") +
  xlab("Cluster Radius (m)") + 
  theme_bw()  + theme(
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
    legend.position=c(0.8, 0.7)) 
p_invrad2 


png('results/clusters_investradius.png', width = 12000, height = 10000, res=1200, units="px")

p_invrad2 

dev.off()


p_invdur2 <- ggplot() +
  geom_point(data = allctdurrm, aes(x = allctdurrm$CluDurHours, y = propInv,  col = "RMNP"), alpha = 0.5) +
  geom_point(data = allctdur26, aes(x = allctdur26$CluDurHours, y = propInv,  col = "GHA 26"), alpha = 0.5) +
  geom_smooth(data = allctdurrm, aes(x = allctdurrm$CluDurHours, y = propInv,  col = "RMNP"), alpha = 0.5, se = F) +
  geom_smooth(data = allctdur26, aes(x = allctdur26$CluDurHours, y = propInv,  col = "GHA 26"), alpha = 0.5, se = F) +
  ylab("Proportion Investigated") +
  xlab("Cluster Duration (Hours)") + 
  theme_bw()  + theme(
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
    legend.position=c(0.8, 0.7)) 
p_invdur2 


png('results/clusters_investduration.png', width = 12000, height = 10000, res=1200, units="px")

p_invdur2 

dev.off()
