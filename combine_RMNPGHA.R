



###run explore cluRMNP and cluGHA26
###combine hists and scatterplot 

###both study sites in one
p_invfix2 <- ggplot() +
  geom_point(data = allctrm, aes(x = allctrm$Act_fixes, y = propInv,  col = "RMNP"), alpha = 0.5) + 
  geom_smooth(data = allctrm, aes(x = allctrm$Act_fixes, y = propInv,  col = "RMNP"), span = .75,  se = F, size = 0.9) +
  geom_point(data = allct26 , aes(x = allct26 $Act_fixes, y = propInv, col = "GHA 26"), alpha = 0.5) + 
  geom_smooth(data = allct26 , aes(x = allct26 $Act_fixes, y = propInv, col = "GHA 26"), span = .75, se = F, size = 0.9) +
  ylab("Proportion Investigated") +
  xlab("Number of locations") +   xlim(0,100) +
  ggtitle("c.") + 
  scale_colour_manual(values = c("#440154","#21918c"),labels=c('GHA 26', 'RMNP'), name = "Study Area") +
  scale_fill_manual(values = c("#440154","#21918c"),labels=c('GHA 26', 'RMNP'), name = "Study Area") +
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
    legend.position=c(0.8, 0.9)) 
p_invfix2


###arranging plots 

png('results/clusters_invest.png', width = 25000, height = 20000, res=1200, units="px")



((p_histfixrm/p_histfix26)|p_invfix2)/(p_behavfixrm|p_behavfix26)


dev.off()
