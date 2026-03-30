### Models
## Individual Dataset
indiv.ds <- fread("Pack_Dataset/Final_dataset/Individual_dataset_full_updated_2025-08-15.csv")

indiv.ds <- indiv.ds[!(pack == "Great-Falls-Lone" | pack == "Black-River")]
indiv.ds <- indiv.ds[!(animal_id == "W02" & season == "SnowFree")]
colnames(indiv.ds) <- gsub("-", "_", colnames(indiv.ds))
indiv.ds[, decmix := deciduous + mixed]
indiv.ds[, pack := as.factor(pack)]
indiv.ds[, season := as.factor(season)]
indiv.ds[, studyarea := as.factor(ifelse(study_area == "RMNP", "RMNP", "SEMB"))]
#indiv.ds$study_area <- factor(indiv.ds$study_area, levels = c("RMNP", "GHA"))
indiv.ds[, sl_km := sl_mean/1000]
indiv.ds[, packyr := paste(pack, year, sep = "_")]
indiv.ds[, wolf_dens := ifelse(study_area == "RMNP", population/30, population/72)]

indiv.seas.ds <- indiv.ds[!season == "full-year",]
indiv.fy.ds <- indiv.ds[season == "full-year"]
indiv.s.ds <- indiv.ds[season == "SnowFree"]
indiv.w.ds <- indiv.ds[season == "Snow"]

indiv.semb <- indiv.ds[studyarea == "SEMB" & !season == "full-year",]
indiv.rmnp <- indiv.ds[studyarea == "RMNP" & !season == "full-year",]

## Plot to check
hab.plot <- melt(indiv.ds, id.vars = c("animal_id", "season", "study_area"), 
                 measure.vars = c("conifer", "deciduous", "mixed", "decmix", "open",
                                  "water_wetland"))
chk.plot <- indiv.seas.ds[!(pack == "Great-Falls-Lone"|pack == "Black-River")]

ggplot() + 
  geom_density(data = chk.plot,
               aes(x = (moose_density_averaged), fill = study_area), alpha = 0.5) +
  facet_wrap(~season) + 
  theme_bw()

ggplot(data = chk.plot, aes(x = log(moose_density_averaged), y = log(area.95))) + 
  geom_point(aes(color = study_area)) + 
  geom_smooth(aes(color = study_area)) + 
  theme_bw()

ggplot() + 
  geom_density(data = indiv.fy.ds[!(pack == "Great-Falls-Lone"|pack == "Black-River")],
               aes(x = moose_density_averaged, fill = study_area), alpha = 0.5) +
  facet_wrap(~season) + 
  theme_bw()

ggplot() + 
  geom_density(data = chk.plot[!(pack == "Snowshoe")],
               aes(x = (niche_breadth), fill = study_area), alpha = 0.5) +
  facet_wrap(~season) + 
  theme_bw()

ggplot(data = chk.plot[!(pack == "Snowshoe") & !is.na(niche_breadth)], aes(x = niche_breadth, y = log(area.95))) + 
  geom_point(aes(color = study_area)) + 
  geom_smooth(aes(color = study_area)) + 
  theme_bw()

moose.full <- rbind(prey.cast.emb, prey.cast.rmnp, fill = T)
ggplot(data = moose.full) + 
  geom_boxplot(aes(y = Moose_averaged, colour = study_area)) + 
  facet_wrap(~season)

ggplot(data = prey.cast.emb, aes(x = season, colour = pack)) + 
  geom_point(aes(y = Moose_averaged)) + 
  geom_line(aes(y = Moose_averaged, group = pack)) + 
  facet_wrap(~year)

ggplot(data = prey.cast.emb, aes(x = year, colour = pack)) + 
  geom_point(aes(y = Moose_averaged)) + 
  geom_line(aes(y = Moose_averaged, group = pack)) + 
  facet_wrap(~season)


pack.ds <- fread("Pack_Dataset/Final_dataset/Pack_dataset_full_updated_2025-06-18.csv")
colnames(pack.ds) <- gsub("-", "_", colnames(pack.ds))
pack.ds[, decmix := deciduous + mixed]

pack.ds.seas <- pack.ds[!season == "full-year"]

## Home Range
library(lme4)
library(effects)
library(ggplot2)

m1 <- lmer(log(area.95) ~ 0 + season + moose_density_averaged + lf_dens + 
             wolf_dens + decmix + pack_size + 
             (1|pack),
           data = indiv.seas.ds)

m2 <- lmer(log(area.95) ~ 0 + studyarea + season + moose_density_averaged + lf_dens + 
             wolf_dens + decmix + pack_size + 
             (1|pack),
           data = indiv.seas.ds)

m3 <- lmer(log(area.95) ~ 0 + studyarea + season:studyarea + moose_density_averaged:studyarea + 
             lf_dens:studyarea + wolf_dens:studyarea + decmix:studyarea + pack_size:studyarea + 
             (1|pack),
           data = indiv.seas.ds)

logLik(m1)
logLik(m2)
logLik(m3)

summary(m3)
a <- logLik(m1)
b <- logLik(m2)
c <- logLik(m3)
-2 * (as.numeric(a) - as.numeric(c))
-2 * (as.numeric(b) - as.numeric(c))

## Home Range
# Seasonal
hr.mod <- lmer(log(area.95) ~ 0 + 
                 (moose_density_averaged):studyarea + 
                 season:studyarea + 
                 decmix:studyarea + 
                 #water_wetland:studyarea + 
                 (wolf_dens):studyarea + 
                 lf_dens:studyarea + 
                 pack_size:studyarea + 
                 studyarea + 
                 (1|pack),
               data = indiv.seas.ds)
hr_mod <- broom.mixed::tidy(hr.mod, conf.int=TRUE)
hr_mod

logLik(hr.mod)

dens_hr_effect <- Effect(c('studyarea','lf_dens'), hr.mod, xlevels = 20)
lf_site_df <- as.data.table(dens_hr_effect)

lfdens <- ggplot() +
  geom_line(data = lf_site_df[studyarea =='RMNP' & lf_dens < 0.58,], aes(lf_dens, fit), colour = "#21918c") +
  geom_line(data = lf_site_df[studyarea =='SEMB' & lf_dens > 0.1,], aes(lf_dens,fit,), colour = "#440154") +
  geom_ribbon(data = lf_site_df[studyarea =='RMNP' & lf_dens < 0.58,], aes(lf_dens, fit, ymin=lower,ymax=upper),fill="#21918c",alpha=0.1) +
  geom_ribbon(data = lf_site_df[studyarea =='SEMB' & lf_dens > 0.1,], aes(lf_dens,fit,ymin=lower,ymax=upper),fill="#440154",alpha=0.1) +
  geom_rug(data= dens_hr_effect$data, aes(lf_dens, y=NULL, colour = studyarea),sides="b") +
  scale_colour_manual(values = c("#21918c","#440154"), name = "Study Site") +
  xlab(expression(Linear~Feature~Density~(km/km^2))) +
  ylab(expression(ln~aKDE~Area~(km^2))) +
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
    legend.key = element_blank())
lfdens
#ggsave(lfdens, filename = "HomeRange_LinearFeature_Plot_2025-08-30.png", width = 7, height = 5)

# No interactions
hr.mod.v2 <- lmer(log(area.95) ~ 0 + 
                    moose_density_averaged + 
                    season + 
                    decmix + 
                    #water_wetland:studyarea + 
                    wolf_dens + 
                    lf_dens + 
                    pack_size + 
                    studyarea + 
                    (1|pack),
                  data = indiv.seas.ds)
hr_mod2 <- broom.mixed::tidy(hr.mod.v2, conf.int=TRUE)
hr_mod2

logLik(hr.mod.v2)

# Combine models for output
hr_full <- rbind(as.data.frame(hr_mod) %>% mutate(model = "Interactions"), 
                 as.data.frame(hr_mod2)%>% mutate(model = "No Interactions"))

#fwrite(hr_full, file = "HR_FullModel_Output_2025-08-22.csv")

# Summer
hr.summ <- lmer(log(area.95) ~ 0 + 
                  (moose_density_averaged):studyarea + 
                  decmix:studyarea + 
                  #water_wetland + 
                  (wolf_dens):studyarea + 
                  lf_dens:studyarea + 
                  pack_size:studyarea + 
                  studyarea + 
                  (1|pack),
                data = indiv.s.ds)
hr_summ <- broom.mixed::tidy(hr.summ, conf.int=TRUE)
hr_summ

dens_hr_effect_summ <- Effect(c('studyarea','moose_density_averaged'), hr.summ)

# Winter
hr.wint <- lmer(log(area.95) ~ 0 + 
                  (moose_density_averaged):studyarea + 
                  decmix:studyarea + 
                  #water_wetland + 
                  (wolf_dens):studyarea + 
                  lf_dens:studyarea + 
                  pack_size:studyarea + 
                  studyarea + 
                  (1|pack),
                data = indiv.w.ds)
hr_wint <- broom.mixed::tidy(hr.wint, conf.int=TRUE)
hr_wint

dens_hr_effect_wint <- Effect(c('studyarea','moose_density_averaged'), hr.wint)

seas_dens_df <- rbind(as.data.frame(dens_hr_effect_summ) %>% mutate(season = "Snow Free"), 
                      as.data.frame(dens_hr_effect_wint)%>% mutate(season = "Snow"))


#fwrite(hr_seas_output, "HomeRange_Seasonal_Model_Output.csv")

ggplot(seas_dens_df,
       aes(moose_density_averaged, fit, fill = studyarea, colour = studyarea))+
  geom_line()+
  xlab(expression(Moose~Density~(moose/km^2))) + 
  ylab(expression(log~aKDE~Area~(km^2))) + 
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper)) +
  scale_colour_manual(values = c("#21918c","#440154"), name = "Study Site") +
  scale_fill_manual(values = c("#21918c","#440154"), name = "Study Site") +
  facet_wrap(~season) + 
  ## add rug plot based on original data
  geom_rug(data=seas_dens_df$data, aes(y=NULL),sides="b") +
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
    legend.key = element_blank())

## Niche Breadth
# Seasonal Model
nb.mod <- lmer(niche_breadth ~ 0 + 
                 (moose_density_averaged):studyarea + 
                 season:studyarea + 
                 decmix:studyarea + 
                 #water_wetland + 
                 (wolf_dens):studyarea + 
                 lf_dens:studyarea + 
                 pack_size:studyarea + 
                 studyarea +
                 (1|pack),
               data = indiv.seas.ds)
nb_mod<- broom.mixed::tidy(nb.mod, conf.int=TRUE)
nb_mod

logLik(nb.mod)

# No Interaction
nb.mod.v2 <- lmer(niche_breadth ~ 0 + 
                    moose_density_averaged+ 
                    season + 
                    decmix + 
                    #water_wetland + 
                    wolf_dens + 
                    lf_dens + 
                    pack_size + 
                    studyarea +
                    (1|pack),
                  data = indiv.seas.ds)
nb_mod2 <- broom.mixed::tidy(nb.mod.v2, conf.int=TRUE)
nb_mod2
logLik(nb.mod.v2)

# Combine models for output
nb_full <- rbind(as.data.frame(nb_mod) %>% mutate(model = "Interactions"), 
                 as.data.frame(nb_mod2)%>% mutate(model = "No Interactions"))

#fwrite(nb_full, file = "NB_FullModel_Output_2025-08-22.csv")

## Step Length
# Seasonal Model
sl.seas <- lmer(log(sl_mean) ~ 0 + 
                  (moose_density_averaged):studyarea + 
                  season:studyarea + 
                  decmix:studyarea + 
                  #water_wetland + 
                  (wolf_dens):studyarea + 
                  lf_dens:studyarea + 
                  pack_size:studyarea + 
                  studyarea + 
                  (1|pack),
                data = indiv.seas.ds)
sl_seas <- broom.mixed::tidy(sl.seas, conf.int=TRUE)
sl_seas

logLik(sl.seas)

pred_sl_effect <- Effect(c('studyarea','wolf_dens'), sl.seas, xlevels = 20)
pred_sl_df <- as.data.table(pred_sl_effect)

sl.wolf <- ggplot() +
  geom_line(data = pred_sl_df[studyarea =='RMNP' & wolf_dens > 1.9,], aes(wolf_dens, fit), colour = "#21918c") +
  geom_line(data = pred_sl_df[studyarea =='SEMB' & wolf_dens < 2 ,], aes(wolf_dens,fit,), colour = "#440154") +
  geom_ribbon(data = pred_sl_df[studyarea =='RMNP' & wolf_dens > 1.9,], aes(wolf_dens, fit, ymin=lower,ymax=upper),fill="#21918c",alpha=0.1) +
  geom_ribbon(data = pred_sl_df[studyarea =='SEMB' & wolf_dens < 2,], aes(wolf_dens,fit,ymin=lower,ymax=upper),fill="#440154",alpha=0.1) +
  geom_rug(data= pred_sl_effect$data, aes(wolf_dens, y=NULL, colour = studyarea),sides="b") +
  scale_colour_manual(values = c("#21918c","#440154"), name = "Study Site") +
  xlab(expression(paste("Wolf Density (wolves/10", km^2,")"))) +
  ylab("ln Step Length (m/2h)") +
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
    legend.key = element_blank())
sl.wolf

moose_sl_effect <- Effect(c('studyarea','moose_density_averaged'), sl.seas, xlevels = 20)

moose_sl_df <- as.data.table(moose_sl_effect)

sl.moose <- ggplot() +
  geom_line(data = moose_sl_df[studyarea =='RMNP' & moose_density_averaged > 0.5,], aes(moose_density_averaged, fit), colour = "#21918c") +
  geom_line(data = moose_sl_df[studyarea =='SEMB' & moose_density_averaged < 0.5 ,], aes(moose_density_averaged,fit,), colour = "#440154") +
  geom_ribbon(data = moose_sl_df[studyarea =='RMNP' & moose_density_averaged > 0.5,], aes(moose_density_averaged, fit, ymin=lower,ymax=upper),fill="#21918c",alpha=0.1) +
  geom_ribbon(data = moose_sl_df[studyarea =='SEMB' & moose_density_averaged < 0.5,], aes(moose_density_averaged,fit,ymin=lower,ymax=upper),fill="#440154",alpha=0.1) +
  geom_rug(data= moose_sl_effect$data, aes(moose_density_averaged, y=NULL, colour = studyarea),sides="b") +
  scale_colour_manual(values = c("#21918c", "#440154"), name = "Study Site") +
  xlab(expression(paste("Moose Density (moose/", km^2,")"))) +
  ylab("ln Step Length (m/2h)") +
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
    legend.key = element_blank())
sl.moose

lf_sl_effect <- Effect(c('studyarea','lf_dens'), sl.seas, xlevels = 20)
lf_sl_df <- as.data.table(lf_sl_effect)

sl.lf <- ggplot() +
  geom_line(data = lf_sl_df[studyarea =='RMNP' & lf_dens < 0.58,], aes(lf_dens, fit), colour = "#21918c") +
  geom_line(data = lf_sl_df[studyarea =='SEMB' & lf_dens > 0.1,], aes(lf_dens,fit,), colour = "#440154") +
  geom_ribbon(data = lf_sl_df[studyarea =='RMNP' & lf_dens < 0.58,], aes(lf_dens, fit, ymin=lower,ymax=upper),fill="#21918c",alpha=0.1) +
  geom_ribbon(data = lf_sl_df[studyarea =='SEMB' & lf_dens > 0.1,], aes(lf_dens,fit,ymin=lower,ymax=upper),fill="#440154",alpha=0.1) +
  geom_rug(data= lf_sl_effect$data, aes(lf_dens, y=NULL, colour = studyarea),sides="b") +
  scale_colour_manual(values = c("#21918c", "#440154"), name = "Study Site") +
  xlab(expression(Linear~Feature~Density~(km/km^2))) +
  ylab("ln Step Length (m/2h)") +
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
    legend.key = element_blank())
sl.lf

hab_sl_effect <- Effect(c('studyarea','decmix'), sl.seas, xlevels = 20)
hab_sl_df <- as.data.table(hab_sl_effect)

sl.hab <- ggplot() +
  geom_line(data = hab_sl_df[studyarea =='RMNP' & decmix > 0.45,], aes(decmix, fit), colour = "#21918c") +
  geom_line(data = hab_sl_df[studyarea =='SEMB' & decmix > 0.1,], aes(decmix,fit,), colour = "#440154") +
  geom_ribbon(data = hab_sl_df[studyarea =='RMNP' & decmix > 0.45,], aes(decmix, fit, ymin=lower,ymax=upper),fill="#21918c",alpha=0.1) +
  geom_ribbon(data = hab_sl_df[studyarea =='SEMB' & decmix > 0.1,], aes(decmix,fit,ymin=lower,ymax=upper),fill="#440154",alpha=0.1) +
  geom_rug(data= hab_sl_effect$data, aes(decmix, y=NULL, colour = studyarea),sides="b") +
  scale_colour_manual(values = c("#21918c", "#440154"), name = "Study Site") +
  xlab("Proportion of Deciduous-Mixed") +
  ylab("ln Step Length (m/2h)") +
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
    legend.key = element_blank())
sl.hab


library(patchwork)

(lfdens) / (sl.moose + sl.wolf) / (sl.hab + sl.lf) + 
  plot_layout(guides = "collect", tag_level = "new") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom", 
        plot.tag = element_text(size = 20),
        plot.tag.position = c(0, 1),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 20))

ggsave(filename = "Final_Figures/High_Res/HR_StepLength_PanelFigure.png", width = 10, height = 14, dpi = 300)

## No interactions
sl.seas.v2 <- lmer(log(sl_mean) ~ 0 + 
                     (moose_density_averaged)+ 
                     season + 
                     decmix + 
                     #water_wetland + 
                     (wolf_dens) + 
                     lf_dens + 
                     pack_size + 
                     studyarea + 
                     (1|pack),
                   data = indiv.seas.ds)
sl_seas2 <- broom.mixed::tidy(sl.seas.v2, conf.int=TRUE)
sl_seas2

logLik(sl.seas.v2)

# Combine models for output
sl_full <- rbind(as.data.frame(sl_seas) %>% mutate(model = "Interactions"), 
                 as.data.frame(sl_seas2)%>% mutate(model = "No Interactions"))

fwrite(sl_full, file = "SL_FullModel_Output_2025-08-22.csv")

# Summer
hr.summ <- lmer(log(sl_km) ~ 0 + 
                  (moose_density_averaged):studyarea + 
                  decmix:studyarea + 
                  #water_wetland + 
                  (wolf_dens):studyarea + 
                  lf_dens:studyarea + 
                  pack_size:studyarea + 
                  studyarea + 
                  (1|pack),
                data = indiv.s.ds)
hr_summ <- broom.mixed::tidy(hr.summ, conf.int=TRUE)
hr_summ

dens_hr_effect_summ <- Effect(c('studyarea','moose_density_averaged'), hr.summ)

# Winter
hr.wint <- lmer(sl_km ~ 0 + 
                  (moose_density_averaged):studyarea + 
                  decmix:studyarea + 
                  #water_wetland + 
                  (wolf_dens):studyarea + 
                  lf_dens:studyarea + 
                  pack_size:studyarea + 
                  studyarea + 
                  (1|pack),
                data = indiv.w.ds)
hr_wint <- broom.mixed::tidy(hr.wint, conf.int=TRUE)
hr_wint