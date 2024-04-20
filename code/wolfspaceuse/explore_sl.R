### Movement summary ====
# Julie Turner
# Started: 21 September 2023

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'terra', 'sf', 
          'tidyr', 'ggplot2', 'ggdist', 'patchwork')
lapply(libs, require, character.only = TRUE)


### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')

### combining all data ----
#### meta data ----
dat.meta <- fread(file.path(raw, 'wolf_metadata_all.csv'), header = T)
dat.meta$death_date <- paste(dat.meta$death_date, '23:59:59', sep = ' ')
dat.meta$death_date<- as.POSIXct(dat.meta$death_date, tz = 'UTC', "%Y-%m-%d %H:%M:%S")
dat.meta[,'year'] <- as.numeric(format(dat.meta$death_date, '%Y'))

dat.forays <- fread(file.path(raw, 'Foray_Remove_aKDE.csv'), header = T)
dat.forays$foray <- 'foray'

# utm zone 14n
# wgs84 32614
# nad83 26914
# project 
crs <- st_crs(4326)$wkt
utm14N <- "+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs14 <- sf::st_crs(32614)

set.seed(57)

#### RMNP ----
datRMNP <- fread(file.path(raw, 'RMNPwolf_rarified.csv'))
datRMNP$datetime <- paste(datRMNP$gmtDate, datRMNP$gmtTime)
datRMNP$datetime <- as.POSIXct(datRMNP$datetime, tz = 'UTC', "%Y-%m-%d %H:%M:%S")
datRMNP[,.(min=min(gmtDate),max=max(gmtDate)), by=.(WolfID)]

# ID forays
datRMNP.foray <- datRMNP |>
  left_join(dat.forays[Study_Site == 'RMNP',.(WolfID = Animal_ID, Foray_Start, Foray_End, foray)],
            by = join_by(WolfID, between(Date, Foray_Start, Foray_End)))

#### GHA26 ----
dat2014_15 <- readRDS(file.path(raw, 'GHA26_Full_2014-15_2hr.Rds'))
dat2016 <- readRDS(file.path(raw, 'GHA26_Full_2016_2hr.Rds'))
dat2016_st <- readRDS(file.path(raw, 'GHA26_ST.Full_2016_2hr.Rds'))
dat2017 <- readRDS(file.path(raw, 'GHA26_Full_2017_2hr.Rds'))
dat2018 <- readRDS(file.path(raw, 'GHA26_Full_2018_2hr.Rds'))

dat2014_15 <- dat2014_15[,.(Collar_ID, MBts = ts, Latitude, Longitude)]
dat2014_15$MBts <- as.POSIXct(dat2014_15$MBts, tz = 'MST', "%Y-%m-%d %H:%M:%S")
dat2016 <- dat2016[,.(Collar_ID, MBts, Latitude, Longitude)]
dat2016_st <- dat2016_st[,.(Collar_ID, MBts = ts, Latitude, Longitude)]
dat2017 <- dat2017[,.(Collar_ID, MBts, Latitude, Longitude)]
dat2018 <- dat2018[,.(Collar_ID, MBts, Latitude, Longitude)]

datGHA <- rbind(dat2014_15, dat2016, dat2016_st, dat2017, dat2018)

# project long/lat

datGHA[, Longitude := as.numeric(Longitude)]
datGHA[, Latitude := as.numeric(Latitude)]

sfpts.GHA <- st_as_sf(datGHA, coords = c('Longitude', 'Latitude'),
                  crs = crs)
outpts.GHA <- st_transform(sfpts.GHA, crs14)
datGHA <- setDT(sfheaders::sf_to_df(outpts.GHA, fill = T))


datGHA[,'year'] <- as.numeric(format(datGHA$MBts,'%Y'))

collarID <- fread(file.path(raw, 'GHA26_coll_wolf_id.csv'), header = T)
collarID$Collar_ID <- as.character(collarID$Collar_ID)


datGHA.all <- merge(datGHA, collarID, by = c('Collar_ID', 'year'), all.x = T)
datGHA.all[,'WolfID'] <- ifelse(datGHA.all$Collar_ID=='5148', 'W17', datGHA.all$WolfID)
datGHA.all[,'PackID'] <- ifelse(datGHA.all$Collar_ID=='5148', 'MA', datGHA.all$PackID)

colnames(datGHA.all)[colnames(datGHA.all)=="MBts"] <- "datetime"

datGHA.all <- datGHA.all[!is.na(WolfID)]

# ID forays
# make naming conventions consistent
dat.forays <- dat.forays[,WolfID := stringr::str_remove(Animal_ID, pattern = "ER_")]
dat.forays[,WolfID := gsub("_", "", WolfID)]

# make a date column
datGHA.all[,date:=(as.IDate(datetime))]

datGHA.foray <- datGHA.all |>
  left_join(dat.forays[Study_Site == 'GHA26',.(WolfID, Foray_Start, Foray_End, foray)],
            by = join_by(WolfID, between(date, Foray_Start, Foray_End)))


# gather needed columns
datRMNP2 <- datRMNP.foray[,.(WolfID, PackID, pop = "RMNP", year = Year, datetime, x = X, y = Y, foray)]
datGHA2 <- datGHA.foray[,.(WolfID, PackID, pop = "GHA26", year, datetime, x, y, foray)]

# actually combine
dat.all <- rbind(datRMNP2, datGHA2)
dat.all[,id:= paste(WolfID, pop, sep = '_')]

dat <- na.omit(unique(dat.all, by = c('id', 'datetime')),
                        cols = c('x', 'y', 'datetime'))
dat[, season := ifelse(month(datetime) >= 5 & month(datetime) <= 10, 'SnowFree', 'Snow')]


#### making tracks and steps ####

trk <- dat[is.na(foray)] %>% make_track(x,y, datetime, crs = crs14, all_cols = T) %>%
  nest(data = -'id') %>%
  mutate(steps = map(data, function(x) 
    x %>% track_resample(rate = hours(2), tolerance = minutes(5)) %>%
      steps_by_burst(keep_cols = 'end'))) %>%
  select(id, steps) %>% unnest(cols = steps)


## can't make distributions at the pop level... 
distr <- dat[is.na(foray)] %>% make_track(x,y, datetime, crs = crs14, all_cols = T) %>%
  track_resample(rate = hours(2), tolerance = minutes(5)) %>%
  amt::filter_min_n_burst(min_n = 3) %>%
  steps_by_burst() %>%
  filter(., sl_ <= 35000) %>%
  random_steps() %>%
  sl_distr_params()
# distr2 <- trk %>%
#   filter(., sl_ <= 50000) %>%
#   random_steps() %>%
#   sl_distr_params()
distr$shape*distr$scale

distr.RMNP <- dat[pop=='RMNP'&is.na(foray)] %>% make_track(x,y, datetime, crs = crs14, all_cols = T) %>%
  track_resample(rate = hours(2), tolerance = minutes(5)) %>%
  amt::filter_min_n_burst(min_n = 3) %>%
  steps_by_burst() %>%
  filter(., sl_ <= 35000) %>%
  random_steps() %>%
  sl_distr_params()
distr.RMNP$shape*distr.RMNP$scale

distr.RMNP.snow <- dat[pop=='RMNP' & season == 'Snow'&is.na(foray)] %>% make_track(x,y, datetime, crs = crs14, all_cols = T) %>%
  track_resample(rate = hours(2), tolerance = minutes(5)) %>%
  amt::filter_min_n_burst(min_n = 3) %>%
  steps_by_burst() %>%
  filter(., sl_ <= 35000) %>%
  random_steps() %>%
  sl_distr_params()
distr.RMNP.snow$shape*distr.RMNP.snow$scale

distr.RMNP.snowfree <- dat[pop=='RMNP' & season == 'SnowFree'&is.na(foray)] %>% make_track(x,y, datetime, crs = crs14, all_cols = T) %>%
  track_resample(rate = hours(2), tolerance = minutes(5)) %>%
  amt::filter_min_n_burst(min_n = 3) %>%
  steps_by_burst() %>%
  filter(., sl_ <= 35000) %>%
  random_steps() %>%
  sl_distr_params()
distr.RMNP.snowfree$shape*distr.RMNP.snowfree$scale


distr.GHA26 <- dat[pop=='GHA26'&is.na(foray)] %>% make_track(x,y, datetime, crs = crs14, all_cols = T) %>%
  track_resample(rate = hours(2), tolerance = minutes(5)) %>%
  amt::filter_min_n_burst(min_n = 3) %>%
  steps_by_burst() %>%
  filter(., sl_ <= 35000) %>%
  random_steps() %>%
  sl_distr_params()
distr.GHA26$shape*distr.GHA26$scale

distr.GHA26.snow <- dat[pop=='GHA26' & season == 'Snow'&is.na(foray)] %>% make_track(x,y, datetime, crs = crs14, all_cols = T) %>%
  track_resample(rate = hours(2), tolerance = minutes(5)) %>%
  amt::filter_min_n_burst(min_n = 3) %>%
  steps_by_burst() %>%
  filter(., sl_ <= 35000) %>%
  random_steps() %>%
  sl_distr_params()
distr.GHA26.snow$shape*distr.GHA26.snow$scale

distr.GHA26.snowfree <- dat[pop=='GHA26' & season == 'SnowFree'&is.na(foray)] %>% make_track(x,y, datetime, crs = crs14, all_cols = T) %>%
  track_resample(rate = hours(2), tolerance = minutes(5)) %>%
  amt::filter_min_n_burst(min_n = 3) %>%
  steps_by_burst() %>%
  filter(., sl_ <= 35000) %>%
  random_steps() %>%
  sl_distr_params()
distr.GHA26.snowfree$shape*distr.GHA26.snowfree$scale
#######

quantile(trk$sl_)
# 0%         25%         50%         75%        100% 
# 0.00000    28.83217   140.31807  1297.13463 89905.03123 
quantile(trk$sl_, probs = c(0.95, 0.99))
#   95%      99% 
#   4295.428 6942.282 

# remove erroneous long steps
trk.sub <- setDT(trk)[sl_ <= 25000]
quantile(trk.sub$sl_)
trk.sub[,.(quantile(sl_)), by = .(pop)]
trk.sub[,.(quantile(sl_)), by = .(pop, season)]

## plots ----
col_pal <- c("#440154","#21918c")

p.box <- ggplot(trk.sub, aes(season, sl_)) +
  geom_jitter(aes(color = season, alpha =0.25), show.legend = F) + 
  geom_boxplot(aes(fill = season), outlier.shape = NA) +
  scale_colour_manual(values = col_pal,labels=c('Snow', 'Snow free'), name = "Season") +
  scale_fill_manual(values = col_pal,labels=c('Snow', 'Snow free'), name = "Season") +
  facet_wrap(~pop) +
  theme_bw() +theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, linewidth=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", linewidth = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20)#,
   # legend.position=c(0.8, 0.9)
   ) 
p.box

ggplot(trk.sub, aes(season, sl_, color = season, fill = season)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.5,
    alpha = .02,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) +
  scale_colour_manual(values = col_pal,labels=c('Snow', 'Snow free'), name = "Season") +
  scale_fill_manual(values = col_pal,labels=c('Snow', 'Snow free'), name = "Season") +
  facet_wrap(~pop) +
  theme_bw() +theme_bw()  + theme(
    panel.background =element_rect(colour = "black", fill=NA, linewidth=1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, hjust = .98, vjust = -8),
    axis.line = element_line(colour = "black", linewidth = .1),
    axis.text.x = element_text(size=20),
    axis.title = element_text(size=20),
    axis.text.y = element_text(size=20),
    legend.title=element_text(size=20),
    legend.text = element_text(size = 20)#,
    # legend.position=c(0.8, 0.9)
  ) 



sum.pop <- trk.sub[, .(median = median(sl_, na.rm = T), 
                       mean = mean(sl_, na.rm = T), sd = sd(sl_, na.rm = T)), by = .(pop)]
ggplot(trk.sub, aes(sl_)) + 
  geom_density(aes(fill = pop), alpha = 0.4) +
  geom_vline(data = sum.pop, aes(xintercept=mean, color = pop)) +
  xlim(c(0, 10000)) +
  scale_colour_manual(values = col_pal,labels=c('GHA 26', 'RMNP'), name = "Study Area") +
  scale_fill_manual(values = col_pal,labels=c('GHA 26', 'RMNP'), name = "Study Area") +
  xlab("Step lengths (m)/2-hours")+
  theme_bw() + theme(
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

sum.pop.season <- trk.sub[, .(median = median(sl_, na.rm = T), 
                       mean = mean(sl_, na.rm = T),
                       sd = sd(sl_, na.rm = T)), by = .(pop, season)]

ggplot(trk.sub, aes(sl_)) + 
  geom_density(aes(fill = season), alpha = 0.4) +
  geom_vline(data = sum.pop.season, aes(xintercept=mean, color = season)) +
  xlim(c(0, 5000)) +
  scale_colour_manual(values = col_pal,labels=c('Snow', 'Snow free'), name = "Season") +
  scale_fill_manual(values = col_pal,labels=c('Snow', 'Snow free'), name = "Season") +
  facet_wrap(~pop) +  
  xlab("Step lengths (m)/2-hours")+
  theme_bw() + theme(
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
    legend.text = element_text(size = 20)#,
    #legend.position=c(0.8, 0.9
                      ) 
# I can't get this to align 
ggplot(trk.sub, aes(x = season, y = sl_, fill = season)) +
  # add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.2,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
   # width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  ) +
  ggdist::stat_dots(
    # ploting on left side
    side = "left",
    # adjusting position
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = 0.25
  ) +
  theme_bw() +
  ylim(c(0, 10000)) +
  scale_colour_manual(values = col_pal,labels=c('Snow', 'Snow free'), name = "Season") +
  scale_fill_manual(values = col_pal,labels=c('Snow', 'Snow free'), name = "Season") +
  facet_wrap(~pop) 
  
