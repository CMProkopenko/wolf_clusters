### Movement summary ====
# Julie Turner
# Started: 21 September 2023

### Packages ----
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'terra', 'sf', 'tidyr', 'ggplot2', 'patchwork')
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

#### RMNP ----
datRMNP <- fread(file.path(raw, 'RMNPwolf_rarified.csv'))
datRMNP$datetime <- paste(datRMNP$gmtDate, datRMNP$gmtTime)
datRMNP$datetime <- as.POSIXct(datRMNP$datetime, tz = 'UTC', "%Y-%m-%d %H:%M:%S")
datRMNP[,.(min=min(gmtDate),max=max(gmtDate)), by=.(WolfID)]


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

# utm zone 14n
# wgs84 32614
# nad83 26914
# project raster
crs <- st_crs(4326)$wkt
utm14N <- "+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs14 <- sf::st_crs(32614)

set.seed(57)

datGHA[, Longitude := as.numeric(Longitude)]
datGHA[, Latitude := as.numeric(Latitude)]

sfboo <- st_as_sf(datGHA, coords = c('Longitude', 'Latitude'),
                  crs = crs)
outboo <- st_transform(sfboo, crs14)
datGHA <- setDT(sfheaders::sf_to_df(outboo, fill = T))


datGHA[,'year'] <- as.numeric(format(datGHA$MBts,'%Y'))

collarID <- fread(file.path(raw, 'GHA26_coll_wolf_id.csv'), header = T)
collarID$Collar_ID <- as.character(collarID$Collar_ID)


datGHA.all <- merge(datGHA, collarID, by = c('Collar_ID', 'year'), all.x = T)
datGHA.all[,'WolfID'] <- ifelse(datGHA.all$Collar_ID=='5148', 'W17', datGHA.all$WolfID)
datGHA.all[,'PackID'] <- ifelse(datGHA.all$Collar_ID=='5148', 'MA', datGHA.all$PackID)

colnames(datGHA.all)[colnames(datGHA.all)=="MBts"] <- "datetime"

datGHA.all <- datGHA.all[!is.na(WolfID)]


# gather needed columns
datRMNP2 <- datRMNP[,.(WolfID, PackID, pop = "RMNP", year = Year, datetime, x = X, y = Y)]
datGHA2 <- datGHA.all[,.(WolfID, PackID, pop = "GHA26", year, datetime, x, y)]

# actually combine
dat.all <- rbind(datRMNP2, datGHA2)
dat.all[,id:= paste(WolfID, pop, sep = '_')]

dat <- na.omit(unique(dat.all, by = c('id', 'datetime')),
                        cols = c('x', 'y', 'datetime'))


#### making tracks and steps ####

trk <- dat %>% make_track(x,y, datetime, crs = crs14, all_cols = T) %>%
  track_resample(rate = hours(2), tolerance = minutes(5)) %>%
  amt::filter_min_n_burst(min_n = 3) %>%
  steps_by_burst(keep_cols = 'end')

quantile(trk$sl_)
quantile(trk$sl_, probs = c(0.95, 0.99))
  
trk.sub <- setDT(trk)[sl_ <= 50000]
## plots ----
col_pal <- c("#440154","#21918c")

ggplot(trk.sub, aes(pop, sl_)) +
  #geom_jitter(aes(group = PackID, color = pop)) + 
  geom_boxplot(aes(group = PackID, color = pop), outlier.shape = NA) +
  scale_colour_manual(values = col_pal,labels=c('GHA 26', 'RMNP'), name = "Study Area") +
  scale_fill_manual(values = col_pal,labels=c('GHA 26', 'RMNP'), name = "Study Area") +
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
    legend.text = element_text(size = 20)#,
   # legend.position=c(0.8, 0.9)
   ) 

ggplot(trk.sub[sl_<=10000], aes(sl_)) + 
  geom_density(aes(fill = pop), alpha = 0.4) +
  scale_colour_manual(values = col_pal,labels=c('GHA 26', 'RMNP'), name = "Study Area") +
  scale_fill_manual(values = col_pal,labels=c('GHA 26', 'RMNP'), name = "Study Area") +
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

ggplot(trk.sub[sl_<=10000], aes(sl_)) + 
  geom_density(aes(fill = PackID), alpha = 0.4) +
  # scale_colour_manual(values = col_pal,labels=c('GHA 26', 'RMNP'), name = "Study Area") +
  # scale_fill_manual(values = col_pal,labels=c('GHA 26', 'RMNP'), name = "Study Area") +
  facet_wrap(~pop) +  
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
