########## LOAD DATA & PACKAGES #############
#devtools::install_github('https://github.com/tylerdrudolph/wmKDE')
libs <- c('dplyr','data.table', 'lubridate', 'ctmm', 'amt', 
          'scales', 'wmKDE', 'sf', 'terra')
lapply(libs, require, character.only=TRUE)

##These files were copied over to:
GHAlocs <- fread("~/Documents/Masters/Data/Wolf iSSA/Input/Wolf_Dataset_2014-2019_Updated_2023-04-21.csv")
GHAcollars <- fread("~/Documents/Masters/Data/Wolf GPS data/GHA26_wolf_collar_data_2014-2020.csv")
all_forays <- fread("DataGHA/Foray_Remove_aKDE.csv")

################### GHA ########################
##Clean Data
# Remove end date
GHAlocs <- merge(GHAlocs, GHAcollars[, c("Animal_ID", "Start_Date", "End_Date")],
                 by = "Animal_ID")

GHAlocs <- GHAlocs[Date >= Start_Date & Date < End_Date, ]

# fix ts (not currently in local time)
GHAlocs[, ts := as.POSIXct(paste(Date, ' ', Time), format = '%Y-%m-%d %H:%M:%S', tz = "Etc/GMT+6")]

# check if all observations are complete
all(complete.cases(GHAlocs)) 

# If True, no action required. If False:
#GHAlocs <- na.omit(GHAlocs, cols= 'ts', invert= FALSE)

# check for duplicated time stamps by ID
# If we have some duplicated time stamps, these need to be removed prior to creating a track.
any(duplicated(GHAlocs, by = c("ts", "Animal_ID")))
GHAlocs[, tsN := seq.int(.N), by = .(ts, Animal_ID)]
GHAlocs <- GHAlocs[!(tsN > 1)]

GHAlocs <- GHAlocs[order(Animal_ID, ts)]

# Check for fixes less than two hours apart
GHAlocs[, difTime := as.numeric(difftime(ts, lag(ts), unit = 'mins')), by = Animal_ID]
GHAlocs <- GHAlocs[!difTime < 115]

##Need an IDSeason
#Format the date column so I can pull out the Month and year
GHAlocs[, Year := year(ts)]
GHAlocs[, Month := month(ts)]

##Define seasons and create unique year/season identifiers
GHAlocs[, Season := ifelse(Month >= 5 & Month <= 10, 'SnowFree', 'Snow')]
#GHAlocs[, SeasonYr := as.factor(paste(Year, Season, sep = "_"))]

#Remove forays
GHA_clean <- merge(GHAlocs, all_forays[Study_Site == "GHA26", c("Animal_ID", "Season", "Foray_Start", "Foray_End")],
                   by = c("Animal_ID", "Season"), all = T)
GHA_clean <- GHA_clean[(Date < Foray_Start | Date > Foray_End) | is.na(Foray_Start), ]

## Create year-specific Pack labels
GHA_clean[, PackID := ifelse(Animal_ID == "ER_W_38", "Great-Falls-Lone", Pack)]
GHA_clean[, PackYr := as.factor(paste(gsub("[^[:alnum:]]+","-", PackID), Deployment_Year, sep = "_"))]
GHA_clean[, PackSeasonYr := as.factor(paste(PackYr, Season, sep = "_"))]
GHA_clean[, IndSeason := as.factor(paste(Animal_ID, Season, sep = "_"))]

## Filter to a pack and make a template raster
#crs <- 4326
GHA.tib <- as_tibble(GHA_clean) %>% make_track(.x = Longitude, .y = Latitude, 
                                               .t = ts, crs = 4326, all_cols = T)

# All packs
all_packs <- GHA_clean[, unique(PackYr)]
all_ids <- GHA_clean[, unique(Animal_ID)]
all_seasons <- GHA_clean[, unique(PackSeasonYr)]

rast_packs <- lapply(all_packs, function(pack) {
  trast <- make_trast(GHA.tib |> filter(PackYr == pack), res = 50)
})
names(rast_packs) <- all_packs

resolution <- 50

kde_indiv <- lapply(all_ids, function(id) {
  Wolf.ind <- GHA.tib |> filter(Animal_ID == id)
  Wolf.pack  <- unique(Wolf.ind$PackYr)
  W.akde <- hr_akde(Wolf.ind, model = fit_ctmm(Wolf.ind, "ou"),
                    trast = rast_packs[names(rast_packs) == Wolf.pack][[1]],
                    levels = c(0.5, 0.95))
  W.scale <- W.akde[['ud']]
  W.scale[] <- scales::rescale(values(W.scale)*resolution*resolution, 
                                 to = c(0, 1))
  return(W.scale)
})
names(kde_indiv) <- all_ids

pack_akdes <- lapply(all_packs, function(focal_pack) {
  # Subset akdes for each focal pack
  Wolf.pack <- GHA.tib |> filter(PackYr == focal_pack)
  Wolf.ind  <- unique(Wolf.pack$Animal_ID)
  wolf.stack <- kde_indiv[names(kde_indiv) %in% Wolf.ind]
  names(wolf.stack) <- c("x", rep(NULL, length(wolf.stack)-1))
  # Average and rescale
  pack.dens = do.call(terra::mean, append(wolf.stack, list(na.rm = TRUE)))
  values(pack.dens) <- values(pack.dens) / sum(values(pack.dens)) / resolution / resolution
  #pack.dens$PackID <- focal_pack
  return(pack.dens)
})

names(pack_akdes) <- all_packs

#pack <- pack_akdes[[1]]
#i <- "Gem-Flintstone_2014"

iso_95 <- lapply(seq_along(pack_akdes), function(i) {
  pack <- pack_akdes[[i]]
  nm <- names(pack_akdes)[[i]]
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
  mypath <- file.path("KDE_HomeRanges/GHAPack/95UD", paste("aKDE", nm, "shp", sep = "."))
  writeVector(gf95, mypath, overwrite = T)
})

iso_50 <- lapply(seq_along(pack_akdes), function(i) {
  pack <- pack_akdes[[i]]
  nm <- names(pack_akdes)[[i]]
  gf50 <- vect(hr_isopleths(pack, levels = 0.50)$geometry)
  mypath <- file.path("KDE_HomeRanges/GHAPack/50UD", paste("aKDE_50", nm, "shp", sep = "."))
  writeVector(gf50, mypath, overwrite = T)
})

iso_95 <- lapply(pack_akdes, function(pack) {
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
})


iso_50 <- lapply(pack_akdes, function(pack) {
  gf50 <- vect(hr_isopleths(pack, levels = 0.5)$geometry)
})


akde_area <- lapply(pack_akdes, function(pack) {
  gf95.area <- hr_isopleths(pack, levels = 0.95)$area
})


area.dt <- data.table(do.call(cbind, akde_area))
area.dt <- as.data.table(gather(area.dt, columnNames, values))
area.dt[, area.95 := values/1000000]
area.dt[, c("Pack", "Year") := tstrsplit(columnNames, "_", fixed=TRUE)]
area.dt[, Season := "Year-round"]

fwrite(area.dt, file = "Output/GHA26_aKDE_areas_year-round.csv")

### Seasonal ###
all_ids_seasons <- GHA_clean[, unique(IndSeason)]
all_seasons <- GHA_clean[, unique(PackSeasonYr)]

rast_packs_season <- lapply(all_seasons, function(pack) {
  trast <- make_trast(GHA.tib |> filter(PackSeasonYr == pack), res = 50)
})
names(rast_packs_season) <- all_seasons

#id <- "ER_W_01_Snow"

kde_indiv_season <- lapply(all_ids_seasons, function(id) {
  Wolf.ind <- GHA.tib |> filter(IndSeason == id)
  Wolf.pack  <- unique(Wolf.ind$PackSeasonYr)
  W.akde <- hr_akde(Wolf.ind, model = fit_ctmm(Wolf.ind, "ou"),
                    trast = rast_packs_season[names(rast_packs_season) == Wolf.pack][[1]],
                    levels = c(0.5, 0.95))
  W.scale <- W.akde[['ud']]
  W.scale[] <- scales::rescale(values(W.scale)*resolution*resolution, 
                               to = c(0, 1))
  return(W.scale)
})
names(kde_indiv_season) <- all_ids_seasons

season_akdes <- lapply(all_seasons, function(focal_pack) {
  # Subset akdes for each focal pack
  Wolf.pack <- GHA.tib |> filter(PackSeasonYr == focal_pack)
  Wolf.ind  <- unique(Wolf.pack$IndSeason)
  wolf.stack <- kde_indiv_season[names(kde_indiv_season) %in% Wolf.ind]
  names(wolf.stack) <- c("x", rep(NULL, length(wolf.stack)-1))
  # Average and rescale
  pack.dens = do.call(terra::mean, append(wolf.stack, list(na.rm = TRUE)))
  values(pack.dens) <- values(pack.dens) / sum(values(pack.dens)) / resolution / resolution
  return(pack.dens)
})

names(season_akdes) <- all_seasons

plot(season_akdes[[1]])

akde_area_season <- lapply(season_akdes, function(pack) {
  gf95.area <- hr_isopleths(pack, levels = 0.95)$area
})


area.season <- data.table(do.call(cbind, akde_area_season))
area.season <- as.data.table(gather(area.season, columnNames, values))
area.season[, area.95 := values/1000000]

area.season[, c("Pack", "Year", "Season") := tstrsplit(columnNames, "_", fixed=TRUE)]
fwrite(area.season, file = "Output/GHA26_aKDE_areas_seasonal.csv")

iso_95_season <- lapply(season_akdes, function(pack) {
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
})


iso_50 <- lapply(pack_akdes, function(pack) {
  gf50 <- vect(hr_isopleths(pack, levels = 0.5)$geometry)
})


################### RMNP ########################
##Clean Data
# load RMNP data

RMNPlocs <- fread("DataRMNP/RMNP-locdataAll.csv")

# fix ts (not currently in local time)
RMNPlocs[, ts := as.POSIXct(paste(Date, ' ', Time), format = '%Y-%m-%d %H:%M:%S', tz = "Etc/GMT+6")]

# check if all observations are complete
all(complete.cases(RMNPlocs$ts)) 

# If True, no action required. If False:
#RMNPlocs <- na.omit(RMNPlocs, cols= 'ts', invert= FALSE)

# check for duplicated time stamps by ID
# If we have some duplicated time stamps, these need to be removed prior to creating a track.
any(duplicated(RMNPlocs, by = c("ts", "WolfID")))
#GHAtest[, tsN := seq.int(.N), by = .(ts, Animal_ID)]
#GHAtest <- GHAtest[!(tsN > 1)]

RMNPlocs <- RMNPlocs[order(WolfID, ts)]

# Check for fixes less than two hours apart
RMNPlocs[, difTime := as.numeric(difftime(ts, lag(ts), unit = 'mins')), by = WolfID]
RMNPlocs <- RMNPlocs[!difTime < 115]
##Need an IDSeason
#Format the date column so I can pull out the Month and year
#GHAtest[, Year := year(ts)]
#GHAtest[, Month := month(ts)]

##Define seasons and create unique year/season identifiers
RMNPlocs[, Season := ifelse(Month >= 5 & Month <= 10, 'SnowFree', 'Snow')]
RMNPlocs[, SeasonYr := as.factor(paste(Year, Season, sep = ""))]
summary(RMNPlocs$SeasonYr)

#Remove forays
RMNPlocs[, Animal_ID := WolfID]
RMNP_clean <- merge(RMNPlocs, all_forays[Study_Site == "RMNP", c("Animal_ID", "Season", "Year", "Foray_Start", "Foray_End")],
                    by = c("Animal_ID", "Season", "Year"), all = T)
RMNP_clean <- RMNP_clean[(Date < Foray_Start | Date > Foray_End) | is.na(Foray_Start), ]

#test <- RMNP_clean[!(Date >= Foray_Start & Date <= Foray_End), ]

##combine pack and Year to get IDYr
RMNP_clean[, Deployment_Year := Year]
RMNP_clean[Animal_ID == "W14" | Animal_ID == "W15" | Animal_ID == "W19", Deployment_Year := "2017"]
RMNP_clean[, PackYr := as.factor(paste(PackID, Deployment_Year, sep = "_"))]
RMNP_clean[, PackSeasonYr := as.factor(paste(PackYr, Season, sep = "_"))]
RMNP_clean[, IndSeason := as.factor(paste(Animal_ID, Season, sep = "_"))]

## Filter to a pack and make a template raster
crs <- 4326
RMNP.tib <- as_tibble(RMNP_clean) %>% make_track(.x = Longitude, .y = Latitude, 
                                               .t = ts, crs = 4326, all_cols = T)

# All packs
rmnp_packs <- RMNP_clean[, unique(PackYr)]
rmnp_ids <- RMNP_clean[, unique(Animal_ID)]

rast_rmnp <- lapply(rmnp_packs, function(pack) {
  trast <- make_trast(RMNP.tib |> filter(PackYr == pack), res = 50)
})
names(rast_rmnp) <- rmnp_packs

resolution <- 50

kde_indiv_rmnp <- lapply(rmnp_ids, function(id) {
  Wolf.ind <- RMNP.tib |> filter(Animal_ID == id)
  Wolf.pack  <- unique(Wolf.ind$PackYr)
  W.akde <- hr_akde(Wolf.ind, model = fit_ctmm(Wolf.ind, "ou"),
                    trast = rast_rmnp[names(rast_rmnp) == Wolf.pack][[1]],
                    levels = c(0.5, 0.95))
  W.scale <- W.akde[['ud']]
  W.scale[] <- scales::rescale(values(W.scale)*resolution*resolution, 
                               to = c(0, 1))
  return(W.scale)
})
names(kde_indiv_rmnp) <- rmnp_ids


rmnp_pack_akdes <- lapply(rmnp_packs, function(focal_pack) {
  # Subset akdes for each focal pack
  Wolf.pack <- RMNP.tib |> filter(PackYr == focal_pack)
  Wolf.ind  <- unique(Wolf.pack$Animal_ID)
  wolf.stack <- kde_indiv_rmnp[names(kde_indiv_rmnp) %in% Wolf.ind]
  names(wolf.stack) <- c("x", rep(NULL, length(wolf.stack)-1))
  # Average and rescale
  pack.dens = do.call(terra::mean, append(wolf.stack, list(na.rm = TRUE)))
  values(pack.dens) <- values(pack.dens) / sum(values(pack.dens)) / resolution / resolution
  return(pack.dens)
})

names(rmnp_pack_akdes) <- rmnp_packs
plot(rmnp_pack_akdes[[5]])

iso_95 <- lapply(seq_along(rmnp_pack_akdes), function(i) {
  pack <- rmnp_pack_akdes[[i]]
  nm <- names(rmnp_pack_akdes)[[i]]
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
  mypath <- file.path("KDE_HomeRanges/RMNP_Pack/95UD", paste("aKDE", nm, "shp", sep = "."))
  writeVector(gf95, mypath, overwrite = T)
})

iso_50 <- lapply(seq_along(rmnp_pack_akdes), function(i) {
  pack <- rmnp_pack_akdes[[i]]
  nm <- names(rmnp_pack_akdes)[[i]]
  gf50 <- vect(hr_isopleths(pack, levels = 0.50)$geometry)
  mypath <- file.path("KDE_HomeRanges/RMNP_Pack/50UD", paste("aKDE_50", nm, "shp", sep = "."))
  writeVector(gf50, mypath, overwrite = T)
})


iso_95 <- lapply(rmnp_pack_akdes, function(pack) {
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
})

rmnp_area <- lapply(rmnp_pack_akdes, function(pack) {
  gf95.area <- hr_isopleths(pack, levels = 0.95)$area
})


area.rmnp <- data.table(do.call(cbind, rmnp_area))
area.rmnp <- as.data.table(gather(area.rmnp, columnNames, values))
area.rmnp[, area.95 := values/1000000]

fwrite(area.rmnp, file = "Output/RMNP_aKDE_areas_year-round.csv")

### Seasonal ###
rmnp_ids_seasons <- RMNP_clean[, unique(IndSeason)]
rmnp_seasons <- RMNP_clean[, unique(PackSeasonYr)]

rast_rmnp_season <- lapply(rmnp_seasons, function(pack) {
  trast <- make_trast(RMNP.tib |> filter(PackSeasonYr == pack), res = 50)
})
names(rast_rmnp_season) <- rmnp_seasons

#rmnp_ids_seasons <- "W10_Snow"
#id <- "W15_Snow"

kde_rmnp_season <- lapply(rmnp_ids_seasons, function(id) {
  #print(id)
  Wolf.ind <- RMNP.tib |> filter(IndSeason == id)
  Wolf.pack  <- unique(Wolf.ind$PackSeasonYr)
  W.akde <- hr_akde(Wolf.ind, model = fit_ctmm(Wolf.ind, "ou"),
                    trast = rast_rmnp_season[names(rast_rmnp_season) == Wolf.pack][[1]],
                    levels = c(0.5, 0.95))
  W.scale <- W.akde[['ud']]
  W.scale[] <- scales::rescale(values(W.scale)*resolution*resolution, 
                               to = c(0, 1))
  return(W.scale)
})
names(kde_rmnp_season) <- rmnp_ids_seasons

plot(kde_rmnp_season[[1]])

rmnp_season_akdes <- lapply(rmnp_seasons, function(focal_pack) {
  # Subset akdes for each focal pack
  Wolf.pack <- RMNP.tib |> filter(PackSeasonYr == focal_pack)
  Wolf.ind  <- unique(Wolf.pack$IndSeason)
  wolf.stack <- kde_rmnp_season[names(kde_rmnp_season) %in% Wolf.ind]
  names(wolf.stack) <- c("x", rep(NULL, length(wolf.stack)-1))
  # Average and rescale
  pack.dens = do.call(terra::mean, append(wolf.stack, list(na.rm = TRUE)))
  values(pack.dens) <- values(pack.dens) / sum(values(pack.dens)) / resolution / resolution
  return(pack.dens)
})

names(rmnp_season_akdes) <- rmnp_seasons

plot(rmnp_season_akdes[[1]])

rmnp_area_season <- lapply(rmnp_season_akdes, function(pack) {
  gf95.area <- hr_isopleths(pack, levels = 0.95)$area
})


rmnp.area.season <- data.table(do.call(cbind, rmnp_area_season))
rmnp.area.season <- as.data.table(gather(rmnp.area.season, columnNames, values))
rmnp.area.season[, area.95 := values/1000000]

rmnp.area.season[, c("Pack", "Year", "Season") := tstrsplit(columnNames, "_", fixed=TRUE)]

fwrite(rmnp.area.season, file = "Output/RMNP_aKDE_areas_seasonal.csv")

