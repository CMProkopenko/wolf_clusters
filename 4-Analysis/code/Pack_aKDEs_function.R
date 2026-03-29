########## LOAD DATA & PACKAGES #############
#devtools::install_github('https://github.com/tylerdrudolph/wmKDE')
libs <- c('dplyr','data.table', 'lubridate', 'ctmm', 'amt', 'ggplot2',
          'scales', 'wmKDE', 'sf', 'terra', "tidyr")
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
GHA_clean <- merge(GHAlocs, all_forays[Study_Site == "GHA26", c("Animal_ID", "Season", "Year","Month", "Foray_Start", "Foray_End")],
                   by = c("Animal_ID", "Season", "Year", "Month"), all = T)
GHA_clean <- GHA_clean[(Date < Foray_Start | Date > Foray_End) | is.na(Foray_Start), ]

## Create year-specific Pack labels
GHA_clean[, PackID := ifelse(Animal_ID == "ER_W_38", "Great-Falls-Lone", Pack)]
GHA_clean[, PackYr := as.factor(paste(gsub("[^[:alnum:]]+","-", PackID), Deployment_Year, sep = "_"))]
GHA_clean[, PackSeasonYr := as.factor(paste(PackYr, Season, sep = "_"))]
GHA_clean[, IndSeason := as.factor(paste(Animal_ID, Season, sep = "_"))]

## Double check forays have been removed correctly
GHA_locs_sf <- st_as_sf(GHA_clean, coords = c("Easting", "Northing"), crs = 32614)
GHA26 <- st_read("~/Documents/Masters/Data/GIS/EasternMB_Files_RSF/Study Area/Study_area_Eastern_MB.shp")
GHA26 <- st_transform(GHA26, 32614)

ggplot() +
  geom_sf(data = GHA26) + 
  geom_sf(data = GHA_locs_sf, alpha = 0.1) + 
  facet_wrap(~ Animal_ID)

#st_write(GHA_locs_sf %>% filter(Animal_ID == "ER_W_26"), dsn = "Output/ER_W_26_locs.shp")

## Filter to a pack and make a template raster
#crs <- 32614
GHA.tib <- as_tibble(GHA_clean) %>% make_track(.x = Easting, .y = Northing, 
                                               .t = ts, crs = 32614, all_cols = T)

# All packs
all_packs <- GHA_clean[, unique(PackYr)]
all_ids <- GHA_clean[, unique(Animal_ID)]
all_seasons <- GHA_clean[, unique(PackSeasonYr)]

rast_packs <- lapply(all_packs, function(pack) {
  trast <- make_trast(GHA.tib |> filter(PackYr == pack), res = 50)
})
names(rast_packs) <- all_packs

#all_ids <- "ER_W_01"
resolution <- 50

kde_indiv_gha <- lapply(all_ids, function(id) {
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
names(kde_indiv_gha) <- all_ids

## Plot to check
akde_list <- lapply(kde_indiv_gha, function(indiv) {
  gf95 <- vect(hr_isopleths(indiv, levels = 0.95)$geometry)
})

sf_list_indiv <- mapply(function(spat, name) {
  st_as_sf(spat) %>%
    mutate(Animal_ID = name)
}, akde_list, names(akde_list), SIMPLIFY = FALSE)

# Combine into one sf object
iso_95_sf_indiv <- bind_rows(sf_list_indiv)

ggplot() + 
  geom_sf(data = GHA26) + 
  geom_sf(data = GHA_locs_sf, 
          aes(colour = Animal_ID), alpha = 0.1) + 
  geom_sf(data = iso_95_sf_indiv, fill = NA, linewidth = 2) +
  facet_wrap(~ Animal_ID) + 
  theme_bw()

## Save shapefiles

iso_95_indiv <- lapply(seq_along(kde_indiv), function(i) {
  pack <- kde_indiv[[i]]
  nm <- names(kde_indiv)[[i]]
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
  mypath <- file.path("KDE_HomeRanges/GHAIndividual/95UD", paste(nm, "_aKDE_95", ".shp", sep=""))
  writeVector(gf95, mypath, overwrite = T)
})

iso_50_indiv <- lapply(seq_along(kde_indiv_rmnp), function(i) {
  pack <- kde_indiv_rmnp[[i]]
  nm <- names(kde_indiv_rmnp)[[i]]
  gf50 <- vect(hr_isopleths(pack, levels = 0.50)$geometry)
  mypath <- file.path("KDE_HomeRanges/RMNPIndividual/50UD", paste(nm, "_aKDE_50", ".shp", sep=""))
  writeVector(gf50, mypath, overwrite = T)
})

## Calculate area
gha_indiv_area <- lapply(kde_indiv_gha, function(id) {
  gf95.area <- hr_isopleths(id, levels = 0.95)$area
})

area.gha.indiv <- data.table(do.call(cbind, gha_indiv_area))
area.gha.indiv <- as.data.table(gather(area.gha.indiv, columnNames, values))
area.gha.indiv[, area.95 := values/1000000]

fwrite(area.gha.indiv, file = "KDE_HomeRanges/GHAIndividual/GHA_indiv_aKDE_areas_year-round.csv")

## Pack Function
#focal_pack <- "Gem-Flintstone_2014"

gha_pack_akdes <- lapply(all_packs, function(focal_pack) {
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

names(gha_pack_akdes) <- all_packs

# Plot to check
akde_list <- lapply(gha_pack_akdes, function(pack) {
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
})

sf_list <- mapply(function(spat, name) {
  parts <- str_split(name, "_", simplify = TRUE)
  pack <- parts[1]
  year <- parts[2]
  
  st_as_sf(spat) %>%
    mutate(PackID = pack, Year_plot = year)
}, akde_list, names(akde_list), SIMPLIFY = FALSE)

# Combine into one sf object
iso_95_sf <- bind_rows(sf_list)

GHA_locs_sf$PackID <- sub(" ", "-", GHA_locs_sf$PackID)

ggplot() + 
  geom_sf(data = GHA26) + 
  geom_sf(data = GHA_locs_sf %>% mutate(Year_plot = Deployment_Year), 
          aes(colour = Animal_ID), alpha = 0.1) + 
  geom_sf(data = iso_95_sf, fill = NA, size = 2) +
  facet_wrap(~ PackID + Year_plot) + 
  theme_bw()

## Save shapefiles

#pack <- pack_akdes[[1]]
#i <- "Gem-Flintstone_2014"

iso_95 <- lapply(seq_along(gha_pack_akdes), function(i) {
  pack <- gha_pack_akdes[[i]]
  nm <- names(gha_pack_akdes)[[i]]
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
  mypath <- file.path("KDE_HomeRanges/GHAPack/95UD", paste(nm, "_aKDE_95", ".shp", sep = ""))
  writeVector(gf95, mypath, overwrite = T)
})

iso_50 <- lapply(seq_along(gha_pack_akdes), function(i) {
  pack <- gha_pack_akdes[[i]]
  nm <- names(gha_pack_akdes)[[i]]
  gf50 <- vect(hr_isopleths(pack, levels = 0.50)$geometry)
  mypath <- file.path("KDE_HomeRanges/GHAPack/50UD", paste(nm, "_aKDE_50", ".shp", sep = ""))
  writeVector(gf50, mypath, overwrite = T)
})

# Calculate Area
gha_akde_area <- lapply(gha_pack_akdes, function(pack) {
  gf95.area <- hr_isopleths(pack, levels = 0.95)$area
})

gha.area.dt <- data.table(do.call(cbind, gha_akde_area))
gha.area.dt <- as.data.table(gather(gha.area.dt, columnNames, values))
gha.area.dt[, area.95 := values/1000000]
gha.area.dt[, c("Pack", "Year") := tstrsplit(columnNames, "_", fixed=TRUE)]
gha.area.dt[, Season := "Year-round"]

fwrite(gha.area.dt, file = "KDE_HomeRanges/GHAPack/GHA26_Pack_aKDE_areas_year-round.csv")

### Seasonal ###
all_ids_seasons <- GHA_clean[, unique(IndSeason)]
all_seasons <- GHA_clean[, unique(PackSeasonYr)]

rast_packs_season <- lapply(all_seasons, function(pack) {
  trast <- make_trast(GHA.tib |> filter(PackSeasonYr == pack), res = 50)
})
names(rast_packs_season) <- all_seasons

#id <- "ER_W_01_Snow"
resolution <- 50

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

## Plot to check
akde_list <- lapply(kde_indiv_season, function(indiv) {
  gf95 <- vect(hr_isopleths(indiv, levels = 0.95)$geometry)
})

sf_list_indiv <- mapply(function(spat, name) {
  parts <- strsplit(name, "_")[[1]]  
  id <- paste(parts[1:3], collapse = "_")
  season <- parts[4]
  
  st_as_sf(spat) %>%
    mutate(Animal_ID = id, Season = season)
}, akde_list, names(akde_list), SIMPLIFY = FALSE)

# Combine into one sf object
iso_95_sf_indiv <- bind_rows(sf_list_indiv)

ggplot() + 
  geom_sf(data = GHA26) + 
  geom_sf(data = GHA_locs_sf, 
          aes(colour = Animal_ID), alpha = 0.1) + 
  geom_sf(data = iso_95_sf_indiv, aes(linetype = Season),fill = NA, linewidth = 1) +
  facet_wrap(~ Animal_ID) + 
  theme_bw()

## Save shapefiles

iso_95_indiv <- lapply(seq_along(kde_indiv_season), function(i) {
  pack <- kde_indiv_season[[i]]
  nm <- names(kde_indiv_season)[[i]]
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
  mypath <- file.path("KDE_HomeRanges/GHAIndividual_Season/95UD", paste(nm, "_aKDE_95", ".shp", sep=""))
  writeVector(gf95, mypath, overwrite = T)
})

iso_50_indiv <- lapply(seq_along(kde_indiv_season), function(i) {
  pack <- kde_indiv_season[[i]]
  nm <- names(kde_indiv_season)[[i]]
  gf50 <- vect(hr_isopleths(pack, levels = 0.50)$geometry)
  mypath <- file.path("KDE_HomeRanges/GHAIndividual_Season/50UD", paste(nm, "_aKDE_50", ".shp", sep=""))
  writeVector(gf50, mypath, overwrite = T)
})

## Calculate area
gha_indiv_area <- lapply(kde_indiv_season, function(id) {
  gf95.area <- hr_isopleths(id, levels = 0.95)$area
})

area.gha.indiv <- data.table(do.call(cbind, gha_indiv_area))
area.gha.indiv <- as.data.table(gather(area.gha.indiv, columnNames, values))
area.gha.indiv[, area.95 := values/1000000]
area.gha.indiv[, `:=`(Animal_ID = sapply(strsplit(columnNames, "_"), function(x) paste(x[1:3], collapse = "_")),
          Season = sapply(strsplit(columnNames, "_"), function(x) x[4]))]

fwrite(area.gha.indiv, file = "KDE_HomeRanges/GHAIndividual_Season/GHA_indiv_aKDE_areas_seasonal.csv")

## Pack Function
gha_season_akdes <- lapply(all_seasons, function(focal_pack) {
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

names(gha_season_akdes) <- all_seasons

## Plot to check
akde_list <- lapply(gha_season_akdes, function(pack) {
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
})

sf_list <- mapply(function(spat, name) {
  name_clean <- str_remove(name, "^\\$")  # remove leading $ if present
  parts <- str_split(name_clean, "_", simplify = TRUE)
  
  st_as_sf(spat) %>%
    mutate(
      PackID = parts[1],
      Year_plot = parts[2],
      Season = parts[3]
    )
}, akde_list, names(akde_list), SIMPLIFY = FALSE)

# Combine all into one sf object
season_akde_95 <- bind_rows(sf_list)

ggplot() + 
  geom_sf(data = GHA26) + 
  geom_sf(data = GHA_locs_sf %>% mutate(Year_plot = Deployment_Year), 
          aes(colour = Animal_ID), alpha = 0.1) + 
  geom_sf(data = season_akde_95, fill = NA, size = 2) +
  facet_wrap(~ PackID + Year_plot + Season) + 
  theme_bw()

## Save seasonal pack aKDEs
iso_95 <- lapply(seq_along(gha_season_akdes), function(i) {
  pack <- gha_season_akdes[[i]]
  nm <- names(gha_season_akdes)[[i]]
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
  mypath <- file.path("KDE_HomeRanges/GHAPack_Season/95UD", paste(nm, "_aKDE_95", ".shp", sep = ""))
  writeVector(gf95, mypath, overwrite = T)
})

iso_50 <- lapply(seq_along(gha_season_akdes), function(i) {
  pack <- gha_season_akdes[[i]]
  nm <- names(gha_season_akdes)[[i]]
  gf50 <- vect(hr_isopleths(pack, levels = 0.50)$geometry)
  mypath <- file.path("KDE_HomeRanges/GHAPack_Season/50UD", paste(nm, "_aKDE_95", ".shp", sep = ""))
  writeVector(gf50, mypath, overwrite = T)
})

## Calculate area

akde_area_season <- lapply(gha_season_akdes, function(pack) {
  gf95.area <- hr_isopleths(pack, levels = 0.95)$area
})

area.season <- data.table(do.call(cbind, akde_area_season))
area.season <- as.data.table(gather(area.season, columnNames, values))
area.season[, area.95 := values/1000000]

area.season[, c("Pack", "Year", "Season") := tstrsplit(columnNames, "_", fixed=TRUE)]
#fwrite(area.season, file = "KDE_HomeRanges/GHAPack_Season/GHA26_pack_aKDE_areas_seasonal.csv")

################### RMNP ########################
##Clean Data
# load RMNP data
RMNPlocs <- fread("DataRMNP/RMNP-locdataAll.csv")
RMNP_collars <- fread("DataRMNP/RMNP_WolfCollars_2020update.csv")
colnames(RMNP_collars) <- gsub(" ", "_", colnames(RMNP_collars))
colnames(RMNP_collars) <- gsub("/", "_", colnames(RMNP_collars))
RMNP_collars[, Drop_Date := gsub("/", "-", Drop_Off_Mort_Date)]
RMNP_collars[, Drop_Date := as.IDate(Drop_Date, format = "%d-%b-%y")]
RMNP_collars[, Animal_ID := ifelse(Recap_Ear_Tag == "W17" | Recap_Ear_Tag == "W18", 
                                   Recap_Ear_Tag, Ear_Tag)]

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

##Define seasons and create unique year/season identifiers
RMNPlocs[, Season := ifelse(Month >= 5 & Month <= 10, 'SnowFree', 'Snow')]
RMNPlocs[, SeasonYr := as.factor(paste(Year, Season, sep = ""))]
summary(RMNPlocs$SeasonYr)

#Remove forays
RMNPlocs[, Animal_ID := WolfID]
RMNP_clean <- merge(RMNPlocs, all_forays[Study_Site == "RMNP", c("Animal_ID", "Season", "Year", "Month",
                                                                 "Foray_Start", "Foray_End")],
                    by = c("Animal_ID", "Season", "Year", "Month"), all = T)
RMNP_clean <- RMNP_clean[(Date < Foray_Start | Date > Foray_End) | is.na(Foray_Start), ]

# Remove mortalities
RMNP_clean <- merge(RMNP_clean, RMNP_collars[, c("Animal_ID", "Drop_Date")], by = "Animal_ID")
RMNP_clean <- RMNP_clean[Date < Drop_Date]

#View(RMNP_clean[!is.na(Foray_Start)])
#RMNP_clean[, range(Date), by = WolfID]

##combine pack and Year to get IDYr
RMNP_clean[, Deployment_Year := ifelse((Animal_ID == "W14" | Animal_ID == "W15" | Animal_ID == "W19"),
                                       2017, Year)]
RMNP_clean[, PackYr := as.factor(paste(PackID, Deployment_Year, sep = "_"))]
RMNP_clean[, PackSeasonYr := as.factor(paste(PackYr, Season, sep = "_"))]
RMNP_clean[, IndSeason := as.factor(paste(Animal_ID, Season, sep = "_"))]

## Double check forays have been removed correctly
RMNP_locs_sf <- st_as_sf(RMNP_clean, coords = c("X", "Y"), crs = 32614)
RMNP <- st_read("DataRMNP/RMNP_StudyArea/rmnp2.shp") %>% st_transform(crs = 32614)

ggplot() +
  geom_sf(data = RMNP) + 
  geom_sf(data = RMNP_locs_sf, alpha = 0.1) + 
  facet_wrap(~ Animal_ID)

## Filter to a pack and make a template raster
crs <- 32614
RMNP.tib <- as_tibble(RMNP_clean) %>% make_track(.x = X, .y = Y, 
                                               .t = ts, crs = 32614, all_cols = T)

# All packs
rmnp_packs <- RMNP_clean[, unique(PackYr)]
rmnp_ids <- RMNP_clean[, unique(Animal_ID)]

rast_rmnp <- lapply(rmnp_packs, function(pack) {
  trast <- make_trast(RMNP.tib |> filter(PackYr == pack), res = 50)
})
names(rast_rmnp) <- rmnp_packs

resolution <- 50

#rmnp_ids <- "W14"

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

## Plot to check
akde_list <- lapply(kde_indiv_rmnp, function(indiv) {
  gf95 <- vect(hr_isopleths(indiv, levels = 0.95)$geometry)
})

sf_list_indiv <- mapply(function(spat, name) {
  st_as_sf(spat) %>%
    mutate(Animal_ID = name)
}, akde_list, names(akde_list), SIMPLIFY = FALSE)

# Combine into one sf object
iso_95_sf_indiv <- bind_rows(sf_list_indiv)

ggplot() + 
  geom_sf(data = RMNP) + 
  geom_sf(data = RMNP_locs_sf, 
          aes(colour = Animal_ID), alpha = 0.1) + 
  geom_sf(data = iso_95_sf_indiv, fill = NA, linewidth = 2) +
  facet_wrap(~ Animal_ID) + 
  theme_bw()

## Save shapefiles

iso_95_indiv <- lapply(seq_along(kde_indiv_rmnp), function(i) {
  pack <- kde_indiv_rmnp[[i]]
  nm <- names(kde_indiv_rmnp)[[i]]
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
  mypath <- file.path("KDE_HomeRanges/RMNPIndividual/95UD", paste(nm, "_aKDE_95", ".shp", sep=""))
  writeVector(gf95, mypath, overwrite = T)
})

iso_50_indiv <- lapply(seq_along(kde_indiv_rmnp), function(i) {
  pack <- kde_indiv_rmnp[[i]]
  nm <- names(kde_indiv_rmnp)[[i]]
  gf50 <- vect(hr_isopleths(pack, levels = 0.50)$geometry)
  mypath <- file.path("KDE_HomeRanges/RMNPIndividual/50UD", paste(nm, "_aKDE_50", ".shp", sep=""))
  writeVector(gf50, mypath, overwrite = T)
})

## Calculate area
rmnp_indiv_area <- lapply(kde_indiv_rmnp, function(pack) {
  gf95.area <- hr_isopleths(pack, levels = 0.95)$area
})

area.rmnp.indiv <- data.table(do.call(cbind, rmnp_indiv_area))
area.rmnp.indiv <- as.data.table(gather(area.rmnp.indiv, columnNames, values))
area.rmnp.indiv[, area.95 := values/1000000]

#fwrite(area.rmnp.indiv, file = "KDE_HomeRanges/RMNPIndividual/RMNP_indiv_aKDE_areas_year-round.csv")

### Pack Function
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
#plot(rmnp_pack_akdes[[5]])

# Plot to check
akde_list <- lapply(rmnp_pack_akdes, function(pack) {
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
})

sf_list <- mapply(function(spat, name) {
  parts <- str_split(name, "_", simplify = TRUE)
  pack <- parts[1]
  year <- parts[2]
  
  st_as_sf(spat) %>%
    mutate(PackID = pack, Year_plot = year)
}, akde_list, names(akde_list), SIMPLIFY = FALSE)

# Combine into one sf object
iso_95_sf <- bind_rows(sf_list)

ggplot() + 
  geom_sf(data = RMNP) + 
  geom_sf(data = RMNP_locs_sf %>% mutate(Year_plot = Deployment_Year), 
          aes(colour = Animal_ID), alpha = 0.1) + 
  geom_sf(data = iso_95_sf, fill = NA, size = 2) +
  facet_wrap(~ PackID + Year_plot) + 
  theme_bw()

## Save shapefiles

iso_95 <- lapply(seq_along(rmnp_pack_akdes), function(i) {
  pack <- rmnp_pack_akdes[[i]]
  nm <- names(rmnp_pack_akdes)[[i]]
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
  mypath <- file.path("KDE_HomeRanges/RMNPPack/95UD", paste(nm, "_aKDE_95", ".shp", sep = ""))
  writeVector(gf95, mypath, overwrite = T)
})

iso_50 <- lapply(seq_along(rmnp_pack_akdes), function(i) {
  pack <- rmnp_pack_akdes[[i]]
  nm <- names(rmnp_pack_akdes)[[i]]
  gf50 <- vect(hr_isopleths(pack, levels = 0.50)$geometry)
  mypath <- file.path("KDE_HomeRanges/RMNPPack/50UD", paste(nm, "_aKDE_50", ".shp", sep = ""))
  writeVector(gf50, mypath, overwrite = T)
})


## Calculate area
rmnp_area <- lapply(rmnp_pack_akdes, function(pack) {
  gf95.area <- hr_isopleths(pack, levels = 0.95)$area
})

area.rmnp <- data.table(do.call(cbind, rmnp_area))
area.rmnp <- as.data.table(gather(area.rmnp, columnNames, values))
area.rmnp[, area.95 := values/1000000]
area.rmnp[, c("Pack", "Year") := tstrsplit(columnNames, "_", fixed=TRUE)]
area.rmnp[, Season := "Year-round"]

fwrite(area.rmnp, file = "KDE_HomeRanges/RMNPPack/RMNP_pack_aKDE_areas_year-round.csv")

### Seasonal ###
rmnp_ids_seasons <- RMNP_clean[, unique(IndSeason)]
rmnp_seasons <- RMNP_clean[, unique(PackSeasonYr)]

rast_rmnp_season <- lapply(rmnp_seasons, function(pack) {
  trast <- make_trast(RMNP.tib |> filter(PackSeasonYr == pack), res = 50)
})
names(rast_rmnp_season) <- rmnp_seasons

#rmnp_ids_seasons <- "W14_SnowFree"
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

## Plot to Check
akde_list <- lapply(kde_rmnp_season, function(indiv) {
  gf95 <- vect(hr_isopleths(indiv, levels = 0.95)$geometry)
})

akde_list <- lapply(kde_rmnp_season, function(indiv) {
  gf50 <- vect(hr_isopleths(indiv, levels = 0.50)$geometry)
})

sf_list_indiv <- mapply(function(spat, name) {
  parts <- str_split(name, "_", simplify = TRUE)
  id <- parts[1]
  season <- parts[2]
  
  st_as_sf(spat) %>%
    mutate(Animal_ID = id, Season = season)
}, akde_list, names(akde_list), SIMPLIFY = FALSE)

# Combine into one sf object
season_95_sf_indiv <- bind_rows(sf_list_indiv)
season_50_sf_indiv <- bind_rows(sf_list_indiv)

ggplot() + 
  geom_sf(data = RMNP) + 
  geom_sf(data = RMNP_locs_sf, aes(colour = Animal_ID), alpha = 0.5) + 
  #scale_color_viridis() + 
  geom_sf(data = season_95_sf_indiv, 
          linetype = "solid", fill = NA, linewidth = 1) +
  geom_sf(data = season_50_sf_indiv, 
          linetype = "dashed", fill = NA, linewidth = 1) +
  facet_wrap(~ Animal_ID + Season) + 
  theme_bw()

#st_write(RMNP_locs_sf %>% filter(Animal_ID == "W14" & Season == "Snow"), dsn = "Output/W14_Snow_locs.shp")

## Save seasonal pack aKDEs
indiv_95_seaon <- lapply(seq_along(kde_rmnp_season), function(i) {
  pack <- kde_rmnp_season[[i]]
  nm <- names(kde_rmnp_season)[[i]]
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
  mypath <- file.path("KDE_HomeRanges/RMNPIndiv_Season/95UD", paste(nm, "_aKDE_95", ".shp", sep = ""))
  writeVector(gf95, mypath, overwrite = T)
})

indiv_50_seaon <- lapply(seq_along(kde_rmnp_season), function(i) {
  pack <- kde_rmnp_season[[i]]
  nm <- names(kde_rmnp_season)[[i]]
  gf50 <- vect(hr_isopleths(pack, levels = 0.50)$geometry)
  mypath <- file.path("KDE_HomeRanges/RMNPIndiv_Season/50UD", paste(nm, "_aKDE_50", ".shp", sep = ""))
  writeVector(gf50, mypath, overwrite = T)
})

## Calculate area
indiv_area_season <- lapply(kde_rmnp_season, function(id) {
  gf95.area <- hr_isopleths(id, levels = 0.95)$area
})

rmnp.indiv.season <- data.table(do.call(cbind, indiv_area_season))
rmnp.indiv.season <- as.data.table(gather(rmnp.indiv.season, columnNames, values))
rmnp.indiv.season[, area.95 := values/1000000]

rmnp.indiv.season[, c("Animal_ID", "Season") := tstrsplit(columnNames, "_", fixed=TRUE)]

fwrite(rmnp.indiv.season, file = "KDE_HomeRanges/RMNPIndiv_Season/RMNP_Individual_aKDE_areas_seasonal.csv")

## Seasonal Pack Function
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

## Plot to check
akde_list <- lapply(rmnp_season_akdes, function(pack) {
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
})

sf_list <- mapply(function(spat, name) {
  name_clean <- str_remove(name, "^\\$")  # remove leading $ if present
  parts <- str_split(name_clean, "_", simplify = TRUE)
  
  st_as_sf(spat) %>%
    mutate(
      PackID = parts[1],
      Year_plot = parts[2],
      Season = parts[3]
    )
}, akde_list, names(akde_list), SIMPLIFY = FALSE)

# Combine all into one sf object
season_akde_95 <- bind_rows(sf_list)

ggplot() + 
  geom_sf(data = RMNP) + 
  geom_sf(data = RMNP_locs_sf %>% mutate(Year_plot = Deployment_Year), 
          aes(colour = Animal_ID), alpha = 0.1) + 
  geom_sf(data = season_akde_95, fill = NA, size = 2) +
  facet_wrap(~ PackID + Year_plot + Season) + 
  theme_bw()

## Save seasonal pack aKDEs
iso_95 <- lapply(seq_along(rmnp_season_akdes), function(i) {
  pack <- rmnp_season_akdes[[i]]
  nm <- names(rmnp_season_akdes)[[i]]
  gf95 <- vect(hr_isopleths(pack, levels = 0.95)$geometry)
  mypath <- file.path("KDE_HomeRanges/RMNPPack_Season/95UD", paste(nm, "_aKDE_95", ".shp", sep = ""))
  writeVector(gf95, mypath, overwrite = T)
})

iso_50 <- lapply(seq_along(rmnp_season_akdes), function(i) {
  pack <- rmnp_season_akdes[[i]]
  nm <- names(rmnp_season_akdes)[[i]]
  gf50 <- vect(hr_isopleths(pack, levels = 0.50)$geometry)
  mypath <- file.path("KDE_HomeRanges/RMNPPack_Season/95UD", paste(nm, "_aKDE_95", ".shp", sep = ""))
  writeVector(gf50, mypath, overwrite = T)
})

## Calculate area
rmnp_area_season <- lapply(rmnp_season_akdes, function(pack) {
  gf95.area <- hr_isopleths(pack, levels = 0.95)$area
})

rmnp.area.season <- data.table(do.call(cbind, rmnp_area_season))
rmnp.area.season <- as.data.table(gather(rmnp.area.season, columnNames, values))
rmnp.area.season[, area.95 := values/1000000]

rmnp.area.season[, c("Pack", "Year", "Season") := tstrsplit(columnNames, "_", fixed=TRUE)]

fwrite(rmnp.area.season, file = "KDE_HomeRanges/RMNPPack_Season/RMNP_pack_aKDE_areas_seasonal.csv")

## Boxplot of areas
gha.seasonal <- fread("KDE_HomeRanges/GHAPack_Season/GHA26_pack_aKDE_areas_seasonal.csv")
gha.yearly <- fread("KDE_HomeRanges/GHAPack/GHA26_pack_aKDE_areas_year-round.csv")
#gha.year.akde[, c("Pack", "Year") := tstrsplit(columnNames, "_", fixed=TRUE)]
#gha.year.akde[, Season := "Full Year"]
rmnp.seasonal <- fread("KDE_HomeRanges/RMNPPack_Season/RMNP_pack_aKDE_areas_seasonal.csv")
rmnp.yearly <- fread("KDE_HomeRanges/RMNPPack/RMNP_pack_aKDE_areas_year-round.csv")
#rmnp.year.akde[, c("Pack", "Year") := tstrsplit(columnNames, "_", fixed=TRUE)]
rmnp.yearly[, Season := "Year-round"]

plot_area <- rbind(gha.seasonal[, Study_Area := "SEMB"], rmnp.seasonal[, Study_Area := "RMNP"])

plot_area$Study_Area <- factor(plot_area$Study_Area, levels = c("RMNP", "SEMB"))

boxplot.95 <- ggplot(data = plot_area, aes(Season, area.95)) + 
  geom_boxplot() + 
  #geom_point(position=position_jitterdodge()) + 
  #geom_jitter(aes(color = Pack)) + 
  geom_jitter() + 
  facet_wrap(~Study_Area,
             labeller = labeller(Study_Area = c(
               RMNP = "RMNP\n(High Resource - Low Disturbance)",
               SEMB = "SEMB\n(Low Resource - High Disturbance)"))) +
  labs(y = expression(paste("95% aKDE Home Range Area ", (km^2)))) + 
  #labs(x = expression(paste("x axis ", ring(A)^2)), y = "y axis") + 
  coord_cartesian(ylim = c(0, 5000)) + 
  theme_bw()
boxplot.95

#ggsave(boxplot.95, filename = "Final_Figures/High_Res/Figure7_ClusterManuscript_2026-03-29.png", width = 7, height = 5, dpi = 300)


table_area <- rbind(gha.seasonal[, Study_Area := "SEMB"], 
                   gha.yearly[, Study_Area := "SEMB"],
                   rmnp.seasonal[, Study_Area := "RMNP"],
                   rmnp.yearly[, Study_Area := "RMNP"])

akde_median <- table_area[!(Pack == "Black-River" | Pack == "Great-Falls-Lone"), 
           median(area.95), by = c("Study_Area", "Season")]

fwrite(table_area[order(Study_Area, Season, Year, Pack)], "Output/Seasonal_aKDE_area_full.csv")
fwrite(akde_median, "Output/Median_seasonal_aKDE_areas.csv")

## Individuals
gha.seasonal <- fread("KDE_HomeRanges/GHAIndividual_Season/GHA_indiv_aKDE_areas_seasonal.csv")
gha.yearly <- fread("KDE_HomeRanges/GHAIndividual/GHA_indiv_aKDE_areas_year-round.csv")
gha.yearly[, Season := "full-year"]
setnames(gha.yearly, "columnNames", "Animal_ID")

rmnp.seasonal <- fread("KDE_HomeRanges/RMNPIndiv_Season/RMNP_individual_aKDE_areas_seasonal.csv")
rmnp.yearly <- fread("KDE_HomeRanges/RMNPIndividual/RMNP_indiv_aKDE_areas_year-round.csv")
rmnp.yearly[, Season := "full-year"]
setnames(rmnp.yearly, "columnNames", "Animal_ID")

table_area <- rbind(gha.seasonal[, Study_Area := "GHA"], 
                    gha.yearly[, Study_Area := "GHA"],
                    rmnp.seasonal[, Study_Area := "RMNP"],
                    rmnp.yearly[, Study_Area := "RMNP"], fill = T)

fwrite(table_area[, c("Animal_ID", "Study_Area", "Season", "area.95")], file = "Pack_Dataset/Summary/Seasonal_aKDE_area_individual.csv")
