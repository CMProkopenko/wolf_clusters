libs <- c('dplyr','data.table', 'ggplot2')
lapply(libs, require, character.only=TRUE)

## All RMNP data
rmnp <- fread("DataRMNP/2023-02-22_RMNP_K_All_Clu.csv")
#diet <- fread("DataRMNP/RMNP_diet.csv")
scat <- fread("DataRMNP/RMNP_cluster_scat_data_to-merge.csv")
scat2 <- fread("DataRMNP/RMNP_cluster_scat_data.csv")

## Scat samples with proportion of prey remains
scat <- scat[, c("Notes") := NULL]
scat[scat==''|scat == 'NA']<-NA
scat <- scat[!is.na(Sample_ID),]
setnames(scat, "Cluster_ID", "invCID")

scat_final <- merge(scat[, c("invCID", "Sample_ID")], scat2, by = "Sample_ID")

## Investigated clusters with GPS locations
#rmnp <- rmnp[JoinStatus == "Matched"]
clust_sub <- rmnp[JoinStatus == "Matched", c("invCID", "CentID", "PackID", "Behaviour_1", "Behaviour_2", "Prey_1", "Prey_2", "Calf", "FfixY",
                      "FfixM", "Act_fixes", "Clus_rad_m", "First_time", "finalIgnore")]
clust_sub <- clust_sub[finalIgnore == 0]

## Merge both data frames using Cluster_ID
scat.data <- merge(scat_final, clust_sub, by = "invCID", all.x = TRUE, 
                   all.y = FALSE)
scat.data <- scat.data[!is.na(invCID)]

# Multiple wolves may be associated with an investigated cluster, 
# We just want the first wolf visit so order by time and remove duplicated samples
scat.data[, First_time := as.ITime(First_time)]
data_ordered <- scat.data[order(Sample_ID, First_time), ]      # Order data
data_ordered  
data_highest <- data_ordered[!duplicated(Sample_ID), ]   # Unique rows of ordered data
data_highest 

# Can also remove additional samples from a given site and only keep first sample
sample1 <- data_highest[!(Sample_Number > 1)]
#sample2 <- data_highest[Sample_Number == 2]

# Remove samples that were not collected at a cluster
matchClu <- sample1[!is.na(CentID)]
setnames(matchClu, "Unknown content", "Unknown_content")

matchClu <- matchClu[, Unknown := ifelse(Unknown_content > 0, 1, 0)]
## Melt scat table to get prey totals at all investigated and matched sites
site_sum <- melt(matchClu, id.vars = c("Pack", "Behaviour_1", "FfixM"),
                 measure.vars = c("Beaver", "Moose_Adult", 
                                  "Elk_Adult", "Deer_Adult", 
                                  "Moose_Calf", "Deer_Calf", "Elk_Calf", 
                                  "Hare", "Bear", "Fish", "Bird", "Fox", "Canid", 
                                  "Rodent", "Veg", "Unknown_content"),
                 variable.name = "Species", value.name = "Occurrence")

# Specify snow or snow free seasons
site_sum[, Season := ifelse(FfixM == '5' | FfixM == '6' | FfixM == '7'
                            | FfixM == '8' | FfixM == '9' | FfixM == '10', 
                            'Snow Free', 'Snow')]
site_sum <- site_sum[Occurrence == 1]
site_sum[, Data := "Scat"]
setnames(site_sum, "Pack", "PackID")

## Investigated Kill Sites
#inv.dat <- rmnp[JoinStatus == "Matched" & Behaviour_1 == "Kill"]
inv.dat <- rmnp[finalIgnore == 0 & JoinStatus == "Matched" & 
                  Act_fixes >= 6 &
                  (Behaviour_1 == "Kill" | Behaviour_1 == "Scavenge" |
                     Behaviour_1 == "Probable kill")]
inv.dat <- inv.dat[!duplicated(invCID), ]
inv.dat[, Prey := ifelse(Calf == 1, paste(Prey_1,"Calf", sep = "_"), Prey_1)]

inv_sum <- melt(inv.dat, id.vars = c("PackID", "FfixM", "Behaviour_1"),
                measure.vars = c("Prey", "Prey_2"),
                variable.name = "Prey_Order", value.name = "Species")

inv_sum[, Season := ifelse(FfixM == '5' | FfixM == '6' | FfixM == '7'
                           | FfixM == '8' | FfixM == '9' | FfixM == '10', 
                           'Snow Free', 'Snow')]

inv_sum[Species==''|Species==' ']<-NA
inv_sum <- inv_sum[!is.na(Species)]

inv_sum[, `:=` (Occurrence = "1", Data = "Site")]

#rmnp_comb <- rbind(site_sum[!Occurrence == 0, ], inv_sum[, Prey_Order := NULL])
rmnp_comb <- rbind(site_sum, inv_sum[, Prey_Order := NULL])
rmnp_comb[, Site := "RMNP"]

## Load GHA26 Data
gha26 <- fread("DataGHA/2023-08-01_GHA_K_All_Clu.csv")
scat.gha <- fread('~/Documents/Masters/Data/Kaylas Honours/Input/GHA26_cluster_scat_data.csv')
biomass <- fread("DataGHA/Biomass_values_both.csv")

## Scat samples with proportion of prey remains
scat.gha <- scat.gha[, c("Notes") := NULL]
scat.gha[scat.gha==''|scat.gha == 'NA']<-NA
scat.gha <- scat.gha[!is.na(Sample_Number),]
setnames(scat.gha, "Cluster_ID", "invCID")

## Investigated clusters with GPS locations
#gha26 <- gha26[JoinStatus == "Matched"]
clust_gha <- gha26[finalIgnore == 0 & JoinStatus == "Matched", 
                   c("invCID", "CentID", "PackID", "Behaviour_1", 
                     "Behaviour_2", "Prey_1", "Prey_2", "KillAge", 
                     "FfixY","FfixM", "Act_fixes", "Clus_rad_m", 
                     "First_time", "finalIgnore")]

## Merge both data frames using Cluster_ID
gha.scat.inv <- merge(scat.gha, clust_gha, by = "invCID", all.x = TRUE, 
                      all.y = FALSE)
gha.scat.inv <- gha.scat.inv[!is.na(invCID)]

# Multiple wolves may be associated with an investigated cluster, 
# We just want the first wolf visit so order by time and remove duplicated samples
gha.scat.inv[, First_time := as.ITime(First_time)]
first_wolf <- gha.scat.inv[order(invCID, First_time), ]      # Order data
first_wolf <- first_wolf[!duplicated(invCID), ]   # Unique rows of ordered data
first_wolf[, .N, by = Sample_Number]

# Remove samples that were not collected at a cluster
gha.matchClu <- first_wolf[!is.na(CentID)]

## Melt scat table to get prey totals at all investigated and matched sites
gha.all.sites <- melt(gha.matchClu, id.vars = c("PackID", "Behaviour_1", "FfixM"),
                      measure.vars = c("Beaver", "Moose_Adult", "Deer_Adult", 
                                       "Moose_Calf", "Deer_Calf",  
                                       "Hare", "Bear", "Fish", "Bird", "Fox", "Rodent", "Unknown"),
                      variable.name = "Species", value.name = "Occurrence")

gha.all.sites[, Season := ifelse(FfixM == '5' | FfixM == '6' | FfixM == '7'
                                 | FfixM == '8' | FfixM == '9' | FfixM == '10', 
                                 'Snow Free', 'Snow')]
gha.all.sites[, Occurrence := ifelse(Occurrence == "Y", 1, 0)]
gha.all.sites <- gha.all.sites[Occurrence == 1]
gha.all.sites[, Data := "Scat"]

### Investigated Kill Sites
#inv.dat.gha <- gha26[JoinStatus == "Matched" & Behaviour_1 == "Kill"]
inv.dat.gha <- gha26[finalIgnore == 0 & JoinStatus == "Matched" & 
                       Act_fixes >= 6 & 
                       (Behaviour_1 == "Kill" | Behaviour_1 == "Scavenging" |
                          Behaviour_1 == "Probable kill")]
inv.dat.gha <- inv.dat.gha[!duplicated(invCID),]
inv.dat.gha[, Prey := ifelse(Calf == 1, paste(Prey_1,"Calf", sep = "_"), Prey_1)]
#inv.dat.gha[, Prey := ifelse(KillAge == "Calf" | KillAge == "Fawn", paste(Prey_1,"Calf", sep = "_"), Prey_1)]
inv.dat.gha[, Prey_2 := ifelse(IfOther_Species == "", NA, paste(IfOther_Species))]

## Summarize prey order 
gha_inv_sum <- melt(inv.dat.gha, id.vars = c("PackID", "FfixM", "Behaviour_1"),
                    measure.vars = c("Prey", "Prey_2"),
                    variable.name = "Prey_Order", value.name = "Species")

gha_inv_sum[, Season := ifelse(FfixM == '5' | FfixM == '6' | FfixM == '7'
                               | FfixM == '8' | FfixM == '9' | FfixM == '10', 
                               'Snow Free', 'Snow')]

gha_inv_sum[Species==''|Species==' ']<-NA
gha_inv_sum <- gha_inv_sum[!is.na(Species)]
gha_inv_sum[, `:=` (Occurrence = "1", Data = "Site")]

gha_comb <- rbind(gha.all.sites, gha_inv_sum[, Prey_Order := NULL])
gha_comb[, Site := "GHA26"]

## Combine both sites
sites_comb <- rbind(rmnp_comb, gha_comb)
sites_comb[, .N, by = c("Behaviour_1", "Site", "Data")]

# Clean names
sites_comb[Behaviour_1 == 'Scavenging', Behaviour_1 := 'Scavenge']
#setnames(sites_comb, "Species", "Species_Old")
sites_comb[, .N, by = Species]

sites_comb[Species == 'Moose' | Species == 'Moose?', Species := "Moose_Adult"]
sites_comb[Species == 'Elk', Species := "Elk_Adult"]
sites_comb[Species == 'WTD' | Species == 'Deer', Species := "Deer_Adult"]
sites_comb[Species == 'WTD_Calf', Species := "Deer_Calf"]
sites_comb[Species == "Ungulate", Species := "Ungulate_Adult"]
sites_comb[grep('Grouse', Species), Species := "Grouse"]
sites_comb[grep('Goose', Species), Species := "Goose"]
sites_comb[grep("hare", Species), Species := "Hare"]
sites_comb[grep("SSH", Species), Species := "Hare"]
sites_comb[grep("Hare", Species), Species := "Hare"]
sites_comb[grep("Unknown", Species), Species:= "Unknown"]
sites_comb[grep("fish", Species), Species := "Fish"]
sites_comb[grep("Beaver", Species), Species := "Beaver"]

#fwrite(sites_comb, "Output/BothSites_AllDiet_CleanedData_2023-11-05.csv")
sites_comb <- fread("Output/BothSites_AllDiet_CleanedData_2023-11-05.csv")

#summed_sites <- sites_comb[, N := sum(as.numeric(Occurrence)), by = c("Species", "Site", "Data")]
summed_sites <- sites_comb[!(Species == "Canid"), .N, by = c("Species", "Site", "Data")]
summed_sites[, Perc_Diet := N/sum(N)*100, by = c("Site", "Data")]
ord_site <- summed_sites[order(-Site, Data, -rank(N))]
fwrite(ord_site, "Output/Diet_summary_for manuscript_2023-11-05.csv")

## Seasonal Kill Sites
seasonal_sites <- sites_comb[!(Species == "Canid"), .N, by = c("Species", "Site", "Data", "Season")]
seasonal_sites[, Perc_Diet := N/sum(N)*100, by = c("Site", "Data", "Season")]
ord_site_seasonal <- seasonal_sites[order(-Site, Data, Season, -rank(N))]
fwrite(ord_site_seasonal, "Output/Diet_summary_Seasonal_2023-11-05.csv")

p <- ggplot(data = summed_sites, 
            aes(x=reorder(Species, -Perc_Diet), y= Perc_Diet,
                fill= Data)) +
  geom_bar(stat="identity", position =position_dodge()) + 
  facet_wrap(~ Site, scales = "free_x") + 
  geom_hline(yintercept = 5, linetype = "dashed")
p

library(gtsummary)


table1 <- sites_comb %>% select(Species, Site, Data)
table1 %>%
  tbl_summary(by = Data)

#leven's niche breadth = 1/sum(pi^2) where pi = proportion of all items in resource item i

## Calculate percent lost if not investigating small
inv_sum[, Site := "RMNP"]
gha_inv_sum[, Site := "GHA26"]

lg.clust <- rbind(inv_sum, gha_inv_sum)

lg.clust[Species == 'Moose' | Species == 'Moose?', Species := "Moose_Adult"]
lg.clust[Species == 'Elk', Species := "Elk_Adult"]
lg.clust[Species == 'WTD' | Species == 'Deer', Species := "Deer_Adult"]
lg.clust[Species == 'WTD_Calf', Species := "Deer_Calf"]
lg.clust[Species == "Ungulate", Species := "Ungulate_Adult"]
lg.clust[grep('Grouse', Species), Species := "Grouse"]
lg.clust[grep('Goose', Species), Species := "Goose"]
lg.clust[grep("hare", Species), Species := "Hare"]
lg.clust[grep("SSH", Species), Species := "Hare"]
lg.clust[grep("Hare", Species), Species := "Hare"]
lg.clust[grep("Unknown", Species), Species:= "Unknown"]
lg.clust[grep("fish", Species), Species := "Fish"]
lg.clust[grep("Beaver", Species), Species := "Beaver"]

lg.clust <- lg.clust[, .N, by = c("Species", "Site")]

setnames(lg.clust, "N", "N_Sens")

#ord_site[, Sensored := "N"]

check <- merge(lg.clust, ord_site[Data == "Site"], by = c("Species", "Site"))


check[, perc_lost := (N-N_Sens)/N]
check[order(-Perc_Diet)]