
###packages
libs <- c('data.table', 'dplyr', 'tidyr', 'tidyverse', 'lme4')
lapply(libs, require, character.only = TRUE)


### load All Sites data after cleaning protocol
datrm <- fread('data/2021-09-17_RMNP_All_Sites.csv')

### clusters matched with investigated sites
inv_datrm <-subset(datrm, JoinStatus == 'Matched')
head(inv_datrm)


