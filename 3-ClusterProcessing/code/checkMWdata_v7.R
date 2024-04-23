rm(list=ls(all=TRUE))

library(maptools)
gpclibPermit()

library(sp)
library(rgdal)

# Program Name and Version
progname = "checkMWdata_v7"

#
# Define program defaults
#

####################################################################
#
# Output list of unique CentdID, invCID, invCVD, Species, Behaviours
#
# v1 - initial verison
#
#   Input Files: 
#      MW output file
#
#   Output File: message file stored
#
# v2 - mw statistics
#
# v4 - unique statistics for CentIDs (GPS Clusters)
# 
# v5 - unique statistics for invCIDs 
#
# v6 - add sort order before analysis
#
# v7 - 2023-09-22
#    - fix ranking to match list priciple investigator
#      Mortality>Den>Rendez-vous>Kill>Kill Revisit>Probable Kill >Scavenge>Resting>Scat>Unknown
#      
#    - update ambiguity checking to clarify criteria
#
####################################################################
####################################################################
##
## This software is licensed under: https://opensource.org/license/mit
##
## For more information refer to the GitHub Project: https://github.com/CMProkopenko/wolf_clusters
##
####################################################################

#
# Define program defaults
#

##
## CentID_flag - output CentID details
##
CentID_Flag = FALSE
unfiltered_Flag = FALSE

# Debug output
debugFlag = FALSE
errorCount= 0

# Sink console to file
sinkFlag  = TRUE
sinkFlag  = FALSE

# select dump of ids
dumpFlag  = FALSE

cluTypes = c("K","KPk","KPkS")
cluSelect = 1



####################################################################################################
####################################################################################################
##
##
##  Project Selection
##
##
####################################################################################################
####################################################################################################

#
# Select Project by setting myproj:
#    DataKill     ==>  RMNP
#    DataGHA      ==>  GHA-26
#
myproj = "DataKill"
myproj = "DataGHA"


####################################################################################################
####################################################################################################
##
##
##  HELPER FUNCTIONS
##
##
####################################################################################################
####################################################################################################

#Function
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

root_dir<-function(){
  # Look for the root directory in order of preference
  dir1 = paste("C:/Wolf-Projects/",projectname,sep="")
  if( file.exists(dir1) ) return(dir1)
  dir1 = paste("D:/Wolf-Projects/",projectname,sep="")
  if( file.exists(dir1) ) return(dir1)
  dir1 = paste("E:/Wolf-Projects/",projectname,sep="")
  if( file.exists(dir1) ) return(dir1)
  dir1 = "C:/Wolf-Projects/DataKill"
  if( file.exists(dir1) ) return(dir1)
  dir1 = "D:/Wolf-Projects/DataKill"
  if( file.exists(dir1) ) return(dir1)
  dir1 = "E:/Wolf-Projects/DataKill"
  if( file.exists(dir1) ) return(dir1)
  # default
  dir1 = "C:/DataKill"
  return (dir1)
}

clu_stats<-function(tag1,title1, data1, flag1 ){
  message(" >>> Statistics: ",tag1," --  ",title1)
  nr1 = nrow(data1)
  nc1 = ncol(data1)
  message(" >>>    Num Clusters: ",nr1)
  avgtotal =round(mean(data1$mwKdurTotal),digits=3)
  avgmean  =round(mean(data1$mwKdurMean), digits=3)
  medmedian=round(median(data1$mwKdurMedian), digits=3)
  message(" >>>    Avg    Total  Dur: ",avgtotal)
  message(" >>>    Avg    Mean   Dur: ",avgmean)
  message(" >>>    Median Median Dur: ",avgmean)
  foo = c(tag1,nr1,avgtotal,avgmean,medmedian)
  return (foo)
}

skip_line<-function(){
  message(" >>>")
}

blank_line<-function(){
  message(" ")
}

pad_int<-function(n,scale){
  out_string<-paste(10*scale + n,sep='')
  out_string<-substr(out_string,2,nchar(out_string))
  return(out_string)
}

file_name<-function(d,f){
  return(paste(d,"/",f,sep=""))
}

getmerge<-function(){
  data = {}
  fname1  <- investFile
  fnameI  <- paste(myDir,"/",fname1,sep="")
  skip_line()
  if (promptFileNames) {
    message(" >>> Select: Merged Location File: ")
    fnameI = file.choose()
  }
  data  = read.csv(fnameI,stringsAsFactors=FALSE)
  ldata = length(data[,1])
  message(" >>> Processing: Investigation File: ","  Length: ",ldata,"  Name:", fnameI)
  skip_line()
  return(data)
}

# Define File Types
ft_loc = 1    # location file
ft_ana = 2    # analysis file
ft_clu = 3    # cluster  file

# Which File Type to Merge
merge_ft = ft_loc

file_type<-function(id,name){
  ft = 0
  if(length(grep("_location",name))>0)     ft = ft_loc
  if(length(grep("_analysis.csv",name))>0) ft = ft_ana
  if(length(grep("_mikecluster2",name))>0) ft = ft_clu
  
  # check if wolf id matches as well
  
  if(length(grep(id,name))==0) ft = 0
  
  return(ft)
}

Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}

findLocation<-function(mc2row,fixnum){
  #
  # Find the correct row in the merged loction file
  # for  the given fixnum 
  # from the given merged cluster mc2row
  #
  v1 = cludata$ID[mc2row]
  v2 = cludata$Findex[mc2row]
  
  lrow = which(newdata$ID == v1 & newdata$FixNum == fixnum & newdata$Findex == v2)
  
  # One and only one row should match
  
  if (length(lrow) != 1) {
    message("ERROR ----- Cannot Find Location Row ",mc2row," ",fixnum," ",v1," ",v2," ",lrow)
    errorCount = errorCount + 1
    if (errorCount > 10) stop(0)
  }
  
  # file names should match
  
  f1 = cludata$Fname[mc2row]
  f1t = substr(f1,1,nchar(f1)-nchar("analysis-mikeclusters2.csv"))
  f2 = newdata$FnameLoc[lrow]
  f2t = substr(f2,1,nchar(f2)-nchar("location.csv"))
  
  if (f1t != f2t) {
    message("ERROR ----- File names do not match ",mc2row," ",fixnum," ",v1," ",v2," ",lrow," ",f1t,"!=",f2t)
    errorCount = errorCount + 1
    if (errorCount > 10) halt(0)
  }
  
  return (lrow)
}

checkCluUnique <- function(dataclu){
  #
  # Check that the Cluster are unique when finalIgnore == 1 is filtered out
  #
  ucentid    = unique(dataclu$CentID)
  nucentid   = length(ucentid)
  
  dataclu1   = dataclu[dataclu$Site_Cat == "Kill" , ]
  ucentid1   = unique(dataclu1$CentID)
  nucentid1  = length(ucentid1)
  
  dataclu2   = dataclu[dataclu$Site_Cat == "Kill" & dataclu$finalIgnore == 0 ,]
  ucentid2   = unique(dataclu2$CentID)
  nucentid2  = length(ucentid2)
  message(" >>> Checking CentID Clusters  --  Total = ",nucentid,"  Total Kill = ",nucentid1,"  Total Kill with finalIgnore(0) = ",nucentid2)
  #
  # Verify that the Site_Cat are not ambigous
  #
  for (cid in ucentid2) {
    dataclu3 = dataclu2[dataclu2$CentID == cid , ]
    ncid = nrow(dataclu3)
    if (ncid > 1) {
      #
      # Check Site_Cat are the same
      #
      usc  = unique(dataclu3$Site_Cat)
      nusc = length(usc)
      if (nusc > 1) {
        message(" >>> Warning:  CentID = ",cid,"  num = ","  more than 1 Site_Cat = ",usc)
      }
    }
  }
  
  return (nucentid2)
}


##########################################################################################
##########################################################################################
#####
##### Statistics
#####
##########################################################################################
##########################################################################################

##########################################################################################
##
## Overall Ranking for Categories
##
##  OLD: Mortality>Den>Rendez-vous>Kill>Kill Revisit>Probable Kill >Scavenge>Resting>Scat>Unknown
##
##  Review - 2023-09-22
##    - fix ranking to match list from CP
##  NEW: Mortality> Den> Rendez-vous> Kill> Kill Revisit> Probable Kill> Scavenge> Resting> Scat> Unknown

## GHA has slightly different names from RMNP
#
# "Kill"           - same
# "Resting"        - same
# "Scavenging"     - different
# "Probable kill"  - same
# "Unknown"        - same      
# "Revisit"        - same
# "Den"            - same
# "Rendez-vous"    - same

cat_list = c("MORT","Non collared wolf mort","Den","Rendez-vous","Kill","Revisit","Probable kill","Scavenging","Scavenge","Resting","Probable resting","Probable prey encounter","Probable scavenge","Scat","Unknown")
cat_rank = c(    95,                      90,   85,           80,    75,       70,             65,          60,        60,       55,                50,                       45,                 40,    35,       20)

cat2rank <- function(mycat) {
  ## 
  ## Lookup the rank of the category
  ## 0 (lowest rank) returned if not found
  ##
  catl = which(cat_list==mycat)
  if (length(catl) != 1) return(0)
  cati = catl[1]
  return (cat_rank[cati])
}

prey_list =c("Elk","Moose","WTD","Deer","Bait","Ungulate","Beaver","Bear","Ruffed Grouse","Grouse","Canada Goose","Garbage","Pigs","Mink","Hare","Snowshoe hare","Fish")
prey_rank =c(   80,     90,   70,    70,    10,        60,      25,    50,             40,      40,            45,       11,    30,    20,    21,             21,    15)

prey2rank <- function(myprey) {
  ## 
  ## Lookup the rank of the prey
  ## 0 (lowest rank) returned if not found
  ##
  preyl = which(prey_list==myprey)
  if (length(preyl) != 1) return(0)
  preyi = preyl[1]
  return (prey_rank[preyi])
}

mtitle <- function(string1,string2){
  blank_line()
  message("**********************************************************************")
  message("**********************************************************************")
  message("****  ",string1)
  message("****  ",string2)
  message("**********************************************************************")
  message("**********************************************************************")
}

stitle <- function(string1,string2){
  blank_line()
  message(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  message(">>>  ",string1)
  message(">>>  ",string2)
  message(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
}

preyStats <- function(cdata,cname) {
  ##
  ## find unique list of prey: filter then sort
  ##
  uprey1   = unique(cdata$Prey_1)
  uprey1   = uprey1[!is.na(uprey1)]
  uprey1   = uprey1[uprey1 != ""]
  uprey1   = uprey1[uprey1 != " "]
  nuprey1  = length(uprey1)
  ##
  ## List of Prey Species
  ##
  blank_line()
  suprey1 = sort(uprey1)
  nrcdata=nrow(cdata)
  coli = which(colnames(cdata)=="Prey_1","  Total Rows: ",nrcdata)
  message(" >>> Prey_1: ",cname," (Num: ",nuprey1,")")
  for (preyi in 1:nuprey1) {
    prey = suprey1[preyi]
    xdata = cdata[!is.na(cdata[,coli]),]
    xdata = xdata[xdata[,coli]==prey,]
    nprey = nrow(xdata)
    ##
    ## count number of rows thay match value
    message("     ",preyi," : ",prey,"  (Rows: ",nprey,")")
  }
}

fieldStats <- function(cdata,cname,fname) {
  ##
  ## find unique list of column(fname): filter then sort
  ##
  nrcdata0=nrow(cdata)
  coli = which(colnames(cdata)==fname)
  #message("DEBUG 1 ",cname," ",fname," fcol ",coli," Total Rows ",nrcdata)
  ##
  ## filter cdata
  ##
  ddata=cdata[!is.na(cdata[,coli]),]
  edata=ddata[ddata[,coli]!="",]
  fdata=edata[edata[,coli]!=" ",]
  xdata    = fdata[,coli]
  uprey1   = unique(xdata)
  #uprey1   = uprey1[!is.na(uprey1)]
  #uprey1   = uprey1[uprey1 != ""]
  #uprey1   = uprey1[uprey1 != " "]
  nuprey1  = length(uprey1)
  ##
  ## List of Prey Species
  ##
  blank_line()
  suprey1 = sort(uprey1)
  nrcdata=nrow(fdata)
  message(" >>> Field: ",fname," [column: ",coli,"] - ",cname," (Num: ",nuprey1,")  Total Rows: ",nrcdata0,"  Filtered Rows: ",nrcdata)
  btotal = 0
  for (preyi in 1:nuprey1) {
    prey = suprey1[preyi]
    xdata = cdata[!is.na(cdata[,coli]),]
    xdata = xdata[xdata[,coli]==prey,]
    nprey = nrow(xdata)
    btotal = btotal + nprey
    ##
    ## count number of rows that match value
    message("     ",preyi," : ",prey,"  (Rows: ",nprey,")")
  }
  eflag = "  (Ok) )"
  if (btotal != nrcdata) eflag = "  (*** Warning Breakdown does not match #filtered rows ***) )"
  message("     ","All: All   (Rows: ",btotal,eflag)
}

basicStats <- function(cdata,cname){
  blank_line()
  stitle("Basic Statistics",paste("Set Name: ",cname,sep=""))

  skip_line()
  
  nrcdata  = nrow(cdata)
  nccdata  = ncol(cdata)
  ucluid   = unique(cdata$CentID)
  nucluid  = length(ucluid)
  uinvid   = unique(cdata$invCID)
  nuinvid  = length(uinvid)
  
  message(" >>>    Rows:             ",nrcdata)
  message(" >>>    Cols:             ",nccdata)
  message(" >>>    #unique CentID:   ",nucluid)
  message(" >>>    #unique InvCID:   ",nuinvid)
  
  ##
  ## cluRep and invRep breakdown
  ##
  crzdata  = cdata[cdata$cluRep==0,]
  ncrzdata = nrow(crzdata)
  irzdata  = cdata[cdata$invRep==0,]
  nirzdata = nrow(irzdata)

  brzdata  = crzdata[crzdata$invRep==0,]
  nbrzdata = nrow(brzdata)
  
  ##
  ## Ambigous Flags
  ##
  scddata     = cdata[cdata$cluSiteCatDiff,]
  nscddata    = nrow(scddata)
  psddata     = cdata[cdata$cluPreySppDiff,]
  npsddata    = nrow(psddata)
  iscddata    = cdata[cdata$invSiteCatDiff,]
  niscddata   = nrow(iscddata)
  
  rendkmdata  = cdata[cdata$cluRendezKillMix,]
  nrendkmdata = nrow(rendkmdata)
  scavkmdata  = cdata[cdata$cluScavengeKillMix,]
  nscavkmdata = nrow(scavkmdata)
  revikmdata  = cdata[cdata$cluRevisitKillMix,]
  nrevikmdata = nrow(revikmdata)
  
  figdata     = cdata[cdata$finalIgnore==1,]
  nfigdata    = nrow(figdata)
    
  skip_line()
  message(" >>>    finalIgnore==0:   ",nfigdata)
  message(" >>>    finalIgnore==1:   ",nrcdata-nfigdata)
    
  skip_line()
  message(" >>>    invRep Zero:      ",nirzdata)
  message(" >>>    invRep Non-Zero:  ",nrcdata-nirzdata)
  
  skip_line()
  message(" >>>    cluRep Zero:      ",ncrzdata)
  message(" >>>    cluRep Non-Zero:  ",nrcdata-ncrzdata)
  skip_line()
  message(" >>>    cluCatSiteDiff:   ",nscddata)
  message(" >>>    cluPreySppDiff:   ",npsddata)
  message(" >>>    invCatSiteDiff:   ",niscddata)
  message(" >>>    cluRendKillMix:   ",nrendkmdata)
  message(" >>>    cluScavKillMix:   ",nscavkmdata)
  message(" >>>    cluReviKillMix:   ",nrevikmdata)

  skip_line()
  message(" >>>    Both   Zero:      ",nbrzdata)

  ##
  ## Unique Lists (remove NA and blanks)
  ##
  usitecat = unique(cdata$Site_Cat)
  nusitecat= length(usitecat)
  
  ##
  ## List of Unique Site Categories
  ##
  if (CentID_Flag) {
    stitle("Site Categories: Including Replicated CentIDs (GPS Clusters)",cname)
    fieldStats(cdata,  " Category Breakdown ","Site_Cat")
    fieldStats(scddata," Category Breakdown (when CluSiteCatDiff is True) ","Site_Cat")
  }
  
  ##
  ## List of Unique Prey Species
  ##
  if (CentID_Flag) {
    stitle("Prey Species: Including Replicated CentIDs (GPS Clusters)",cname)
    fieldStats(cdata,  " Species Breakdown ","Prey_1")
    fieldStats(scddata," Species Breakdown (when CluSiteCatDiff is True) ","Prey_1")
  }

  ##
  ## Create List of Unique CentIDs by filtering on cluRep < 2 
  ##
  if (CentID_Flag) {
    stitle("Highest Ranked Site Categories: Unique CentIDs (GPS Clusters)",cname)
    udata1= cdata[cdata$cluRep < 2,]
    fieldStats(udata1," Category Breakdown (only unique CentID)","chrSite_Cat")
  }

  ##
  ## Create List of Unique invCID by filtering on hrinvRep < 1 then
  ## Count the highest ranked site category
  ##
  stitle("Highest Ranked Site Category: Unique invCID (Investigation Clusters)",cname)
  udata2= cdata[cdata$hrinvRep < 1,]
  fieldStats(udata2," Category Breakdown (only unique invCID)","ihrSite_Cat")
  
  ##
  ## Count the first site category 
  ##
  stitle("Pick the first (based on sort) Site Category: Unique invCID (Investigation Clusters)",cname)
  fieldStats(udata2," Category Breakdown (only unique invCID)","Site_Cat")
  
  ##
  ## Count the Highest Ranked Prey_1 species 
  ##
  stitle("Highest Ranked Prey_1 Species: Unique invCID (Investigation Clusters)",cname)
  fieldStats(udata2," Category Breakdown (only unique invCID)","ihrPrey_1")
  
  ##
  ## Count the first Prey_1 species
  ##
  stitle("Pick the first (based on sort) Prey_1 Species: Unique invCID (Investigation Clusters)",cname)
  fieldStats(udata2," Category Breakdown (only unique invCID)","Prey_1")
  
  blank_line()
  
}

##########################################################################################
#####
##### Project Settings
#####
##########################################################################################

##########################################################################################
#####
##### Start Project Settings for  --  Default RMNP
#####
##########################################################################################

###
# Results Directory
#
projectname  = "DataKill"
myDir = root_dir()

resultsDir = "locdataAll-CCv31 - R300H96 - Final/Results"

###
# Investigation File
#
#investFile = "GHA26_cleaned-cluster_Sept-11-2019_UpdatedLocs_nh3.csv"
#investFile = "InvestigatedPoints0131_cleaned20180614.csv"

###
# Joined File
#
groupFile = "mergedWmc2-locdataAll-CCv31 - R300H96 - Test-BLEND_ooeoeeexoeoooeoexxoeoexeoooxxx.csv"
#groupFile = "mergedWmc2-locdataAll-CCv31 - R300H96-BLEND_1111111x11111111xx1111x1111xxx.csv"
#groupFile = "mergedWmc2-locdataAll-CCv32-BLEND_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.csv"

####
# Cluster Tolerance (tolerance in meters added to buffer)
#
cluster_tolerance = 0
#cluster_tolerance = 15
cluster_tolerance = 25
#cluster_tolerance = 50

###
# Cluster Site Files
#
Clu_Prefix = "2023-02-22_RMNP_K_"

ksname = paste(Clu_Prefix,"Matched_Clu_sortedInv.csv",sep="")
ksname2= paste(Clu_Prefix,"Matched_Clu_sortedClu.csv",sep="")
jsname = paste(Clu_Prefix,"All_Sites.csv",sep="")
ssname = paste(Clu_Prefix,"SumBy_Wolf.csv",sep="")
jcname = paste(Clu_Prefix,"Column_Names.csv",sep="")

ksfile   <- paste(myDir,"/",resultsDir,"/",ksname,sep="")
ksfile2  <- paste(myDir,"/",resultsDir,"/",ksname2,sep="")
jsfile   <- paste(myDir,"/",resultsDir,"/",jsname,sep="")
ssfile   <- paste(myDir,"/",resultsDir,"/",ssname,sep="")
jcfile   <- paste(myDir,"/",resultsDir,"/",jcname,sep="")

###
# Manual Remove File
#
manRemFile = "Manual_Actions_2021-07-01.csv"
#
# Disable the Actions for Testing
# manRemFile = "Manual_Remove_Empty.csv"


#
# Cross Reference File
#xrefFile = paste("Join-Matched-CTM",cluster_tolerance,"-",groupFile,sep="")
xrefFile = "ClusterMatch_SZcleaningMod20210502.csv"


# Wolf ID Range to scan
# RMNP
listWolfID = c("W01","W02","W03","W04","W05","W06","W07","W08","W09","W10","W11","W12","W13","W14","W15","W16","W17","W18","W19","W20","W21","W22","W23","W24","W25","W26","W27","W28","W29","W30")
# GHA
#listWolfID = c("35333","35334","35335","35336","37926","37927","1870","2020","5137","5138","5139","5140","5141","5142","5143","5144","5145","5146","5147","5148","5149","5150","5151","5153","5154","5155","5156","5915","5916","5917","5918","5919")

# Input Folder Name 
#locFolder = "locdataAll-CCv31"
#locFolder = "loctest"
#locFolder = "locdataAll-CCv32"
#locFolder = "locdataAll-CCv31 - R300H96 - Test"
locFolder = "locdataAll-CCv31 - R300H96 - Final"

# Prompt for Files instead of using defaults
promptFileNames = FALSE

# Results Folder
resultsFolder = "Results"

# Merged Location Files

mergeFile   = "mergedWloc-"
tmergeFile  = "mergedTloc-"
kmergeFile  = "mergedKloc-"
bmergeFile  = "mergedBloc-"

# INPUT: Merged Location File

mergeFile  = "mergedWloc-locdataAll-CCv31 - R300H96 - Test-BLEND_ooeoeeexoeoooeoexxoeoexeoooxxx"
#mergeFile = "mergedWloc-locdataAll-CCv31 - R300H96 - Test-tBLEND_1111111x11111111xx1111x1111xxx"
#mergeFile = "mergedWloc-locdataAll-CCv32 - Test-BLEND_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

# Merged Location File with tfk processing added

tmergeFile  = "mergedTloc-locdataAll-CCv31 - R300H96 - Final-BLEND_ooeoeeexoeoooeoexxoeoexeoooxxx"
#tmergeFile = "mergedTloc-locdataAll-CCv31 - R300H96 - Test-BLEND_1111111x11111111xx1111x1111xxx"
#tmergeFile = "mergedTloc-locdataAll-CCv32 - Test-BLEND_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

# Merged Location File with tfk processing EXCLUDING POINTS IN AN AGGREGATE CLUSTER (Between kill sites)

bmergeFile  = "mergedBloc-locdataAll-CCv31 - R300H96 - Final-BLEND_ooeoeeexoeoooeoexxoeoexeoooxxx"
#bmergeFile = "mergedBloc-locdataAll-CCv31 - R300H96 - Test-BLEND_1111111x11111111xx1111x1111xxx"
#bmergeFile = "mergedBloc-locdataAll-CCv32 - Test-BLEND_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

# Merged Location File with tfk processing INCLUDING POINTS IN AN AGGREGATE CLUSTER  (only Kill sites)

kmergeFile  = "mergedKloc-locdataAll-CCv31 - R300H96 - Final-BLEND_ooeoeeexoeoooeoexxoeoexeoooxxx"
#kmergeFile = "mergedKloc-locdataAll-CCv31 - R300H96 - Test-BLEND_1111111x11111111xx1111x1111xxx"
#kmergeFile = "mergedKloc-locdataAll-CCv32 - Test-BLEND_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

# Kill Site List File
#killSiteFile = "CluSiteList"
killSiteFile = "ClusterMatch_SZcleaningMod20210502"

# Aggregate Wolf Cluster File ID


# Merged Cluster File
mergedWmc2 = "mergedWmc2-"

##########################################################################################
#####
##### End Project Settings for         RMNP
#####
##########################################################################################

if (myproj == "DataGHA") {
  
  ##########################################################################################
  #####
  ##### Start Project Settings for    GHA-26
  #####
  ##########################################################################################
  
  ###
  # Results Directory
  #
  projectname = "DataGHA"
  myDir = root_dir()
  
  resultsDir = "loc-CCv33utm15/results"
  
  ###
  # Joined File
  #
  groupFile = "mergedWmc2-loc-CCv33utm15-BLEND_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.csv"
  
  ####
  # Cluster Tolerance (tolerance in meters added to buffer)
  #
  cluster_tolerance = 0
  #cluster_tolerance = 15
  cluster_tolerance = 25
  #cluster_tolerance = 50
  
  ###
  # Cluster Site Files
  #
  Clu_Prefix = "2023-08-01_GHA_K_"
  ksname = paste(Clu_Prefix,"Matched_Clu_sortedInv.csv",sep="")
  ksname2= paste(Clu_Prefix,"Matched_Clu_sortedClu.csv",sep="")
  jsname = paste(Clu_Prefix,"All_Sites.csv",sep="")
  ssname = paste(Clu_Prefix,"SumBy_Wolf.csv",sep="")
  jcname = paste(Clu_Prefix,"Column_Names.csv",sep="")
  
  #
  # C:/Wolf-Projects/DataGHA/loc-CCv33utm15/results/2023-08-01_GHA_K_Matched_Sites_sortedClu.csv
  # 2023-08-01_GHA_K_Matched_Clu_sortedClu.csv
  #
  ###
  # Manual Remove File
  #
  manRemFile = "Manual_Remove_Empty.csv"
  
  ###
  # Cross Reference File
  #
  xrefFile = paste("Join-Matched-CTM",cluster_tolerance,"-",groupFile,sep="")
  
  ##########################################################################################
  #####
  ##### End Project Settings for         GHA-26
  #####
  ##########################################################################################
  
}

####################################################################
####################################################################
####################################################################
#
# Start Main Script
#
####################################################################
####################################################################
####################################################################

message(">>>>>>>>>> Create: ",progname,"  --  Time: ",Sys.time())
blank_line()
skip_line()
message(" >>> Checking Root Directories <<< ")
myDir = root_dir()
message(" >>> Root Dir = ",myDir)

#### Path to main folder
locDir = paste(myDir,"/",resultsDir,sep="")

# first part of name needs to be replaced
killFile1   = paste(locDir,"/",ksname2,sep="")
#killFile1    = ksfile
#killFile1    = jsfile

#killFile1 = "C:/Wolf-Projects/DataKill/locdataAll-CCv31 - R300H96 - Final/Results/2022-05-14_RMNP_K_Matched_Sites_sortedClu_edit1.csv"

# C:\Wolf-Projects\DataGHA\loc-CCv33utm15\results

blank_line()
skip_line()

csdata   <- read.csv(killFile1,stringsAsFactors=FALSE)

#############################################################################################################################
##
## Create: new column with highest ranked Category when there are multiple Site Categories for a unique CentID (GPS cluster)
##
##   (1) create Highest Rank Site Category
##       ohrSite - original for debugging
##       chrSite - CentID (GPS Cluster)
##       ihrSite - invCID (Investigated Site)
##   (2) over-ride with highest ranked Category
##
#############################################################################################################################
csdata$hrCentID    = csdata$CentID
csdata$hrinvCID    = csdata$invCID
csdata$hrinvRep    = -1
csdata$ohrSite_Cat = csdata$Site_Cat
csdata$chrSite_Cat = csdata$Site_Cat
csdata$ihrSite_Cat = csdata$Site_Cat

## extract rows which need to be over-ridden
xdata   = csdata[csdata$cluSiteCatDiff,]
ucentid = unique(xdata$CentID)

for (id in ucentid) {
  idlist = which(xdata$CentID==id)
  nidlist = length(idlist)
  if (nidlist < 2) message("$$$$$$$$$$ ERROR idlist CentID: ",id," ",nidlist," ",idlist)
  hrank = 0
  hsite = "ERROR"
  ##
  ## find highest ranked category
  ##
  for (j in 1:nidlist) {
    cat1 = xdata$Site_Cat[idlist[j]]
    rank1 = cat2rank(cat1)
    if (rank1 > hrank) {
      hrank = rank1
      hsite = cat1
    }
  }
  ##
  ## over-ride original Site_Cat with hsite
  ##
  rlist = which(csdata$CentID==id)
  csdata$chrSite_Cat[rlist] = hsite
}

#############################################################################################################################
##
## Repeat for invCID
##
##   (1) index invCID so it is easy to extract unique ones 
##   (2) over-ride with highest ranked Category
##
#############################################################################################################################

## extract rows which need to be over-ridden
xdata  = csdata[csdata$invSiteCatDiff,]
uinvid = unique(xdata$invCID)

for (id in uinvid) {
  idlist = which(xdata$invCID==id)
  nidlist = length(idlist)
  if (nidlist < 2) message("$$$$$$$$$$ ERROR idlist invCID: ",id," ",nidlist," ",idlist)
  hrank = 0
  hsite = "ERROR"
  ##
  ## find highest ranked category
  ##
  for (j in 1:nidlist) {
    cat1 = xdata$Site_Cat[idlist[j]]
    rank1 = cat2rank(cat1)
    if (rank1 > hrank) {
      hrank = rank1
      hsite = cat1
    }
  }
  ##
  ## over-ride original Site_Cat with hsite
  ##
  rlist = which(csdata$invCID==id)
  csdata$ihrSite_Cat[rlist] = hsite
}

##
## create index for invCID 
##

uinvcid  = unique(csdata$invCID)
nuinvcid = length(uinvcid)

for (id in uinvcid) {
  idlist  = which(csdata$invCID==id)
  nidlist = length(idlist)
  for (j in 1:nidlist) {
    csdata$hrinvRep[idlist[j]] = j-1
  }
}

#############################################################################################################################
##
## Check: if there are ambiguous Prey_1 for unique invCID
##
##   (1) create Highest Rank Prey Category
##       ohrPrey_1 - original for debugging
##       chrPrey_1 - CentID (GPS Cluster)
##       ihrPrey_1 - invCID (Investigated Site)
##   (2) over-ride with highest ranked Prey
##
#############################################################################################################################

csdata$ohrPrey_1 = csdata$Prey_1
csdata$chrPrey_1 = csdata$Prey_1
csdata$ihrPrey_1 = csdata$Prey_1

## extract all rows 
xdata  = csdata
uinvid = unique(xdata$invCID)

for (id in uinvid) {
  idlist = which(xdata$invCID==id)
  nidlist = length(idlist)
  if (nidlist > 1) {
    hrank = 0
    hprey = "none"
    ##
    ## find highest ranked prey
    ##
    for (j in 1:nidlist) {
      prey1 = xdata$Prey_1[idlist[j]]
      rank1 = prey2rank(prey1)
      if (rank1 > hrank) {
        hrank = rank1
        hprey = prey1
      }
    }
    ##
    ## over-ride original Site_Cat with hsite
    ##
    rlist = which(csdata$invCID==id)
    csdata$ihrPrey_1[rlist] = hprey
  }
}

#############################################################################################################################
#############################################################################################################################
##
## MW CLuster Statistics
##
#############################################################################################################################
#############################################################################################################################

nrcsdata <- nrow(csdata)
nccsdata <- ncol(csdata)

if (sinkFlag) {
  mydir = root_dir()
  confile = paste(mydir,"/myconsole.txt",sep="")
  zz <- file(confile, open = "wt")
  message(" >>>>> Note - starting sink to",confile)
  sink(zz)
  sink(zz,type = "message")
}

message(" >>> Processing: Cluster Site File: ",killFile1,"  Rows=",nrcsdata,"  Cols=",nccsdata)  
skip_line()

#### Select clusters that have mw Kill Ids and are the first cluster

kdata   = csdata[(csdata$mwKfirst==1),]
nrkdata <- nrow(kdata)
nckdata <- ncol(kdata)
message(" >>> Processing: Cluster marked as first ","  Rows=",nrkdata,"  Cols=",nckdata)  
skip_line()

nuksvec= length(unique(kdata$CentID))

checkCluUnique(csdata)
skip_line()

message(" >>> Number Unique: MultiWolf (mw) Kill Sites CentID Selected for Analysis: ",nuksvec)  
skip_line()


#for (i in 1:10) {
#  message(" >>> Test: ",i)
#}

# message(">>>>>>>>>>>>>>>>>>>>>> Investigation IDs")
# for (iid in s_invcid) message(iid)
# message(">>>>>>>>>>>>>>>>>>>>>>")
# message(">>>>>>>>>>>>>>>>>>>>>> Investigation Dates")
# for (iid in s_invcvd) message(iid)


###
###
### Behaviour
###
###

u_beh_1 = unique(csdata$Behaviour_1)
u_beh_2 = unique(csdata$Behaviour_2)
u_sc_1  = unique(csdata$Site_Cat)
u_sc_2  = unique(csdata$oSite_Cat)

sort(u_beh_1)
sort(u_beh_2)
sort(u_sc_1)
sort(u_sc_2)

if (dumpFlag) {
  blank_line()
  message(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  message(">>>>>>>>>>>>>>>>>>>>>> Site_Cat")
  for (id in sort(u_sc_1)) message(id)
  
  
  blank_line()
  message(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  message(">>>>>>>>>>>>>>>>>>>>>> Behaviour_1")
  for (id in sort(u_beh_1)) message(id)
  
  blank_line()
  message(">>>>>>>>>>>>>>>>>>>>>> Behaviour_2")
  for (id in sort(u_beh_2)) message(id)
  
  blank_line()
  message(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  message(">>>>>>>>>>>>>>>>>>>>>> Original Site_Cat")
  for (id in sort(u_sc_2)) message(id)
  
  ###
  ###
  ### Prey
  ###
  ###
  
  u_prey_1 = unique(csdata$Prey_1)
  u_prey_2 = unique(csdata$Prey_2)
  sort(u_prey_1)
  sort(u_prey_2)
  
  blank_line()
  blank_line()
  message(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  message(">>>>>>>>>>>>>>>>>>>>>> Prey_1")
  for (id in sort(u_prey_1)) message(id)
  
  blank_line()
  message(">>>>>>>>>>>>>>>>>>>>>> Prey_2")
  for (id in sort(u_prey_2)) message(id)
  
}


###
###
### Investigation IDs and Dates
###
###

u_invcid = unique(csdata$invCID)
u_invcvd = unique(csdata$invCVD)
sort(u_invcid)
sort(u_invcvd)


if (dumpFlag) {
  
  blank_line()
  blank_line()
  message(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  message(">>>>>>>>>>>>>>>>>>>>>> Investigation IDs")
  for (id in sort(u_invcid)) {
    mflag ="."
    if (nchar(id) != 17) mflag = "**********"
    message(id,"     ",mflag)
  }
  
  blank_line()
  message(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  message(">>>>>>>>>>>>>>>>>>>>>> Investigation Dates")
  for (id in sort(u_invcvd)) message(id)
  
  blank_line()
  blank_line()
  
  ###
  ###
  ### Cluster IDs
  ###
  ###
  
  u_centid = unique(csdata$CentID)
  sort(u_centid)
  
  message(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
  message(">>>>>>>>>>>>>>>>>>>>>> Cluster IDs (CentID)")
  for (id in sort(u_centid)) {
    mflag ="."
    if (nchar(id) != 22) mflag = "**********"
    message(id,"     ",mflag)
  }
  
  
  blank_line()
  blank_line()
}

if (sinkFlag) {
  sink(type = "message")
  sink()
}

###
### data sets      -- single kills only
###
### adults         -- all adults
### wtd_adult      -- adult wtd
### elk_adult      -- adult elk
### moose_adult    -- adult moose
### ungulate_adult -- adult ungulate
### beaver         -- beaver
###
### calf data set  -- WTD, Moose, Ungulate
###
### calves         -- all calves
###
### more than single kill data set (2 or 3) -- WTD, ELK, Moose, Beaver
###
### multikill
###

### adults

adults         = kdata[kdata$Calf==0 & kdata$Total==1,]
wtd_adult      = adults[adults$WTD==1,]
elk_adult      = adults[adults$Elk==1,]
moose_adult    = adults[adults$Moose==1,]
ungulate_adult = adults[adults$Ungulate==1,]
beaver         = adults[adults$Beaver==1,]

### calves

calves        = kdata[kdata$Calf==1,]

### multi-kills 

multikill     = kdata[kdata$Total>1,]

###
### Generate Stats
###

detailFlag = FALSE

blank_line()
raa=clu_stats("AllAdults"," All Adults      (Single Kill)",adults,detailFlag)

blank_line()
rwa=clu_stats("WTDAdults"," WTD Adults      (Single Kill)",wtd_adult,detailFlag)

blank_line()
rea=clu_stats("ElkAdults"," Elk Adults      (Single Kill)",elk_adult,detailFlag)

blank_line()
rma=clu_stats("MooseAdults"," Moose Adults    (Single Kill)",moose_adult,detailFlag)

blank_line()
rua=clu_stats("UngAdults"," Ungulate Adults (Single Kill)",ungulate_adult,detailFlag)

blank_line()
rba=clu_stats("Beaver"," Beaver          (Single Kill)",beaver,detailFlag)

blank_line()
rca=clu_stats("Calves"," All Calves      (Single Kill)",calves,detailFlag)

blank_line()
rmk=clu_stats("MultiKill"," All Multikills",multikill,detailFlag)

blank_line()
blank_line()

message(" >>> Summary of Statistics")

blank_line()
message("Group, NumClu, AvgTotalDur, AvgMeanDur, MedianMedianDur")
message(raa[1],", ",raa[2],", ",raa[3],", ",raa[4],", ",raa[5])
message(rwa[1],", ",rwa[2],", ",rwa[3],", ",rwa[4],", ",rwa[5])
message(rea[1],", ",rea[2],", ",rea[3],", ",rea[4],", ",rea[5])
message(rma[1],", ",rma[2],", ",rma[3],", ",rma[4],", ",rma[5])
message(rua[1],", ",rua[2],", ",rua[3],", ",rua[4],", ",rua[5])
message(rba[1],", ",rba[2],", ",rba[3],", ",rba[4],", ",rba[5])
message(rca[1],", ",rca[2],", ",rca[3],", ",rca[4],", ",rca[5])
message(rmk[1],", ",rmk[2],", ",rmk[3],", ",rmk[4],", ",rmk[5])
blank_line()


#######################################################################
##
## Analyze the number of Unique Clusters 
##
##   (1) All Clusters (controlled by unfilter_Flag)
##  
##   (2) Excluding finalIgnore==1
##
##   (3) Excluding finalIgnore==1  and  Updated sort order (redefine what First means) 

## (1) All Clusters
if (unfiltered_Flag) {
  mtitle("All Clusters","Counts")
  basicStats(csdata,"Clusters (Include finalIgnore==1)")
}

###
### TO DO - clean up Filtering on hrinvRep (which is a bad name) and just select the first
###

## (2) Filter Clusters with finalIgnore==1
#
# filter and re-index
#
erdata   = csdata[csdata$finalIgnore==0,]

uinvcid  = unique(erdata$invCID)
nuinvcid = length(uinvcid)

for (id in uinvcid) {
  idlist  = which(erdata$invCID==id)
  nidlist = length(idlist)
  for (j in 1:nidlist) {
    erdata$hrinvRep[idlist[j]] = j-1
  }
}

mtitle("Filter Clusters: Exclude finalIgnore==1","Counts")
basicStats(erdata,"Clusters (Exclude finalIgnore==1)")

## (3) new sort
#
# also need to re-index invCID whenever a new subset is extracted!
##### ToDo: Check Sort Order
#
ersort   = erdata[order(erdata$invCID,erdata$invCVD,erdata$First_date,erdata$First_time),]
##
## create index for invCID 
##
uinvcid  = unique(ersort$invCID)
nuinvcid = length(uinvcid)

for (id in uinvcid) {
  idlist  = which(ersort$invCID==id)
  nidlist = length(idlist)
  for (j in 1:nidlist) {
    ersort$hrinvRep[idlist[j]] = j-1
  }
}

mtitle("Filter Clusters: Exclude finalIgnore==1 and sort(invCID, invCVD, First_date, First_Time)","Counts")
basicStats(ersort,"Clusters (Exclude finalIgnore==1 and Sort)")

#######################################################################
##
## Dump Mapping Tables

## Mapping of Site_cat to Rank 
mtitle("Mapping: Site Category to Rank","Decreasing Order of Rank")
p1=order(cat_rank,decreasing=TRUE)
blank_line()
message(">>> Site Category [ Rank# ]")
for (i in 1:length(p1)) {
  message("    ",cat_list[p1[i]],"  [ ",cat_rank[p1[i]]," ]")
}

## Mapping of Site_cat to Rank 
mtitle("Mapping: Prey_1 to Rank","Decreasing Order of Rank")
p1=order(prey_rank,decreasing=TRUE)
blank_line()
message(">>> Prey_1 [ Rank# ]")
for (i in 1:length(p1)) {
  message("    ",prey_list[p1[i]],"  [ ",prey_rank[p1[i]]," ]")
}

#######################################################################
##
## Write out files with extra columns to help analysis of ambiguity

## Write out extended dataset
#
blank_line()
blank_line()

fnl1 = nchar(killFile1)
fnp1 = substr(killFile1,1,fnl1-4)
killstats = paste(fnp1,"-stats.csv",sep="")
message(" >>> Generating: Cluster Statistics File: ",killstats,"  Rows: ",nrcsdata,"  Cols:",ncol(csdata))
write.table(csdata, file = killstats, sep = ",", col.names = T, row.names =F) #save to csv


## sort by invCID

#
blank_line()

csdatas = csdata[order(csdata$invCID,csdata$hrinvRep,csdata$cluRep),]
killstatss = paste(fnp1,"-statssort.csv",sep="")
message(" >>> Generating: Cluster Statistics Sorted on invCID File: ",killstatss,"  Rows: ",nrcsdata,"  Cols:",ncol(csdata))
write.table(csdatas, file = killstatss, sep = ",", col.names = T, row.names =F) #save to csv

blank_line()

## extract delta rows
#
# extract rows where ranking of Prey_1 or Site_Cat creates a different result
# filter on finalIgnore==0
# ignore minor deltas ("" vs " " vs none)
# 
csdelta = csdata[csdata$ohrPrey_1!=csdata$ihrPrey_1 | csdata$ohrSite_Cat != csdata$ihrSite_Cat,]
csdelta = csdelta[csdelta$finalIgnore==0,]
csdelta = csdelta[csdelta$ihrPrey_1!="none",]

killdelta = paste(fnp1,"-statsdelta.csv",sep="")
message(" >>> Generating: Cluster Statistics Delta (finalIgnore==0) File: ",killdelta,"  Rows: ",nrow(csdelta),"  Cols:",ncol(csdelta))
write.table(csdelta, file = killdelta, sep = ",", col.names = T, row.names =F) #save to csv

blank_line()

## also sort on invCID
#
# extract rows where ranking of Prey_1 or Site_Cat creates a different result
# filter on finalIgnore==0
# ignore minor deltas ("" vs " " vs none)
# 
csdeltas = csdelta[order(csdelta$invCID,csdelta$hrinvRep),]

killdeltas = paste(fnp1,"-statsdeltasort.csv",sep="")
message(" >>> Generating: Cluster Statistics Delta (finalIgnore==0) Sorted on invCID File: ",killdeltas,"  Rows: ",nrow(csdeltas),"  Cols:",ncol(csdeltas))
write.table(csdeltas, file = killdeltas, sep = ",", col.names = T, row.names =F) #save to csv

## Output all unique Ambiguities in invCID order
##

sclist = c("foobar")
p1list = c("foobar")

nrcsds = nrow(csdeltas)

blank_line()
message(" >>> Output all Ambiguities per invCID")
blank_line()

for (i in 1:nrcsds) {
  
  aname1 = paste(csdatas$hrinvCID[i]," SCa:",csdatas$ohrSite_Cat[i]," SCb:",csdatas$ihrSite_Cat[i],sep="")
  if (!is.na(csdatas$ohrSite_Cat[i]) & !is.na(csdatas$ihrSite_Cat[i])) {
    if (csdatas$ohrSite_Cat[i] != csdatas$ihrSite_Cat[i]) {
      if (length(which(sclist==aname1)) < 1) {
        message("Index: ",i," Site_Cat: ",aname1)
        sclist=append(sclist,aname1) 
      }
    }
  }
  
  aname2 = paste(csdatas$hrinvCID[i]," P1a:",csdatas$ohrPrey_1[i]," P1b:",csdatas$ihrPrey_1[i],sep="")
  if (!is.na(csdatas$Prey_1[i]) & !is.na(csdatas$Prey_1[i])) {
    if (csdatas$ohrPrey_1[i] != csdatas$ihrPrey_1[i] & csdatas$ihrPrey_1[i]!= "none" & csdatas$ohrPrey_1[i]!= "" & csdatas$ohrPrey_1[i]!= " ") {
      if(length(which(p1list==aname2)) < 1 ) {
        message("Index: ",i," Prey_1:   ",aname2)
        p1list=append(p1list,aname2)
      }
    }
  }
}

blank_line()
blank_line()

message(" >>>>> Note - stopped sink")

