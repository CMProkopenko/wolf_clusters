####################################################################
#
# Generate test kill site lists
#
# v1 -  initial verison
#
#       Generate File: CluSiteList - of all matched Clusters for all 12 site categories
#         Kill
#         Probable kill
#         Scavenge
#         Probable scavenge
#         Resting
#         Probable resting
#         Probable prey encounter
#         Unknown
#         Den
#         Rendez-vous
#         MORT
#         Scat
#
#       Generate File: AmbSiteList - of all Clusters that were Ambigous (>1 site category)
#
# v2 -  add new output file - breakdown by wolf by month for each site category
#
#       Generate File: kSumBy_Wolf
#
#       There are 15 rows per WolfID - the 12 site categories
#         Kill
#         Probable kill
#         Scavenge
#         Probable scavenge
#         Resting
#         Probable resting
#         Probable prey encounter
#         Unknown
#         Den
#         Rendez-vous
#         MORT
#         Scat
#       Followed by the 3 totals
#         Total_sites               - the sum across all 12 site categories
#         Kill_and_Pkill            - the total of Kill and Probable kill
#         Ambigous_sites            - how many sites in this month are Ambigous (with two different site categories) *** NOT IMPLEMENTED ***
#
# v3 -  use Cluster Match_SZ cleaning.csv as input file
#
# v4 -  check which kill site matched are the first match - EARLY matches are used as a last resort
#         earlyFlag [0 or 1]
#         daysEarly
#         killSiteMatches - total number of matches of Clusters with Investigated Kill Sites
#         firstFlag - earliest Cluster match, first pick matches that are not early, then use early
#
#
# v5 -  copy over species (Prey_spp)
#
#       add threshold limit to determine if too early when setting earlyFlag
#
thres_maxearly = 7  # in days, do not match if earlier than this
#
# v6 -  change directory names to Final
#
# v7  - rename program from testKill to invMatch
#
#       defined Winter to be Nov thru Apr for consistency
#
#       add support for cross matching investigated kill sites across different wolf IDs
#       iff invCID is the same
#          earlist match (that isnt deemed early) defines the start time
#          calculate total matches across all wolves
#          also calculate duration as (last point of latest cluster) - (first point of earliest cluster)
#          extract UTC month and hour values for easy binning
#
#       in addition to checking if same cluster matches two different investigated site categories
#          also check if the species are different between any of the Kills
#     
# v8  - add in a lateness check - is investigation too late versus last point of cluster?
#
thres_maxlate = 60  # in days, do not match if later than this
#
#       lateFlag            is set based on threshold, complement to earlyFlaG
#
#       remRow              row is to be removed based on key values in manual remove file, a value>0 indicates which row in file matched
#                           file name: Manual_Remove_YYYY-DD-MM.csv
#
#       ignoreFlag          ignore this match when calcuating multi-wolf clusters
#                           = earlyFlag != 0  |  lateFlag != 0  |  ignoreBeaver != 0
#
#       Clarify ambiguity flags:
#       - ambFlg            one or more conditions detected
#       - cluSiteCatDiff    different Site Category for the matching clusters
#       - cluPreySppDiff    different Prey Species for the matching Kill clusters
#       - invSiteCatDiff    different Site_Cat for the same Investigation ID
#       - cluRendezKillMix  a mix of Rendezvous and Kill site categories for the same cluster
#
# v9  - add additional duration calculations
#       - mwKdurMin
#       - mwKdurMax
#       - mwKdurMean
#       - mwKdurMedian
#
#       add a flag to check for scavenge
#       - cluScavengeKillMix  a mix of Scavenge and Kill site categories for the same cluster
#
#       add an ignoreBeaver  -  if the Beaver Kill site is mixed with Rendezvous
#
# v10 - add gapcount to each group = number of clusters (except for the first) that do not overlapwith at least one other
#
# v11 - create invRep "replica-number" to identify investigations with the same invCID that have different (Notes or Site Category)
#       copy matchMdelta (meters difference between Cluster Centroid and Investigation location)
#       rename flags: cluxxx ==> wwa algorithm related check and invxxx = investigated site check
#
# v12 - add project flag up from to differentiate between 
#       Data     ==> RMNP Data
#       DataGHA  ==> GHA-26 Data
#
# v13 - remove Columns ending in ".M"
#
# v14 - Output all columns in the Match_Sites and sort both by Clu and inv
#
# v15 - handle different names for WTD (Either Deer or WTD)
#
# v16 - add CluRep field
#     - use all JoinStatus for calculations - otherwise added columns willbe empty
#
# v17 - add dupIndex column, check if keys (CentID	cluRep	Site_Cat	invCID	invRep) are unique
#     - add Calf flag column, based on Prey_age_class
#     - add cmdRow    (last one found)
#     - add cmdCount  (count number of commnds affecting same row)
#     - add cmdAction (last one found)
#     - add Behaviour_1, Prey_1, Behaviour_2, Prey_2	
#     - add Notes_2, CMP_Comments
#
# v18 - move cluRep calculation before command processing
#     - save original Site_Cat in oSiteCat
#     - save original Prey_spp in oPrey_spp
#     - default Behavior_1 to Site_Cat
#     - default Prey_1     to Prey_spp
#     - process Actions from manual file
#     
#     - add a finalIgnore (that applies Actions)
#
# v19 - replace cluGroup with WolfCount 
#     - calculate WolfCount as the number of unique Wolf IDs in a Multi-Wolf Group
#     - Modify commands Ok and Edit ==> clear finalIgnore flag
#
# v20 - add in unique counts
#
# v21 - rename program to cluMatchv21 - and add in the ability to count all kills and related clusters
#
#     - switch myproj and directory to DataKill
#     - behGroup selection
#       1=Kill only
#       2=Kill and Probable kill
#       3=Kill, Probable kill and Scavenge
#     - twoBeavers 
#       0=disabled
#       1=enabled (look for special string "Two beavers" and set count to 2 instead of 1)
#     - add columns
#       Ungulate
#       Birds (Raven, Ruffed Grouse, Canada Goose)
#       Bear
#       Pig
#       Mink
#       Total
#     - Add in parsed date & time fields
#       year
#       day
#       min
#     - the earlist cluster for each wolf in the mw cluster has mwKf4Wolf==1
#     - counts are copied to all wolves in mw cluster
#
# v22 - Implement behGroup
#
# v23 - Add #columns to file of column names
#     - change file names from ...Sites to ...Clu
#
# v24 - prototyping
#
# v25 - Add support for GHA-26
#     - input column changed to Comments (from CMP_Comments)
#     - copied KillAge column to Prey_age_class column (both are present)
#     - added option to change cluRep and invRep, from 0, to NA
#     - generated statistics for cluRep and InvRep
#     - add default mac directory
#
# v26 - "Fawn" should be treated as "Calf" for analysis
#     - new option fawn2calf (default TRUE)
#     - fixed some minor bugs in file size output (wrong # rows displayed)
#
# v27 - provide breakdown of KillAge and Prey_age_class
#
# v28 - allow spaces and mixed cases in commands
#
# v29 - fixes to separate Collar IDs and Wolf IDs
#
# v30 - only enable GHA specific options when GHA
#       fix_CollarId
#       fawn2calf
#
# ToDo-P1
#     - add prefix to column names file
#     - read in GHA26_wolf_collar_data_2014-2020_sorted.csv
#     - CollarID being used incorrectly as WolfIDs
#       oCollarID = original CollarID
#       CollarID  = function of Animal_ID
#
# ToDo-P2
#
# ToDo-P3
#     - review "Double Kill" processing
#     - audit column names
#     - check K, Pk, groupings
#
####################################################################
####################################################################
##
## This software is licensed under: https://opensource.org/license/mit
##
## For more information refer to the GitHub Project: https://github.com/CMProkopenko/wolf_clusters
##
####################################################################

####################################################################
####################################################################
#
# Select Project Type
#
# Data     ==> RMNP
# DataGHA  ==> GHA-26
#
myproj = "DataKill"
myproj = "DataGHA"

#
# Select which type of Behaviors to use for "Kill" counts
#
# behGroup:
#    1: Kill
#    2: Kill, Probable kill
#    2: Kill, Probable kill, Scavenge
#
behGroup = 1

behName =      c("Kill","Probable kill","Scavenge")
behGroupName = c("K",   "KPk",          "KPkS")

#
# Select if extra beaver count based on string search for "Two beavers" in Notes_2 (RMNP specific)
#    0: no extra counts
#    1: count 2 if Notes contains "Two beavers"
#
twoBeavers=1


####################################################################
####################################################################

### Load packages 
#
libs <- c('dplyr','data.table', 'ggplot2','grid','gridExtra','lattice')
lapply(libs, require, character.only=TRUE)

#
# Define program defaults
#
# Program Name and Version
progname = "cluMatch_v30"


#
# Debug output
debugFlag = TRUE
debugW    = "Wxx"

#
# Prompt for Files instead of using defaults
promptFileNames = FALSE

##########################################################################################
#####
##### Start Project Settings for    RMNP
#####
##########################################################################################
#
# Results directory   to use
# Merged Cluster file to use
# Cluster Tolerance to use to create name for Matched file

###
# Results Directory
#
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
#cluster_tolerance = 0
#cluster_tolerance = 15
#cluster_tolerance = 50
cluster_tolerance = 25

###
# Cluster Site Files
#
Clu_Prefix = paste("2023-10-21_RMNP_",behGroupName[behGroup],"_",sep="")

ksname = paste(Clu_Prefix,"Matched_Clu_sortedInv.csv",sep="")
ksname2= paste(Clu_Prefix,"Matched_Clu_sortedClu.csv",sep="")
jsname = paste(Clu_Prefix,"All_Clu.csv",sep="")
ssname = paste(Clu_Prefix,"SumBy_Wolf.csv",sep="")
jcname = paste(Clu_Prefix,"Column_Names.csv",sep="")

###
# Manual Remove File
#
manRemFile = "Manual_Actions_2021-07-01.csv"
manFixZero = TRUE  ## if zeros are used map them to NA to simply key creation
manVerbose = FALSE  ## output each command as it is processed
#
# Disable the Actions for Testing
# manRemFile = "Manual_Remove_Empty.csv"

#
# Cross Reference File
#xrefFile = paste("Join-Matched-CTM",cluster_tolerance,"-",groupFile,sep="")
xrefFile = "ClusterMatch_SZcleaningMod20210502.csv"

#
# fix CollarID if TRUE
#
fix_CollarID = FALSE

#
# change Fawn to Calf when KillAge is cloned to Prey_age_class
#
fawn2calf = FALSE

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
  resultsDir = "loc-CCv33utm15/results"
  
  ###
  # Manual Edits
  #
  manFixZero   = TRUE  ## if zeros are used map them to NA to simplify key creation
  #manRemFile   = "GHA26_Manual_Edits_05-03-2023_jp1.csv"
  #manRemFile   = "GHA26_Manual_Edits_Empty.csv"
  #manRemFile   = "GHA26_Manual_Edits_05-24-2023.csv"
  manRemFile   = "GHA26_Manual_Edits_05-24-2023jp1.csv"

  ###
  # Joined File
  #
  #groupFile = "mergedWmc2-loc-CCv33utm15-BLEND_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.csv"
  groupFile  = "mergedWmc2-loc-CCv33utm15-BLEND_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.csv"
  
  ####
  # Cluster Tolerance (tolerance in meters added to buffer)
  #
  #cluster_tolerance = 0
  #cluster_tolerance = 15
  cluster_tolerance = 25
  #cluster_tolerance = 50
  
  ###
  # Cluster Site Files
  #
  
  Clu_Prefix = paste("2023-10-21_GHA_",behGroupName[behGroup],"_",sep="")
  ksname = paste(Clu_Prefix,"Matched_Clu_sortedInv.csv",sep="")
  ksname2= paste(Clu_Prefix,"Matched_Clu_sortedClu.csv",sep="")
  jsname = paste(Clu_Prefix,"All_Clu.csv",sep="")
  ssname = paste(Clu_Prefix,"SumBy_Wolf.csv",sep="")
  jcname = paste(Clu_Prefix,"Column_Names.csv",sep="")

  ###
  # Cross Reference File
  #
  xrefFile = paste("Join-Matched-CTM",cluster_tolerance,"-",groupFile,sep="")
  
  #
  # fix CollarID if TRUE
  #
  fix_CollarID = TRUE
  fix_map      = "GHA26_wolf_collar_data_2014-2020_sorted3.csv"
  
  #
  # change Fawn to Calf when KillAge is cloned to Prey_age_class
  #
  fawn2calf = TRUE

  ##########################################################################################
  #####
  ##### End Project Settings for         GHA-26
  #####
  ##########################################################################################
  
}


root_dir<-function(){
	# Look for the root directory in order of preference
	dir1 = "C:/Wolf-Projects/"
	dir1 = paste(dir1,myproj,sep="")
	if( file.exists(dir1) ) return(dir1)
	dir1 = "D:/Wolf-Projects/"
	dir1 = paste(dir1,myproj,sep="")
	if( file.exists(dir1) ) return(dir1)
	dir1 = "E:/Wolf-Projects/"
	dir1 = paste(dir1,myproj,sep="")
	if( file.exists(dir1) ) return(dir1)
	# default
	#dir1 = "C:/"
	dir1 = dir1 = "~/Documents/Masters/Data/GHA26_Wolf_Clusters"
	return (dir1)
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

getInvest<-function(){
  data = {}
  fname1  <- investFile
  fnameI  <- paste(myDir,"/",resultsDir,"/",fname1,sep="")
  skip_line()
  if (promptFileNames) {
    message(" >>> Select: Wolf Investigation  File: ")
    fnameI = file.choose()
  }
  data  = read.csv(fnameI,stringsAsFactors=FALSE)
  ldata = length(data[,1])
  message(" >>> Processing: Investigation File: ","  Length: ",ldata,"  Name:", fnameI)
  skip_line()
  return(data)
}

getGroup<-function(){
  data = {}
  fname1  <- groupFile
  fnameI  <- paste(myDir,"/",resultsDir,"/",fname1,sep="")
  if (promptFileNames) {
    message(" >>> Select: Wolf Cluster Group File: ")
    fnameI = file.choose()
  }
  skip_line()
  data  = read.csv(fnameI,stringsAsFactors=FALSE)
  ldata = length(data[,1])
  message(" >>> Processing: Merged MC2    File: ","  Length: ",ldata,"  Name:", fnameI)
  skip_line()
  return(data)
}

getXref<-function(){
  data = {}
  fname1  <- xrefFile
  fnameI  <- paste(myDir,"/",resultsDir,"/",fname1,sep="")
  if (promptFileNames) {
    message(" >>> Select: Wolf Cluster Match File: ")
    fnameI = file.choose()
  }
  skip_line()
  data  = read.csv(fnameI,stringsAsFactors=FALSE)
  ldata = length(data[,1])
  message(" >>> Reading: Join-  File: ","  Length: ",ldata,"  Name:", fnameI)
  skip_line()
  return(data)
}

check_cmd<-function(ocmd) {
  # change to lower case and remove white space
  lcmd = tolower(ocmd)
  lcmd = gsub(" ","",lcmd)
  ccmd = ocmd
  if (lcmd == "ok")     ccmd = "Ok"
  if (lcmd == "edit")   ccmd = "Edit"
  if (lcmd == "review") ccmd = "Review"
  if (lcmd == "early")  ccmd = "Early"
  if (lcmd == "late")   ccmd = "Late"
  if (lcmd == "remove") ccmd = "Review"
  if (lcmd == "skip")   ccmd = "Skip"
  return(ccmd)
}

###################################################################
###################################################################
#
#                    Start Main Script
#
###################################################################
###################################################################

options(stringsAsFactors = FALSE)

message(">>>>>>>>>> Create: ",progname,"  --  Time: ",Sys.time())
skip_line()
message(" >>> Checking Root Directories <<< ")
myDir = root_dir()
message(" >>> Root Dir = ",myDir)
blank_line()

ksfile   <- paste(myDir,"/",resultsDir,"/",ksname,sep="")
ksfile2  <- paste(myDir,"/",resultsDir,"/",ksname2,sep="")
jsfile   <- paste(myDir,"/",resultsDir,"/",jsname,sep="")
ssfile   <- paste(myDir,"/",resultsDir,"/",ssname,sep="")
jcfile   <- paste(myDir,"/",resultsDir,"/",jcname,sep="")

#### Join File

xrefData   = getXref()
nrxref     = length(xrefData[,1])
ncxref     = length(xrefData[1,])

##
##  Add a xref id so that the aggregate kill clusters can be rejoined later
##

xrefID   = xrefData$rowID
xrefID[] = 0
xrefID   = seq(1,nrxref)
xrefData = cbind(xrefID,xrefData)

message(" >>> Processing: Cluster File: ",xrefFile,"  Rows: ",nrxref,"  Cols: ",ncxref)
skip_line()
message(" >>> Matching: ",behGroup," Behaviors  Tag: ",behGroupName[behGroup])
for (i in 1:behGroup) {
  message("     Behaviour[",i,"] = ",behName[i])
}

blank_line()
if (twoBeavers==1) {
  message(" >>> Extra count for Two beavers: Enabled") 
} else {
    message(" >>> Extra count for Two beavers: Disabled")
  }
blank_line()

#######################################
#
#   Add in new Columns
#
#######################################

matdata = xrefData

### 
### save original Collar ID 
###
oCollarID = matdata$CollarID

skip_line()
if (fix_CollarID) {
  message(" >>> Fixing GHA COllarIDs: Enabled") 
  ffile   <- paste(myDir,"/",resultsDir,"/",fix_map,sep="")
  message(" >>>                 File: ",ffile)
  fixmap  = data  = read.csv(ffile,stringsAsFactors=FALSE)
  nrfix   = length(fixmap[,1])
  ncfix   = length(fixmap[1,])
  message(" >>> Processing: Fix Map: ","  Rows: ",nrfix,"  Cols:", ncfix)
  #
  # create a Wolf ID from the Animal ID
  #
  fixmap$WolfID=paste("W",substr(fixmap$Animal_ID,6,7),sep="")
  fixmap$start_yy = as.integer(substr(fixmap$Start_Date,3,4))
  fixmap$end_yy   = as.integer(substr(fixmap$End_Date,3,4))
} else {
  message(" >>> Fixing GHA COllarIDs: Disabled")
}
blank_line()


# message(" >>>      Matched Rows: ",nrmatdata)

daysEarly     = matdata$matchDdelta
daysEarly     = -daysEarly
earlyFlag     = ifelse(daysEarly > thres_maxearly, 1, 0)
lateFlag      = ifelse(daysEarly > (-thres_maxlate), 0, 1)
KillSiteMatch = ifelse(daysEarly > thres_maxearly, 0, 0)
firstFlag     = ifelse(daysEarly > thres_maxearly, 0, 0)
remRow        = ifelse(daysEarly > thres_maxearly, 0, 0)
ignoreBeaver  = ifelse(daysEarly > thres_maxearly, 0, 0)


###
### define a invCID replicate count - invRep
###
invCID   = matdata$invCID
invRep   = matdata$matchDdelta
invRep[] = 0


###
### define columns for the Manual Commands
###
### cmdAction - Ok, Edit, Remove, Late, Early, Review
### cmdRow
###
###
cmdAction      = matdata$invCID
cmdAction[]    = "."
cmdRow         = cmdAction
cmdRow[]       = 0
cmdCount       = cmdRow
cmdCount[]     = 0
Behaviour_1    = cmdAction
Behaviour_1[]  = ""
Behaviour_2    = Behaviour_1
Prey_1         = Behaviour_1
Prey_2         = Prey_1
Notes_2        = Behaviour_1
Comments       = Notes_2

###
### save original columns
###
oSite_Cat = matdata$Site_Cat
oPrey_spp = matdata$Prey_spp

###
### define a CentID replicate count - cluRep
###
cluRep     = invRep

###
### Mark Duplicates of the 4 Keys 
###
dupIndex  = invRep
dupIndex[] = 0

### determine invRep

uinvcid  = unique(invCID)
nuinvcid = length(uinvcid)

ninvreplicas = 0 
for (icid in uinvcid) {
  ilist = which(invCID == icid)
  nilist= length(ilist)
  rilist= integer(nilist)
  rilist[]= 0
  ninv  = max(length(unique(matdata$Site_Cat[ilist])),length(unique(matdata$Notes[ilist])))
  if(ninv > 1) {
    #message(" $$$$$ DEBUG Checking =",icid," ",nilist)
    # one or more of Site_Cat or Notes are different for the same invCID
    ninvreplicas = ninvreplicas + 1
    # add _replica# to each invCID
    # assume the first invCID is replica 1
    index            = 1
    rilist[1]        = 1
    invRep[ilist[1]] = index
    #invCID[ilist[1]] = paste(invCID[ilist[1]],"_",index,sep="")
    for (i in 2:nilist) {
      jfound = FALSE
      for (j in 1:(i-1)) {
        #message(" $$$$$ DEBUG (i) ",i," ",ilist[i]," ",matdata$Site_Cat[ilist[i]],"  (j) ",j," ",ilist[j]," ",matdata$Site_Cat[ilist[j]])
        if ((matdata$Site_Cat[ilist[i]] == matdata$Site_Cat[ilist[j]]) & (matdata$Notes[ilist[i]] == matdata$Notes[ilist[j]]) ) 
          {
            # match for replica 
            #invCID[ilist[i]] = paste(invCID[ilist[i]],"_",rilist[j],sep="")
            invRep[ilist[i]] = rilist[j]
            rilist[i] = rilist[j]
            jfound = TRUE
            #message(" $$$$$ DEBUG Found =",icid," ",ninv," ",index," ",i," ",j," ", invCID[ilist[i]])
            break
          }
      }
      if (!jfound) {
        index = index + 1
        #message(" $$$$$ DEBUG #Not Found =",icid," ",ninv," ",index," ",i)
        
        #invCID[ilist[i]] = paste(invCID[ilist[i]],"_",index,sep="")
        invRep[ilist[i]]  = index
        rilist[i] = index
      }
    }
  }
}
#message(" $$$$$ DEBUG #Replicas =", ninvreplicas)

####
####
#### GHA-26 does not contain a Prey_age_class
#### so create one on the fly and copy the KillAge column
#### 

if (myproj == "DataGHA") {
  matdata$Prey_age_class   = matdata$PackID
  matdata$Prey_age_class[] = ""
}

matdata$Prey_age_class = matdata$KillAge

####
#### Change "Fawn" to "Calf" 
####

nfawno = length(which(matdata$Prey_age_class == "Fawn"))
nfawna = nfawno

if (fawn2calf == FALSE) {
  skip_line()
  message(" >>> Convert Fawn to Calf Disabled")
  message( ">>>         Num Fawn in Prey_age_Class: ",nfawno)
}

if (fawn2calf == TRUE) {
  skip_line()
  message(" >>> Convert Fawn to Calf Enabled")
  message(" >>>         Before: Num Fawn in Prey_age_Class: ",nfawno)
  rfawn=which(matdata$Prey_age_class=="Fawn")
  matdata$Prey_age_class[rfawn] = "Calf"
  nfawna = length(which(matdata$Prey_age_class == "Fawn"))
  message(" >>>         After:  Num Fawn in Prey_age_Class: ",nfawna)
}

####
####
#### set a common ignoreFlag - that is independent of manual commands
####
####

ignoreFlag    = ifelse( (earlyFlag != 0)  |  (lateFlag != 0)  |  (ignoreBeaver != 0), 1, 0) 

####
#### the finalIgnore flag can be overridden by manual commands
####

finalIgnore   = ignoreFlag

kdata           = cbind(matdata$xrefID,matdata$rowID,matdata$CollarID,oCollarID,matdata$PackID,matdata$First_date,matdata$First_time,matdata$Last_date,matdata$Last_time,matdata$CentID,cluRep,matdata$Site_Cat,oSite_Cat,matdata$invCID,invRep,matdata$invCVD,cmdRow,cmdCount,cmdAction,Behaviour_1,Prey_1,Behaviour_2,Prey_2,Notes_2,Comments,finalIgnore,ignoreFlag,ignoreBeaver,earlyFlag,lateFlag,daysEarly,matdata$matchMdelta,KillSiteMatch,firstFlag,matdata$Prey_spp,oPrey_spp,matdata$Prey_age_class,matdata$Notes,dupIndex)
kdata           = data.frame(kdata,stringsAsFactors = FALSE)
zdata           = kdata

####
#         CentID            - Cluster Centroid ID
#         cluRep            - if the CentID is a Repliace this is the Replica# to use
#         SiteCat           - type of site
#         invCID            - Investigation Cluster ID   
#         invRep            - If the invCID is a Replica this is the Replica# to use
#         invCVD            - Investigation Date
#         ignoreFlag        - this row is to be ignored
#         ignoreBeaver      - ignore Beaver Kill sites mixed with Rendezvous sites
#         earlyFlag         - [0 or 1] = Investigation happended thres_maxearly before (last point of) matched cluster
#         lateFlag          - [0 or 1] = Investigation happended thres_maxlate  after  (last point of) matched cluster
#         daysEarly         - [0 to n] = Cluster Date - Investigation Date 
#         matchMdelta       - #meters between the Cluster Centroid and The Investigation coordinates
#         killSiteMatch     - total number of matches of Clusters with Investigated Kill Sites
#         firstFlag         - earliest Cluster match, only pick matches that are not early (within a same )
#
# CollarID	First_date	First_yr	First_time	Last_date	Last_time
#
####
#
# Behaviour_1, Prey_1, Behaviour_2, Prey_2	
#
colnamesk       = c("xrefID","rowID","CollarID","origCollarID","PackID","First_date","First_time","Last_date","Last_time","CentID","cluRep","Site_Cat","oSite_Cat","invCID","invRep","invCVD","cmdRow","cmdCount","cmdAction","Behaviour_1","Prey_1","Behaviour_2","Prey_2","Notes_2","Comments","finalIgnore","ignoreFlag","ignoreBeaver","earlyFlag","lateFlag","daysEarly","matchMdelta","KillSiteMatch","firstFlag","Prey_spp","oPrey_spp","Prey_age_class","Notes","dupIndex")
colnames(kdata) = colnamesk
nrkdata         = length(kdata[,1])
kdata           = kdata[order(kdata$CollarID,kdata$invCID,kdata$invRep,kdata$Site_Cat,kdata$First_date,kdata$First_time),]
kdata$xrefID    = as.numeric(kdata$xrefID)
kdata$cmdCount  = as.numeric(kdata$cmdCount)


####
#### update CollarID column to be the WolfID based on the Year
#### This should only be enabled for GHA
####
if (fix_CollarID) {
  blank_line()
  message(" >>> Mapping CollarIDs")

  bnucol_data = length( unique(kdata$CollarID) ) 
  nucol_map   = length( unique(fixmap$Collar_ID) )
  nuwid_map   = length( unique(fixmap$WolfID) )
  message(" >>>         Before: Num Unique CollarID in Data: ",bnucol_data)
  message(" >>>         Before: Num Unique CollarID in Map : ",nucol_map)
  message(" >>>         Before: Num Unique WolfID   in Map : ",nuwid_map)
  skip_line()
  nrk1        = length(kdata[,1])
  message(" >>> Processing CollarIDs  Rows=",nrk1)
  skip_line()
  #
  # map each row
  #
  nmerr   = 0
  nmlimit = 100
  for (irow in 1:nrk1) {
    mrow = 0
    colid   = kdata$CollarID[irow]
    # skip the matching if ColarID is NA
    if ( is.na(colid) ) break
    fixlist = fixmap[fixmap$Collar_ID==colid,]
    nlist = nrow(fixlist)
    if (nlist < 1) {
      #### ERROR
      nmerr = nmerr + 1
      if (nmerr <= nmlimit) {
        message(" >>> ERROR CollarID NOT Found in fixmap: ",colid)
      }
    }
    if (nlist == 1) {
      #### Only a single match for CollarID
      #### defer date range check
      mrow = 1
    }
    if (nlist > 1) {
      #### Search for Match
      mrow = 0
      row_date = kdata$First_date[irow]
      nlist = nrow(fixlist)
      for (jrow in 1:nlist) {
        ### Use eXtended Dates to get an initial match - confirm against exact dates later
        if ( (row_date >= fixlist$Start_DateX[jrow]) & (row_date <= fixlist$End_DateX[jrow]) ) mrow = jrow
      }
    }
    if (mrow == 0) {
      #### ERROR
      nmerr = nmerr + 1
      if (nmerr <= nmlimit) {
        if (nlist == 0) {
          message(" >>> ERROR CollarID NOT Found: ",colid,"  row: ",irow,"  Len List: ",nlist)
        }
        if (nlist == 1) {
          message(" >>> SWERR CollarID NOT Found - even though mapping found: ",colid,"  row: ",irow,"  Len List: ",nlist)
        }
        if (nlist > 1) {
          message(" >>> ERROR CollarID NOT Found - Date Range Mismatch: ",colid,"  row: ",irow,"  xrefID: " ,kdata$xrefID[irow],"  Len List: ",nlist)
          message(" >>>                                                 CentID: ",kdata$CentID[irow],"  Site_Cat: ",kdata$Site_Cat[irow]," First_date: ",kdata$First_date[irow])
          for (jrow in 1:nlist) {
            message(" >>>    Match dates: ",jrow,"  Start: ",fixlist$Start_Date[jrow],  "End: ",fixlist$End_Date[jrow])
          }
        }
      }
    }
    
    if (mrow > 0) {
      #### Update CollarID
      kdata$CollarID[irow] = fixlist$WolfID[mrow]
    }
    
    if (mrow > 0) {
      #### Check detailed date range
      row_date = kdata$First_date[irow]
      wolfid   = kdata$CollarID[irow]
      ocolid   = kdata$origCollarID[irow]
      if ( (row_date < fixlist$Start_Date[mrow]) | (row_date > fixlist$End_Date[mrow]) ) {
        message(" >>> Warning - Date Range Mismatch: new CollarID(WolfID): ",wolfid,"  origCollarID: ",ocolid,"  row: ",irow,"  xrefID: " ,kdata$xrefID[irow],"  Len List: ",nlist)
        message(" >>>                                                 CentID: ",kdata$CentID[irow],"  Site_Cat: ",kdata$Site_Cat[irow]," First_date: ",kdata$First_date[irow])
      }
      
        
    if (mrow > 0) {
      #### Check PackID
      row_date = kdata$First_date[irow]
      wolfid   = kdata$CollarID[irow]
      ocolid   = kdata$origCollarID[irow]
      packd = kdata$PackID[irow]
      packm = fixlist$Pack_Code[mrow]
      if (packd != packm) {
        nmerr = nmerr + 1
        if (nmerr <= nmlimit) {
          message(" >>> Warning - Pack ID Mismatch: new CollarID(WolfID): ",wolfid,"  origCollarID: ",ocolid,"  row: ",irow,"  xrefID: " ,kdata$xrefID[irow],"  Len List: ",nlist)
          message(" >>>                                  Cluster  PackID: ",packd,"  mrow: ",mrow,"  map Pack_Code: ",packm)
          message(" >>>                                           CentID: ",kdata$CentID[irow],"  Site_Cat: ",kdata$Site_Cat[irow]," First_date: ",kdata$First_date[irow])
        }
      }
    }
  }
  }
  ucolid = unique(kdata$CollarID) 
  ucolid = ucolid[!is.na(ucolid)]
  nucid = length( ucolid ) 
  skip_line()
  message(" >>>         After:  Num Unique CollarID (now WolfID) in Data: ",nucid)
}

#####
##### default Behaviour_1 to Site_Cat and Prey_1 to Prey_spp
#####

kdata$Behaviour_1  = kdata$Site_Cat
kdata$Prey_1       = kdata$Prey_spp

blank_line()
message(" >>> Cluster List #rows: ",nrkdata)
skip_line()

####
#### Output summary of #Clusters by Colf ID and Pack ID
####

uwid  = unique(kdata$CollarID)
uwid  = sort(uwid)
nuwid = length(uwid)

upid  = unique(kdata$PackID)
upid  = sort(upid)
nupid = length(upid)

blank_line()
message(" >>> Cluster counts by Wolf ID and Pack ID")

###
### Extract rows that have CollarIDs
###
kdatac = kdata[!is.na(kdata$CollarID),]

for (i in 1:nuwid) {
  if (is.na(uwid[i])) break
  for (j in 1:nupid) {
    if (is.na(upid[j])) break
    kdataw = kdatac[kdatac$CollarID==uwid[i],]
    nclu1   = length(kdataw[,1])
    if (nclu1 > 0) {
      kdatawp = kdataw[kdataw$PackID==upid[j],]
      nclu2 = length(kdatawp[,1])
      if(nclu2 > 0) {
        message(" >>>     WolfID: ",uwid[i],"  PackID: ",upid[j],"  Clusters: ",nclu2)
      }
    }
  }
}
blank_line()

###
### Identify any Wolf IDs without Clusters
###

blank_line()
message(">>> Identifying CollarID(aka WolfID) that have zero(0) Clusters")

uwmap = unique(fixmap$WolfID)
uwmap = sort(uwmap)

for (i in uwmap) {
  nmap = length(which(uwid==i))
  if (nmap==0) {
    message(">>> CollarID/WolfID: ",i,"  Zero(0) Clusters")
  }
}

###########################################################################
###########################################################################
##
##   Calculate cluRep
##
###########################################################################
###########################################################################

kdata2  = kdata[ order(kdata$CentID,kdata$cluRep,kdata$invCID,kdata$invRep) , ]

nrkdata2= nrow(kdata2)
ucentid = unique(kdata2$CentID)

####
#### TODO verify clurep calculation
####      wht does the code mix kdata and kdata2
####

for (ic in ucentid) {
  if ( !is.na(ic)) {
    xlist = which(kdata2$CentID == ic)
    nx = length(xlist)
    if (nx > 1) {
      # CentID present more than once
      # use xrefID to update kdata
      for (j in 1:nx) {
        ixref = kdata2$xrefID[xlist[j]]
        irows = which(kdata$xrefID == ixref)
        irow = irows[1]
        if (length(irows) != 1) message(" ERROR calculating cluRep ",ic," ",ixref," ",irows," ",irow)
        kdata$cluRep[irow] = j
      }
    }
  }
}

###########################################################################
###########################################################################
##
##   Process "Manual Remove File" if it exists
##
##   (1) Check all actions are allowed
##   (2) Check key is one of the 4 options
##   (3) check key matches
##   (4) Count rows affected
##   (5) Perform Actions
##
###########################################################################
###########################################################################

fname1  <- manRemFile
fnameI  <- paste(myDir,"/",resultsDir,"/",fname1,sep="")
nrmanrem = 0
ncmanrem = 0

blank_line()
blank_line()

if (file.exists(fnameI)) {
  mrdata   = read.csv(fnameI,stringsAsFactors=FALSE)
  nrmanrem = length(mrdata[,1])
  ncmanrem = length(mrdata[1,])
  message(" >>> Processing: Manual Remove File: ",fnameI,"  Rows: ",nrmanrem,"  Cols: ",ncmanrem)
} else {
  message(" >>> NOT FOUND: Manual Remove File: ",fnameI)
}

############################################################################
##
## sanity check of commands
##
############################################################################

blank_line()

totnumrem = 0
keynumrem = 0

ncerr   = 0
ncwrn   = 0
ncrows  = 0
numskip = 0

ncopt1   = 0
nclist1  = 0
ncopt2   = 0
nclist2  = 0
ncopt3   = 0
nclist3  = 0
ncopt4   = 0
nclist4  = 0

if (nrmanrem > 1) {
  message(" >>>>>>>>> Checking Commands")
  blank_line()
  ##
  ## First count all cluRep and invRep set to zero
  ##
  n0clurep = length(which(mrdata$cluRep==0))
  n0invrep = length(which(mrdata$invRep==0))
  if ((n0clurep+n0invrep) == 0) {
    message(" >>> All cluRep and invRep non-zero ")
    blank_line()
  } else {
    message(" >>> num zero cluRep: ",n0clurep)
    message(" >>> num zero invRep: ",n0invrep)
    blank_line()
  }
  
  if (manFixZero) {
    skip_line()
    message(" >>> Fixing zero cluRep and invRep enabled -- map to NA")
    ##
    ## fix zero's
    ##
      for (mr in 1:nrmanrem) {
        # message("$$$ checking ",mr," ",mrdata$cluRep[mr]," ",mrdata$invRep[mr])
        if (n0clurep > 0) {
          if(! is.na(mrdata$cluRep[mr])) {
            if (mrdata$cluRep[mr] == 0) {mrdata$cluRep[mr] = NA}
          }
        }
        if (n0invrep > 0) {
          if(! is.na(mrdata$invRep[mr])) {
            if (mrdata$invRep[mr] == 0) {mrdata$invRep[mr] = NA}
          }
        }
      }
    n0clurep = length(which(mrdata$cluRep==0))
    n0invrep = length(which(mrdata$invRep==0))
    message(" >>> num zero cluRep: ",n0clurep)
    message(" >>> num zero invRep: ",n0invrep)
    blank_line()
  }
  
  #
  # check syntax of file
  #
  for (mr in 1:nrmanrem) {
    
    centkey = mrdata$CentID[mr]
    invkey  = mrdata$invCID[mr]
    clurep  = mrdata$cluRep[mr]
    invrep  = mrdata$invRep[mr]
    cb1     = mrdata$Behaviour_1[mr]
    cb2     = mrdata$Behaviour_2[mr]
    cp1     = mrdata$Prey_1[mr]
    cp2     = mrdata$Prey_2[mr]
    cscat   = mrdata$Site_Cat[mr]
    caction = mrdata$Action[mr]
    cnotes2 = mrdata$Notes_2[mr]
    ccmpc   = mrdata$Comments[mr]
    
    ##
    ## support commands in any case
    ##
    caction = check_cmd(caction)
  
    #
    # Check Action Correct
    #
    if ( (caction != "Ok") &  (caction != "Edit") &  (caction != "Review") &  (caction != "Early") &  (caction != "Late") &  (caction != "Remove") &  (caction != "Skip")) {
      message(" ERROR - Bad Command: ",caction," cmd# ",mr)
      ncerr = ncerr + 1
    }
    
    if ( (caction == "Skip") ) {
      numskip = numskip + 1
      next
    }
    
    #
    # Match Different Variants
    #
    # Mandatory Keys: CentID  oSite_Cat  invCID
    #
    # Note: use original Site_Cat (oSite_Cat) since it will be modified by earlier Actions
    #
    # Optional Keys:
    #    (1)  cluRep(NA) invRep(NA) 
    #    (2)  cluRep(#)  invRep(NA)
    #    (3)  cluRep(NA) invRep(#)
    #    (4)  cluRep(#)  invRep(#)
    #
    
    klist = which(kdata$CentID == centkey & kdata$oSite_Cat == cscat & kdata$invCID == invkey )
    if (length(klist) == 0) {
      blank_line()
      message("Cmd#: ",mr,"  ERROR - Mandatory Key (CentID,Site_Cat,invCID) Not Found: (",centkey,", ",cscat,", ",invkey,")")
      ncerr = ncerr + 1
    }
    
    #
    # Option (1)
    #
    if ( is.na(clurep) & is.na(invrep)  ) {
      copt   = 1
      clist  = klist
      ncopt1 = ncopt1  + 1
      nclist1= nclist1 + length(clist)
    }
    
    # Option (2)
    #
    if ( !is.na(clurep) & is.na(invrep)  ) {
      copt  = 2
      clist = which(kdata$CentID == centkey & kdata$oSite_Cat == cscat & kdata$invCID == invkey & kdata$cluRep == clurep )
      ncopt2 = ncopt2  + 1
      nclist2= nclist2 + length(clist)
    }
    
    # Option (3)
    #
    if ( is.na(clurep) & !is.na(invrep)  ) {
      copt  = 3
      clist = which(kdata$CentID == centkey & kdata$oSite_Cat == cscat & kdata$invCID == invkey & kdata$invRep == invrep )
      ncopt3 = ncopt3  + 1
      nclist3= nclist3 + length(clist)
    }
    
    # Option (4)
    #
    if ( !is.na(clurep) & !is.na(invrep)  ) {
      copt  = 4
      clist = which(kdata$CentID == centkey & kdata$oSite_Cat == cscat & kdata$invCID == invkey & kdata$cluRep == clurep & kdata$invRep == invrep )
      ncopt4 = ncopt4  + 1
      nclist4= nclist4 + length(clist)
    }
    
    if (length(clist) == 0) {
      message("Cmd#: ",mr,"  ERROR - Extended Key (Ext Type,CentID,cluRep,Site_Cat,invCID,invRep) Not Found: (",copt,", ",centkey,", ",clurep,", ",cscat,", ",invkey,", ",invrep,")")
      ncerr = ncerr + 1
    }
    
    ncrows = ncrows + length(clist)
    
    
  }
  blank_line()
  blank_line()
  message(" >>> Cmds Total: ",nrmanrem,"  Cmds Skipped: ",numskip,"  Cmds Net: ",nrmanrem-numskip,"  Rows Affected: ",ncrows,"  Number Errors: ",ncerr,"  Number Warnings: ",ncwrn)
  message(" >>>      All keys must contain (CentID, oSite_Cat, invCID)")
  message(" ")
  message(" >>>      Four extended key types:")
  message(" >>>         Type (1) cluRep(NA) invRep(NA) Num: ",ncopt1,"  Rows affected: ",nclist1)
  message(" >>>         Type (2) cluRep(#)  invRep(NA) Num: ",ncopt2,"  Rows affected: ",nclist2)
  message(" >>>         Type (3) cluRep(NA) invRep(#)  Num: ",ncopt3,"  Rows affected: ",nclist3)
  message(" >>>         Type (4) cluRep(#)  invRep(#)  Num: ",ncopt4,"  Rows affected: ",nclist4)
  
}

############################################################################
##
## execute commands
##
############################################################################

totnumrem = 0
keynumrem = 0

ncerr   = 0
ncwrn   = 0
ncrows  = 0
numskip = 0

if (nrmanrem > 1) {
  blank_line()
  message(">>>>>>>>> Executing Commands")
  blank_line()
  #
  # check syntax of file
  #
  for (mr in 1:nrmanrem) {
    
    centkey = mrdata$CentID[mr]
    invkey  = mrdata$invCID[mr]
    clurep  = mrdata$cluRep[mr]
    invrep  = mrdata$invRep[mr]
    cb1     = mrdata$Behaviour_1[mr]
    cb2     = mrdata$Behaviour_2[mr]
    cp1     = mrdata$Prey_1[mr]
    cp2     = mrdata$Prey_2[mr]
    cscat   = mrdata$Site_Cat[mr]
    caction = mrdata$Action[mr]
    cnotes2 = mrdata$Notes_2[mr]
    if(is.null(cnotes2)) cnotes2 = NA
    ccmpc   = mrdata$Comments[mr]
    
    ##
    ## support commands in any case
    ##
    caction = check_cmd(caction)
    
    if (manVerbose) (
      message(">>> CMD#: ",mr,"  Action: ",caction," ")
    )

    #
    # Check Action Correct
    #
    if ( (caction != "Ok") &  (caction != "Edit") &  (caction != "Review") &  (caction != "Early") &  (caction != "Late") &  (caction != "Remove") &  (caction != "Skip")) {
      message(" ERROR - Bad Command: ",caction," cmd# ",mr)
      ncerr = ncerr + 1
    }
    
    if ( (caction == "Skip") ) {
      numskip = numskip + 1
      next
    }
    
    #
    # Match Different Variants
    #
    # Mandatory Keys: CentID  oSite_Cat  invCID
    #
    # Note: use original Site_Cat since it will be modified by earlier Actions
    #
    # Optional Keys:
    #    (1)  cluRep(NA) invRep(NA) 
    #    (2)  cluRep(#)  invRep(NA)
    #    (3)  cluRep(NA) invRep(#)
    #    (4)  cluRep(#)  invRep(#)
    #
    
    klist = which(kdata$CentID == centkey & kdata$oSite_Cat == cscat & kdata$invCID == invkey )
    if (length(klist) == 0) {
      message(" ERROR - Mandatory Key (CentID,Site_Cat,invCID) Not Found: ",centkey," ",cscat," ",invkey," cmd# ",mr)
      ncerr = ncerr + 1
    }
    #
    # Option (1)
    #
    if ( is.na(clurep) & is.na(invrep)  ) {
      copt  = 1
      clist = klist
    }
    
    # Option (2)
    #
    if ( !is.na(clurep) & is.na(invrep)  ) {
      copt  = 2
      clist = which(kdata$CentID == centkey & kdata$oSite_Cat == cscat & kdata$invCID == invkey & kdata$cluRep == clurep )
    }
    
    # Option (3)
    #
    if ( is.na(clurep) & !is.na(invrep)  ) {
      copt  = 3
      clist = which(kdata$CentID == centkey & kdata$oSite_Cat == cscat & kdata$invCID == invkey & kdata$invRep == invrep )
    }
    
    # Option (4)
    #
    if ( !is.na(clurep) & !is.na(invrep)  ) {
      copt  = 4
      clist = which(kdata$CentID == centkey & kdata$oSite_Cat == cscat & kdata$invCID == invkey & kdata$cluRep == clurep & kdata$invRep == invrep )
    }
    
    if (length(clist) == 0) {
      message(" ERROR - Optional Key (Type,CentID,cluRep,Site_Cat,invCID,invRep) Not Found: ",copt," : ",centkey," : ",clurep," : ",cscat," : ",invkey," : ",invrep," cmd# ",mr)
      ncerr = ncerr + 1
    }
    
    ncrows = ncrows + length(clist)
    
    ##
    ## Common to all Actions
    ##
    ## - mr:        update cmdRow
    ## - caction:   update cmdAction
    ## - if field is !na
    ##   - ccmpc:   update Comments
    ##   - cnotes2: update Notes_2
    ##   - cb1:     update Behaviour_1 and Site_cat
    ##   - cp1:     update Prey_1 and Prey_spp
    ##   - cb2:     update Behaviour_2 
    ##   - cp2:     update Prey_2 and Prey_spp
    
    for (crow in clist) {
      
      xrid = kdata$xrefID[crow]
      
      if (kdata$cmdCount[crow] > 0) {
        message(" +++ WARNING - Multiple Commands affecting same data row: ",crow," xrefID: ",xrid,"  Manual Action cmd#: ",mr," overriding Action: ",kdata$cmdAction[crow]," from Manual Action cmd#: ",kdata$cmdRow[crow])
      }
      kdata$cmdRow[crow]      = mr
      kdata$cmdCount[crow]    = kdata$cmdCount[crow] + 1
      kdata$cmdAction[crow]   = caction
      
      if ( !is.na(ccmpc)) {
        if ( nchar(ccmpc) > 0) kdata$Comments[crow] = ccmpc
      }
      if ( !is.na(cnotes2)) {
        if ( nchar(cnotes2) > 0) kdata$Notes_2[crow] = cnotes2
      }      
      if ( !is.na(cb1)) {
        if ( nchar(cb1) > 0) kdata$Behaviour_1[crow] = cb1
        if ( nchar(cb1) > 0) kdata$Site_Cat[crow]    = cb1
      }      
      if ( !is.na(cp1)) {
        if ( nchar(cp1) > 0) kdata$Prey_1[crow]      = cp1
        if ( nchar(cp1) > 0) kdata$Prey_spp[crow]    = cp1
      }      
      if ( !is.na(cb2)) {
        if ( nchar(cb2) > 0) kdata$Behaviour_2[crow] = cb2
      }      
      if ( !is.na(cp2)) {
        if ( nchar(cp2) > 0) kdata$Prey_2[crow]      = cp2
      } 
      
      ##
      ## Action: Ok
      ## 
      ## - override: clear finalIgnore flag (handled later in code)
      ##
      
      ##
      ## Action: Edit
      ## 
      ## - override: clear finalIgnore flag (handled later in code)
      ##
      
      ##
      ## Action: Review
      ## 
      ## - only common actions needed
      ##
      
      ##
      ## Action: Skip
      ## 
      ## - skip processing of Manual Action
      ##
      
      ##
      ## Action: Remove
      ## 
      ## - ooverride: set finalIgnore flag (handled later in code)
      ##
      
      ##
      ## Action: Early
      ## 
      ## - override: set finalIgnore flag (handled later in code)
      ##
      
      ##
      ## Action: Late
      ## 
      ## - override: set finalIgnore flag (handled later in code)
      ##
      
    }
    
  }
  
  message(" >>> Commands Processed: ",nrmanrem,"  Rows Affected: ",ncrows,"  Number Errors: ",ncerr)
  
}

##
## set the ignoreBeaver flag
##
ucid = unique(kdata$CentID)
for (cid in ucid) {

  ##
  ## ToDo behGroup
  ##
  if (behGroup == 1) {
    dlist = which( (kdata$CentID == cid) & kdata$Site_Cat == "Kill" & !is.na(kdata$Prey_spp ))
  }
  if (behGroup == 2) {
    dlist = which( (kdata$CentID == cid) & (kdata$Site_Cat == "Kill" | kdata$Site_Cat == "Probable kill") & !is.na(kdata$Prey_spp))
  }
  if (behGroup == 3) {
    dlist = which( (kdata$CentID == cid) & (kdata$Site_Cat == "Kill" | kdata$Site_Cat == "Probable kill" | kdata$Site_Cat == "Scavenge") & !is.na(kdata$Prey_spp) )
  }
  
  #
  # check if the Cluster matches both Kill and Rendez-vous
  #
  rlist = which( (kdata$CentID == cid) & kdata$Site_Cat == "Rendez-vous")
  if (length(rlist) >= 1  &  length(dlist) >= 1 ) {
    #message(" ++ DEBUG: ignoreBeaver ",cid," ",rlist," ",dlist)
    #
    # Cluster has both Kill and Rendez-vous Site Categorites
    #
    # If the Kill cluster species is Beaver then set the ignoreBeaver flag
    #
    blist = which( (kdata$CentID == cid) & (kdata$Site_Cat == "Kill") & (kdata$Prey_spp == "Beaver"))
    if (length(blist) >= 1) {
      #message(" ++ Hit ",blist)
      kdata$ignoreBeaver[blist] = 1
    }
  }
}

####
####
#### Update common ignoreFlag
####
####

kdata$ignoreFlag    = ifelse( (kdata$earlyFlag != 0)  |  (kdata$lateFlag != 0)  |  (kdata$ignoreBeaver != 0), 1, 0) 
kdata$finalIgnore   = kdata$ignoreFlag

####
####
#### Iterate over Actions and override finalIgnore flag
####
####

nsetIgnore= 0
nclrIgnore= 0

for (i in 1:nrkdata) {
  action = kdata$cmdAction[i]
  oldFlag = kdata$finalIgnore[i]
  if (!is.na(action)) {
    
    if (action == "Ok") {
      kdata$finalIgnore[i] = 0
      if (oldFlag == 1) nclrIgnore = nclrIgnore + 1
    }
    
    if (action == "Edit") {
      kdata$finalIgnore[i] = 0
      if (oldFlag == 1) nclrIgnore = nclrIgnore + 1
    }
    
    if (action == "Early") {
      kdata$finalIgnore[i] = 1
      if (oldFlag == 0) nsetIgnore = nsetIgnore + 1
    }
    
    if (action == "Late") {
      kdata$finalIgnore[i] = 1
      if (oldFlag == 0) nsetIgnore = nsetIgnore + 1
    }
    
    if (action == "Remove") {
      kdata$finalIgnore[i] = 1
      if (oldFlag == 0) nsetIgnore = nsetIgnore + 1
    }
  }
}

skip_line()

numdignore = length(which(kdata$ignoreFlag   != 0))
numdearly  = length(which(kdata$earlyFlag    != 0))
numdlate   = length(which(kdata$lateFlag     != 0))
numdbeaver = length(which(kdata$ignoreBeaver != 0))


numrem      = length(which(kdata$cmdAction == "Remove"))
numlate     = length(which(kdata$cmdAction == "Late"))
numearly    = length(which(kdata$cmdAction == "Early"))
numok       = length(which(kdata$cmdAction == "Ok"))
numedit     = length(which(kdata$cmdAction == "Edit"))
numrev      = length(which(kdata$cmdAction == "Review"))

numfinal    = length(which(kdata$finalIgnore != 0))

blank_line()
message(" >>> Stats - Data Driven")
message(" >>>    Num Early (Inv before Clu)   Rows: ",numdearly,"  Threshold Max Early (Days): ",thres_maxearly)
message(" >>>    Num Late  (Inv after  Clu)   Rows: ",numdlate, "  Threshold Max Late  (Days): ",thres_maxlate)
message(" >>>    Num Beaver                   Rows: ",numdbeaver)
message(" >>>    Num Data Ignore (ignoreFlag) Rows: ",numdignore)
blank_line()
message(" >>> Stats - Manual Actions")
message(" >>>    Num Remove:        ",numrem)
message(" >>>    Num Late:          ",numlate)
message(" >>>    Num Early:         ",numearly)
message(" >>>    Num Ok:            ",numok)
message(" >>>    Num Edit:          ",numedit)
message(" >>>    Num Review:        ",numrev)
message(" >>>    Num Skip:          ",numskip)
blank_line()
message(" >>> Stats - Action overrides of finalIgnore           ")
message(" >>>    Num Overrides: from Set to Clear: ",nclrIgnore)
message(" >>>    Num Overrides: from Clear to Set: ",nsetIgnore)
blank_line()
message(" >>> Stats (finalIgnore) Rows:            ",numfinal,"  (#ignoreFlag (Union of flags: early/late/beaver) - #overrides(Set to Clear) + #overrides(Clear to Set) )")
blank_line()

####
#   Calculate firstFlag
#
#   for each wolf select kill sites
#   for each investigation ID
#   count number kill site matches
#   pick earlist one
#   Use EARLY matches only as a fallback
####

kdata = kdata[order(kdata$CollarID,kdata$invCID,kdata$Site_Cat,kdata$First_date,kdata$First_time),]

wid = unique(kdata$CollarID)

for (w in wid) {
  ##
  ## ToDo behGroup
  ##
  if (behGroup == 1) {
    wlist = (kdata$CollarID == w) & (kdata$Site_Cat == "Kill") & !is.na(kdata$Prey_spp)
  }
  if (behGroup == 2) {
    wlist = (kdata$CollarID == w) & ((kdata$Site_Cat == "Kill") | (kdata$Site_Cat == "Probable kill") ) & !is.na(kdata$Prey_spp)
  }
  if (behGroup == 3) {
    wlist = (kdata$CollarID == w) & ((kdata$Site_Cat == "Kill") | (kdata$Site_Cat == "Probable kill") | (kdata$Site_Cat == "Scavenge") ) & !is.na(kdata$Prey_spp)
  }
  iid   = unique(kdata$invCID[wlist])

  for (i in iid) {
    ilist = (kdata$invCID == i) & wlist
    nlist = length(which(ilist))
    kdata$KillSiteMatch[ilist] = nlist
    slist = (kdata$finalIgnore == 0) & ilist
    # 
    # non empty slist means that at least one of the kill site matches was NOT EARLY
    #
    if (length(which(slist)) > 0) {
      kdata$firstFlag[min(which(slist))] = 1
    }
    # DO NOT MATCH SITES THAT ARE TOO EARLY
    #else {
    #  kdata$firstFlag[min(which(ilist))] = 1
    #}
  }
}

################################################################################
##
## Add columns which will define multiwolf (mw) Clusters
##
## mwKid     - unique ID (1,2,...) for the first mw Kill Cluster
## mwKcount  - count of wolves with common Kill Cluster (same invCID)
## mwKfirst  - first (earliest) common Kill Cluster flag (first == 1)
## mwKmonth  - month  of first point of first (earliest) common Kill Cluster
## mwKhour   - hour   of first point of first (earliest) common Kill Cluster
## mwKspecie - specie of first (earliest) common Kill Cluster (in case it needs to override Prey_spp)
## mwKdurTotal  - time (latest last point across all common clusters) - time (first point across all common Kill Clusters) (in hours)
## mwKdurMin - minimum duration across all mw clusters
## mwKdurMax - maximum duration across all mw clusters
## mwKdurMean- mean duration across all mw clusters
## mwKdurMedian - median duration across all mw clusters

kdata$mwKid     = 0    #  [1..m] => mw Kill Cluster  (same id used for all common Kill Clusters)
kdata$mwKcount  = 0    #  >0     => mw Kill Cluster
kdata$mwKfirst  = 0    #  1      => the first (earlyist) mw Kill Cluster -- across all wolves
kdata$mwKf4Wolf = 0    #  1      => the first (earlyist) mw Kill Cluster -- for the given Wolf
kdata$WolfCount = 0    # Count number of unique wolf IDs in each group
kdata$mwKyear   = as.numeric(substr(kdata$First_date,1,4))
kdata$mwKmonth  = as.numeric(substr(kdata$First_date,6,7))
kdata$mwKday    = as.numeric(substr(kdata$First_date,9,10))
kdata$mwKhour   = NA   #  [0..23] 
kdata$mwKmin    = NA   #  [0..59] 
kdata$mwKspecie = NA   #  <specie> iff this is a Kill Cluster
kdata$WTD       = 0
kdata$Elk       = 0
kdata$Moose     = 0
kdata$Ungulate  = 0
kdata$Beaver    = 0
kdata$Birds     = 0
kdata$Bear      = 0
kdata$Pig       = 0
kdata$Mink      = 0
kdata$Total     = 0
kdata$Calf      = 0
kdata$mwKdurTotal  = NA   #  >0     iff this is the first (earlist) mw Kill Cluster
kdata$mwKdurMin    = NA   #  >0     iff this is the first (earlist) mw Kill Cluster
kdata$mwKdurMax    = NA   #  >0     iff this is the first (earlist) mw Kill Cluster
kdata$mwKdurMean   = NA   #  >0     iff this is the first (earlist) mw Kill Cluster
kdata$mwKdurMedian = NA   #  >0     iff this is the first (earlist) mw Kill Cluster
kdata$mwKgapCount  = 0    #  >=     count of all Kill Clusters (except first) whose start point does not overlap with another cluster in the group

### Confirmed - First_date, First_time, last_date, Last_time are all Local Time (UTC - 6)

pfirsttime      = ifelse(nchar(kdata$First_time) == 8,kdata$First_time,paste("0",kdata$First_time,sep=""))
kdata$First_dt  = paste(kdata$First_date," ",pfirsttime,sep="")
kdata$mwKhour   = as.numeric(substr(pfirsttime,1,2))
kdata$mwKmin    = as.numeric(substr(pfirsttime,4,5))

plasttime       = ifelse(nchar(kdata$Last_time) == 8,kdata$Last_time,paste("0",kdata$Last_time,sep=""))
kdata$Last_dt   = paste(kdata$Last_date," ",plasttime,sep="")

### Calculate a Cluster Duration (CluDurHours) for reference

kdata$CluDurHours = NA

for (i in 1:nrkdata) {
  starttime = kdata$First_dt[i]
  # using utc is OK because we just want to clculate a difference
  iposst    = as.POSIXct(strptime(starttime, "%Y-%m-%d %H:%M:%S") , tz="utc")
  endtime   = kdata$Last_dt[i]
  iposet    = as.POSIXct(strptime(endtime,    "%Y-%m-%d %H:%M:%S") , tz="utc")
  cdursec   = dursec   = as.numeric((iposet - iposst), units = "secs")
  cdurhour  = round(cdursec/3600.0, digits = 4)
  kdata$CluDurHours[i] = cdurhour
}

### ToDo - Default mwKspecie to Prey_spp for Kill Sites
### Note: Are more complicated rules needed to prioritize one species over another?

##
## ToDo behGroup
##
if (behGroup == 1) {
  kdata$mwKspecie = ifelse(kdata$Site_Cat == "Kill" & !is.na(kdata$Prey_spp), kdata$Prey_spp, kdata$mwKspecie )
}
if (behGroup == 2) {
  kdata$mwKspecie = ifelse((kdata$Site_Cat == "Kill" | kdata$Site_Cat == "Probable kill") & !is.na(kdata$Prey_spp) , kdata$Prey_spp, kdata$mwKspecie )
}
if (behGroup == 3) {
  kdata$mwKspecie = ifelse((kdata$Site_Cat == "Kill" | kdata$Site_Cat == "Probable kill" | kdata$Site_Cat == "Scavenge") & !is.na(kdata$Prey_spp), kdata$Prey_spp, kdata$mwKspecie )
}

### Keep a clone for debugging
ckdata = kdata

###
###
### Sort Cluster File
###
###    sort order: invCID, First_dt, 
###
###
kdata666 = kdata
kdata = kdata[order(kdata$invCID,kdata$First_dt),]

### 
###
### Iterate over all invCID that are Kills  AND  Not Early  AND  Not Late
###
### assign mwKid sequentially

###
### ToDo: add in behGroup
###    1: Kill                                 -- implemented
###    2: Kill and Probable kill               -- ToDo
###    3: Kill,    Probable kill and Scavenge  -- ToDo

if (behGroup == 1) {
  klist = which(kdata$Site_Cat == "Kill"  &  kdata$finalIgnore == 0)
}
if (behGroup == 2) {
  klist = which((kdata$Site_Cat == "Kill" | kdata$Site_Cat == "Probable kill") & !is.na(kdata$Prey_spp) &  kdata$finalIgnore == 0)
}
if (behGroup == 3) {
  klist = which((kdata$Site_Cat == "Kill" | kdata$Site_Cat == "Probable kill" | kdata$Site_Cat == "Scavenge") & !is.na(kdata$Prey_spp) &  kdata$finalIgnore == 0)
}

ilist = unique(kdata$invCID[klist])

mwkid = 0

for (i in ilist) {
  
  # clist - list of clusters that share the same invCID  (excluding ignored clusters)
  
  ##
  ## ToDo behGroup
  ##
  
  if (behGroup == 1) {
    clist = which(kdata$Site_Cat == "Kill"  &  kdata$finalIgnore == 0  & !is.na(kdata$Prey_spp) &  kdata$invCID == i)
  }
  if (behGroup == 2) {
    clist = which((kdata$Site_Cat == "Kill" | kdata$Site_Cat == "Probable kill") & !is.na(kdata$Prey_spp) &  kdata$finalIgnore == 0  &  kdata$invCID == i)
  }
  if (behGroup == 3) {
    clist = which((kdata$Site_Cat == "Kill" | kdata$Site_Cat == "Probable kill" | kdata$Site_Cat == "Scavenge") & !is.na(kdata$Prey_spp) &  kdata$finalIgnore == 0  &  kdata$invCID == i)
  }
  
  
  # length(clist) == 0 then no action required
  # length(clist) == 1 then no shared invCID
  # length(clist) >  1 then they all have to be updated
  
  nclist = length(clist)
  
  if (nclist == 0) {
    message(" ++++++++++ Warning  invCID Not Found: ",i)
  }
  
  if (nclist >  0) {
    kdata$mwKfirst[clist[1]] <- 1
    mwkid = mwkid + 1
    kdata$mwKid[clist] = mwkid
    kdata$mwKcount[clist] = nclist
  }
}

###
### Calculate Duration of Overall Common Kill Clusters
###
###    Start  Time: First date&time of first Cluster
###    End    Time: Max( Last date%time) 
###
### Assign the overall duration to the first Cluster

## iterate overall mwKid
##    calculate start time
##    iterate over all cluster
##       calculate and find maximum last date&time
##    calculate end time
##    calculate duration (End - Start)
##    assign duration to first cluster

umwkid = max(kdata$mwKid,na.rm = TRUE)

for (id in 1:umwkid) {
  ilist = which(kdata$mwKid == id)
  nilist= length(ilist)
  if (nilist < 1) {
    message(" ++++++++++ ERROR - BAD mwKid clist length: ",id," ",nilist)
  }
  # check first row is marked as the first
  if (kdata$mwKfirst[ilist[1]] != 1) {
    message(" ++++++++++ ERROR - BAD mwKid clist[1]: ",id," ",ilist[1])
  }
  starttime = kdata$First_dt[ilist[1]]
  # using utc is OK because we just want to clculate a difference
  iposst    = as.POSIXct(strptime(starttime, "%Y-%m-%d %H:%M:%S") , tz="utc")
  #
  # now interate over all end times and find the maximum
  #
  endtime  = kdata$Last_dt[ilist[1]]
  iposet   = as.POSIXct(strptime(endtime,    "%Y-%m-%d %H:%M:%S") , tz="utc")
  dursec   = dursec   = as.numeric((iposet - iposst), units = "secs")
  maxdursec= dursec
  #
  if (nilist > 1) {
    for (j in 2:nilist) {
      endtime  = kdata$Last_dt[ilist[j]]
      iposet   = as.POSIXct(strptime(endtime,"%Y-%m-%d %H:%M:%S") , tz="utc")
      dursec   = as.numeric((iposet - iposst), units = "secs")
      if (dursec > maxdursec) maxdursec = dursec
    }
  }
  #
  # convert to hours 
  #
  maxdurhour = round(maxdursec/3600.0, digits = 4)
  if (maxdurhour < 1.5) {
    message(" ++++++++++ ERROR - BAD max duration: ",id," ",maxdurhour," ",maxdursec)
  }
  kdata$mwKdurTotal[ilist[1]] = maxdurhour
  
  kdata$mwKdurMin[ilist[1]]     = min(kdata$CluDurHours[ilist])
  kdata$mwKdurMax[ilist[1]]     = max(kdata$CluDurHours[ilist])
  kdata$mwKdurMean[ilist[1]]    = mean(kdata$CluDurHours[ilist])
  kdata$mwKdurMedian[ilist[1]]  = median(kdata$CluDurHours[ilist])
  
  ##
  ## Check if there are multiple species to count in bins
  ##
  if (nilist > 0) {
    for (j in 1:nilist) {
      preyspp    = kdata$Prey_1[ilist[j]]
      prey2      = kdata$Prey_2[ilist[j]]
      preynotes2 = kdata$Notes_2[ilist[j]]
      if (preyspp == "WTD" | preyspp == "Deer")   kdata$WTD[ilist[1]] = 1
      if (prey2   == "WTD" | prey2   == "Deer")   kdata$WTD[ilist[1]] = 1
      if (preyspp == "Elk")                       kdata$Elk[ilist[1]] = 1
      if (prey2   == "Elk")                       kdata$Elk[ilist[1]] = 1
      if (preyspp == "Moose")                     kdata$Moose[ilist[1]] = 1
      if (prey2   == "Moose")                     kdata$Moose[ilist[1]] = 1
      if (preyspp == "Bear")                      kdata$Bear[ilist[1]] = 1
      if (prey2   == "Bear")                      kdata$Bear[ilist[1]] = 1
      if (preyspp == "Pigs")                      kdata$Pig[ilist[1]] = 1
      if (prey2   == "Pigs")                      kdata$Pig[ilist[1]] = 1
      if (preyspp == "Mink")                      kdata$Mink[ilist[1]] = 1
      if (prey2   == "Mink")                      kdata$Mink[ilist[1]] = 1
      if (preyspp == "Ungulate")                  kdata$Ungulate[ilist[1]] = 1
      if (prey2   == "Ungulate")                  kdata$Ungulate[ilist[1]] = 1
      if (preyspp == "Raven" | preyspp == "Ruffed Grouse" | preyspp == "Canada Goose") kdata$Birds[ilist[1]] = 1
      if (prey2   == "Raven" | prey2   == "Ruffed Grouse" | prey2   == "Canada Goose") kdata$Birds[ilist[1]] = 1
      if (preyspp == "Beaver" | prey2 == "Beaver") {
        kdata$Beaver[ilist[1]] = 1
        if (twoBeavers == 1 && preynotes2 == "Two beavers") {
          kdata$Beaver[ilist[1]] = 2
        }
      }
      
    }
    kdata$Total[ilist[1]] = kdata$WTD[ilist[1]] + kdata$Elk[ilist[1]] + kdata$Moose[ilist[1]] + kdata$Bear[ilist[1]] + kdata$Pig[ilist[1]] + kdata$Mink[ilist[1]] + kdata$Ungulate[ilist[1]] + kdata$Birds[ilist[1]] + kdata$Beaver[ilist[1]]
  }  # nilist > 0
}

###
### Set Calf Flag
###

icalf = which( kdata$Prey_age_class == "Calf" )
if (length(icalf) > 0) {
  kdata$Calf[icalf] = 1
}

### 
### Check if the Groups have any gaps 
###
### Gap is define as the start point of an Aggregate Kill cluster not overlapping another cluster in the sam group
###

for (id in 1:umwkid) {
  ilist = which(kdata$mwKid == id)
  nilist= length(ilist)
  ngap  = 0
  if (nilist < 1) {
    message(" ++++++++++ ERROR - BAD mwKid clist length: ",id," ",nilist)
  }
  # check first row is marked as the first
  if (kdata$mwKfirst[ilist[1]] != 1) {
    message(" ++++++++++ ERROR - BAD mwKid clist[1]: ",id," ",ilist[1])
  }
  ##
  ## Only need to check for overlap if more than one cluster in group
  ##
  if (nilist > 1) {
    for (j in 2:nilist) {
      strtime  = kdata$First_dt[ilist[j]]
      iposst   = as.POSIXct(strptime(strtime,"%Y-%m-%d %H:%M:%S") , tz="utc")
      ##
      ## compare [j] against all other clusters [k] except for itself
      ##
      gapflag = TRUE
      for (k in 1:nilist) {
        if (k != j) {
          s2time    = kdata$First_dt[ilist[k]]
          iposs2    = as.POSIXct(strptime(s2time,"%Y-%m-%d %H:%M:%S") , tz="utc")
          e2time    = kdata$Last_dt[ilist[k]]
          ipose2    = as.POSIXct(strptime(e2time,"%Y-%m-%d %H:%M:%S") , tz="utc")
          dursecs2  = as.numeric((iposst - iposs2), units = "secs")
          dursece2  = as.numeric((ipose2 - iposst), units = "secs")
          if(dursecs2 >= 0 & dursece2 >=0) gapflag = FALSE
        }
      }
      if (gapflag) ngap = ngap + 1
    }
  }
  kdata$mwKgapCount[ilist] = ngap
  
  ##
  ## ToDo - WolfCount Calculation
  ##
  
  ilist = which(kdata$mwKid == id)
  nilist= length(ilist)
  
  nwolf = length(unique(kdata$CollarID[ilist]))
  
  kdata$WolfCount[ilist] = nwolf

}

###################################################################################
###
###
###  New Bahaviour
###
###  for each mw cluster:
###  (1) identify the earliest cluster for each wolf id in the mw cluster (mwKf4Wolf <- 1)
###  (2) copy the kill counts to all rows tagged with mwKf4Wolf == 1
###
###

## Take care of all of the single Wolf cases by 
##    selecting mwKfirst == 1 then setting mwKf4Wolf <-- 1
##    this also sets mwKf4Wolf for the wolf with the earlist cluster

ilist = which(kdata$mwKfirst == 1)
kdata$mwKf4Wolf[ilist] <- 1

for (id in 1:umwkid) {
  ilist = which(kdata$mwKid == id)
  nilist= length(ilist)
  nwolf = kdata$WolfCount[ilist[1]]
  
  ##
  ## no further processing if a single wolf
  ##
  if (nwolf < 2) {
    next
  }
  
  ## need the row for the first wolf
  ## rows associated with the earlist cluster do not need to be updated
  flist  = which(kdata$mwKid == id  &  kdata$mwKfirst == 1)
  nflist = length(flist)
  wfirst = kdata$CollarID[flist[1]]
  if (nflist != 1) message("$$$$$$$$$$ ERROR nflist != 1  mwKid= ",id,"  CollarID= ",wfirst)
  rfirst = flist[1]
  
  ##
  ## Case: multiwolf 
  ##       for each wolf
  ##          find first cluster
  ##          set mwKf4Wolf
  ##          copy kill counts from row with mwKfirst == 1
  ##
  wlist  = unique(kdata$CollarID[ilist])
  nwlist = length(wlist)
  if (nwlist < 2) message("$$$$$$$$$$ ERROR nwlist <  2  mwKid= ",id,"  CollarID first Wolf= ",wfirst)
  
  for (wid in wlist) {
    if (wid == wfirst) {
      next
    }
    ## 
    ## now we need to first the first cluster for wid
    ##
    jlist  = which(kdata$mwKid == id  &  kdata$CollarID == wid)
    njlist = length(jlist)
    jrow = jlist[1]
    jfdt = kdata$First_dt[jrow]
    
    if (njlist > 1) {
      ##
      ## update jrow to point to the actual earlist cluster for wid
      ##
      for (k in jlist) {
        if (kdata$First_dt[k] < jfdt ) k = jrow
      }
    }
    ##
    ## jrow is the earlist cluster
    ##
    kdata$mwKf4Wolf[jrow] <- 1
    #
    # kdata$Total[ilist[1]]  kdata$Pig[ilist[1]] + kdata$Mink[ilist[1]] + kdata$Ungulate[ilist[1]] + kdata$Birds[ilist[1]] + kdata$Beaver[ilist[1]]
    # 
    kdata$WTD[jrow]   <- kdata$WTD[rfirst]
    kdata$Elk[jrow]   <- kdata$Elk[rfirst]
    kdata$Moose[jrow] <- kdata$Moose[rfirst]
    kdata$Ungulate[jrow] <- kdata$Ungulate[rfirst]
    kdata$Bear[jrow]  <- kdata$Bear[rfirst]
    kdata$Pig[jrow]   <- kdata$Pig[rfirst]
    kdata$Mink[jrow]  <- kdata$Mink[rfirst]
    kdata$Birds[jrow] <- kdata$Birds[rfirst]
    kdata$Beaver[jrow]<- kdata$Beaver[rfirst]
    kdata$Total[jrow] <- kdata$WTD[jrow] + kdata$Elk[jrow] + kdata$Moose[jrow] + kdata$Ungulate[jrow] + kdata$Bear[jrow] + kdata$Pig[jrow] + kdata$Mink[jrow] + kdata$Birds[jrow] + kdata$Beaver[jrow]
    kdata$Calf[jrow]  <- kdata$Calf[rfirst]
  
  }  ## wid in wlist
}  ## id in umwKid


## move writing file to end after flagging ambiguities
#
# message(" >>> Generating: Cluster List File: ",ksfile,"  Rows: ",nrkdata)
# write.table(kdata, file = ksfile, sep = ",", col.names = T, row.names =F) #save to csv

###################################################################################
####
#### Look for Ambiguities in the matches
####

#### Check which multi-match Clusters have different Site_Cat

#ambFlag = logical(nrkdata)
#ambFlag[] = FALSE
#adata = data.frame(kdata,stringsAsFactors = FALSE)
#adata = cbind(adata,ambFlag)
adata = kdata
adata$ambFlag = FALSE
adata$cluSiteCatDiff = FALSE
adata$cluPreySppDiff = FALSE
adata$invSiteCatDiff = FALSE
adata$cluRendezKillMix = FALSE
adata$cluScavengeKillMix = FALSE
adata$cluRevisitKillMix = FALSE

ucid = unique(adata$CentID)   # Centroid      ID from Cluster finding step
uinv = unique(adata$invCID)   # Investigation ID
ndupcid = 0
nambcid = 0
nambpspp= 0
nrendezkillmix= 0
nscavengekillmix= 0
nrevisitkillmix= 0

ndupinv = 0
nambinv = 0

##
## check how many matches for each Centroid
##

for (cid in ucid) {
  clist = which(adata$CentID == cid)
  if (length(clist) > 1) {
    #
    # more than 1 match - possibly ambigous
    # now check if the site categories are all the same
    #
    ndupcid = ndupcid + 1
    slist = unique(adata$Site_Cat[clist])
    if (length(slist) > 1) {
      #
      # Different Site Category for the matching clusters ==>  set cluSiteCatDiff
      #
      nambcid = nambcid + 1
      adata$ambFlag[clist] = TRUE
      adata$cluSiteCatDiff[clist] = TRUE
    }
    
  }
  ##
  ## ToDo behGroup
  ##
  dlist = which( (adata$CentID == cid) & adata$Site_Cat == "Kill")
  
  if (behGroup == 1) {
    dlist = which( (adata$CentID == cid) & adata$Site_Cat == "Kill" & !is.na(adata$Prey_spp))
  }
  if (behGroup == 2) {
    dlist = which( (adata$CentID == cid) & (adata$Site_Cat == "Kill" | adata$Site_Cat == "Probable kill") & !is.na(adata$Prey_spp))
  }
  if (behGroup == 3) {
    dlist = which( (adata$CentID == cid) & (adata$Site_Cat == "Kill" | adata$Site_Cat == "Probable kill" | adata$Site_Cat == "Scavenge") & !is.na(adata$Prey_spp) )
  }
  
  
  if (length(dlist) > 1) {
    #
    # more than 1 match - possibly ambigous
    # now check if the species are all the same
    #
    slist = unique(adata$Prey_spp[dlist])
    if (length(slist) > 1) {
      #
      # Different Prey Species for the matching Kill clusters  ==>  set cluPreySppDiff
      #
      nambpspp = nambpspp + 1
      adata$ambFlag[dlist] = TRUE
      adata$cluPreySppDiff[dlist] = TRUE
    }
  }
  
  #
  # check if the Cluster matches both Kill and Rendez-vous
  #
  rlist = which( (adata$CentID == cid) & adata$Site_Cat == "Rendez-vous")
  if (length(rlist) >= 1  &  length(dlist) >= 1 ) {
    #
    # Cluster has both Kill and Rendez-vous Site Categorites
    #
    nrendezkillmix = nrendezkillmix + 1
    adata$ambFlag[dlist] = TRUE
    adata$cluRendezKillMix[dlist] = TRUE
    adata$ambFlag[rlist] = TRUE
    adata$cluRendezKillMix[rlist] = TRUE
  }
  #
  # check if the Cluster matches both Kill and Scavenge
  #
  rlist = which( (adata$CentID == cid) & adata$Site_Cat == "Scavenge")
  if (length(rlist) >= 1  &  length(dlist) >= 1 ) {
    #
    # Cluster has both Kill and SCavenge Site Categorites
    #
    nscavengekillmix = nscavengekillmix + 1
    adata$ambFlag[dlist] = TRUE
    adata$cluScavengeKillMix[dlist] = TRUE
    adata$ambFlag[rlist] = TRUE
    adata$cluScavengeKillMix[rlist] = TRUE
  }
  #
  # check if the Cluster matches both Kill and Revisit
  #
  rlist = which( (adata$CentID == cid) & adata$Site_Cat == "Revisit")
  if (length(rlist) >= 1  &  length(dlist) >= 1 ) {
    #
    # Cluster has both Kill and Revisit Site Categorites
    #
    nrevisitkillmix = nrevisitkillmix + 1
    adata$ambFlag[dlist] = TRUE
    adata$cluRevisitKillMix[dlist] = TRUE
    adata$ambFlag[rlist] = TRUE
    adata$cluRevisitKillMix[rlist] = TRUE
  }
}
  
##
## check how many matches for each Investigation ID
##

for (cid in uinv) {
    clist = which(adata$invCID == cid)
    if (length(clist) > 1) {
      #
      # more than 1 match - possibly ambigous
      # now check if the site categories are all the same
      #
      ndupinv = ndupinv + 1
      slist = unique(adata$Site_Cat[clist])
      if (length(slist) > 1) {
        #
        # Different Site_Cat for the same Investigation ID  ==>  set invSiteCatDiff  
        #
        nambinv = nambinv + 1
        adata$ambFlag[clist] = TRUE
        adata$invSiteCatDiff[clist] = TRUE
      }
      
    }

}

bdata   = adata[adata$ambFlag,]
nrbdata = length(bdata[,1])

skip_line()
message(" >>> Checking:   Cluster Ambiguities:  #Dup CentID: ",ndupcid,"  #Ambigous Site_Cat:        ",nambcid)
message(" >>>                                                ","     #Rendez-vous n Kill Mixed: ",nrendezkillmix)
message(" >>>                                                ","     #Scavenge    n Kill Mixed: ",nscavengekillmix)
message(" >>>                                                ","     #Revisit     n Kill Mixed: ",nrevisitkillmix)
message(" >>>                                                ","     #Ambigous Prey Species:    ",nambpspp)
message(" >>>                                   #Dup invCID: ",ndupinv,"  #Ambigous Investigated:    ",nambinv)


###########################################################################################################################
#
# Generate Combination of the Aggregate Group Kill Clusters and the Complete Join File
#
#   Join on xrefID
#   Leave Column Names for Matched Entries the same
#   Add Suffix of ".M" for duplicated Columns from the Join/Match File

class(adata$xrefID) = "integer"

jdata = full_join(adata,xrefData,by="xrefID",suffix=c("",".M"))
nrjdata = length(jdata[,1])

###########################################################################################################################
#
# Remove All Columns ending in ".M"
#
#   Count number of columns and do a quick check that the non ".M" Columns have the same value
#
allname = colnames(jdata)
nrcdata = length(allname)
cdotm   = 0
dotmstr = ".M"
duplist = c()

for (i in 1:nrcdata) {
  lname = nchar(allname[i])
  if (substr(allname[i],lname-1,lname) == dotmstr) {
    cdotm = cdotm + 1
    norig = substr(allname[i],1,lname-2)
    ndup  = allname[i]
    duplist = c(duplist,-i)
    if (jdata[1,norig] != jdata[1,ndup]) {
      message(" $$$$$ WARNING Column Mismatch: ",norig,"  Dup: ",ndup)
    }
  }
}

blank_line()
message(" >>> Removing Columns Ending in: ",dotmstr,"  Count: ",cdotm)
blank_line()

if (cdotm > 0 ) {
  jdata2 = jdata[ , duplist ]
}

############ Check for unique keys
##
## (CentID	cluRep	Site_Cat	invCID	invRep)
##
############

ydata = jdata2[order(jdata2$CentID,jdata2$cluRep,jdata2$Site_Cat,jdata2$invCID,jdata2$invRep),]

ndup = 0
for (i in 2:nrkdata) {
  if (!is.na(ydata$CentID[i]) & !is.na(ydata$cluRep[i]) & !is.na(ydata$Site_Cat[i]) & !is.na(ydata$invCID[i]) & !is.na(ydata$invRep[i]))
    if (  (ydata$CentID[i] == ydata$CentID[i-1]) & (ydata$cluRep[i] == ydata$cluRep[i-1]) & (ydata$Site_Cat[i] == ydata$Site_Cat[i-1]) & (ydata$invCID[i] == ydata$invCID[i-1]) & (ydata$invRep[i] == ydata$invRep[i-1]) ) {
      ndup = ndup + 1
      id1  = ydata$rowID[i-1]
      id2  = ydata$rowID[i]
      kd1  = which(jdata2$rowID == id1)
      kd2  = which(jdata2$rowID == id2)
      jdata2$dupIndex[kd1] = ndup
      jdata2$dupIndex[kd2] = ndup
      
      if (ndup < 10) message(" WARNING - Duplicate Key ydata Row=",i,"  rowID1=",ydata$rowID[i-1],"  rowID2=",ydata$rowID[i])
    }
}

if (ndup > 0 ) {
  message(" WARNING - Total Duplicate Rows=",ndup)
  message(" ")
}

###
### Sort All Sites file 
###

jdata2 = jdata2[order(jdata2$JoinStatus,jdata2$CollarID,jdata2$invCID,jdata2$invRep,jdata2$Site_Cat,jdata2$First_date,jdata2$First_time),]

nrjdata2 = nrow(jdata2)

blank_line()
message(" >>> Generating: Combination (Joined) Cluster List File: ",jsfile,"  Rows: ",nrjdata2)
skip_line()

write.table(jdata2, file = jsfile, sep = ",", col.names = T, row.names =F) #save to csv
skip_line()
blank_line()

jdata3 = jdata2[jdata2$JoinStatus == "Matched" , ]

jdata3 = jdata3[order(jdata3$invCID,jdata3$invRep,jdata3$Site_Cat,jdata3$First_date,jdata3$First_time),]

nrjdata3 = nrow(jdata3)

message(" >>> Generating: Matched Cluster List File Sorted Inv: ",ksfile,"  Rows: ",nrjdata3)
skip_line()
message(" >>>    Total Ambigous Rows: ",nrbdata)
write.table(jdata3, file = ksfile, sep = ",", col.names = T, row.names =F) #save to csv
skip_line()
blank_line()

jdata4 = jdata3[ order(jdata3$CentID,jdata3$cluRep,jdata3$invCID,jdata3$invRep) , ]
nrjdata4 = nrow(jdata4)

message(" >>> Generating: Matched Cluster List File Sorted Clu: ",ksfile2,"  Rows: ",nrjdata4)
skip_line()
message(" >>>    Total Ambigous Rows: ",nrbdata)
write.table(jdata4, file = ksfile2, sep = ",", col.names = T, row.names =F) #save to csv
skip_line()
blank_line()

###########################################################################################################################
#
# Generate summary file
#
#   For All Wolves
#   For All Categories
#   For All Months

skip_line()
message(" >>> Summary of Site Categories for All Wolves")

matdata2 = matdata
matdata  = jdata

matdata   = matdata[matdata$JoinStatus == "Matched",]
nrmatdata = nrow(matdata)

wid = unique(matdata$CollarID)
nwid= length(wid)

sitecat = unique(matdata$Site_Cat)
nsitecat= length(sitecat)

# Determine Start Month

startMonth = as.numeric(substr(matdata$First_date,6,7))

# Size of Table
# 
# For each wid
#
#    rows   = # site categories + 1 for total + 1 for Kill/Probable kill + 1 for Ambigous
#
#    columns = Wolf Id, Row Name, 12 Months, 1 for Total, 1 for Winter total
#
rdatalen = nsitecat + 3
rdata0= vector(mode="integer", length = rdatalen)
for (j in 1:rdatalen) {
  rdata0[j] = 0
}

sumcat = c(sitecat, "Total_sites", "Kill_and_Pkill", "Ambigous_sites")

sdata = NULL

# create the empty data table

for (w in wid) {
  wdata = data.frame(w, sumcat, rdata0, rdata0, rdata0, rdata0, rdata0, rdata0, rdata0, rdata0, rdata0, rdata0, rdata0, rdata0, rdata0, rdata0, rdata0, rdata0,stringsAsFactors=FALSE)
  sdata = rbind(sdata,wdata)
}

sscolnames = c("WolfID","SummaryCat","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec", "TotalAll", "TotalWin", "TBD_1","TBD_2")
colnames(sdata) = sscolnames
nrsdata = nrow(sdata)

# add counts to the table

First_mon = as.numeric(substr(matdata$First_date, 6, 7))

for (irow in 1:nrmatdata) {
  iwid = matdata$CollarID[irow]
  icat = matdata$Site_Cat[irow]
  imon = First_mon[irow]
  
  srow = which(sdata$WolfID==iwid & sdata$SummaryCat==icat)
  if (length(srow) > 1) message(" >>>>>> ERROR - srow = ",srow)
  
  count = sdata[srow,(imon+2)]
  
  count = sdata[srow,(imon+2)]
  countp1 = count + 1
  set(sdata,srow,(imon+2),countp1)
  if (iwid == debugW) {
    message(" >>> DEBUG1 irow:", irow, " iwid:", iwid, " icat:", icat, " imon:",imon," srow:",srow, " count:",count)
    message(" >>>    DEBUG2 countp1:",countp1)   
  }

}

# calculate row totals

jrows = seq(1,length(sumcat)*(nwid-1)+1,by=length(sumcat))
kilrow = which(sumcat=="Kill")
pklrow = which(sumcat=="Probable kill")
totrow = which(sumcat=="Total_sites")
kpkrow = which(sumcat=="Kill_and_Pkill")
ambrow = which(sumcat=="Ambigous_sites")

for (j in jrows) {

### total sites by month
  
    sdata$Jan[j+totrow-1] = sum(sdata$Jan[j:(j+11)])
    sdata$Feb[j+totrow-1] = sum(sdata$Feb[j:(j+11)])
    sdata$Mar[j+totrow-1] = sum(sdata$Mar[j:(j+11)])
    
    sdata$Apr[j+totrow-1] = sum(sdata$Apr[j:(j+11)])
    sdata$May[j+totrow-1] = sum(sdata$May[j:(j+11)])
    sdata$Jun[j+totrow-1] = sum(sdata$Jun[j:(j+11)])
    
    sdata$Jul[j+totrow-1] = sum(sdata$Jul[j:(j+11)])
    sdata$Aug[j+totrow-1] = sum(sdata$Aug[j:(j+11)])
    sdata$Sep[j+totrow-1] = sum(sdata$Sep[j:(j+11)])
    
    sdata$Oct[j+totrow-1] = sum(sdata$Oct[j:(j+11)])
    sdata$Nov[j+totrow-1] = sum(sdata$Nov[j:(j+11)])
    sdata$Dec[j+totrow-1] = sum(sdata$Dec[j:(j+11)])
    
### total of Kill + Probable kill   

    sdata$Jan[j+kpkrow-1] = sdata$Jan[j+kilrow-1] + sdata$Jan[j+pklrow-1]
    sdata$Feb[j+kpkrow-1] = sdata$Feb[j+kilrow-1] + sdata$Feb[j+pklrow-1]
    sdata$Mar[j+kpkrow-1] = sdata$Mar[j+kilrow-1] + sdata$Mar[j+pklrow-1]
    
    sdata$Apr[j+kpkrow-1] = sdata$Apr[j+kilrow-1] + sdata$Apr[j+pklrow-1]
    sdata$May[j+kpkrow-1] = sdata$May[j+kilrow-1] + sdata$May[j+pklrow-1]
    sdata$Jun[j+kpkrow-1] = sdata$Jun[j+kilrow-1] + sdata$Jun[j+pklrow-1]
    
    sdata$Jul[j+kpkrow-1] = sdata$Jul[j+kilrow-1] + sdata$Jul[j+pklrow-1]
    sdata$Aug[j+kpkrow-1] = sdata$Aug[j+kilrow-1] + sdata$Aug[j+pklrow-1]
    sdata$Sep[j+kpkrow-1] = sdata$Sep[j+kilrow-1] + sdata$Sep[j+pklrow-1]
    
    sdata$Oct[j+kpkrow-1] = sdata$Oct[j+kilrow-1] + sdata$Oct[j+pklrow-1]
    sdata$Nov[j+kpkrow-1] = sdata$Nov[j+kilrow-1] + sdata$Nov[j+pklrow-1]
    sdata$Dec[j+kpkrow-1] = sdata$Dec[j+kilrow-1] + sdata$Dec[j+pklrow-1]
    
}

# calculate column totals

sdata$TotalAll = sdata$Jan + sdata$Feb + sdata$Mar  + sdata$Apr + sdata$May  + sdata$Jun + sdata$Jul  + sdata$Aug + sdata$Sep  + sdata$Oct + sdata$Nov  + sdata$Dec
sdata$TotalWin = sdata$Jan + sdata$Feb + sdata$Mar  + sdata$Apr + sdata$Nov + sdata$Dec

# output the summary table

skip_line()
message(" >>> Generating: Summary Site Category File: ",ssfile,"  Rows: ",nrsdata)

write.table(sdata, file = ssfile, sep = ",", col.names = sscolnames, row.names =F) #save to csv
skip_line()
blank_line()

###########################################################################################################################
#
# Generate file with All Column Names
#
### 
### Code to Dump Column Names
###

allname = colnames(jdata2)
nrcdata = length(allname)

skip_line()
### add number of columns to file name
jcname = paste(Clu_Prefix,"Column_",nrcdata,"_Names.csv",sep="")
jcfile   <- paste(myDir,"/",resultsDir,"/",jcname,sep="")

message(" >>> Generating: File of Column Names: ",jcfile,"  Rows: ",nrcdata)

ccnames = c("colNum")
write.table(allname, file = jcfile,col.names=ccnames ,sep = ",") #save to csv
skip_line()
blank_line()

###########################################################################################################################
#
# Check Number of Unique IDs
#

u_osite_cat = unique(jdata2$oSite_Cat)
u_site_cat  = unique(jdata2$Site_Cat)
u_beh_1     = unique(jdata2$Behaviour_1)
u_beh_2     = unique(jdata2$Behaviour_2)

u_oprey_spp = unique(jdata2$oPrey_spp)
u_prey_spp  = unique(jdata2$Prey_spp)
u_prey_1    = unique(jdata2$Prey_1)
u_prey_2    = unique(jdata2$Prey_2)

u_invcid    = unique(jdata2$invCID)
u_invcvd    = unique(jdata2$invCVD)
u_centid    = unique(jdata2$CentID)

n_osite_cat = length(u_site_cat)
n_site_cat  = length(u_site_cat)
n_beh_1     = length(u_beh_1)
n_beh_2     = length(u_beh_2)

n_oprey_spp = length(u_oprey_spp)
n_prey_spp  = length(u_prey_spp)
n_prey_1    = length(u_prey_1)
n_prey_2    = length(u_prey_2)

n_invcid    = length(u_invcid)
n_invcvd    = length(u_invcvd)
n_centid    = length(u_centid)

blank_line()
message(" >>> TOTAL UNIQUE COUNTS")
skip_line()
message(" >>> Behaviours:   Orig_Site_Cat=",n_osite_cat,"  Site_Cat=",n_site_cat,"  Beh_1 =",n_beh_1, "  Beh_2 =",n_beh_2)
message(" >>> Prey:         Orig_Prey_spp=",n_oprey_spp,"  Prey_spp=",n_prey_spp,"  Prey_1=",n_prey_1,"  Prey_2=",n_prey_2)
message(" >>> Investigated: invCID       =",n_invcid,   "  invCVD  =",n_invcvd)
message(" >>> Clusters:     CentID       =",n_centid)
skip_line()
blank_line()

message(" >>> Behavior_1 Breakdown")
skip_line()
for (b1 in u_beh_1) {
  message("   ",b1)
}
skip_line()
blank_line()

message(" >>> Behavior_2 Breakdown")
skip_line()
for (b2 in u_beh_2) {
  message("   ",b2)
}
skip_line()
blank_line()

message(" >>> Prey_1 Breakdown")
skip_line()
for (b1 in u_prey_1) {
  message("   ",b1)
}
skip_line()
blank_line()

message(" >>> Prey_2 Breakdown")
skip_line()
for (b2 in u_prey_2) {
  message("   ",b2)
}

skip_line()
blank_line()
message(" >>> KillAge Breakdown")
skip_line()
u_killage = unique(jdata2$KillAge)
for (b1 in u_killage) {
  message("   ",b1)
}
skip_line()
blank_line()

message(" >>> Prey_age_class Breakdown")
skip_line()
u_preyageclass = unique(jdata2$Prey_age_class)
for (b2 in u_preyageclass) {
  message("   ",b2)
}

skip_line()
blank_line()

message(">>>>>>>>>> Complete: ",progname,"  --  Time: ",Sys.time())


