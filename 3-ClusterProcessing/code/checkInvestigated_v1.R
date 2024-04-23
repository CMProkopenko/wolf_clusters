####################################################################
#
# Check Files
#
# v1 -  initial verison
#
#       Input File: investigated sites
#     
####################################################################
####################################################################
##
## This software is licensed under: https://opensource.org/license/mit
##
## For more information refer to the GitHub Project: https://github.com/CMProkopenko/wolf_clusters
##
####################################################################

### Load packages 
#
libs <- c('dplyr','data.table', 'ggplot2','grid','gridExtra','lattice','stringr')
lapply(libs, require, character.only=TRUE)

#
# Define program defaults
#
# Program Name and Version
progname = "checkInvestigated_v1"

#
# Debug output
debugFlag = FALSE
debugW    = "Wxx"

#
# Prompt for Files instead of using defaults
promptFileNames = FALSE

##### Change the following names as required #####
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
investFile = "InvestigatedPoints0131_cleaned20180614.csv"

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
Clu_Prefix = "2021-01-03-CM_SZcM_"
ksname = paste(Clu_Prefix,"CluSiteList.csv",sep="")
asname = paste(Clu_Prefix,"AmbSiteList.csv",sep="")
ssname = paste(Clu_Prefix,"SumBy_Wolf.csv",sep="")

##### End of file name tweaks #####

#
# Cross Reference File
#xrefFile = paste("Join-Matched-CTM",cluster_tolerance,"-",groupFile,sep="")
xrefFile = "ClusterMatch_SZcleaningMod.csv"


root_dir<-function(){
	# Look for the root directory in order of preference
	dir1 = "C:/Wolf-Projects/Data"
	if( file.exists(dir1) ) return(dir1)
	dir1 = "D:/Wolf-Projects/Data"
	if( file.exists(dir1) ) return(dir1)
	dir1 = "E:/Wolf-Projects/Data"
	if( file.exists(dir1) ) return(dir1)
	# default
	dir1 = "C:/Data"
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
  #
  # TODO - select file
  #
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
  #
  # TODO - select file
  #
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
  #
  # TODO - select file
  #
  fname1  <- xrefFile
  fnameI  <- paste(myDir,"/",resultsDir,"/",fname1,sep="")
  if (promptFileNames) {
    message(" >>> Select: Wolf Cluster Match File: ")
    fnameI = file.choose()
  }
  skip_line()
  data  = read.csv(fnameI,stringsAsFactors=FALSE)
  ldata = length(data[,1])
  message(" >>> Processing: Join-  File: ","  Length: ",ldata,"  Name:", fnameI)
  skip_line()
  return(data)
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
asfile   <- paste(myDir,"/",resultsDir,"/",asname,sep="")
ssfile   <- paste(myDir,"/",resultsDir,"/",ssname,sep="")

#### Join File

xrefData   = getXref()
nrxref     = length(xrefData[,1])
ncxref     = length(xrefData[1,])

#### Select key rows

matdata    = xrefData[xrefData$JoinStatus == "Matched",]
nrmatdata  = length(matdata[,1])

message(" >>>      Matched Rows: ",nrmatdata)
message(" ")

invData    = getInvest()
nrinv      = length(invData[,1])
ncinv      = length(invData[1,])

####
#### Check for Duplicated investigation IDs
####

fname1  <- investFile
fnameI  <- paste(myDir,"/",resultsDir,"/",fname1,sep="")

message(" ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>> ")
message(" >>> Start Check #1  Duplicated IDs (Cluster_ID) in File: ", fnameI)
message(" >>> ")
message(" >>>                 Output all rows with the same Cluster_ID")
message(" >>> ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")

message(" ")

ndupid  = 0
nduprow = 0

uinvCID = unique(invData$Cluster_ID)
for (i in uinvCID) {
  linvCID = which(invData$Cluster_ID == i)
  nlinvCID= length(linvCID)
  if (nlinvCID > 1) {
    ndupid = ndupid + 1
    nduprow = nduprow + nlinvCID
    message("")
    message(" >>>>>> Check #1: Duplicate Cluster ID: ",i,"  Count: ",nlinvCID)
    for (j in linvCID) {
      pspp = invData$Prey_spp[j]
      if (is.na(pspp)) pspp ="NA"
      message(" >>>    Row: ",pad_int(j,1000)," Clstr_Vst: ",invData$Clstr_Vst[j]," Prey_spp: ",str_pad(pspp,10,"right")," Site_Cat: ",str_pad(invData$Site_Cat[j],15,"right")," Notes: ",invData$Notes[j])
    }
  }
}

message(" ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>> ")
message(" >>> End   Check #1  Duplicated IDs (Cluster_ID) in File: ", fnameI)
message(" >>> ")
message(" >>>                 Total     IDs : ",length(uinvCID))
message(" >>>                 Duplicate IDs : ",ndupid)
message(" >>>                 Total     Rows: ",nrinv)
message(" >>>                 Duplicate Rows: ",nduprow)
message(" >>> ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" ")

####
#### Check for Funny Dates
####

fname1  <- investFile
fnameI  <- paste(myDir,"/",resultsDir,"/",fname1,sep="")

message(" ")

message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>> ")
message(" >>> Start Check #2  Potential Bad Dates in File: ", fnameI)
message(" >>> ")
message(" >>>                 Cluster_ID (Embedded Date) != Clstr_Vst")
message(" >>>                 Ignore Cluster_IDs containing DEAD")
message(" >>> ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" ")

ndupid  = 0
nduprow = 0

uinvCID = unique(invData$Cluster_ID)
for (i in uinvCID) {
  linvCID = which(invData$Cluster_ID == i)
  nlinvCID= length(linvCID)
  
  dateFlag = FALSE
  
  invcid = i
  syy = substring(invcid,3,6)
  smm = substring(invcid,7,8)
  sdd = substring(invcid,9,10)
  sdate = paste(syy,"-",smm,"-",sdd,sep="")
  if (sdate != invData$Clstr_Vst[linvCID[1]]) dateFlag = TRUE
  
  ##
  ## skip Cluster_IDs for Dead Wolves
  ##
  if (grepl("DEAD",invcid)) dateFlag = FALSE
  
  if (dateFlag) {
    ndupid = ndupid + 1
    nduprow = nduprow + nlinvCID
    message("")
    message(" >>>>>> Check #2: Bad Date Cluster ID: ",i,"  Count: ",nlinvCID)
    for (j in linvCID) {
      pspp = invData$Prey_spp[j]
      if (is.na(pspp)) pspp ="NA"
      message(" >>>    Row: ",pad_int(j,1000)," Clstr_Strt: ",invData$Clstr_Strt[j]," Clstr_End: ",invData$Clstr_End[j]," Clstr_Vst: ",invData$Clstr_Vst[j]," Prey_spp: ",str_pad(pspp,10,"right")," Site_Cat: ",str_pad(invData$Site_Cat[j],15,"right"))
    }
  }
}

message(" ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>> ")
message(" >>> End   Check #2  Potential Bad Dates in File: ", fnameI)
message(" >>> ")
message(" >>>                 Total      IDs : ",length(uinvCID))
message(" >>>                 Bad Dates  IDs : ",ndupid)
message(" >>>                 Total      Rows: ",nrinv)
message(" >>>                 Bad Date   Rows: ",nduprow)
message(" >>> ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" ")


####
#### Check: ClusterMatch_SZcleaningMod
####

fname1  <- xrefFile
fnameI  <- paste(myDir,"/",resultsDir,"/",fname1,sep="")

message(" ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>> ")
message(" >>> Start Check #3  Potential Bad Dates in File: ", fnameI)
message(" >>> ")
message(" >>>                 invCID (Embedded Date) != invCVD")
message(" >>>                 Ignore Cluster_IDs containing DEAD")
message(" >>> ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")

message(" ")

####
#### Check for Funny Dates
####

ndupid  = 0
nduprow = 0

uinvCID = unique(xrefData$Cluster_ID)
for (i in uinvCID) {
  linvCID = which(xrefData$Cluster_ID == i)
  nlinvCID= length(linvCID)
  
  dateFlag = FALSE
  nuinvCVD= 1
  
  invcid = i
  if (!is.na(invcid)) {
    syy = substring(invcid,3,6)
    smm = substring(invcid,7,8)
    sdd = substring(invcid,9,10)
    sdate = paste(syy,"-",smm,"-",sdd,sep="")
    if (sdate != xrefData$invCVD[linvCID[1]]) dateFlag = TRUE
    #
    # Only report:
    # (1) invCID embedded date does not match invCVD
    # (2) invCVDs are different for same invCID
    #
    nuinvCVD = length(unique(xrefData$invCVD[linvCID]))
  }
  
  ##
  ## skip Cluster_IDs for Dead Wolves
  ##
  if (grepl("DEAD",invcid)) dateFlag = FALSE

  if (dateFlag | nuinvCVD > 1) {
    ndupid = ndupid + 1
    nduprow = nduprow + nlinvCID
    message("")
    message(" >>>>>> Check #3: Bad Date invCID: ",i,"  Count: ",nlinvCID," Number Different invCVD: ",nuinvCVD)
    for (j in linvCID) {
      pspp = xrefData$Prey_spp[j]
      if (is.na(pspp)) pspp ="NA"
      message(" >>>    Row: ",pad_int(j,1000)," invCVD: ",xrefData$invCVD[j]," CentID: ",xrefData$CentID[j]," Prey_spp: ",str_pad(pspp,10,"right")," Site_Cat: ",str_pad(xrefData$Site_Cat[j],15,"right")," Notes: ",xrefData$Notes[j])
    }
  }
}

message(" ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>> ")
message(" >>> End   Check #3  Potential Bad Dates in File: ", fnameI)
message(" >>> ")
message(" >>>                 Total      IDs : ",length(uinvCID))
message(" >>>                 Bad Date   IDs : ",ndupid)
message(" >>>                 Total      Rows: ",nrxref)
message(" >>>                 Matched    Rows: ",nrmatdata)
message(" >>>                 Bad Date   Rows: ",nduprow)
message(" >>> ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" ")

####
#### Check for Duplicated investigation IDs
####

message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>> ")
message(" >>> Start Check #4  Duplicated invCID rows in File: ", fnameI)
message(" >>> ")
message(" >>>                 CentID (Centroid ID) Exists")
message(" >>>                 #Duplicate rows for invCID != #unique CentIDs")
message(" >>> ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")

ndupid  = 0
nduprow = 0

uinvCID = unique(xrefData$Cluster_ID)
for (i in uinvCID) {
  linvCID = which(xrefData$Cluster_ID == i)
  nlinvCID= length(linvCID)
  #
  # Only report:
  # (1) invCID duplicates that have different CentIDs
  # (2) CentID exists
  #
  if (is.na(xrefData$CentID[linvCID[1]]) ) {
    ncentid = 0
  }
  else {
    ncentid = length(unique(xrefData$CentID[linvCID]))
  }
  #### if (nlinvCID > 1) {message(" Debug nlinvCID: ",nlinvCID," ncentID: ",ncentid)}
  
  if ((nlinvCID > 1)  &   ncentid != nlinvCID  &  ncentid != 0) {
    ndupid = ndupid + 1
    nduprow = nduprow + nlinvCID
    message("")
    message(" >>>>>> Check #4: Duplicate invCID: ",i,"  #Duplicate Rows: ",nlinvCID,"  #Unique CentID: ",ncentid)
    for (j in linvCID) {
      pspp = xrefData$Prey_spp[j]
      if (is.na(pspp)) pspp ="NA"
      message(" >>>    Row: ",pad_int(j,1000)," invCVD: ",xrefData$invCVD[j]," CentID: ",xrefData$CentID[j]," Prey_spp: ",str_pad(pspp,10,"right")," Site_Cat: ",str_pad(xrefData$Site_Cat[j],15,"right")," Notes: ",xrefData$Notes[j])
    }
  }
}

message(" ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" >>> ")
message(" >>> End   Check #4  Duplicated invCID rows in File:: ", fnameI)
message(" >>> ")
message(" >>>                 Total     IDs : ",length(uinvCID))
message(" >>>                 Duplicate IDs : ",ndupid)
message(" >>>                 Total     Rows: ",nrxref)
message(" >>>                 Matched   Rows: ",nrmatdata)
message(" >>>                 Duplicate Rows: ",nduprow)
message(" >>> ")
message(" >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
message(" ")


message(">>>>>>>>>> Complete: ",progname,"  --  Time: ",Sys.time())


