####################################################################
#
# Match Invedstigated Wolf Kill Sites with Cluster Groups
#
# v1 - initial verison
#      uses centroid + buffer + cluster_tolerance 
#      to check if investigated site matches
#
# v2 - added correlation table based on time
#
# v3 - updated Sep 10, 2018
#      added Cluster Tolerance in Meters to file name
#      assumes extended mc2m file as input (eg. extended-mergedW-locdataAll-CCv31.csv)
#
# v4 - add additional fields and align naming convention
#      inv: investigation file
#      mc2: Mike Cluster2 file
#      cor: Correlation Information
#
# v5 - retrieve information from ClusterCentroids2016011820171219_cleaned.csv
#      and append to correlation file
#
# v6 - new file names 20190224
#      added a directory corDir to use for input and output files
#
# v7 - generalize to use different MC2 merge files
#      use "match" instead of correlate
#
# v8 - change column headers to facilitate joining later
#      use NA instead of -1
#      include file names
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
# Program Name and Version
progname = "matchWsites_v8"
#
# Debug output
debugFlag = FALSE
#
# Prompt for Files instead of using defaults
promptFileNames = FALSE


##### Change the following file names and cluster tolerance as required #####
#
#
# Cluster Tolerance (tolerance in meters added to buffer)
#

#cluster_tolerance = 50
cluster_tolerance = 25
#cluster_tolerance = 15
#cluster_tolerance = 0

#
# Investigation File
investFile = "GHA26_cleaned-cluster_Sept-11-2019_UpdatedLocs_nh3.csv"
#investFile = "InvestigatedPoints0131_cleaned20180614.csv"

#
# Merged MikeCluster2 File"
#groupFile = "mergedWmc2-locdataAll-CCv31 - R300H96-BLEND_ooeoeeexoeoooeoexxoeoexeoooxxx.csv"
#groupFile = "mergedWmc2-locdataAll-CCv31 - R300H96-BLEND_1111111x11111111xx1111x1111xxx.csv"
groupFile = "mergedWmc2-locdataAll-CCv32-BLEND_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.csv"

#
# Results Directory
resultsDir = "locdataTEST/Results"
resultsDir = "locdataAll-CCv31 - R300H96/Results"
resultsDir = "locdataAll-CCv32/Results"

##### End of needed changes #####

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
  message(" >>> Processing: Extended-mc2   File: ","  Length: ",ldata,"  Name:", fnameI)
  skip_line()
  return(data)
}

getXref<-function(){
  data = {}
  #
  # TODO - select file
  #
  fname1  <- xrefFile
  fnameI  <- paste(myDir,"/",fname1,sep="")
  if (promptFileNames) {
    message(" >>> Select: Wolf Cluster Group File: ")
    fnameI = file.choose()
  }
  skip_line()
  data  = read.csv(fnameI,stringsAsFactors=FALSE)
  ldata = length(data[,1])
  message(" >>> Processing: Xref         File: ","  Length: ",ldata,"  Name:", fnameI)
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

message(">>>>>>>>>> Create: ",progname,"  --  Time: ",Sys.time())
skip_line()
message(" >>> Checking Root Directories <<< ")
myDir = root_dir()
message(" >>> Root Dir = ",myDir)
blank_line()

investData = getInvest()
ninvest    = length(investData[,1])

groupData  = getGroup()
ngroup     = length(groupData[,1])

#xrefData   = getGroup()
#nxref      = length(xrefData[,1])

# reduce size when debugging
if (debugFlag) ninvest <- min(ninvest,10)

xinvest = as.numeric(investData$X_proj)
yinvest = as.numeric(investData$Y_proj)
finvest = investData$FID
dinvest = as.Date(investData$Clstr_Vst)

xgroup  = groupData$X
ygroup  = groupData$Y
rgroup  = groupData$Clus_rad_m
cgroup  = groupData$CentID
fdtgroup= groupData$First_date
ldtgroup= groupData$Last_date
fidgroup= groupData$rowID
fday    = as.Date(substr(fdtgroup,1,10))
lday    = as.Date(substr(ldtgroup,1,10))

corindex    = 0
cornull     = 0
nwarnNoIDATE= 0
nwarnEARLY  = 0
invRrow   = {}
invFID   = {}
invCID   = {}
invCVD   = {}
mc2Rrow    = {}
mc2CID    = {}
mc2FID    = {}
corDwarn     = {}
corDdelta    = {}
corMdelta    = {}
invFile      = {}
cluFile      = {}

for (irow in 1:ninvest) 
{
    x1 = xinvest[irow]
    y1 = yinvest[irow]
    dist = sqrt((x1-xgroup)^2 + (y1-ygroup)^2)
    mgroup <- dist < (rgroup + cluster_tolerance)
    vmatch = which(mgroup)
    nmatch = length(vmatch)
    if (debugFlag) {
      message("***** invesigation row: ",irow)
      message("***** Num Matches: ",nmatch)
    }
    

    
    if (nmatch == 0) {
      # indicate there was no match by adding an entry to the table with NA as the clusterGroup
      corindex    = corindex + 1
      cornull     = cornull + 1
      invRrow     = append(invRrow,irow)
      invFID      = append(invFID,finvest[irow])
      invCID      = append(invCID,investData$Cluster_ID[irow])
      invCVD      = append(invCVD,investData$Clstr_Vst[irow])
      mc2Rrow     = append(mc2Rrow,NA)
      mc2CID      = append(mc2CID,NA)
      mc2FID      = append(mc2FID,NA)
      corDwarn    = append(corDwarn,NA)
      corDdelta   = append(corDdelta,NA)
      corMdelta   = append(corMdelta,NA)
      # always add file names
      invFile      = append(invFile,investFile)
      cluFile      = append(cluFile,NA)
    
      #message(" Cor Index: ",corindex,"  Invest Row: ",irow,"  Invest FID: ",finvest[irow]," Group Row: ",NA," Cluster Group:",NA)
    } else {
      for (jrow in 1:nmatch) {
        cgrow    = vmatch[jrow]
        corindex = corindex + 1
        ddelta   = 0
        mdelta   = dist[cgrow]
        twarn    = " "
        
        # Check if there is a Date for the Investigation
        if (is.na(dinvest[irow])) {
          twarn = "NoIDATE"
          nwarnNoIDATE = nwarnNoIDATE + 1
        } else {
          # Date of investigation before First Date of Cluster Group
          tempdelta = as.numeric(dinvest[irow] - fday[cgrow])
          if (tempdelta < 0) {
            ddelta  = as.numeric(dinvest[irow] - fday[cgrow])
            twarn   = "EARLY"
            nwarnEARLY = nwarnEARLY + 1
          } 
          
          # Date of Investigation after last Date of Cluster Group
          tempdelta = as.numeric(dinvest[irow] - lday[cgrow])
          if (tempdelta > 0) {
            ddelta  = as.numeric(dinvest[irow] - lday[cgrow])
          }
        
        } # Investigation Date Exists
        
        invRrow     = append(invRrow,irow)
        invFID      = append(invFID,finvest[irow])
        invCID      = append(invCID,investData$Cluster_ID[irow])
        invCVD      = append(invCVD,investData$Clstr_Vst[irow])
        mc2Rrow    = append(mc2Rrow,cgrow)
        mc2CID    = append(mc2CID,cgroup[cgrow])
        mc2FID    = append(mc2FID,fidgroup[cgrow])
        corDwarn     = append(corDwarn,twarn)
        corDdelta    = append(corDdelta,ddelta)
        corMdelta    = append(corMdelta,mdelta)
        # always add file names
        invFile      = append(invFile,investFile)
        cluFile      = append(cluFile,groupFile)
        #message(" Cor Index: ",corindex,"  Invest Row: ",irow,"  Invest FID: ",finvest[irow]," Group Row: ",cgrow," Cluster Group: ",cgroup[cgrow])
      }
    }
      
}

matchMdelta = corMdelta
matchDdelta = corDdelta
matchDwarn  = corDwarn

# keep original column names to facilitate joining later
FID = invFID
rowID = mc2Rrow

corData = cbind(FID,invCID,invCVD,mc2FID,mc2CID,matchMdelta,matchDdelta,matchDwarn,invFile,invRrow,cluFile,rowID)
ncor = length(corData[,1])
if (ncor != corindex) message("****** Error: Match Table Wrong Length")

skip_line()
message(" >>> Match Analysis - Cluster Tolerance (in meters) (CTMnn) == ",cluster_tolerance)


skip_line()
message(" >>> Total Entries in Match Table: ",corindex)
message(" >>> Match Entries in Match Table: ",corindex-cornull)
message(" >>> Null  Entries in Match Table: ",cornull)
skip_line()
message(" >>> Total Missing Investigation Dates:  ",nwarnNoIDATE)
message(" >>> Total EARLY   Investigation Dates:  ",nwarnEARLY)
skip_line()

fnameO = paste("Matched-CTM",cluster_tolerance,"-",groupFile,sep="")
corFile = file_name(paste(myDir,"/",resultsDir,sep=""),fnameO)
message(" >>> Generating: Match File: ",corFile)

write.table(corData, file = corFile, sep = ",", col.names = T, row.names =F) #save to csv

blank_line()
message(">>>>>>>>>> Complete: ",progname,"  --  Time: ",Sys.time())




