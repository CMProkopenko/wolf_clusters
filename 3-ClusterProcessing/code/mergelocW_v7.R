####################################################################
#
# Merge all Wolf "_location" files
#
# v1 - initial verison
#
#   Input Files: all files containing "_location" in a given directory
#
#   Output File: mergedW-<input folder name>.csv (default)
#
# v2 - Updated 2017-10-02
#
#   Added check for unique IDs
#   Added species
#   Added histogram of fix rate
#
# v5 - Updated 2019-04-19
#
#   Add blended merge using Control File
#
# v6 - Updated 2019-04-20
#
#   Add in columns that used to be done with extended merge 
#   Skip adding columns already added by program rarifyWloc
#   Make Blend tag account for skipped wolf IDs in the range  [1,30]
#   Calculate Step Duration
#
# v7 - Updated 2019-09-14
#
#   Add in new fields that provide information on how points are used in Cluster
#
####################################################################
##
## This software is licensed under: https://opensource.org/license/mit
##
## For more information refer to the GitHub Project: https://github.com/CMProkopenko/wolf_clusters
##
####################################################################


library(sp)
library(rgdal)

#
# Define program defaults
#

# 
# Select Project
#
projectname = "RMNP"
projectname = "GHA"
projectname = "RMNP"

# Program Name and Version
progname = "mergelocW_v7"

# Debug output
debugFlag = FALSE
errorCount= 0

# Wolf ID Range to scan
listWolfID = c("W01","W02","W03","W04","W05","W06","W07","W08","W09","W10","W11","W12","W13","W14","W15","W16","W17","W18","W19","W20","W21","W22","W23","W24","W25","W26","W27","W28","W29","W30")
listWolfID = c("35333","35334","35335","35336","37926","37927","1870","2020","5137","5138","5139","5140","5141","5142","5143","5144","5145","5146","5147","5148","5149","5150","5151","5153","5154","5155","5156","5915","5916","5917","5918","5919")

# Input Folder Name 
locFolder = "locdataAll-CCv31"
#locFolder = "loctest"
locFolder = "locdataAll-CCv32"
#locFolder = "locdataAll-CCv31 - R300H96 - Test"
#locFolder = "locdataAll-CCv31 - R300H96"

# Prompt for Files instead of using defaults
promptFileNames = FALSE

# Merge Control file
mctrlfile = "bbbMergeCtrlFile.csv"

# Results Folder
resultsFolder = "Results"

# Output: Merged File
imergeFile  = "mergediloc-"
mergeFile   = "mergedWloc-"

# Input: Merged Cluster File
mergedWmc2 = "mergedWmc2-"

library(sp)
library(rgdal)

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


####################################################################
#
# Start Main Script
#
####################################################################


message(">>>>>>>>>> Create: ",progname,"  --  Time: ",Sys.time())
blank_line()
skip_line()
message(" >>> Checking Root Directories <<< ")
myDir = root_dir()
message(" >>> Root Dir = ",myDir)

#
# Read in Control File
#
#### Path to main folder
locDir = file_name(myDir,locFolder)

# Read in control file
inFilePath =  paste(locDir,"/",mctrlfile,sep='')

indata <- read.csv(inFilePath,stringsAsFactors=FALSE)

nrindata <- nrow(indata)
ncindata <- ncol(indata)

message("   ")
message(">>> Control File:     ",inFilePath)
message(">>>    Rows:          ",nrindata)
message(">>>    Cols:          ",ncindata)

uwolfid  = unique(indata$WolfID)
nuwolfid = length(uwolfid)

message("   ")
message(">>> #Unique Wolf IDs: ",nuwolfid)
#message(">>> Wolf IDs:         ",uwolfid)
message(">>>")

# 
# Control File
#
#   Col 1:    WolfID
#   Col 2:    merge control set 1
#   Col 3..N: merge control set 2 to N-1
#   Col N+1 : comments
#

for (j in seq(2,ncindata-1)) {
  
  # Process Each File
  #
  # Keep only the first "loctruncate" columns
  #
  # The CC Collar Cleaning v31 genrates a fixed number of columns
  #
  loctruncate = 22
  alldata   = {}
  firstdata = TRUE
  
  # Determine Merge Tag
  # R1    : 1
  # R2all : a
  # R2even: e
  # R2odd : o
  # other : x
  
  mtag = "BLEND_"
  for (wid in listWolfID){
    wn = which(indata[,1]==wid)
    wtag = "x"
    if(length(wn) == 1) {
      rfolder = indata[wn,j]
      wid     = indata[wn,1]
      if(rfolder == "R1")     wtag = "1"
      if(rfolder == "R2all")  wtag = "a"
      if(rfolder == "R2even") wtag = "e"
      if(rfolder == "R2odd")  wtag = "o"
    }
    mtag = paste(mtag,wtag,sep="")
  }
  
  message("   ")
  message(">>> Merge Control Set: ",j-1," Tag:",mtag)
  message(">>>")
  
  for (i in 1:nrindata) {
    rfolder = indata[i,j]
    wid     = indata[i,1]
    message("   ")
    message(">>> WolfID: ",wid," Use: ",rfolder)
    
    # look for files in folder "rfolder" for wolf "wid"
    
    mDir = file_name(myDir,paste(locFolder,"/",rfolder,sep=""))
    
    message(" >>> Checking Folder:      ",mDir)
    
    setwd(mDir)
    flist = list.files()  
    # count the number of matching files in the directory
    numloc = 0
    
    for (f1 in flist)
    {	
      fnameIn  <- f1
      ntype = file_type(wid,fnameIn)
      if (ntype == merge_ft) numloc = numloc + 1
    }
    
    message(" >>> Matching File Count: ",numloc)
    
    Findex = 0
    for (f1 in flist)
    {	
      fnameIn  <- f1
      ntype = file_type(wid,fnameIn)
      if (ntype == merge_ft) {
        Findex = Findex + 1
        # Processing Steps - per file
        #   Read in file
        #   Truncate extra columns (there should be none unless Collar Cleaning is changed)
        #   Append files together
        
        locdata  = read.csv(fnameIn,stringsAsFactors=FALSE)
        nrlocdata= length(locdata[,1])
        nclocdata= length(locdata[1,])
        message(" >>> Processing: File[",Findex,"]: ",fnameIn,"  Rows=",nrlocdata,"  Cols=",nclocdata)
        
        locdata1 = locdata[,1:loctruncate]
        
        # Append Rfolder and Findex
        Rfolder  = rfolder
        FnameLoc = fnameIn
        FnameClu = NA
        locdata1 = cbind(locdata1,Rfolder)
        locdata1 = cbind(locdata1,FnameLoc)
        locdata1 = cbind(locdata1,FnameClu)
        locdata1 = cbind(locdata1,Findex)
        
        if (firstdata){
          alldata   = locdata1
          firstdata = FALSE
        } else {
          alldata = rbind(alldata,locdata1)
        }
      }
    }
  }
  
  # All files now merged - fix up the results
  
  #
  # Sort Table
  #
  
  alldata = alldata[order(alldata$WolfID,alldata$Date,alldata$Time,alldata$Findex),]
  
  skip_line()
  nralldata = length(alldata[,1])
  message(" >>> Total Rows                              =",nralldata)
  
  # Remove rows where Lat == 0  or  Long == 0
  alldata = alldata[alldata$Latitude!=0,]
  alldata = alldata[alldata$Longitude!=0,]
  nralldata = length(alldata[,1])
  message(" >>> Total Rows  (Minus Lat/Long == 0)       =",nralldata)
  # remove NA rows
  alldata = alldata[!is.na(alldata$Latitude),]
  alldata = alldata[!is.na(alldata$Longitude),]
  nralldata = length(alldata[,1])
  message(" >>> Total Rows  (Minus Lat/Long == NA)      =",nralldata)
  
  # remove out of range rows
  alldata = alldata[alldata$Longitude <= -90.0,]
  alldata = alldata[alldata$Longitude >= -108.0,]
  nralldata = length(alldata[,1])
  message(" >>> Total Rows  (Minus Out of Range Long)   =",nralldata)
  
  # remove rows that are duplicates
  notdup = {}
  ndf    = TRUE  # True is used for Not a Duplicate Row
  
  notdup = append(notdup,ndf)
  
  for (i in 2:nralldata){
    ndf = TRUE
    if ((alldata$WolfID[i] == alldata$WolfID[i-1]) & (alldata$Date[i] == alldata$Date[i-1]) & (alldata$Time[i] == alldata$Time[i-1])) ndf = FALSE
    #if(!ndf) message("Duplicate: ",i)
    notdup = append(notdup,ndf)
  }
  
  alldata = alldata[notdup,]
  nralldata = length(alldata[,1])
  message(" >>> Total Rows  (Minus Duplicate Rows  )    =",nralldata)

  # Add fixID as first column
  
  fixID  = seq(1,nralldata)
  
  # Repair ID if needed (some files have just WolfID without a "W" prefix)
  
  repairWflag = substr(alldata$WolfID[1],1,1) != "W"
  if (repairWflag) alldata$ID = alldata$WolfID
  
  # Check WolfID and ID have the same number of unique values
  
  nuwolfid = length(unique(alldata$WolfID))
  nuid     = length(unique(alldata$ID))
  
  message(" >>> ")
  message(" >>> # Unique WolfIDs       =",nuwolfid)
  message(" >>> # Unique numeric IDs   =",nuid)
  message(" >>> ")
  
  if ( !(nuwolfid == nuid) ) stop(" Numeric ID is not unique")
  
  # Calculate Step Duration
  
  stepdur   = {}
  previd    = -99999
  
  for (i in 1:nralldata) {
    
    # Use WolfID as ID - but check that numeric part is still unique
    # if there is no "W" as prefix then don't trim first character as it is all numeric (GHA collar id)
    trimWflag = FALSE
    id1 = alldata$WolfID[i]
    trimWflag = substr(id1,1,1) == "W"
    id2 = id1
    if (trimWflag) id2 = as.numeric(substr(id1,2,nchar(id1)))
    
    # if this the first row of a different wolf id then handle separately (also handles first row in file)
    firstFlag = previd != id2
    
    if (firstFlag) {
      stepdur1     = NA
    }
    else {
      # step duration in hours and seconds
      stepdur1   = (alldata$cumhr[i]-alldata$cumhr[i-1])
      stepdur1s  = (alldata$cumhr[i]-alldata$cumhr[i-1])*3600
      if(stepdur1 <= 0){
        message(" ERROR: BAD STEP DURATION: ROW: ",i,"  VALUE=",stepdur1)
        stop(0)
      }
    }
    
    # create vectors for column binding later
    
    stepdur = c(stepdur,stepdur1)
    previd = id2
    
  }
  
  # Add new columns
  #
  # CluCentID     Cross reference to Cluster file CentID
  # CluFirstPt    Count #times this is a  first point of any cluster, must   be [0,1]
  # CluLastPt     Count #times this is a  last  point of any cluster, must   be [0,1]
  # CluInnerPt    Count #times this is an inner point of any cluster, can    be [0,1]
  # CluAwayPt     Count #times this is an away  point of any cluster, can    be [0,n]
  # CluFirstRowID Cross reference to rowID of the merged Wmc2 file
  # CluLastRowID  Cross reference to rowID of the merged Wmc2 file
  # CluInnerRowID Cross reference to rowID of the merged Wmc2 file
  # CluAwayRowID  Cross reference to rowID of the merged Wmc2 file
  #               for the last Away point found since a single location can be an Away point in multiple Clusters
  
  CluFirstPt      = fixID
  CluFirstPt[]    = 0
  CluLastPt       = CluFirstPt
  CluInnerPt      = CluFirstPt
  CluAwayPt       = CluFirstPt
  CluFirstRowID   = CluFirstPt
  CluFirstRowID[] = NA
  CluLastRowID    = CluFirstRowID
  CluInnerRowID   = CluFirstRowID
  CluAwayRowID    = CluFirstRowID
  CluCentID       = CluFirstRowID
  
  newdata  = cbind(fixID,alldata,stepdur,CluCentID,CluFirstPt,CluLastPt,CluInnerPt,CluAwayPt,CluFirstRowID,CluLastRowID,CluInnerRowID,CluAwayRowID)
  
  nrnewdata= length(newdata[,1])
  ncnewdata= length(newdata[1,])
  
  mergeFile1 = paste(locDir,"/",resultsFolder,"/",imergeFile,locFolder,"-",mtag,".csv",sep="")
  
  #########################################
  #
  # Output Merged Location file - Intermediate
  #
  
  blank_line()
  skip_line()  
  message(" >>> Generating: Intermediate Merged File: ",mergeFile1,"  Rows=",nrnewdata,"  Cols=",ncnewdata)
  write.table(newdata, file = mergeFile1, sep = ",", col.names = T, row.names =F) #save to csv
  skip_line()
  
  # first part of name needs to be replaced
  cluFile1   = paste(locDir,"/",resultsFolder,"/",mergedWmc2,locFolder,"-",mtag,".csv",sep="")
  
  # read in already merged "Wmc2" file 
  #
  # then update new columns in "Wloc" dataset with counts of how locations are used in clusters

  blank_line()
  skip_line()
  
  cludata <- read.csv(cluFile1,stringsAsFactors=FALSE)
  
  nrcludata <- nrow(cludata)
  nccludata <- ncol(cludata)
  
  message(" >>> Processing: Merged Cluster File: ",cluFile1,"  Rows=",nrcludata,"  Cols=",nccludata)  
  skip_line()
  
  # read in Intermediate Merged Location file 
  #

  blank_line()
  skip_line()
  
  newdata <- read.csv(mergeFile1,stringsAsFactors=FALSE)
  
  nrnewdata <- nrow(newdata)
  ncnewdata <- ncol(newdata)
  
  message(" >>> Processing: Intermediate Merged Location File: ",mergeFile1,"  Rows=",nrnewdata,"  Cols=",ncnewdata)  
  skip_line()
  
  ##########################################
  #
  # Correlate the Clusters with Locations Points
  #
  # For each cluster row
  #
  # - parse all.fix.numbers.in.cluster 
  #   count First, Last, Inner points
  #
  # - parse all.away.points
  #   count Away points
  #
  for (i in 1:nrcludata){
    s1 = cludata$all.fix.numbers.in.cluster[i]
    if (s1 == "") message("ERROR ----- Bad Cluster Row - no fix numbers ",i)
    if (s1 != "") {
      # extract the fix points into a vector
      v1 = Numextract(s1)
      n1 = strtoi(v1)
      #
      if (length(n1) < 2) message("ERROR ----- Bad Cluster Row - single fix number ",i)
      
      for (j in 1:length(n1)){
        #
        # First, Last, Inner points are handled separately
        #
        lrow = findLocation(i,n1[j])
        if (j == 1){
          # First
          newdata$CluFirstRowID[lrow]  = i
          newdata$CluFirstPt[lrow]     = newdata$CluFirstPt[lrow]  + 1
        } else {
          if (j == length(n1)){
            # Last
            newdata$CluLastRowID[lrow]  = i
            newdata$CluLastPt[lrow]     = newdata$CluLastPt[lrow]  + 1
          } else {
            # Inner
            newdata$CluInnerRowID[lrow] = i
            newdata$CluInnerPt[lrow]    = newdata$CluInnerPt[lrow]  + 1
          }
        }

        # cross reference to centroid and file
        newdata$CluCentID[lrow]      = cludata$CentID[i]
        newdata$FnameClu[lrow]       = cludata$Fname[i]

      }
    }
    
    s1 = cludata$all.away.points[i]
    if (s1 != "") {
      # extract the away points into a vector
      v1 = Numextract(s1)
      n1 = strtoi(v1)
      #
      # Away Point
      #
      if (length(n1) < 1 ) message("ERROR ----- Bad Cluster Row - away fix numbers ",i)
      for (j in 1:length(n1)){
          lrow = findLocation(i,n1[j])
          newdata$CluAwayRowID[lrow]  = i
          newdata$CluAwayPt[lrow]     = newdata$CluAwayPt[lrow]  + 1
      }
    }
  }
  
  #########################################
  #
  # Output Merged Location file - Final
  #
  
  mergeFile2 = paste(locDir,"/",resultsFolder,"/",mergeFile,locFolder,"-",mtag,".csv",sep="")
  
  blank_line()
  skip_line()  
  message(" >>> Generating: Final Merged Location File: ",mergeFile2,"  Rows=",nrnewdata,"  Cols=",ncnewdata)
  write.table(newdata, file = mergeFile2, sep = ",", col.names = T, row.names =F) #save to csv
  skip_line()
  
  #########################################
  #
  # Check fields are set consistently
  #
  
  # Can more than one of the following columns be set at the same time? - Answer NO
  # What is the maximum count? - Answer 1
  #
  #    CluFirstPt 
  #    CluLastPt
  #    CluInnerPt
  
  if (max(newdata$CluFirstPt) > 1 ) message("ERROR ----- CluFirstPt count > 1 (",max(newdata$CluFirstPt),")" )
  if (max(newdata$CluLastPt)  > 1 ) message("ERROR ----- CluLastPt  count > 1 (",max(newdata$CluLastPt),")" )
  if (max(newdata$CluInnerPt) > 1 ) message("ERROR ----- CluInnerPt count > 1 (",max(newdata$CluInnerPt),")" )
  if (max(newdata$CluInnerPt+newdata$CluLastPt+newdata$CluFirstPt) > 1 ) message("ERROR ----- CluIFIL count > 1 (",max(newdata$CluInnerPt+newdata$CluLastPt+newdata$CluFirstPt),")" )
  
}


message(">>>>>>>>>> Complete: ",progname,"  --  Time: ",Sys.time())


