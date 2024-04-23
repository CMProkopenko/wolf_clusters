####################################################################
#
# Merge all Wolf cluster files
#
# v1 - initial verison - starting from mergelocW
#
#   Input Files: all files containing "mikecluster2" in a given directory
#                bbbMergeCtrlFile
#
#   Output File: mergedWmc2-<input-folder-name>-BLEND_<tag>.csv (default)
#
# v2 - Update 
#
#      Theo_fixes to include NA fixes but of time interval beween first and last fix (Total_hours)
#      
#      Fix_success = (Act_fixes + Fixes_away) / Theo_fixes
#
#      Site_revisit [0..n] is the number of times the site was revisited based on an analysis of
#
#                   all.fix.numbers.in.cluster
#                   all.away.points
#
#                   assuming NA fixes are not numbered any gaps in the away points indicates a revisit
#
# v3 - Add in extra fields to facilitate correlation with locations
#
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

# 
# Select Project
#
projectname = "RMNP"
projectname = "GHA"
projectname = "RMNP"

# Program Name and Version
progname = "mergeMC2_v3"

# Debug output
debugFlag = FALSE

# Cluster Parameters
# set value to "" turn off automatically adding it
CluParDefault = "R300H96Aany"

# Wolf ID Range to scan
listWolfID = c("W01","W02","W03","W04","W05","W06","W07","W08","W09","W10","W11","W12","W13","W14","W15","W16","W17","W18","W19","W20","W21","W22","W23","W24","W25","W26","W27","W28","W29","W30")
listWolfID = c("35333","35334","35335","35336","37926","37927","1870","2020","5137","5138","5139","5140","5141","5142","5143","5144","5145","5146","5147","5148","5149","5150","5151","5153","5154","5155","5156","5915","5916","5917","5918","5919")

# Input Folder Name 
#locFolder = "locdataAll-CCv31 - R300H96 - Test"
#locFolder = "locdataTEST"
locFolder = "locdataAll-CCv32"
#locFolder = "locdataAll-CCv31 - R300H96"

# Prompt for Files instead of using defaults
promptFileNames = FALSE

# Merge Control file
mctrlfile = "bbbMergeCtrlFile.csv"

# Results Folder
resultsFolder = "Results"

# Output: Merged File
mergeFile = "mergedWmc2-"

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
merge_ft = ft_clu

file_type<-function(id,name){
  ft = 0
  if(length(grep("_location",name))>0)      ft = ft_loc
  if(length(grep("_analysis.csv",name))>0)  ft = ft_ana
  if(length(grep("-mikeclusters2",name))>0) ft = ft_clu
  
  # check if wolf id matches as well
  
  if(length(grep(id,name))==0) ft = 0
  
  return(ft)
}

Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
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
        #   Add CluPar string (unless it already exists)
        #   Update Theo_fixes
        #   Calculate Site_revisit
        #   Append files together
        
        locdata  = read.csv(fnameIn,stringsAsFactors=FALSE)
        nrlocdata= length(locdata[,1])
        nclocdata= length(locdata[1,])
        message(" >>> Processing: File[",Findex,"]: ",fnameIn,"  Rows=",nrlocdata,"  Cols=",nclocdata)
        
        #truncation should not be needed for cluster files
        #locdata1 = locdata[,1:loctruncate]
        locdata1 = locdata
        
   
        # Add Cluster Parameters - per individual file
        # This step only works if CluPar is the last column in the MC2 files generated by the WWA Python code
        #
        # Skip adding column if there is no default or the MC2 file already has the column
        #
        if (CluParDefault != "") {
          cnames = colnames(locdata1)
          cindex = which("CluPar" == cnames)
          if (length(cindex) == 0) {
            CluPar   = seq(1,nrlocdata)
            CluPar[] = CluParDefault
            locdata1  = cbind(locdata1,CluPar)
          }
        }

        #####################################################################################
        #
        # Update:
        #
        #   Theo_fixes   -  number of Theoretical Fixes to include Miss_fixes (NA fixes)
        #   Fix_success  -  using updated Theo_fixes ((actual+away)/theoretical)
        #
        # Add:
        #
        #   Away_ratio   - ratio of Away/(Away+Actual) fixes
        #   Miss_fixes   - fixes that were removed from the input file (no lat/long)
        #   Fix_interval -estimated interval between fixes for the MC2 file
        #
        # Given:
        #
        #   Total_hours (from first to last fix of cluster)
        #   Act_fixes   (number of fixes in cluster)
        #   Fixes_away  (between first and last fix of cluster)
        #   
        # Note:
        #
        #   Some files rarified to R1 may only have fixes every two hours
        #   Calculate Fix_interval based on actual interval
        #   
        
        # Default new columns
        Miss_fixes     = seq(1,nrlocdata)
        Fix_interval   = Miss_fixes
        Site_revisit   = Miss_fixes
        Away_ratio     = Miss_fixes
        Miss_fixes[]   = NA
        Fix_interval[] = NA
        Site_revisit[] = NA
        Away_ratio     = NA
        
        # Recalculate fix_interval If R1
        # To calculate NA fixes the fix_interval (in hours) is needed
        #
        fix_interval = 2
        if(rfolder == "R1") fix_interval = 1
        
        if (fix_interval == 1) {
          totalfixes   = sum(locdata1$Act_fixes) + sum(locdata1$Fixes_away)
          totalhours   = sum(locdata1$Total_hours)
          fix_interval = floor((totalhours/totalfixes) + 0.5)
        }
        
        Fix_interval[] = fix_interval
        
        # Append new columnns
        locdata1 = cbind(locdata1,Away_ratio)
        locdata1 = cbind(locdata1,Miss_fixes)
        locdata1 = cbind(locdata1,Fix_interval)
        locdata1 = cbind(locdata1,Site_revisit)
        
        # Recalculate Theo_fixes
        
        num_hours             = pmax(locdata1$Total_hours,(locdata1$Act_fixes+locdata1$Fixes_away-1)*fix_interval )
        num_fixes             = floor(num_hours/fix_interval + 0.9) + 1
        locdata1$Theo_fixes   = num_fixes
        locdata1$Fix_success  = (locdata1$Act_fixes + locdata1$Fixes_away)/locdata1$Theo_fixes
        locdata1$Miss_fixes   = locdata1$Theo_fixes - locdata1$Act_fixes - locdata1$Fixes_away
        locdata1$Away_ratio   = locdata1$Fixes_away /(locdata1$Act_fixes + locdata1$Fixes_away)
        
        # Append file information to trace where clusters come from
        #    Rfolder  Rarification folder name
        #    Fname    File name (can be multiple for the same wolf)
        #    Findex   File index [1..n]
        #
        Rfolder  = rfolder
        Fname    = f1
        locdata1 = cbind(locdata1,Rfolder)
        locdata1 = cbind(locdata1,Fname)
        locdata1 = cbind(locdata1,Findex)
        
        #
        # add data for individual MC2 file to overall frame
        #
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
  #    Extra columns
  #    Numeric Wolf ID 
  #    Sort final table
  
  # First add a column indicating original order in file
  #
  # Add place holders Clust_ID	Clust_Typ  Site_revisit
  #
  nralldata    = length(alldata[,1])
  origID       = seq(1,nralldata)
  Clust_ID     = NA
  Clust_Typ    = NA
  Site_revisit = NA
  
  # Create numeric wolf ID, handle cases with and without "W" prefix
  #
  ID = alldata$CollarID
  trimWflag = substr(ID[1],1,1) == "W"
  if (trimWflag) ID = as.numeric(substr(ID,2,nchar(ID)))
  
  alldata   = cbind(origID,ID,alldata,Clust_ID,Clust_Typ)
  
  #
  # Sort Table
  #
  alldata   = alldata[order(alldata$CollarID,alldata$CentID,alldata$origID),]
  
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
  
  # Calculate X and Y from Lat/Long
  # Lat  == y
  # Long == x
  
  for (i in 1:nralldata) {
    
    lati<-c(alldata$Latitude[i])
    longi<-c(alldata$Longitude[i])
    
    # Zone == 14
    foo = LongLatToUTM(longi,lati,14)
    x1 = foo$X[1]
    y1 = foo$Y[1]
    
    alldata$X[i] = x1
    alldata$Y[i] = y1
  }

  
  # Add rowID as first column
  rowID  = seq(1,nralldata)
  
  newdata  = cbind(rowID,alldata)
  
  # Calculate Site_revisit
  
  for (i in 1:nralldata) {
    #
    # if all.away.points is NULL there were no revisits
    newdata$Site_revisit[i] = 0
    s1 = newdata$all.away.points[i]
    if (s1 != "") {
      # extract the away points into a vector
      v1 = Numextract(s1)
      n1 = strtoi(v1)
      #
      # number of Site_revisit increased each time there is a gap in the away point number
      prev_away = -999
      for (j in 1:length(n1)){
        if (n1[j] != (prev_away+1)) newdata$Site_revisit[i] = newdata$Site_revisit[i] + 1
        prev_away = n1[j]
      }
    }

  }
  
  nrnewdata= length(newdata[,1])
  ncnewdata= length(newdata[1,])
  
  mergeFile1 = paste(locDir,"/",resultsFolder,"/",mergeFile,locFolder,"-",mtag,".csv",sep="")

  message(" >>> Generating: Merged File: ",mergeFile1,"  Rows=",nrnewdata,"  Cols=",ncnewdata)
  
  write.table(newdata, file = mergeFile1, sep = ",", col.names = T, row.names =F) #save to csv
  
}

message(">>>>>>>>>> Complete: ",progname,"  --  Time: ",Sys.time())




