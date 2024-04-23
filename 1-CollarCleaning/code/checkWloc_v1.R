####################################################################
#
# Check Rarify +analysis and _location files match exactly
#
# v1 - initial verison
#
#   Input Files: all files containing "_location" in a given directory
#                all files containing "_analysis" in a given directory
#
#   Output File: na
#
#
####################################################################

## This software is licensed under: https://opensource.org/license/mit

## For more information refer to the GitHub Project: https://github.com/CMProkopenko/wolf_clusters

#
# Define program defaults
#

# Program Name and Version
progname = "checkWloc_v1"

# Debug output
debugFlag = FALSE

# Inout Folder Name 
locFolder = "locdataAll-CCv31 - R300H96 - Test"
#locFolder = "locdataTest"

# original folder
origFolder = "Original"

# Prompt for Files instead of using defaults
promptFileNames = FALSE

# Rarify Folder Names and Thresholds

hourSec    = 3600
hourLimit  = hourSec/2

rareFolder = c("R1","R2all","R2even","R2odd")


#
# Rarification Log
#
# Rn, limit, file-name, before-rows, after-rows
#
nfiles = 0
lfilter = -1

library(sp)
library(rgdal)

#Function

is.even <- function(x) x %% 2 == 0
is.odd  <- function(x) x %% 2 != 0

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

file_type<-function(name){
  ft = 0
  if(length(grep("_location",name))>0) ft = ft_loc
  if(length(grep("mike",name))>0) ft = 0
  return(ft)
}

rarify<-function(xdata,xlimit){
  # remove points < limit (in seconds)
  n = length(xdata[,1])

  frow = {TRUE}
  i = 1
  j = 2
  
  if (n<2) {return(frow)}
  
  while (j <= n) {
    deltat = (xdata$cumhr[j] - xdata$cumhr[i]) * 3600
    if ( deltat < xlimit ) {
      frow = c(frow,FALSE)
      j = j+1
      }
    else {
      frow = c(frow,TRUE)
      i = j
      j = j + 1
      }
  }
  
  return(frow)
}

selevenodd<-function(indata,limit){
  # select points that are either even (limit==2) or odd (limit==1)
  n = length(indata[,1])
  outdata = {}

  j = 1
  
  while (j <= n) {
    sflag = FALSE
    if (is.even(indata$Hour[j]) & (limit == 2)) sflag = TRUE
    if (is.odd(indata$Hour[j])  & (limit == 1)) sflag = TRUE
    if(j == 1) {
      frow = {sflag}
      }
    else {
      frow = c(frow,sflag)
      }
    
    j = j + 1
  }
  
  return(frow)
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

#### Path to main folder
locDir = file_name(myDir,locFolder)


message(" >>>")
message(" >>> Location  Folder:      ",locDir)
message(" >>>")


for (f2 in rareFolder)
{
  #
  # check each folder
  #
  rF2 = paste(locDir,"/",f2,sep='')
  
  setwd(rF2)
  flist = list.files()  
  # count the number of "_location" files in the directory
  numloc = 0
  for (f1 in flist)
  {	
    fnameIn  <- f1
    ntype = file_type(fnameIn)
    if (ntype == ft_loc) numloc = numloc + 1
  }
  
  message(" >>>")
  message(" >>> Rarified Folder:     ",rF2)
  message(" >>> Location File Count: ",numloc)
  message(" >>>")


# Process Each File
#
# Keep only the first "loctruncate" columns
loctruncate = 16

alldata   = {}
firstdata = TRUE

for (f1 in flist)
{	
  fnameIn  <- f1
  ntype = file_type(fnameIn)
  if (ntype == ft_loc) {
    #
    # "_location" file
    #
    # Processing Steps - per file
    #   Read in file
    #   Truncate extra columns

    locdata  = read.csv(fnameIn,stringsAsFactors=FALSE)
    nrlocdata= length(locdata[,1])
    nrlocdataorig = nrlocdata
    nclocdata= length(locdata[1,])
    message(" >>> Processing: Location File: ",fnameIn,"  Rows=",nrlocdata,"  Cols=",nclocdata)

    locdata1 = locdata[,1:loctruncate]
    alldata = locdata1
    
    # 
    # create _analysis file name by replacing "_location" with "_analysis"
    #
    fnameAn = gsub("_location","_analysis",fnameIn)
    
    andata  = read.csv(fnameAn,stringsAsFactors=FALSE)
    nrandata= length(andata[,1])
    nrandataorig = nrandata
    ncandata= length(andata[1,])
    message(" >>> Processing: Analysis File: ",fnameAn,"  Rows=",nrandata,"  Cols=",ncandata)
    
    id1 = alldata[,4]
    id2 = andata[,13]
    idc = id1 == id2
    idn = length(which(idc==FALSE))
    
    if(idn != 0) {
      message(" >>>>>>>>>>> ERROR: ID Fields DO NOT MATCH")
      message(" >>>>>>>>>>> ERROR: ID Mismatch  =",idn)
      message(" >>>>>>>>>>> ERROR: Check Log")
      stop()
    }
    
    id1 = alldata[,14]
    id2 = andata[,15]
    idc = id1 == id2
    idn = length(which(idc==FALSE))
    
    if(idn != 0) {
      message(" >>>>>>>>>>> ERROR: Lat Fields DO NOT MATCH")
      message(" >>>>>>>>>>> ERROR: Lat Mismatch  =",idn)
      message(" >>>>>>>>>>> ERROR: Check Log")
      stop()
    }
  
    id1 = alldata[,15]
    id2 = andata[,16]
    idc = id1 == id2
    idn = length(which(idc==FALSE))
    
    if(idn != 0) {
      message(" >>>>>>>>>>> ERROR: Long Fields DO NOT MATCH")
      message(" >>>>>>>>>>> ERROR: Long andata[1,]
              Mismatch  =",idn)
      message(" >>>>>>>>>>> ERROR: Check Log")
      stop()
    }
    
    
    nralldata = length(alldata[,1])

    if (nrandata != nralldata) {
      message(" >>>>>>>>>>> ERROR: Cleaned _location <> Cleaned _analysis")
      message(" >>>>>>>>>>> ERROR: Analysis Rows  (Minus Out of Range Long)   =",nrandata," <<<<<<<<<<<<")
      message(" >>>>>>>>>>> ERROR: Check Log")
    }
  }
  
}

}

message(">>>>>>>>>> Complete: ",progname,"  --  Time: ",Sys.time())

