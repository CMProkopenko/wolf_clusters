####################################################################
#
# Rarify Wolf "_location" file
#
# v1 - initial verison
#
#   Input Files: all files containing "_location" in a given directory
#                all files containing "_analysis" in a given directory
#
#   Output File: Rn/<same_name>
#
# v3 - add in filtering (filterS) - remove points closer than filterS
#      use control file to select filtering and even/odd rarification per WolfID
#      provide rarification based on evcen/odd hour selection
#
# v4 - put source files in their own folder: Original
#    - reassign ID in files to be sequential after rarifying
#    - keep original ID in new column
#
# v5 - minor updates for GHA data
#
####################################################################

## This software is licensed under: https://opensource.org/license/mit

## For more information refer to the GitHub Project: https://github.com/CMProkopenko/wolf_clusters

#
# Define program defaults
#

# Program Name and Version
progname = "rareWloc_v5"

# Debug output
debugFlag = FALSE

# Set Zone
#
# RMNP        = 14
# Eastern MB  = 15

studyzone = 14

# Top Level Folder Name 
locFolder = "locdataAll-CCv32utm15"
#locFolder = "locdataTest"

# Control File
inFile  = "aaaControlFile.csv"

# Prompt for Files instead of using defaults
promptFileNames = FALSE

# Original files folder
origFolder = "Original"

# Rarify Folder Names and Thresholds

hourSec    = 3600
hourLimit  = hourSec/2

rareFolder = c("R1","R2all","R2even","R2odd")
rareLimit  = c(hourSec-hourLimit,2*hourSec-hourLimit,2*hourSec-hourLimit,2*hourSec-hourLimit)
rareseleo  = c(3,3,2,1)

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

# Read in control file
inFilePath =  paste(locDir,"/",inFile,sep='')

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

message(" >>>")
message(" >>> Rarify  Folder:      ",locDir)
message(" >>>")

origDir = paste(locDir,"/",origFolder,sep="")
setwd(origDir)
flist = list.files() 

# count the number of "_location" files in the directory
numloc = 0

for (f1 in flist)
{	
  fnameIn  <- f1
  ntype = file_type(fnameIn)
  if (ntype == ft_loc) numloc = numloc + 1
}

message(" >>> Location File Count: ",numloc)
message(" >>>")

for (f2 in rareFolder)
{
  #
  # Create folder if it doesn't already exist
  #
  rF2 = paste(locDir,"/",f2,sep='')
  
  if (dir.exists(rF2)) {
    message(" >>> Checking Folder: ",rF2)
    }
  else {
    message(" >>> Creating Folder: ",rF2)
    dir.create(rF2)
    }
}

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
    #   Append files together

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
    
#
# SKIP - Sort Table
#
# alldata = alldata[order(alldata$WolfID),]

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

# now do the same for _analysis file

# Remove rows where Lat == 0  or  Long == 0
andata = andata[andata$LATITUDE!=0,]
andata = andata[andata$LONGITUDE!=0,]

# remove NA rows
andata = andata[!is.na(andata$LATITUDE),]
andata = andata[!is.na(andata$LONGITUDE),]

# remove out of range rows
andata = andata[andata$LONGITUDE <= -90.0,]
andata = andata[andata$LONGITUDE >= -108.0,]
nrandata = length(andata[,1])

if (nrandata != nralldata) {
  message(" >>>>>>>>>>> ERROR: Cleaned _location <> Cleaned _location")
  message(" >>>>>>>>>>> ERROR: Analysis Rows  (Minus Out of Range Long)   =",nrandata," <<<<<<<<<<<<")
  message(" >>>>>>>>>>> ERROR: Check Log")
}

# Data needs to be filtered

nrfndata = -1
nrfadata = -1

skip_line()

#
# Processing steps for merged data
#
# Remove Lat == 0 or Long == 0
#
# Sort Data on WolfID, 
#
# Create ID from WolfID 
# Remove Lat or Long == 0 
# Add X, Y based on UTM14 (???????????)
# Output number of rows and number of columns
# Sort data by ID, Time
# Add cumulative hours (cumhr) - relative to 2010-01-01
# Add fixID

fixID  = {}
ID     = {}
X      = {}
Y      = {}
cumhr  = {}
species= {}

basetime = strptime(paste("2010-01-01","10:00:00"),"%Y-%m-%d %H:%M:%S",tz="EST")

for (i in 1:nralldata) {
  
  fixID = append(fixID,i)
  
  # Calculate X and Y

  # Lat  == y
  # Long == x
  
  lati<-c(alldata$Latitude[i])
  longi<-c(alldata$Longitude[i])
  
  # Zone == 14 or 15
  
  foo = LongLatToUTM(longi,lati,studyzone)
  x1 = foo$X[1]
  y1 = foo$Y[1]

  X = append(X,x1)
  Y = append(Y,y1)
  
  # Use WolfID as ID - but check that numeric part is still unique
  id1 = alldata$WolfID[i]
  id2 = as.numeric(substr(id1,2,nchar(id1)))
  ID = append(ID,id2)
  Species = append(species,"Wolf")
  
  # calculate cumulative hours since 2010-01-01
  rdate = alldata$Date[i]
  rtime = alldata$Time[i]
  cdatetime = paste(rdate,rtime)
  curdt = strptime(cdatetime,"%Y-%m-%d %H:%M:%S",tz="EST")
  deltatimeD = difftime( curdt, basetime, units = "hours",tz="EST")
  deltatime = as.numeric(deltatimeD)
  cumhr = append(cumhr,deltatime)
  
}

# Check WolfID and ID have the same number of unique values

nuwolfid = length(unique(alldata$WolfID))
nuid     = length(unique(ID))

message(" >>> # Unique WolfIDs       =",nuwolfid)
message(" >>> # Unique numeric IDs   =",nuid)

if ( !(nuwolfid == nuid) ) stop(" Fatal Error: Numeric ID is not unique")

if ( !(nuwolfid == 1   ) ) stop(" Fatal Error: More than one WolfID in file")

# Add new columns

newdata  = cbind(fixID,ID,Species,alldata,Y,X,cumhr)
nrnewdata= length(newdata[,1])
ncnewdata= length(newdata[1,])

# Data needs to be filtered based on cumhr delata < lfilter (filterS)
# rows removed from _location file also need to be removed from analysis file
#
# Skip this step for now - data is being rarified to 1 hour in any case

nrfndata = nrnewdata
nrfadata = nrandata

# find row in Control File

lwid = alldata$WolfID[1]

if (nrindata == 0) {
  genR2allFlag  = TRUE
  genR2evenFlag = TRUE
  genR2oddFlag  = TRUE
}

if (nrindata != 0) {
  irow = which(indata$WolfID==lwid)
  
  lfilter       = indata[irow,1]
  genR2allFlag  = indata[irow,3] == 1
  genR2evenFlag = indata[irow,4] == 1
  genR2oddFlag  = indata[irow,5] == 1
}

#
# generate each rarify variant for this file
#
blank_line()

nrare = length(rareLimit)
for (i in 1:nrare)
{
  if((i>1) && (rareseleo[i] == 1) && (!genR2oddFlag) )  next()
  if((i>1) && (rareseleo[i] == 2) && (!genR2evenFlag) ) next()
  if((i>1) && (rareseleo[i] == 3) && (!genR2allFlag) )  next()
  
  sldata1    = newdata
  sldata     = sldata1
  sandata1   = andata
  sandata    = sandata1
  nrsldata   = length(sldata[,1])
  nrsandata  = length(sandata[,1])
  
  if(rareseleo[i] == 2) {
    # select only Even hours
    rsel     = selevenodd(sldata1,2)
    sldata   = sldata1[rsel  == TRUE,]
    sandata  = sandata1[rsel == TRUE,]
  }
  
  if(rareseleo[i] == 1) {
    # select only Odd hours
    rsel     = selevenodd(sldata1,1)
    sldata   = sldata1[rsel  == TRUE,]
    sandata  = sandata1[rsel == TRUE,]
  }

  nrsldata   = length(sldata[,1])
  nrsandata  = length(sandata[,1])
  
  f2  = rareFolder[i]
  rF2 = paste(locDir,"/",f2,sep='')
  f3  = paste(rF2,"/",fnameIn,sep='')
  
  lrare = rareLimit[i]
  
  f3An = gsub("_location","_analysis",f3)
  
  message(" >>> Generating: ",f3)
  message(" >>> Generating: ",f3An)
  message(" >>>    Limit = ",lrare)
  
  #
  # rarify return a LOGICAL vector used to select the rows
  #
  
  rsel    = rarify(sldata,lrare)
  rdata   = sldata[rsel == TRUE,]
  adata   = sandata[rsel  == TRUE,]
  
  nrrdata = length(rdata[,1])
  ndelta  = nrnewdata - nrrdata
  
  # Cluster Analysis need sequential IDs
  # For analysis file first column should be saved in column N# renamed to OrigFixNo
  
  colnames(adata)[colnames(adata)=="N13"] <- "OrigFixNum"
  
  names(adata)[1] <- "FixNum"
  seqid <- seq(nrrdata)
  ofnc  <- which(colnames(adata)=="OrigFixNum")
  adata[,ofnc] = adata[,1]
  adata[,1]    = seqid
  
  # Location file - rename to be consistent with analysis file
  names(rdata)[1] = "FixNum"
  names(rdata)[4] = "OrigFixNum"
  rdata[,1]    = seqid
  
  message(" >>>    Initial Points= ",nrnewdata,"  Final Points= ",nrrdata,"  Removed Points= ",ndelta)
  
  write.table(rdata, file = f3,   sep = ",", col.names = T, row.names = F) #save to csv
  write.table(adata, file = f3An, sep = ",", col.names = T, row.names = F) #save to csv
  
  
  # update log
  nfiles = nfiles + 1
  logrow = c(f2,lwid,lrare,fnameIn,nrlocdataorig,nrnewdata,nrfndata,nrsldata,nrrdata,".")
  if (nfiles == 1) {logdata = logrow}
  else {logdata = rbind(logdata,logrow)}
  
  nfiles = nfiles + 1
  nradata = length(adata[,1])
  flag = "."
  if (nradata != nrrdata) {flag = "ERROR"}
  logrow = c(f2,lwid,lrare,fnameAn,nrandataorig,nrnewdata,nrfadata,nrsandata,nradata,flag)
  logdata = rbind(logdata,logrow)
  
}

blank_line()

  }
}

flog =  paste(locDir,"/","aaaRarityLog.csv",sep='')
message(" ")
message(" >>> Generating Log File: ",flog)

colnames(logdata) <- c("Folder","WolfID","rareLimitS","File","Orig","Cleaned","Filtered","SelectedEO","Rarified","Flag")
write.table(logdata, file = flog, sep = ",", col.names = T, row.names = F) #save to csv
blank_line()

message(">>>>>>>>>> Complete: ",progname,"  --  Time: ",Sys.time())

