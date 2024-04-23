####################################################################
#
# Join Investigated sites with Generated Clusters using Match info 
#
# v1 -  initial verison
#      
#       Combine the Investigated file and the Merged Cluster file
#       Three types of combinations: Ctype
#       INVonly - Investigated site did not match Clusters
#       CLUonly - Cluster di not match Investigated site
#       Match   - Investigated site matched 1 or more Clusters (each one on a separate row)
#                 Clusters cans also match more than 1 Investigated site
#
# v2 -  use join functions
#       Columns with the same names that are not the join key will be suffixed
#          .M for Match file
#          .I for Investigation file
#          .C for Cluster file
#
#       FID   column = FID from Investigated table
#       rowID column = rowID from Merged Cluster table
#
####################################################################
####################################################################
##
## This software is licensed under: https://opensource.org/license/mit
##
## For more information refer to the GitHub Project: https://github.com/CMProkopenko/wolf_clusters
##
####################################################################

library(dplyr)

#
# Define program defaults
#
# Program Name and Version
progname = "joinWsites_v2"

#
# Debug output
debugFlag = FALSE

#
# Prompt for Files instead of using defaults
promptFileNames = FALSE

##### Change the following names as required #####
#
# Results directory   to use
# Investigation  file to use
# Merged Cluster file to use
# Cluster Tolerance to use to create name for Matched file

#
# Results Directory
resultsDir = "locdataTEST/Results"
resultsDir = "locdataAll-CCv31 - R300H96/Results"
resultsDir = "locdataAll-CCv32/Results"

#
# Investigation File
investFile = "GHA26_cleaned-cluster_Sept-11-2019_UpdatedLocs_nh3.csv"
#investFile = "InvestigatedPoints0131_cleaned20180614.csv"

#
# Merged MikeCluster2 File
groupFile = "mergedWmc2-locdataAll-CCv31 - R300H96-BLEND_ooeoeeexoeoooeoexxoeoexeoooxxx.csv"
groupFile = "mergedWmc2-locdataAll-CCv31 - R300H96-BLEND_1111111x11111111xx1111x1111xxx.csv"
groupFile = "mergedWmc2-locdataAll-CCv32-BLEND_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.csv"


#
# Cluster Tolerance (tolerance in meters added to buffer)
#
cluster_tolerance = 0
#cluster_tolerance = 15
#cluster_tolerance = 25
#cluster_tolerance = 50

##### End of needed changes #####

#
# Cross Reference File
xrefFile = paste("Matched-CTM",cluster_tolerance,"-",groupFile,sep="")


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
  message(" >>> Processing: Xref          File: ","  Length: ",ldata,"  Name:", fnameI)
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
ncinvest   = length(investData[1,])

groupData  = getGroup()
ngroup     = length(groupData[,1])
ncgroup    = length(groupData[1,])

xrefData   = getXref()
nxref      = length(xrefData[,1])
ncxref     = length(xrefData[1,])


# Check that all rows in the investigation file are in the match file
# If NOT then the match file is the wrong one

nirows = length(unique(xrefData$invRrow))

if (nirows != ninvest){
  message(">>>>> ERROR: Match file does not contain all Investigated sites ",nirows)
  STOP(1)
}

# 
# dplyr
# Description
# These are generic functions that dispatch to individual tbl methods
# inner_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),...)
# left_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
# right_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),...)
# full_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
# semi_join(x, y, by = NULL, copy = FALSE, ...)
# nest_join(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL,...)
# anti_join(x, y, by = NULL, copy = FALSE, ...)
# Arguments
# x, y tbls to join
# by a character vector of variables to join by. If NULL, the default, * join()
# will do a natural join, using all variables with common names across the
# two tables. A message lists the variables so that you can check they're
# right (to suppress the message, simply explicitly list the variables that
#        you want to join).
# To join by different variables on x and y use a named vector. For example,
# by = c("a" = "b") will match x.a to y.b.
# copy If x and y are not from the same data source, and copy is TRUE, then y
# will be copied into the same src as x. This allows you to join tables across
# srcs, but it is a potentially expensive operation so you must opt into it.
# suffix If there are non-joined duplicate variables in x and y, these suffixes will be
# added to the output to disambiguate them. Should be a character vector
# of length 2.
# ... other parameters passed onto methods, for instance, na matches to control
# how NA values are matched. See join.tbl df for more.
# keep If TRUE the by columns are kept in the nesting joins.
# name the name of the list column nesting joins create. If NULL the name of y is
# used.
# Currently dplyr supports four types of mutating joins, two types of filtering joins, and a
# nesting join.
# Mutating joins combine variables from the two data.frames:
#   inner join() return all rows from x where there are matching values in y, and all columns
# from x and y. If there are multiple matches between x and y, all combination of the
# matches are returned.

# left join() return all rows from x, and all columns from x and y. Rows in x with no match
# in y will have NA values in the new columns. If there are multiple matches between x
# and y, all combinations of the matches are returned.

# right join() return all rows from y, and all columns from x and y. Rows in y with no
# match in x will have NA values in the new columns. If there are multiple matches
# between x and y, all combinations of the matches are returned.

# full join() return all rows and all columns from both x and y. Where there are not
# matching values, returns NA for the one missing.

# Filtering joins keep cases from the left-hand data.frame:
#   semi join() return all rows from x where there are matching values in y, keeping just
# columns from x.
# A semi join differs from an inner join because an inner join will return one row of x
# for each matching row of y, where a semi join will never duplicate rows of x.

# anti join() return all rows from x where there are not matching values in y, keeping just
# columns from x.

# First join the Investigation columns to the Matched columns
# since all Investigated sites are already in the Match table we
# expect the number of rows in jtbl1 to be >= Matched table
# and the number of unique Investigation rows equals the total number of Investigation rows

jtbl1 = left_join(xrefData,investData,by="FID",suffix=c(".M",".I"))


nirows2 = length(unique(jtbl1$invRrow))
if (nirows2 != ninvest){
  message(">>>>> ERROR: jtbl1 table does not contain all Investigated sites ",nirows2)
  STOP(1)
}

if (length(jtbl1[,1]) < nxref){
  message(">>>>> ERROR: jtbl1 table smaller than Matched table ",length(jtbl1[,1]))
  STOP(1)
}

# Second join the First joined table to the Cluster columns
# And do a few sanity checks

jtbl2 = full_join(groupData,jtbl1,by="rowID",suffix=c(".C",".M"))

nirows3 = length(unique( jtbl2$invRrow[!is.na(jtbl2$invRrow)])  )
if (nirows3 != ninvest){
  message(">>>>> ERROR: jtbl2 table does not contain all Investigated sites ",nirows3)
  STOP(1)
}

if (length(jtbl2[,1]) < ngroup){
  message(">>>>> ERROR: jtbl2 table smaller than Cluster table ",length(jtbl1[,1]))
  STOP(1)
}

nirows4 = length( unique(jtbl2$rowID[!is.na(jtbl2$rowID)]) )
if (nirows4 != ngroup){
  message(">>>>> ERROR: jtbl2 table does not contain all Clusters  ",nirows4)
  STOP(1)
}

nrjoin = length(jtbl2[,1])
ncjoin = length(jtbl2[1,])

if (ncjoin != ncinvest + ncxref + ncgroup - 2) {
  message(">>>>> ERROR: jtbl2 table does not contain all Columns from Match, Investigated, Clusters  ",ncjoin)
  STOP(1)
}

skip_line()
message(" >>> Joining Tables")

#
# Add JoinStatus column
#    CLUonly 
#    INVonly
#    Matched

JoinStatus   = {}
cluonly_rows = 0
invonly_rows = 0
matched_rows = 0

for (i in 1:nrjoin) {
  # check all 4 combinations
  cluflag = !is.na(jtbl2$rowID[i])
  invflag = !is.na(jtbl2$FID[i])
  
  if (cluflag && invflag) {
    status       = "Matched"
    matched_rows = matched_rows + 1
  }
  
  if (cluflag && !invflag) {
    status       = "CLUonly"
    cluonly_rows = cluonly_rows + 1
  }
  
  if (!cluflag && invflag) {
    status       = "INVonly"
    invonly_rows = invonly_rows + 1
  }
  
  if (!cluflag && !invflag) {
    status       = "ERROR"
    message(">>>>> ERROR: jtbl2 table has both NA rowID and NA FID ",i)
    STOP(1)
  }
  
  JoinStatus = append(JoinStatus,status)
  total_rows = matched_rows + invonly_rows + cluonly_rows
  
}

# Add JoinStatus Column 
jtbl3  = cbind(JoinStatus,jtbl2)
ncjoin = length(jtbl3[1,])

skip_line()
message(" >>> Total Rows in Join Table: ",nrjoin)
message(" >>> Total Cols in Join Table: ",ncjoin)
skip_line()
message(" >>> JoinStatus:")
message(" >>>      Matched           (Matched) =",matched_rows)
message(" >>>      Investigated Only (INVonly) =",invonly_rows)
message(" >>>      Cluster Only      (CLUonly) =",cluonly_rows)
message(" >>>      Total Rows                  =",total_rows)
skip_line()

# Cross check counts

if (nrjoin != matched_rows + invonly_rows + cluonly_rows) {
  message(">>>>> ERROR: jtbl3 table Status counts do not match ",matched_rows + invonly_rows + cluonly_rows)
  STOP(1)
}

fnameO = paste("Join-",xrefFile,sep="")
joinFile = file_name(paste(myDir,"/",resultsDir,sep=""),fnameO)
message(" >>> Generating: Join File: ",joinFile)

write.table(jtbl3, file = joinFile, sep = ",", col.names = T, row.names =F) #save to csv
skip_line()
blank_line()
message(">>>>>>>>>> Complete: ",progname,"  --  Time: ",Sys.time())

