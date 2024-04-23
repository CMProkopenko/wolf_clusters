#!/usr/bin/env python

##     GPS Collar Cleaning Software
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
##
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
##
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## This takes as input a CSV file of GPS-collar fixes.
## Since each vendor is different two files are generated:
##    original file name + "_location" for generating maps
##    original file name + "_analysis" as input to the cluster analysis algorithm

## For more information refer to the GitHub Project: https://github.com/CMProkopenko/wolf_clusters

##
## Change Log pre-GitHub
##

# v30 Change Log
#
# normalize input date&time to be Local
# add GMT Date and Time to all files
# correct DST for Televilt
# handle multiple formats

# v31 Change Log
#
# align "_location" headers for all collar typesFYear

# v32 Change Log
#
# tweak latitude and longitude range checks 
# - checkLat
# - checkLong
#
# Televit - add a 4th format

# v33 Change Log - 2021-02-17
#
# (1) Detect "No Sats" row and set Lat/Long to 0.0/0.0
#
# (2) Process 2nd Line of Lotek file (previously skipped)
#
# (3) Extend acceptable date range
#
# (4) Clarify DST Messages

# v34 Change Log - 2023-05-19
#
# (1) Tweak Televilt format 4

##############################################################################
#
# Tune program settings 
#
# (1) default folder
# (2) add a "daterange.csv" file containing two dates separated by a command-line
#     A one line file with start date and end date of format YYYY-MM-DD,YYYY-MM-DD
# (3) Set how to handle Televilt files
#
##############################################################################

# change this path for ease of use
#
default_folder = "."

# turn verbose debugging on or off
debug_flag = False

# Global variables for date range selection
# default to no selection
#
# if the file contains the string "daterange" then it will overrise these defaults
#
daterange_parsed= False
daterange_flag  = False
daterange_start = "2014-01-01"
daterange_end   = "2020-12-31"

# Date adjustment flags
#
# televiltDSTpresent - used to calculate GMT
# televiltDSTremove  - used to adjust local time
#
# defaults
#
televiltDSTpresent = 0
televiltDSTremove  = 0

import traceback
import csv
import math
import time
import sys
import os
import os.path
import re
import datetime

## **************** code follows *****************************

# create time deltas for adjusting date/time values

timedelta1H     = datetime.timedelta(hours=1,minutes=0)
timedelta5H     = datetime.timedelta(hours=5,minutes=0)
timedelta6H     = datetime.timedelta(hours=6,minutes=0)
timedeltaM1H    = datetime.timedelta(hours=-1,minutes=0)
timedeltaM6H    = datetime.timedelta(hours=-6,minutes=0)

def adjustDate(year,month,day,hour,minute,second,tdelta):
    told    =datetime.datetime(year,month,day,hour,minute,second)
    tnew    =told+tdelta
    newy    =tnew.year
    newm    =tnew.month
    newd    =tnew.day

    dateY = str(newy)
    dateM = str(newm)
    if len(dateM)<2:
        dateM = '0' + dateM
    dateD = str(newd)
    if len(dateD)<2:
        dateD = '0' + dateD  
    
    newYMD=dateY + '-' + dateM + '-' + dateD

    return newYMD

def adjustDateStr(dateStr,timeStr,tdelta):
    #
    # dateStr = "YYYY-MM-DD"
    # timeStr = "HH:mm:SS"
    #
    year    =int(dateStr[0:4])
    month   =int(dateStr[5:7])
    day     =int(dateStr[8:10])
    hour    =int(timeStr[0:2])
    minute  =int(timeStr[3:5])
    second  =int(timeStr[6:8])
    told    =datetime.datetime(year,month,day,hour,minute,second)
    tnew    =told+tdelta
    newy    =tnew.year
    newm    =tnew.month
    newd    =tnew.day

    dateY = str(newy)
    dateM = str(newm)
    if len(dateM)<2:
        dateM = '0' + dateM
    dateD = str(newd)
    if len(dateD)<2:
        dateD = '0' + dateD  
    
    newYMD=dateY + '-' + dateM + '-' + dateD

    return newYMD

def adjustTimeStr(dateStr,timeStr,tdelta):
    #
    # dateStr = "YYYY-MM-DD"
    # timeStr = "HH:mm:SS"
    #
    year    =int(dateStr[0:4])
    month   =int(dateStr[5:7])
    day     =int(dateStr[8:10])
    hour    =int(timeStr[0:2])
    minute  =int(timeStr[3:5])
    second  =int(timeStr[6:8])
    told    =datetime.datetime(year,month,day,hour,minute,second)
    tnew    =told+tdelta
    newh    =tnew.hour
    newm    =tnew.minute
    news    =tnew.second

    timeH   = str(newh)
    timeM   = str(newm)
    timeS   = str(news)
    if len(timeH)<2:
        timeH = '0' + timeH
    if len(timeM)<2:
        timeM = '0' + timeM
    if len(timeS)<2:
        timeS = '0' + timeS

    newHMS = timeH + ':' + timeM + ':' + timeS

    return newHMS

def adjustTime(year,month,day,hour,minute,second,tdelta):
    told    =datetime.datetime(year,month,day,hour,minute,second)
    tnew    =told+tdelta
    newh    =tnew.hour
    newm    =tnew.minute
    news    =tnew.second

    timeH   = str(newh)
    timeM   = str(newm)
    timeS   = str(news)
    if len(timeH)<2:
        timeH = '0' + timeH
    if len(timeM)<2:
        timeM = '0' + timeM
    if len(timeS)<2:
        timeS = '0' + timeS

    newHMS = timeH + ':' + timeM + ':' + timeS

    return newHMS


def onlyascii(char):
    if ord(char) < 1 or ord(char) > 127: return ''
    else: return char

def get_my_string(file_path):
    f=open(file_path,'r')
    data=f.read()
    f.close()
    filtered_data=filter(onlyascii, data)
    filtered_data = filtered_data.lower()
    return filtered_data

def fixDate(tdt):
    # Input is "YYYY.MM.DD HH:MM:SS"
    # Outut is "YYYY-MM-DD"
    dates = tdt[0:10]
    dates = dates.replace('.','-')
    return dates

def fixTime(tdt):
    # Input is "YYYY.MM.DD HH:MM:SS"
    # Outut is "HH:MM:SS"
    times = tdt[11:]
    return times

def convertMDY(inDate):
    # Input  is "YYYY-MM-DD"
    # Output is "MM/DD/YYYY"
    dateM = inDate[5:7]
    dateD = inDate[8:10]
    dateY = inDate[0:4]

    outDate = dateM + '/' + dateD + '/' + dateY

    return outDate
    

def sortdata(indata,index):

    outdata = sorted(indata, key=lambda idata: idata[index])

    return outdata

def sortdata2(indata,iDate,iTime):

    outdata = sorted(indata, key=lambda idata: idata[iDate]+idata[iTime])

    return outdata

# Check date format YYYY-MM-DD
#
# returns True if ERROR
def checkYMDformat(indata,iDate):

    errorFlag = False

    for row in indata:

        YYYY = row[iDate][0:4]
        MM   = row[iDate][5:7]
        DD   = row[iDate][8:10]

        if (YYYY<"2010"):
            errorFlag = True

        if (YYYY>"2020"):
            errorFlag = True

        if (MM<"01"):
            errorFlag = True

        if (MM>"12"):
            errorFlag = True

        if (DD<"01"):
            errorFlag = True

        if (DD>"31"):
            errorFlag = True

        if errorFlag:
            print '>>>>>>'
            print '>>>>>> ERROR - YYYY-MM-DD Bad Format (%s) <<<<<<' % (row[iDate])
            print '>>>>>>'
            return errorFlag

    return errorFlag

# Check time format HH:MM:SS
#
# returns True if ERROR
def checkHMSformat(indata,iTime):

    errorFlag = False

    for row in indata:

        HH   = row[iTime][0:2]
        MM   = row[iTime][3:5]
        SS   = row[iTime][6:8]

        if (HH<"00"):
            errorFlag = True

        if (HH>"23"):
            errorFlag = True

        if (MM<"00"):
            errorFlag = True

        if (MM>"59"):
            errorFlag = True

        if (SS<"00"):
            errorFlag = True

        if (SS>"59"):
            errorFlag = True

        if errorFlag:
            print '>>>>>>'
            print '>>>>>> ERROR - HH;MM;SS Bad Format (%s) <<<<<<' % (row[iTime])
            print '>>>>>>'
            return errorFlag

    return errorFlag

# Check single row for date format YYYY-MM-DD
#
# returns True if ERROR
def checkYMDrow(row,iDate):

    errorFlag = False

    YYYY = row[iDate][0:4]
    MM   = row[iDate][5:7]
    DD   = row[iDate][8:10]

    if (YYYY<"2010"):
        errorFlag = True

    if (YYYY>"2020"):
        errorFlag = True

    if (MM<"01"):
        errorFlag = True

    if (MM>"12"):
        errorFlag = True

    if (DD<"01"):
        errorFlag = True

    if (DD>"31"):
        errorFlag = True

    if errorFlag:
        print '>>>>>>'
        print '>>>>>> ERROR - YYYY-MM-DD Bad Format (%s) <<<<<<' % (row[iDate])
        print '>>>>>>'
        return errorFlag

    return errorFlag

# Read in date range file
# A one line file with start date and end date of format YYYY-MM-DD,YYYY-MM-DD
#
# returns True if ERROR

def processDateRange(fname):
    
    global  daterange_parsed
    global  daterange_flag
    global  daterange_start
    global  daterange_end

    errorFlag           = True  # assume error while check format
    daterange_parsed    = True

    # Clear rowdata
    rowdata = []

    # open file, read all lines and trim off the first one (column headers)
    try:
    # Old way linesa = open(fname).readlines()
        with open(fname,'rU') as f:
            linesa = list(f)
    except IOError:
        print 'Error opening/reading "%s"...' % fname
        return

    nlinesa = len(linesa)
    if nlinesa > 1:
        print 'Date Range File "%s" to many lines' % fname
        return errorFlag

    linesd  = linesa
    linesf  = []
    
    if debug_flag:
        print '>>> Date Range data' 
    
    if debug_flag:
        for line in linesd:
            print line
    
    # Parse All Fixes as CSV
    csvinput = iter(linesd)
    csv1 = csv.reader(csvinput,delimiter=',')

    count  = 0
    
    for row in csv1:

        count = count + 1
        lrow = len(row)

        if debug_flag:
            print count
            print lrow
            print row
            
        if lrow != 2:   
            print 'Date Range File "%s" needs two fields' % fname
            return errorFlag

    # parse row as two date strings YYY-MM-DD

    if checkYMDrow(row,0):
        print 'Date Range File "%s" start date bad' % fname
        return errorFlag 

    if checkYMDrow(row,1):
        print 'Date Range File "%s" end date bad' % fname
        return errorFlag

    daterange_start = row[0]
    daterange_end   = row[1]
    daterange_flag  = True

    errorFlag = False

    return errorFlag
    

# Check date + time order 
#
# returns True if ERROR
def checkOrder(indata,iDate,iTime):

    firstFlag = True

    for row in indata:
        thiskey = row[iDate] + row[iTime]
        if firstFlag == False:
            if thiskey < lastkey:
                print '>>>>>>'
                print '>>>>>> ERROR - row out of order (lastkey,thiskey) (%s,%s) <<<<<<' % (lastkey,thiskey)
                print '>>>>>>'
                return True
        lastkey = thiskey
        firstFlag = False

    return False

# Check latitude range (50,60)
#
# returns True if ERROR
def checkLat(indata,iLat):

    errorFlag = False

    for row in indata:

        if len(row[iLat])==0:
            continue

        rlat = float(row[iLat])
        if rlat == 0.0:
            continue

        if (rlat<49):
            errorFlag = True

        if (rlat>60):
            errorFlag = True

        if errorFlag:
            print '>>>>>>'
            print '>>>>>> ERROR - Latitude Bad Format/Value (%s) <<<<<<' % (row[iLat])
            print '>>>>>>'
            return errorFlag

    return errorFlag

# Check longitude range
#
# returns True if ERROR *** CHANGE TO WARNING June 14 2017 ***
def checkLong(indata,iLong):
    
    errorFlag = False

    for row in indata:

        if len(row[iLong])==0:
            continue

        rlong = float(row[iLong])
        if rlong == 0.0:
            continue

        if (rlong<-108.0):
            errorFlag = True

        if (rlong>-90.0):
            errorFlag = True

        if errorFlag:
            print '>>>>>>'
            print '>>>>>> Warning - Longitude Value (%s) <<<<<<' % (row[iLong])
            print '>>>>>>'
            errorFlag = False


    return errorFlag

# Filter date range
#
# indata        data to filter
# iDate         index of date to use for filtering
def filter_date(indata,iDate):
    
    errorFlag   = False
    outdata     = []

    for row in indata:
        rowdate = row[iDate]
        if (rowdate >= daterange_start):
            if (rowdate <= daterange_end):
                outdata.append(row)

    return outdata


# Check         Date, Time, Latitude, Longitude
# Sort on       Date and Time
# Check Order   Date - primary (oldest to newest) and Time - secondary (oldest to newest)
#
# returns sorted data if all OK
# else returns empty data
#
def check_n_sort(indata,iDate,iTime,iLat,iLong):

    errorData = []

    if checkYMDformat(indata,iDate):
        return errorData
        
    if checkHMSformat(indata,iTime):
        return errorData

    if checkLat(indata,iLat):
        return errorData

    if checkLong(indata,iLong):
        return errorData

    indata1 = sortdata2(indata,iDate,iTime)

    indata2 = filter_date(indata1,iDate)

    nfilter = len(indata1)-len(indata2)

    if nfilter == 0:
        print '   >>> Note: No Rows removed due to Date Range Filter'
        print ' '
    else:
        print '   >>>'
        print '   >>> Note: Date Range Filter - Rows Removed (%i) ' % nfilter
        print '   >>>'
        
    if checkOrder(indata2,iDate,iTime):
        return errorData

    return indata2


def checkHeader(expected,actual,verbose):
    # compare the expected header to the actual header
    # if expected contains a "skip" then actual can be anything
    # length of actual must be >= length of expected
    #
    # returns error_flag
    #         False header OK
    #         True  some sort of mismatch
    #
    error_flag  = False
    actual2     = actual.split(',')
    
    if len(actual2) < len(expected):
        if verbose:
            print '>>>'
            print '>>>>>> ERROR Actual Header too short'
            print '>>>'
            print '>>>>>> Expected: len(%i) %s' % (len(expected),', '.join(expected))
            print '>>>>>> Actual  : len(%i) %s' % (len(actual2) ,', '.join(actual2))
            print '>>>'
        error_flag = True
        return error_flag

    for i in range(len(expected)):
        if expected[i] != 'skip':
            tstr = actual2[i].strip(' ')
            tstr = tstr.strip('\n')
            if expected[i] != tstr:
                        if verbose:
                            print '>>>'
                            print '>>>>>> ERROR Actual Header does not match Expected Header at item[%i]' % i
                            print '>>>'
                            print '>>>>>> Expected: len(%i) %s' % (len(expected),', '.join(expected))
                            print '>>>>>> Actual  : len(%i) %s' % (len(actual2) ,', '.join(actual2))
                            print '>>>'
                        error_flag = True
                        return error_flag

    return error_flag
        
def genTelonicsLocation(ifn,rowdata):
    #
    # Generate the _location file for Telonics
    #
    print '>>> Telonics Location Generation'
    
    ldata = len(rowdata)
    ofn = ifn[:-4] + '_location.csv'

    # Calculate the Wolf_ID and Pack_ID from the filename
    # VendorID_WolfID_PackID_etc
    #    WolfID is assumed to be delimited by '_' (usually 3 characters)
    #    PackID is assumed to be delimited by '_' (usually 2 characters)

    WolfID = 'Wnn'
    PackID = 'Px'

    split_ofn = ofn.split('_')
    if len(split_ofn) > 1:
        WolfID = split_ofn[1]
    if len(split_ofn) > 2:
        PackID = split_ofn[2]
    
    print '>>>          File "%s"'      % ofn
    print '>>>          Header + %i rows' % ldata
    print '>>>          WolfID = %s' % WolfID
    print '>>>          PackID = %s' % PackID

    # Telonics Format plus converted and calculated fields mapped to _location (LO) offset
    #
    # Fields:
    # Location File                        Input File
    # [00] Fix_#
    # [01] WolfID (also called CollarID)
    # [02] PackID                           
    # [03] Year                             [33] Local Date
    # [04] Month                            [33] Local Date
    # [05] Day                              [33] Local Date
    # [06] Date (reformatted GPS Fix Time)  [33] Local Date
    # [07] Hour                             [34] Local Time
    # [08] Minute                           [34] Local Time
    # [09] Second                           [34] Local Time
    # [10] Time                             [34] Local Time
    # [11] Latitude                         [07] GPS Latitude
    # [12] Longitude                        [08] GPS Longitude
    #      TempC                            [32] Mean Temp (Celcius) (Calculated)
    #      gmtDate                          [28] Date (YYYY-MM-DD)
    #      gmtTime                          [29] Time (HH:MM:SS)
    #                                       [00] Acquisition Time 
    #                                       [01] Acquisition Start Time
    #                                       [02] Iridium CEP Radius
    #                                       [03] Iridium Latitude
    #                                       [04] Iridium Longitude
    #                                       [05] GPS Fix Time            
    #                                       [09] GPS UTM Zone  
    # [13] Northing                         [10] GPS UTM Northing
    # [14] Easting                          [11] GPS UTM Easting
    # [15] GPS Fix Attempt                  [06] GPS Fix Attempt 
    # [16] GPS Altitude                     [12] GPS Altitude
    # [17] GPS Horizontal Error             [13] GPS Horizontal Error
    # [18] GPS Horizontal Dilution          [14] GPS Horizontal Dilution
    # [19] GPS Satellite Bitmap             [15] GPS Satellite Bitmap
    # [20] GPS Satellite Count              [16] GPS Satellite Count
    # [21] GPS Navigation Time              [17] GPS Navigation Time
    #                                       [18] Activity Count
    #                                       [19] Temperature
    #                                       [20] Satellite Uplink
    # [22] Receive Time                     [21] Receive Time
    # [23] Repetition Count                 [22] Repetition Count
    # [24] Low Voltage                      [23] Low Voltage
    # [25] Mortality                        [24] Mortality
    #                                       [25] Iridium Command
    #                                       [26] Predeployment Data
    # [26] Total Activity                   [30] Total Activity
    # [27] Mean Temp (degrees F)            [31] Mean Temp (Farenheight)
    # [28] Error                            [27] Error


    csvfile = open(ofn,'w')
    csvfile.write('''"FixNo","WolfID","PackID","Year","Month","Day","Date","Hour","Minute","Second","Time","Latitude","Longitude","TempC","gmtDate","gmtTime","Northing","Easting","GPS_Fix_Attempt","GPS_Altitude","GPS_Hor_Error","GPS_Hor_Dilution","GPS_Sat_Bitmap","GPS_Sat_Count","GPS_Nav_Time","Receive_Time","Repetition_Count","Low_Voltage","Mortality","Total_Activity","Mean_Temp_F","ERROR"\n''')
    #                  -        -         -       -      -      -      33                             34       7           8        32       28         29        10         11        6                 12             13              14                  15               16             17                21                 22         23             24            30               31        27
    fixNo = 0
    empty_str  = " "
    for row in rowdata:
        fixNo   = fixNo + 1
        fixStr  = str(fixNo)
        
        dateYMD = row[33]
        dateY   = dateYMD[:4]
        dateM   = dateYMD[5:7]
        dateD   = dateYMD[8:10]
        dateMDY = dateM + '/' + dateD + '/' + dateY

        timeHMS = row[34]
        timeH   = timeHMS[0:2]
        timeM   = timeHMS[3:5]
        timeS   = timeHMS[6:8]

        # Output each row in _location format

        csvfile.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' % \
                      (fixStr, WolfID, PackID, dateY, dateM, dateD, dateYMD, timeH, timeM, timeS, timeHMS,\
                       row[7], row[8], row[32], row[28], row[29], row[10],\
                       row[11], row[6],row[12],row[13], row[14], row[15], row[16], row[17],row[21], row[22], row[23], row[24],\
                       row[30], row[31], row[22]))
    csvfile.close()
    
    return

    return

def genTelonicsAnalysis(ifn,rowdata):
    #
    # Generate the _analysis file for Telonics
    #
    print '>>> Telonics Analysis Generation'
    
    ldata = len(rowdata)
    ofn = ifn[:-4] + '_analysis.csv'

    # Calculate the Wolf_ID and Pack_ID from the filename
    # VendorID_WolfID_PackID_etc
    #    WolfID is assumed to be delimited by '_' (usually 3 characters)
    #    PackID is assumed to be delimited by '_' (usually 2 characters)

    WolfID = 'Wnn'
    PackID = 'Px'

    split_ofn = ofn.split('_')
    if len(split_ofn) > 1:
        WolfID = split_ofn[1]
    if len(split_ofn) > 2:
        PackID = split_ofn[2]
    
    print '>>>          File "%s"'      % ofn
    print '>>>          Header + %i rows' % ldata
    print '>>>          WolfID = %s' % WolfID
    print '>>>          PackID = %s' % PackID

    # Format of _analysis file
    # [00] FIX_#
    # [01] CASE
    # [02] GMT_DATE  (as MM/DD/YYYY)
    # [03] GMT_TIME
    # [04] LMT_DATE  (as MM/DD/YYYY)
    # [05] LMT_TIME
    # [06] PackID
    # [07] WolfID
    # [08] N9
    # [09] LATITUDE
    # [10] LONGITUDE
    # [11] HEIGHT (not used)
    # [12] to [20] N13,N14,N15,N16,N17,N18,N19,N20,N21


    csvfile = open(ofn,'w')
    csvfile.write('''"FIX_#","CASE","GMT_DATE","GMT_TIME","LMT_DATE","LMT_TIME","PackID","WolfID","N9","LATITUDE","LONGITUDE","HEIGHT","N13","N14","N15","N16","N17","N18","N19","N20","N21"\n''')
    #                   1      2        3          4          5          6          7        8     9       10         11         12      13    14    15    16    17   18     19    20    21

    fixNo = 0
    empty_str  = " "
    rowsll0 = 0
    for row in rowdata:
        fixNo   = fixNo + 1
        dateY   = row[33][:4]
        dateM   = row[33][5:7]
        dateD   = row[33][8:10]
        dateMDY = dateM + '/' + dateD + '/' + dateY

        gdateY   = row[28][:4]
        gdateM   = row[28][5:7]
        gdateD   = row[28][8:10]
        gdateMDY = gdateM + '/' + gdateD + '/' + gdateY

        latitude    = row[7]
        longitude   = row[8]
        
        #
        # skip rows if Latitude or Longitude are 0
        #
        if (float(latitude)== 0) or (float(longitude)==0):
            rowsll0 = rowsll0 + 1
            continue

        # Output each row in _analysis format
        csvfile.write('%i,%i,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' % \
                      (fixNo, 1, gdateMDY, row[29], dateMDY, row[34],\
                       PackID, WolfID,empty_str, latitude, longitude, empty_str,\
                       empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str))

    csvfile.close()
 
    if rowsll0 != 0:
        print '>>>>>>'
        print '>>>>>>      WARNING Suppressed (%i) Rows with Lat/Long == 0' % rowsll0
        print '>>>>>>'
   
    return

def processTelonics(fname):
    
    print '   >>> Telonics Format'

    # Clear rowdata - contains all of the fixes which are good plus any calcuated fields
    rowdata = []

    # open file, read all lines and trim off the first one (column headers)
    # Old way linesa = open(fname).readlines()
    try:
        with open(fname,'rU') as f:
            linesa = list(f)
    except IOError:
        print 'Error opening/reading "%s"...' % fname
        return

    nlinesa = len(linesa)
    print '   >>> Total Lines     = %i ' % nlinesa

    # Split files into
    #   Junk            (j)
    #   Header          (h)
    #   Data            (d)
    #   Succeeded Fixes (f)
    
    linesj  = linesa[0:23]
    linesh  = linesa[23:24]
    linesd  = linesa[24:]
    linesf  = []

    expected = ('Acquisition Time','Acquisition Start Time','Iridium CEP Radius','Iridium Latitude','Iridium Longitude','GPS Fix Time',\
                'GPS Fix Attempt','GPS Latitude','GPS Longitude','GPS UTM Zone','GPS UTM Northing','GPS UTM Easting','GPS Altitude',\
                'GPS Horizontal Error','GPS Horizontal Dilution','GPS Satellite Bitmap','GPS Satellite Count','GPS Navigation Time','Activity Count',\
                'Temperature','Satellite Uplink','Receive Time','Repetition Count','Low Voltage','Mortality','Iridium Command','Predeployment Data','Error')
    # returns true if there is a header error
    tstru       = linesh[0]
    tstr        = filter(onlyascii, tstru)
    if checkHeader(expected,tstr,True):
        return rowdata

    if debug_flag:
        print '>>> Header = %s ' % linesh
    
    if debug_flag:
        for line in linesd:
            print line

    # The Teleonics data has both an activity column and a temperature (F) column
    # These values may be set in the interim fixes leading up to a succeeded fix
    # sum the activity values (NA if there are none)
    # average the temperature values (NA if there are none)

    # extract the lines where the fixes succeeded or was [Resolved] QFP
    for line in linesd:
        if ('Succeeded' in line) or ('QFP' in line):
            linesf.append(line)

    nlinesf = len(linesf)
    print '   >>> Suceeded or [Resolved] QFP Fixes = %i ' % nlinesf
    
    # Parse All Fixes as CSV
    csvinput = iter(linesd)
    csv1 = csv.reader(csvinput,delimiter=',')

    # counters
    #   all rows
    #   total succeeded fix rows
    #   rows with activity proceeding a succeeded fix row
    #   sum activity
    #   rows with temperature proceeding a succeeded fix row
    #   sum temperature
    count  = 0
    counts = 0
    counta = 0
    suma   = 0
    countt = 0
    sumt   = 0

    # Telonics Format plus converted and calculated fields 
    #
    # Fields:
    # [00] Acquisition Time
    # [01] Acquisition Start Time
    # [02] Iridium CEP Radius
    # [03] Iridium Latitude
    # [04] Iridium Longitude
    # [05] GPS Fix Time
    # [06] GPS Fix Attempt
    # [07] GPS Latitude
    # [08] GPS Longitude
    # [09] GPS UTM Zone
    # [10] GPS UTM Northing
    # [11] GPS UTM Easting
    # [12] GPS Altitude
    # [13] GPS Horizontal Error
    # [14] GPS Horizontal Dilution
    # [15] GPS Satellite Bitmap
    # [16] GPS Satellite Count
    # [17] GPS Navigation Time
    # [18] Activity Count
    # [19] Temperature
    # [20] Satellite Uplink
    # [21] Receive Time,
    # [22] epetition Count
    # [23] Low Voltage
    # [24] Mortality
    # [25] Iridium Command
    # [26] Predeployment Data
    # [27] Error
    #
    # Converted
    # [28] GMT Date (YYYY-MM-DD)
    # [29] GMT Time (HH:MM:SS)
    #
    # Calculated
    # [30] Total Activity
    # [31] Mean Temp (Farenheight)
    # [32] Mean Temp (Celcius)
    #
    # [33] Local Date
    # [34] Local Time
    
    for row in csv1:
        lrow = len(row)
        count = count + 1
        if debug_flag: print '*** Row %i Length %i ' % (count,lrow)
        # check activity
        if len(row[18]) != 0:
            counta = counta + 1
            suma = suma + int(row[18])
        # check temperature
        if len(row[19]) != 0:
            countt = countt + 1
            sumt = sumt + int(row[19])
        # check if Succeeded row or 
        if ('Succeeded' in row) or ('QFP' in row):
            counts = counts + 1
            # convert total activity to string
            if counta == 0:
                activity_str = "NA"
            else:
                activity_str = str(suma)
            # convert total activity to string
            if countt == 0:
                farT_str = "NA"
                celT_str = "NA"
            else:
                farT = float(sumt)/float(countt)
                farT_int = int(round(farT))
                farT_str = str(farT_int)
                celT = (farT-32.0)/(9.0/5.0)
                celT_int = int(round(celT))
                celT_str = str(celT_int)
                
            # check length of row against length of expected headers
            # if row is shorter this is an error so return null data
            # if row is greater then trim (just noise)

            lexpected = len(expected)

            if len(row) < lexpected:
                rowdata = []
                return rowdata

            # build new matrix of all data including new strings appended to the end of each row
            row1 = row[0:lexpected]

            gmtDate = fixDate(row[5])
            gmtTime = fixTime(row[5])

            localDate = adjustDateStr(gmtDate,gmtTime,timedeltaM6H)
            localTime = adjustTimeStr(gmtDate,gmtTime,timedeltaM6H)

            row1.append(gmtDate)
            row1.append(gmtTime)
            row1.append(activity_str)
            row1.append(farT_str)
            row1.append(celT_str)
            row1.append(localDate)
            row1.append(localTime)
            rowdata.append(row1)

            # reset activity and temperature stats
            counta = 0
            suma   = 0
            countt = 0
            sumt   = 0
            
    if debug_flag:
        nrows = len(rowdata)
        print '*** Total Rows = %i' % nrows
        print '*** First Row'
        frow  = rowdata[0]
        lfrow = len(frow)
        for i in range(lfrow):
            print '   Item %i "%s" ' % (i,frow[i])

    return rowdata

def genSirtrackLocation(ifn,rowdata):
    #
    # Generate the _location file for Sirtrack
    #
    print '>>> Sirtrack Location Generation'
    
    ldata = len(rowdata)
    ofn = ifn[:-4] + '_location.csv'

    # Calculate the Wolf_ID and Pack_ID from the filename
    # VendorID_WolfID_PackID_etc
    #    WolfID is assumed to be delimited by '_' (usually 3 characters)
    #    PackID is assumed to be delimited by '_' (usually 2 characters)

    WolfID = 'Wnn'
    PackID = 'Px'

    split_ofn = ofn.split('_')
    if len(split_ofn) > 1:
        WolfID = split_ofn[1]
    if len(split_ofn) > 2:
        PackID = split_ofn[2]
    
    print '>>>          File "%s"'      % ofn
    print '>>>          Header + %i rows' % ldata
    print '>>>          WolfID = %s' % WolfID
    print '>>>          PackID = %s' % PackID

    # Sirtrack Format
    #
    # Fields:
    # [00] Tag_ID
    # [01] UTC_Date (YYYY-MM-DD) but may be just M and/or D
    # [02] UTC_Time (HH:MM:SS)
    # [03] Latitude
    # [04] Longitude
    # [05] hDOP
    # [06] CNR
    # [07] Sats
    # [08] TimeOn
    # [09] MinVolt
    #
    # Any extra input columns will be truncated
    #
    # Calculated
    # [10] DateU (YYYY-MM-DD)     UTC   Date
    # [11] TimeU (HH:MM:SS)       UTC   Time
    # [12] DateL (YYYY-MM-DD)     Local Date
    # [13] TimeL (HH:MM:SS)       Local Time
    #
    # Sirtrack Format
    #
    # Fields:
    # Location File                        Input File
    # [00] Fix_#
    # [01] WolfID (also called CollarID)
    # [02] PackID
    # [03] Year								[12] DateL
    # [04] Month							[12] DateL
    # [05] Day								[12] DateL
    # [06] Date (coverted to Local Time)    [12] DateL (YYYY-MM-DD)
    # [07] Hour								[13] TimeL
    # [08] Minute							[13] TimeL
    # [09] Second							[13] TimeL
    # [10] Time                             [13] Time (HH:MM:SS)
    # [11] Latitude                         [03] GPS Latitude
    # [12] Longitude                        [04] GPS Longitude
    #      TempC (always NA)
    # [13] Tag_ID                           [00] Tag_ID
    # [14] hDOP                             [05] hDOP
    # [15] CNR                              [06] CNR
    # [16] Sats                             [07] Sats
    # [17] TimeOn                           [08] TimeOn
    # [18] MinVolt                          [09] MinVolt

    csvfile = open(ofn,'w')
    csvfile.write('''"FixNo","WolfID","PackID","Year","Month","Day","Date","Hour","Minute","Second","Time","Latitude","Longitude","TempC","gmtDate","gmtTime","Tag_ID","hDOP","CNR","Stats","TimeOn","MinVolt"\n''')

    fixNo       = 0
    empty_str   = " "
    tempC       = "NA"
    date_idx    = 12
    time_idx    = 13
    for row in rowdata:
        fixNo   = fixNo + 1
        fixStr  = str(fixNo)
        
        dateYMD = row[date_idx]
        dateY   = dateYMD[:4]
        dateM   = dateYMD[5:7]
        dateD   = dateYMD[8:10]
        dateMDY = dateM + '/' + dateD + '/' + dateY

        timeHMS     = row[time_idx]
        timeH       = timeHMS[0:2]
        timeM       = timeHMS[3:5]
        timeS       = timeHMS[6:8]

        latitude    = row[3]
        longitude   = row[4]

        dateGMT     = row[10]
        timeGMT     = row[11]

        tempC       = row[14]

        # Output each row in _location format

        csvfile.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' % \
                      (fixStr, WolfID, PackID, dateY, dateM, dateD, dateYMD, timeH, timeM, timeS, timeHMS,\
                       latitude, longitude,tempC,\
                       dateGMT,timeGMT,row[0], row[5], row[6], row[7], row[8], row[9]))

    csvfile.close()

    return

def genSirtrackAnalysis(ifn,rowdata):
    #
    # Generate the _analysis file for Telonics
    #
    print '>>> Sirtrack Analysis Generation'
    
    ldata = len(rowdata)
    ofn = ifn[:-4] + '_analysis.csv'

    # Calculate the Wolf_ID and Pack_ID from the filename
    # VendorID_WolfID_PackID_etc
    #    WolfID is assumed to be delimited by '_' (usually 3 characters)
    #    PackID is assumed to be delimited by '_' (usually 2 characters)

    WolfID = 'Wnn'
    PackID = 'Px'

    split_ofn = ofn.split('_')
    if len(split_ofn) > 1:
        WolfID = split_ofn[1]
    if len(split_ofn) > 2:
        PackID = split_ofn[2]
    
    print '>>>          File "%s"'      % ofn
    print '>>>          Header + %i rows' % ldata
    print '>>>          WolfID = %s' % WolfID
    print '>>>          PackID = %s' % PackID

    # Format of _analysis file
    # [00] FIX_#
    # [01] CASE
    # [02] GMT_DATE
    # [03] GMT_TIME
    # [04] Local DATE  (as MM/DD/YYYY)
    # [05] Local TIME
    # [06] PackID
    # [07] WolfID
    # [08] N9
    # [09] LATITUDE
    # [10] LONGITUDE
    # [11] HEIGHT (not used)
    # [12] to [20] N13,N14,N15,N16,N17,N18,N19,N20,N21


    csvfile = open(ofn,'w')
    csvfile.write('''"FIX_#","CASE","GMT_DATE","GMT_TIME","LMT_DATE","LMT_TIME","PackID","WolfID","N9","LATITUDE","LONGITUDE","HEIGHT","N13","N14","N15","N16","N17","N18","N19","N20","N21"\n''')

    fixNo = 0
    empty_str  = " "
    rowsll0 = 0
    # index of local date/time
    date_idx    = 12
    time_idx    = 13
    # index of gmt date/time
    gmtdate_idx = 10
    gmttime_idx = 11
    for row in rowdata:
        fixNo   = fixNo + 1
        
        dateYMD = row[date_idx]
        dateY   = dateYMD[:4]
        dateM   = dateYMD[5:7]
        dateD   = dateYMD[8:10]
        dateMDY = dateM + '/' + dateD + '/' + dateY

        timeHMS     = row[time_idx]
        timeH       = timeHMS[0:2]
        timeM       = timeHMS[3:5]
        timeS       = timeHMS[6:8]

        gmtYMD = row[gmtdate_idx]
        gmtY   = gmtYMD[:4]
        gmtM   = gmtYMD[5:7]
        gmtD   = gmtYMD[8:10]
        gmtMDY = gmtM + '/' + gmtD + '/' + gmtY

        gmtHMS     = row[gmttime_idx]
        gmtH       = gmtHMS[0:2]
        gmtMin     = gmtHMS[3:5]
        gmtS       = gmtHMS[6:8]
        
        latitude    = row[3]
        longitude   = row[4]
        
        #
        # skip rows if Latitude or Longitude are 0
        #
        try:
            flat = float(latitude)
            flong= float(longitude)
        except ValueError:
            flat  = 0.0
            flong = 0.0
            
        if (flat == 0) or (flong== 0):
            rowsll0 = rowsll0 + 1
            continue 

        # Output each row in _analysis format
        csvfile.write('%i,%i,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' % \
                      (fixNo, 1, gmtMDY, gmtHMS, dateMDY, timeHMS, \
                       PackID, WolfID,empty_str, latitude, longitude, empty_str,\
                       empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str))

    csvfile.close()

    if rowsll0 != 0:
        print '>>>>>>'
        print '>>>>>>      WARNING Suppressed (%i) Rows with Lat/Long == 0' % rowsll0
        print '>>>>>>'
        
    return

def processSirtrack(fname):
    
    print '   >>> Sirtrack Format'

    # Clear rowdata - contains all of the fixes which are good plus any calculated fields
    rowdata = []

    # open file, read all lines and trim off the first one (column headers)
    try:
    # Old way linesa = open(fname).readlines()
        with open(fname,'rU') as f:
            linesa = list(f)
    except IOError:
        print 'Error opening/reading "%s"...' % fname
        return

    nlinesa = len(linesa)
    print '   >>> Total Lines     = %i ' % nlinesa

    # Split files into
    #   Junk            (j)
    #   Header          (h)
    #   Data            (d)
    #   Succeeded Fixes (f)
    
    linesj  = []
    linesh  = linesa[0:1]
    linesd  = linesa[1:]
    linesf  = []

    expected1 = ('Tag_ID',           'UTC_Date','UTC_Time','Latitude','Longitude','hDOP','CNR', 'Sats',   'TimeOn(s)',           'MinVolt')
    #             0                   1          2          3          4           5      6      7         8                      9
    expected2 = ('Tag_ID','Tag_Name','UTC_Date','UTC_Time','Latitude','Longitude','CNR', 'HDOP','Sat Num','Time On',  'Temp (C)','Min Volt')
    #             0        1          2          3          4          5            6     7       8         9          10         11
    # mapping 2 ==> 1
    # 0  2  3  4  5  7  6  8  9  11

# returns true if there is a header error
    tstru       = linesh[0]
    tstr        = filter(onlyascii, tstru)
    formatType = 0
    
    if not checkHeader(expected1,tstr,False):
        print '   >>> Format Type 1'
        formatType = 1

    if formatType == 0:
        if not checkHeader(expected2,tstr,True):
            print '   >>> Format Type 2'
            formatType = 2

    # Could not detect format
    if formatType == 0:
        return rowdata
    
    if debug_flag:
        print '>>> Header = %s ' % linesh
    
    if debug_flag:
        for line in linesd:
            print line

    nlinesf = len(linesd)
    print '   >>> Succeeded Fixes = %i ' % nlinesf
    
    # Parse All Fixes as CSV
    csvinput = iter(linesd)
    csv1 = csv.reader(csvinput,delimiter=',')

    # counters
    #   all rows
    #   total succeeded fix rows

    count  = 0

    # Sirtrack Format
    #
    # Fields:
    # [00] Tag_ID
    # [01] UTC_Date (YYYY-MM-DD) but may be just M and/or D
    # [02] UTC_Time (HH:MM:SS) but may be just H or M or S
    # [03] Latitude
    # [04] Longitude
    # [05] hDOP
    # [06] CNR
    # [07] Sats
    # [08] TimeOn
    # [09] MinVolt
    # 
    # Calculated
    # [10] DateU (YYYY-MM-DD)     UTC   Date
    # [11] TimeU (HH:MM:SS)       UTC   Time
	# [12] DateL (YYYY-MM-DD)     Local Date
	# [13] TimeL (HH:MM:SS)       Local Time
	# [14] TempC either NA or TempC from Format 2
    
    for row in csv1:
        lrow = len(row)
        count = count + 1
        if debug_flag: print '*** Row %i Length %i ' % (count,lrow)

        row1 = []
        blank       = ''
        notavail    = 'NA'

        # convert format 2 to format 1
        # 0  2  3  4  5  7  6  8  9  11
        if formatType == 2:
            row1.append(row[0])
            row1.append(row[2])
            row1.append(row[3])
            row1.append(row[4])
            row1.append(row[5])
            row1.append(row[7])
            row1.append(row[6])
            row1.append(row[8])
            row1.append(row[9])
            row1.append(row[11])

        lexpected = len(expected1)

        if formatType == 1:
           row1 = row[0:lexpected]

        # check length of row against length of expected headers
        # if row is shorter this is an error so return null data
        # if row is greater then trim (just noise)

        if len(row) < lexpected:
            rowdata = []
            return rowdata

        # Fix up UTC_Date
        datestr = row1[1].split('-')
        dateY = datestr[0]
        dateM = datestr[1]
        if len(dateM)<2:
            dateM = '0' + dateM
        dateD = datestr[2]
        if len(dateD)<2:
            dateD = '0' + dateD       
        dateYMD = dateY + '-' + dateM + '-' + dateD

        # Fix up Time
        timestr = row1[2].split(':')
        timeH   = timestr[0]
        timeM   = timestr[1]
        timeS   = timestr[2]
        if len(timeH)<2:
            timeH = '0' + timeH
        if len(timeM)<2:
            timeM = '0' + timeM
        if len(timeS)<2:
            timeS = '0' + timeS
        timeHMS = timeH + ':' + timeM + ':' + timeS

        row1.append(dateYMD)
        row1.append(timeHMS)

        # now calculate local date-time from UTC(GMT) date-time
        localYMD = adjustDate(int(dateY),int(dateM),int(dateD),int(timeH),int(timeM),int(timeS),timedeltaM6H)
        localHMS = adjustTime(int(dateY),int(dateM),int(dateD),int(timeH),int(timeM),int(timeS),timedeltaM6H)
        row1.append(localYMD)
        row1.append(localHMS)

        tempC = " "
        if formatType == 2:
            tempC = row[10]
        row1.append(tempC)
        
        rowdata.append(row1)
            
    if debug_flag:
        nrows = len(rowdata)
        print '*** Total Rows = %i' % nrows
        print '*** First Row'
        frow  = rowdata[0]
        lfrow = len(frow)
        for i in range(lfrow):
            print '   Item %i "%s" ' % (i,frow[i])

    return rowdata

def genATSLocation(ifn,rowdata):
    #
    # Generate the _location file for ATS
    #
    print '>>> ATS Location Generation'
    
    ldata = len(rowdata)
    ofn = ifn[:-4] + '_location.csv'

    # Calculate the Wolf_ID and Pack_ID from the filename
    # VendorID_WolfID_PackID_etc
    #    WolfID is assumed to be delimited by '_' (usually 3 characters)
    #    PackID is assumed to be delimited by '_' (usually 2 characters)

    WolfID = 'Wnn'
    PackID = 'Px'

    split_ofn = ofn.split('_')
    if len(split_ofn) > 1:
        WolfID = split_ofn[1]
    if len(split_ofn) > 2:
        PackID = split_ofn[2]
    
    print '>>>          File "%s"'      % ofn
    print '>>>          Header + %i rows' % ldata
    print '>>>          WolfID = %s' % WolfID
    print '>>>          PackID = %s' % PackID

    # ATS Format
    # CollarSerialNumber	Year	Julianday	Hour	Minute	Activity	Temperature	Latitude	Longitude	HDOP	NumSats	FixTime	2D/3D	Date
    #
    # Fields:
    # [00] CollarSerialNumber
    # [01] Year (YY)
    # [02] Julianday 
    # [03] Hour
    # [04] Minute
    # [05] Activity
    # [06] Temperature
    # [07] Latitude
    # [08] Longitude
    # [09] HDOP
    # [10] NumSats
    # [11] FixTime
    # [12] 2D/3D
    # [13] Date (DD/MM/YYYY)
    #
    # Calculated
    # [14] Date1 (YYYY-MM-DD)
    # [15] Date2 (MM/DD/YYYY)
    # [16] Time  (HH:MM:00)

    # Sirtrack Format
    #
    # Fields:
    # Location File                        Input File
    # [00] Fix_#
    # [01] WolfID (also called CollarID)
    # [02] PackID
    # [03] Year
    # [04] Month
    # [05] Day
    # [06] Date (reformatted GPS Fix Time)  [14] Date (YYYY-MM-DD)
    # [07] Hour
    # [08] Minute
    # [09] Second
    # [10] Time                             [16] Time (HH:MM:SS)
    # [11] Latitude                         [07] GPS Latitude
    # [12] Longitude                        [08] GPS Longitude
    #      TempC (sometimes NA)
    # [13] CSN                              [00] CollarSerialNumber
    # [14] hDOP                             [09] hDOP
    # [15] NumSats                          [10] NumSats
    # [16] FixTime                          [11] FixTime
    # [17] 2D3D                             [12] 2D/3D
 

    csvfile = open(ofn,'w')
    csvfile.write('''"FixNo","WolfID","PackID","Year","Month","Day","Date","Hour","Minute","Second","Time","Latitude","Longitude","TempC","gmtDate","gmtTime","CSN","hDOP","NUMSats","FixTime","2D3D"\n''')

    fixNo       = 0
    empty_str   = " "
    tempC       = "NA"
    for row in rowdata:
        fixNo   = fixNo + 1
        fixStr  = str(fixNo)

        dateYMD = row[14]
        dateY   = dateYMD[:4]
        dateM   = dateYMD[5:7]
        dateD   = dateYMD[8:10]
        dateMDY = dateM + '/' + dateD + '/' + dateY

        timeHMS     = row[16]
        # if no leading zero add one
        if len(timeHMS) < 8:
            timeHMS = '0' + timeHMS
        timeH       = timeHMS[0:2]
        timeM       = timeHMS[3:5]
        timeS       = timeHMS[6:8]
        
        latitude    = row[7]
        longitude   = row[8]

        # calculate gmt 
        gmtDate = adjustDateStr(dateYMD,timeHMS,timedelta6H)
        gmtTime = adjustTimeStr(dateYMD,timeHMS,timedelta6H)

        # Output each row in _location format

        csvfile.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' % \
                      (fixStr, WolfID, PackID, dateY, dateM, dateD, dateYMD, timeH, timeM, timeS, timeHMS,\
                       latitude, longitude, tempC,\
                       gmtDate,gmtTime,row[0], row[9], row[10], row[11], row[12]))

    csvfile.close()

    return

def genATSAnalysis(ifn,rowdata):
    #
    # Generate the _analysis file for ATS
    #
    print '>>> ATS Analysis Generation'
    
    ldata = len(rowdata)
    ofn = ifn[:-4] + '_analysis.csv'

    # Calculate the Wolf_ID and Pack_ID from the filename
    # VendorID_WolfID_PackID_etc
    #    WolfID is assumed to be delimited by '_' (usually 3 characters)
    #    PackID is assumed to be delimited by '_' (usually 2 characters)

    WolfID = 'Wnn'
    PackID = 'Px'

    split_ofn = ofn.split('_')
    if len(split_ofn) > 1:
        WolfID = split_ofn[1]
    if len(split_ofn) > 2:
        PackID = split_ofn[2]
    
    print '>>>          File "%s"'      % ofn
    print '>>>          Header + %i rows' % ldata
    print '>>>          WolfID = %s' % WolfID
    print '>>>          PackID = %s' % PackID
    
    # ATS Format
    # CollarSerialNumber	Year	Julianday	Hour	Minute	Activity	Temperature	Latitude	Longitude	HDOP	NumSats	FixTime	2D/3D	Date
    #
    # Fields:
    # [00] CollarSerialNumber
    # [01] Year (YY)
    # [02] Julianday 
    # [03] Hour
    # [04] Minute
    # [05] Activity
    # [06] Temperature
    # [07] Latitude
    # [08] Longitude
    # [09] HDOP
    # [10] NumSats
    # [11] FixTime
    # [12] 2D/3D
    # [13] Date (DD/MM/YYYY)
    #
    # Calculated
    # [14] Date1 (YYYY-MM-DD)
    # [15] Date2 (MM/DD/YYYY)
    # [16] Time  (HH:MM:00)

    # Format of _analysis file
    # [00] FIX_#
    # [01] CASE
    # [02] GMT_DATE
    # [03] GMT_TIME
    # [04] LMT_DATE  (as MM/DD/YYYY)
    # [05] LMT_TIME
    # [06] PackID
    # [07] WolfID
    # [08] N9
    # [09] LATITUDE
    # [10] LONGITUDE
    # [11] HEIGHT (not used)
    # [12] to [20] N13,N14,N15,N16,N17,N18,N19,N20,N21


    csvfile = open(ofn,'w')
    csvfile.write('''"FIX_#","CASE","GMT_DATE","GMT_TIME","LMT_DATE","LMT_TIME","PackID","WolfID","N9","LATITUDE","LONGITUDE","HEIGHT","N13","N14","N15","N16","N17","N18","N19","N20","N21"\n''')

    fixNo = 0
    empty_str  = " "
    rowsll0 = 0
    for row in rowdata:
        fixNo   = fixNo + 1
        
        dateYMD = row[14]
        dateY   = dateYMD[:4]
        dateM   = dateYMD[5:7]
        dateD   = dateYMD[8:10]
        dateMDY = dateM + '/' + dateD + '/' + dateY

        timeHMS     = row[16]
        # if no leading zero add one
        if len(timeHMS) < 8:
            timeHMS = '0' + timeHMS
        timeH       = timeHMS[0:2]
        timeM       = timeHMS[3:5]
        timeS       = timeHMS[6:8]
        
        latitude    = row[7]
        longitude   = row[8]

        # calculate gmt 
        gmtDate = adjustDateStr(dateYMD,timeHMS,timedelta6H)
        gmtMDY  = convertMDY(gmtDate)
        gmtTime = adjustTimeStr(dateYMD,timeHMS,timedelta6H)
        
        #
        # skip rows if Latitude or Longitude are 0
        #
        if (float(latitude)== 0) or (float(longitude)==0):
            rowsll0 = rowsll0 + 1
            continue

        # Output each row in _analysis format
        csvfile.write('%i,%i,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' % \
                      (fixNo, 1, gmtMDY, gmtTime, dateMDY, timeHMS, \
                       PackID, WolfID,empty_str, latitude, longitude, empty_str,\
                       empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str))

    csvfile.close()
    
    if rowsll0 != 0:
        print '>>>>>>'
        print '>>>>>>      WARNING Suppressed (%i) Rows with Lat/Long == 0' % rowsll0
        print '>>>>>>'
        
    return

def processATS(fname):
    
    print '   >>> ATS Format'

    # Clear rowdata - contains all of the fixes which are good plus any calcuated fields
    rowdata = []

    # open file, read all lines and trim off the first one (column headers)
    try:
    # Old way linesa = open(fname).readlines()
        with open(fname,'rU') as f:
            linesa = list(f)
    except IOError:
        print 'Error opening/reading "%s"...' % fname
        return

    nlinesa = len(linesa)
    print '   >>> Total Lines     = %i ' % nlinesa

    # Split files into
    #   Junk            (j)
    #   Header          (h)
    #   Data            (d)
    #   Succeeded Fixes (f)
    
    linesj  = []
    linesh  = linesa[0:1]
    linesd  = linesa[1:]
    linesf  = []

    expected = ('CollarSerialNumber','Year','Julianday','Hour','Minute','Activity','Temperature','Latitude','Longitude','HDOP','NumSats','FixTime','2D/3D','Date')
    # returns true if there is a header error
    tstru       = linesh[0]
    tstr        = filter(onlyascii, tstru)
    if checkHeader(expected,tstr,True):
        return rowdata
    
    if debug_flag:
        print '>>> Header = %s ' % linesh
    
    if debug_flag:
        for line in linesd:
            print line

    nlinesf = len(linesd)
    print '   >>> Succeeded Fixes = %i ' % nlinesf
    
    # Parse All Fixes as CSV
    csvinput = iter(linesd)
    csv1 = csv.reader(csvinput,delimiter=',')

    # counters
    #   all rows
    #   total succeeded fix rows

    count  = 0

    # ATS Format
    # CollarSerialNumber	Year	Julianday	Hour	Minute	Activity	Temperature	Latitude	Longitude	HDOP	NumSats	FixTime	2D/3D	Date
    #
    # Fields:
    # [00] CollarSerialNumber
    # [01] Year (YY)
    # [02] Julianday 
    # [03] Hour
    # [04] Minute
    # [05] Activity
    # [06] Temperature
    # [07] Latitude
    # [08] Longitude
    # [09] HDOP
    # [10] NumSats
    # [11] FixTime
    # [12] 2D/3D
    # [13] Date  (YYYY-MM-DD)
    #
    # Calculated
    # [14] Date1 (YYYY-MM-DD)
    # [15] Date2 (MM/DD/YYYY)
    # [16] Time  (HH:MM:00)
    
    for row in csv1:
        lrow = len(row)
        count = count + 1
        
        if debug_flag: print '*** Row %i Length %i ' % (count,lrow)
        # check length of row against length of expected headers
        # if row is shorter this is an error so return null data
        # if row is greater then trim (just noise)

        lexpected = len(expected)

        if len(row) < lexpected:
            rowdata = []
            return rowdata

        # build new matrix of all data including new strings appended to the end of each row
        row1 = row[0:lexpected]
        
        dateYMD = row[13]
        dateY   = dateYMD[:4]
        dateM   = dateYMD[5:7]
        dateD   = dateYMD[8:10]
        dateMDY = dateM + '/' + dateD + '/' + dateY
        
        # create MM/DD/YYYY
        dateMDY = dateM + "/" + dateD + "/" + dateY

        timeH   = row[3]
        if len(timeH) < 2:
            timeH = "0" + timeH

        timeM   = row[4]
        if len(timeM) < 2:
            timeM = "0" + timeM

        timeS = "00"
        timeHMS = timeH + ":" + timeM + ":" + timeS

        row1.append(dateYMD)
        row1.append(dateMDY)
        row1.append(timeHMS)
        
        rowdata.append(row1)
            
    if debug_flag:
        nrows = len(rowdata)
        print '*** Total Rows = %i' % nrows
        print '*** First Row'
        frow  = rowdata[0]
        lfrow = len(frow)
        for i in range(lfrow):
            print '   Item %i "%s" ' % (i,frow[i])

    return rowdata


def genTeleviltLocation(ifn,rowdata):
    #
    # Generate the _location file for Televilt    #
    print '>>> Televilt Location Generation'
    
    ldata = len(rowdata)
    ofn = ifn[:-4] + '_location.csv'

    # Calculate the Wolf_ID and Pack_ID from the filename
    # VendorID_WolfID_PackID_etc
    #    WolfID is assumed to be delimited by '_' (usually 3 characters)
    #    PackID is assumed to be delimited by '_' (usually 2 characters)

    WolfID = 'Wnn'
    PackID = 'Px'

    split_ofn = ofn.split('_')
    if len(split_ofn) > 1:
        WolfID = split_ofn[1]
    if len(split_ofn) > 2:
        PackID = split_ofn[2]
    
    print '>>>          File "%s"'      % ofn
    print '>>>          Header + %i rows' % ldata
    print '>>>          WolfID = %s' % WolfID
    print '>>>          PackID = %s' % PackID

    # Televilt Format
    # Date,Time,Latitude,,,Longitude,,Temp C°,,Power,,TTF,Sats,X,Y,Info
    #
    # Fields:
    # [00] Date (M/DD/YYYY)
    # [01] Time (HH:MM)
    # [02] Latitude 
    # [03] <blank>
    # [04] <blank>
    # [05] Longitude
    # [06] <blank>
    # [07] Temperature (C) 
    # [08] <blank>
    # [09] Power
    # [10] <blank>
    # [11] TTF
    # [12] Sats
    # [13] X
    # [14] Y
    # [15] Info
    #
    # Calculated
    # [16] Date1 (YYYY-MM-DD)
    # [17] Time1 (HH:MM:00)

    #  Format
    #
    # Fields:
    # Location File                        Input File
    # [00] Fix_#
    # [01] WolfID (also called CollarID)
    # [02] PackID
    # [03] Year
    # [04] Month
    # [05] Day
    # [06] Date (reformatted GPS Fix Time)  [16] Date (YYYY-MM-DD)
    # [07] Hour
    # [08] Minute
    # [09] Second
    # [10] Time                             [17] Time (HH:MM:SS)
    # [11] Latitude                         [02] GPS Latitude
    # [12] Longitude                        [05] GPS Longitude            
    # [13] TempC                            [07] Temperature
    # [14] Power                            [09] Power
    # [15] TTF                              [11] TTF
    # [16] Sats                             [12] Sats
    # [17] X                                [13] X
    # [18] Y                                [14] Y
    # [19] Info                             [15] Info
 

    csvfile = open(ofn,'w')
    csvfile.write('''"FixNo","WolfID","PackID","Year","Month","Day","Date","Hour","Minute","Second","Time","Latitude","Longitude","TempC","gmtDate","gmtTime","Power","TTF","Sats","X","Y","Info"\n''')

    fixNo = 0
    empty_str  = " "
    rowsll0 = 0
    for row in rowdata:
        fixNo   = fixNo + 1
        fixStr  = str(fixNo)

        dateYMD = row[16]
        dateY   = dateYMD[:4]
        dateM   = dateYMD[5:7]
        dateD   = dateYMD[8:10]
        dateMDY = dateM + '/' + dateD + '/' + dateY

        timeHMS     = row[17]
        # if no leading zero add one
        if len(timeHMS) < 8:
            timeHMS = '0' + timeHMS
        timeH       = timeHMS[0:2]
        timeM       = timeHMS[3:5]
        timeS       = timeHMS[6:8]

        gmtDate = row[18]
        gmtTime = row[19]
        
        latitude    = row[2]
        longitude   = row[5]
        

        # Output each row in _location format

        csvfile.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' % \
                      (fixStr, WolfID, PackID, dateY, dateM, dateD, dateYMD, timeH, timeM, timeS, timeHMS,\
                       latitude, longitude,\
                       row[7], gmtDate, gmtTime, row[9], row[11], row[12], row[13], row[14], row[15]))

    csvfile.close()

    return

def genTeleviltAnalysis(ifn,rowdata):
    #
    # Generate the _analysis file for Televilt
    #
    print '>>> Televilt Analysis Generation'
    
    ldata = len(rowdata)
    ofn = ifn[:-4] + '_analysis.csv'

    # Calculate the Wolf_ID and Pack_ID from the filename
    # VendorID_WolfID_PackID_etc
    #    WolfID is assumed to be delimited by '_' (usually 3 characters)
    #    PackID is assumed to be delimited by '_' (usually 2 characters)

    WolfID = 'Wnn'
    PackID = 'Px'

    split_ofn = ofn.split('_')
    if len(split_ofn) > 1:
        WolfID = split_ofn[1]
    if len(split_ofn) > 2:
        PackID = split_ofn[2]
    
    print '>>>          File "%s"'      % ofn
    print '>>>          Header + %i rows' % ldata
    print '>>>          WolfID = %s' % WolfID
    print '>>>          PackID = %s' % PackID

    # Televilt Format
    # Date,Time,Latitude,,,Longitude,,Temp C°,,Power,,TTF,Sats,X,Y,Info
    #
    # Fields:
    # [00] Date (M/DD/YYYY)
    # [01] Time (HH:MM)
    # [02] Latitude 
    # [03] <blank>
    # [04] <blank>
    # [05] Longitude
    # [06] <blank>
    # [07] Temperature (C) 
    # [08] <blank>
    # [09] Power
    # [10] <blank>
    # [11] TTF
    # [12] Sats
    # [13] X
    # [14] Y
    # [15] Info
    #
    # Calculated
    # [16] Date Local (YYYY-MM-DD)
    # [17] Time Local (HH:MM:00)
    # [18] Date GMT   (YYYY-MM-DD)
    # [19] Time GMT   (HH:MM:00)

    # Format of _analysis file
    # [00] FIX_#
    # [01] CASE
    # [02] GMT_DATE
    # [03] GMT_TIME
    # [04] LMT_DATE  (as MM/DD/YYYY)
    # [05] LMT_TIME
    # [06] PackID
    # [07] WolfID
    # [08] N9
    # [09] LATITUDE
    # [10] LONGITUDE
    # [11] HEIGHT (not used)
    # [12] to [20] N13,N14,N15,N16,N17,N18,N19,N20,N21


    csvfile = open(ofn,'w')
    csvfile.write('''"FIX_#","CASE","GMT_DATE","GMT_TIME","LMT_DATE","LMT_TIME","PackID","WolfID","N9","LATITUDE","LONGITUDE","HEIGHT","N13","N14","N15","N16","N17","N18","N19","N20","N21"\n''')

    fixNo = 0
    empty_str  = " "
    rowsll0 = 0
    for row in rowdata:
        fixNo   = fixNo + 1
        
        dateYMD = row[16]
        dateY   = dateYMD[:4]
        dateM   = dateYMD[5:7]
        dateD   = dateYMD[8:10]
        dateMDY = dateM + '/' + dateD + '/' + dateY

        timeHMS     = row[17]
        # if no leading zero add one
        if len(timeHMS) < 8:
            timeHMS = '0' + timeHMS
        timeH       = timeHMS[0:2]
        timeM       = timeHMS[3:5]
        timeS       = timeHMS[6:8]

        gmtYMD  = row[18]
        gmtMDY  = convertMDY(gmtYMD)
        gmtTime = row[19]
        
        latitude    = row[2]
        longitude   = row[5]
        
        #
        # skip rows if Latitude or Longitude are 0
        #
        if (float(latitude)== 0) or (float(longitude)==0):
            rowsll0 = rowsll0 + 1
            continue

        # Output each row in _analysis format
        csvfile.write('%i,%i,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' % \
                      (fixNo, 1, gmtMDY, gmtTime, dateMDY, timeHMS, \
                       PackID, WolfID,empty_str, latitude, longitude, empty_str,\
                       empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str))

    csvfile.close()

    if rowsll0 != 0:
        print '>>>>>>'
        print '>>>>>>      WARNING Suppressed (%i) Rows with Lat/Long == 0' % rowsll0
        print '>>>>>>'

    return

def processTelevilt(fname):
    
    print '   >>> Televilt Format'

    if (televiltDSTpresent == 1):
        print '   >>> '
        print '   >>> Program Option: DST is Present'
        print '   >>> '
        if (televiltDSTremove == 1):
            print '   >>> '
            print '   >>> Program Option: DST to be Removed'
            print '   >>> '
        else:
            print '   >>> '
            print '   >>> Program Option: DST will NOT be Removed'
            print '   >>> '          
    else:
        print '   >>> '
        print '   >>> Program Option: DST is Not Present'
        print '   >>> '       

    # Clear rowdata - contains all of the fixes which are good plus any calculated fields
    rowdata = []

    # open file, read all lines and trim off the first one (column headers)
    try:
    # Old way linesa = open(fname).readlines()
        with open(fname,'rU') as f:
            linesa = list(f)
    except IOError:
        print 'Error opening/reading "%s"...' % fname
        return

    nlinesa = len(linesa)
    
    print '   >>> Total Lines     = %i ' % nlinesa


    # Split files into
    #   Junk            (j)
    #   Header          (h)
    #   Data            (d)
    #   Succeeded Fixes (f)
    
    linesj  = []
    linesh  = linesa[0:1]
    if debug_flag:
        print '>>> Header = %s ' % linesh
    linesd  = linesa[1:]
    linesf  = []

    # Televilt collars have three formats (1,2,3,4)
    #

    formatType  = 0
    expected1   = ('Date','Time','Latitude','skip','skip','Longitude','skip','skip','skip',   'Power','skip','TTF','Sats',              'X','Y','Info') 
    expected2   = ('Date','Time','Latitude','skip','skip','Longitude','skip','skip','skip',   'Power','skip','TTF','Sats','skip','skip','X','Y','Info')
    expected3   = ('Date','Time','Latitude','skip','skip','Longitude','skip',                 'Power','skip','TTF','Sats','skip'               ,'Info')
    #expected4   = ('Date','Time','Latitude','skip','skip','Longitude','skip','skip',          'Power','skip','TTF','Sats','skip',       'X','Y','Info','skip')
    expected4   = ('Date','Time','Latitude','skip','skip','Longitude','skip','skip', 'skip',  'Power','skip','TTF','Sats','skip',       'X','Y','Info')
    tstru       = linesh[0]
    tstr        = filter(onlyascii, tstru)
    
    if debug_flag:
        print 'expected1, expected2, expected3, expected4, tstru, tstr'
        print expected1
        print expected2
        print expected3
        print expected4
        print tstru
        print tstr
    
    # first check if it is format 2
    if not checkHeader(expected2,tstr,debug_flag):
        #  format 2 - convert data to format 1 later on
        formatType = 2
        print '   >>> Televilt Format 2 Detetcted'

    # check if it is format 3
    if formatType == 0:
        if not checkHeader(expected3,tstr,debug_flag):
            #  format 3 - convert data to format 1 later on
            formatType = 3
            print '   >>> Televilt Format 3 Detetcted'

    # check if it is format 4
    if formatType == 0:
        if not checkHeader(expected4,tstr,debug_flag):
            #  format 4 - convert data to format 1 later on
            formatType = 4
            print '   >>> Televilt Format 4 Detetcted'


    if formatType == 0:
        # return empty data if not format 1
        if checkHeader(expected1,tstr,True):
            return rowdata
        else:
            formatType = 1
            print '   >>> Televilt Format 1 Detetcted'

    if debug_flag:
        print '>>> Header = %s ' % linesh
    
    #if debug_flag:
    #   for line in linesd:
    #      print line

    nlinesf = len(linesd)
    print '   >>> Succeeded Fixes = %i ' % nlinesf
    
    # Parse All Fixes as CSV
    csvinput = iter(linesd)
    csv1 = csv.reader(csvinput,delimiter=',')

    # counters
    #   all rows
    #   total succeeded fix rows

    count  = 0

    # Televilt Format OLD
    # Date,Time,Latitude,,,Longitude,,         Power,,TTF,Sats,   , Info
    #
    # Fields:
    # [00] Date (M/DD/YYYY) or (MM/D/YYYY) or (M/D/YYYY)
    # [01] Time (HH:MM)
    # [02] Latitude 
    # [03] <blank>
    # [04] <blank>
    # [05] Longitude
    # [06] <blank>
    # [07] Power
    # [08] <blank>
    # [09] TTF
    # [10] Sats
    # [11] blank
    # [12] Info
    #
    # Televilt Format NEW
    # Date,Time,Latitude,,,Longitude,,Temp C°,,Power,,TTF,Sats,X,Y,Info
    # 
    # Fields:
    # [00] Date (M/DD/YYYY)
    # [01] Time (HH:MM)
    # [02] Latitude 
    # [03] <blank>
    # [04] <blank>
    # [05] Longitude
    # [06] <blank>
    # [07] Temperature (C) 
    # [08] <blank>
    # [09] Power
    # [10] <blank>
    # [11] TTF
    # [12] Sats
    # [13] X
    # [14] Y
    # [15] Info
    #
    # Calculated:
    # [16] Date Local (YYYY-MM-DD)
    # [17] Time Local (HH:MM:00)
    # [18] Date GMT   (YYYY-MM-DD)
    # [19] Time GMT   (HH:MM:00)
    
    for row in csv1:
        lrow = len(row)
        count = count + 1
        if debug_flag: print '*** Row %i Length %i ' % (count,lrow)

        row1 = []
        blank       = ''
        notavail    = 'NA'

        # convert format 2 to format 1
        if formatType == 2:
            row1.append(row[0])
            row1.append(row[1])
            row1.append(row[2])
            row1.append(row[3])
            row1.append(row[4])
            row1.append(row[5])
            row1.append(row[6])
            row1.append(row[7])
            row1.append(row[8])
            row1.append(row[9])
            row1.append(row[10])
            row1.append(row[11])
            row1.append(row[12])
            row1.append(row[15])
            row1.append(row[16])
            row1.append(row[17])

        # convert format 3 to format 1
        if formatType == 3:
            row1.append(row[0])
            row1.append(row[1])
            row1.append(row[2])
            row1.append(row[3])
            row1.append(row[4])
            row1.append(row[5])
            row1.append(row[6])
            row1.append(blank)
            row1.append(blank)
            row1.append(row[7])
            row1.append(row[8])
            row1.append(row[9])
            row1.append(row[10])
            row1.append(blank)
            row1.append(blank)
            row1.append(row[12])

        # convert format 4 to format 1
        if formatType == 4:
            row1.append(row[0])
            row1.append(row[1])
            row1.append(row[2])
            row1.append(row[3])
            row1.append(row[4])
            row1.append(row[5])
            row1.append(row[6])
            row1.append(row[7])
            row1.append(blank)
            row1.append(row[8])
            row1.append(row[9])
            row1.append(row[10])
            row1.append(row[11])
            row1.append(row[13])
            row1.append(row[14])
            row1.append(row[15])

        lexpected = len(expected1)
        
        if formatType == 1:
            row1 = row[0:lexpected]

        # check length of row against length of expected headers
        # if row is shorter this is an error so return null data
        # if row is greater then trim (just noise)


        if debug_flag:
            print '>>> lexpected = %i ' % lexpected

        if debug_flag:
            print '>>> len(row1) = %i ' % len(row1)
            
        if len(row1) < lexpected:
            rowdata = []
            return rowdata

        # build new matrix of all data including new strings appended to the end of each row
        row1 = row1[0:lexpected]

        # create YYYY-MM-DD
        dateMDY = row1[0].split('/')
        dateM = dateMDY[0]
        if len(dateM)<2:
            dateM = '0'+dateM
        dateD   = dateMDY[1]
        if len(dateD)<2:
            dateD= '0'+dateD
        dateY   = dateMDY[2]
        dateYMD = dateY + "-" + dateM + "-" + dateD

        timeHM  = row1[1]
        if len(timeHM) < 5:
            timeHM = '0' + timeHM
        timeH   = timeHM[0:2]
        timeM   = timeHM[3:5]
        timeS   = "00"
        timeHMS = timeH + ":" + timeM + ":" + timeS

        if (televiltDSTpresent == 1):
            gmtDate = adjustDateStr(dateYMD,timeHMS,timedelta5H)
            gmtTime = adjustTimeStr(dateYMD,timeHMS,timedelta5H)
            if (televiltDSTremove == 1):
                localDate = adjustDateStr(dateYMD,timeHMS,timedeltaM1H)
                localTime = adjustTimeStr(dateYMD,timeHMS,timedeltaM1H)
            else:
                localDate = dateYMD
                localTime = timeHMS
        else:
            localDate = dateYMD
            localTime = timeHMS
            gmtDate   = adjustDateStr(dateYMD,timeHMS,timedelta6H)
            gmtTime   = adjustTimeStr(dateYMD,timeHMS,timedelta6H)

        row1.append(localDate)
        row1.append(localTime)
        row1.append(gmtDate)
        row1.append(gmtTime)
        
        rowdata.append(row1)
            
    if debug_flag:
        nrows = len(rowdata)
        print '*** Total Rows = %i' % nrows
        print '*** First Row'
        frow  = rowdata[0]
        lfrow = len(frow)
        for i in range(lfrow):
            print '   Item %i "%s" ' % (i,frow[i])

    return rowdata
  

def genLotekLocation(ifn,rowdata):
    #
    # Generate the _location file for Lotek
    #
    print '>>> Lotek Location Generation'
    
    ldata = len(rowdata)
    ofn = ifn[:-4] + '_location.csv'

    # Calculate the Wolf_ID and Pack_ID from the filename
    # VendorID_WolfID_PackID_etc
    #    WolfID is assumed to be delimited by '_' (usually 3 characters)
    #    PackID is assumed to be delimited by '_' (usually 2 characters)

    WolfID = 'Wnn'
    PackID = 'Px'

    split_ofn = ofn.split('_')
    if len(split_ofn) > 1:
        WolfID = split_ofn[1]
    if len(split_ofn) > 2:
        PackID = split_ofn[2]
    
    print '>>>          File "%s"'      % ofn
    print '>>>          Header + %i rows' % ldata
    print '>>>          WolfID = %s' % WolfID
    print '>>>          PackID = %s' % PackID

    # Lotek Format
    # Device Name,Device ID,Date & Time [GMT],Date & Time [Local],Latitude,Longitude,Altitude,Fix Status,DOP,Temp [C],Main [V],Back [V]
    #
    # Fields:
    # [00] DeviceName
    # [01] DeviceID
    # [02] DateTime1 "02-13-2017 21:01:42" (GMT) 
    # [03] DateTime2 "02-13-2017 15:01:42" (LMT) 
    # [04] Latitude
    # [05] Longitude
    # [06] Altitude
    # [07] FixStatus 
    # [08] DOP
    # [09] TempC
    # [10] MainV
    # [11] BackV
    #
    # Calculated:
    # [12] Date1 (YYYY-MM-DD)   (LMT)
    # [13] Time1 (HH:MM:SS)     (LMT)

    # Lotek Format
    #
    # Fields:
    # Location File                        Input File
    # [00] Fix_#
    # [01] WolfID (also called CollarID)
    # [02] PackID
    # [03] Year
    # [04] Month
    # [05] Day
    # [06] Date (reformatted GPS Fix Time)  [12] Date (YYYY-MM-DD)(LMT)
    # [07] Hour
    # [08] Minute
    # [09] Second
    # [10] Time                             [17] Time (HH:MM:SS) (LMT)
    # [11] Latitude                         [04] GPS Latitude
    # [12] Longitude                        [05] GPS Longitude            
    # [13] TempC                            [09] Temperature
    # [14] DeviceName                       [00] DeviceName
    # [15] DeviceID                         [01] DeviceID
    # [16] DateTimeGMT                      [02] DateTimeGMT
    # [17] FixStatus                        [07] FixStatus
    # [18] DOP                              [08] DOP
    # [19] MainV                            [10] MainV
    # [20] BackV                            [11] BackV
 
    csvfile = open(ofn,'w')
    csvfile.write('''"FixNo","WolfID","PackID","Year","Month","Day","Date","Hour","Minute","Second","Time","Latitude","Longitude","TempC","gmtDate","gmtTime","DeviceName","DeviceID","dateTimeGMT","FixStatus","DOP","MainV","BackV"\n''')

    fixNo = 0
    empty_str  = " "
    rowsll0 = 0
    for row in rowdata:
        fixNo   = fixNo + 1
        fixStr  = str(fixNo)

        dateYMD = row[12]
        dateY   = dateYMD[:4]
        dateM   = dateYMD[5:7]
        dateD   = dateYMD[8:10]
        dateMDY = dateM + '/' + dateD + '/' + dateY
        
        timeHMS     = row[13]
        # if no leading zero add one
        if len(timeHMS) < 8:
            timeHMS = '0' + timeHMS
        timeH       = timeHMS[0:2]
        timeM       = timeHMS[3:5]
        timeS       = timeHMS[6:8]

        # calculate gmt 
        gmtDate = adjustDateStr(dateYMD,timeHMS,timedelta6H)
        gmtTime = adjustTimeStr(dateYMD,timeHMS,timedelta6H)
        
        latitude    = row[4]
        longitude   = row[5]

        # Output each row in _location format

        csvfile.write('%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' % \
                      (fixStr, WolfID, PackID, dateY, dateM, dateD, dateYMD, timeH, timeM, timeS, timeHMS,\
                       latitude, longitude,\
                       row[9], gmtDate, gmtTime, row[0], row[1], row[2], row[7], row[8], row[10], row[11]))

    csvfile.close()

    return

def genLotekAnalysis(ifn,rowdata):
    #
    # Generate the _analysis file for Lotek
    #
    print '>>> Lotek Analysis Generation'
    
    ldata = len(rowdata)
    ofn = ifn[:-4] + '_analysis.csv'

    # Calculate the Wolf_ID and Pack_ID from the filename
    # VendorID_WolfID_PackID_etc
    #    WolfID is assumed to be delimited by '_' (usually 3 characters)
    #    PackID is assumed to be delimited by '_' (usually 2 characters)

    WolfID = 'Wnn'
    PackID = 'Px'

    split_ofn = ofn.split('_')
    if len(split_ofn) > 1:
        WolfID = split_ofn[1]
    if len(split_ofn) > 2:
        PackID = split_ofn[2]
    
    print '>>>          File "%s"'      % ofn
    print '>>>          Header + %i rows' % ldata
    print '>>>          WolfID = %s' % WolfID
    print '>>>          PackID = %s' % PackID

    # Lotek Format
    # Device Name,Device ID,Date & Time [GMT],Date & Time [Local],Latitude,Longitude,Altitude,Fix Status,DOP,Temp [C],Main [V],Back [V]
    #
    # Fields:
    # [00] DeviceName
    # [01] DeviceID
    # [02] DateTime1 "02-13-2017 21:01:42" (GMT) 
    # [03] DateTime2 "02-13-2017 15:01:42" (LMT) 
    # [04] Latitude
    # [05] Longitude
    # [06] Altitude
    # [07] FixStatus 
    # [08] DOP
    # [09] TempC
    # [10] MainV
    # [11] BackV
    #
    # Calculated:
    # [12] Date1 (YYYY-MM-DD)   (LMT)
    # [13] Time1 (HH:MM:SS)     (LMT)
    #
    # Format of _analysis file
    # [00] FIX_#
    # [01] CASE
    # [02] GMT_DATE
    # [03] GMT_TIME
    # [04] LMT_DATE  (as MM/DD/YYYY)
    # [05] LMT_TIME
    # [06] PackID
    # [07] WolfID
    # [08] N9
    # [09] LATITUDE
    # [10] LONGITUDE
    # [11] HEIGHT (not used)
    # [12] to [20] N13,N14,N15,N16,N17,N18,N19,N20,N21


    csvfile = open(ofn,'w')
    csvfile.write('''"FIX_#","CASE","GMT_DATE","GMT_TIME","LMT_DATE","LMT_TIME","PackID","WolfID","N9","LATITUDE","LONGITUDE","HEIGHT","N13","N14","N15","N16","N17","N18","N19","N20","N21"\n''')

    fixNo = 0
    empty_str  = " "
    rowsll0 = 0
    for row in rowdata:
        fixNo   = fixNo + 1
        
        dateYMD = row[12]
        dateY   = dateYMD[:4]
        dateM   = dateYMD[5:7]
        dateD   = dateYMD[8:10]
        dateMDY = dateM + '/' + dateD + '/' + dateY

        timeHMS     = row[13]
        # if no leading zero add one
        if len(timeHMS) < 8:
            timeHMS = '0' + timeHMS
        timeH       = timeHMS[0:2]
        timeM       = timeHMS[3:5]
        timeS       = timeHMS[6:8]

        # calculate gmt 
        gmtYMD  = adjustDateStr(dateYMD,timeHMS,timedelta6H)
        gmtMDY  = convertMDY(gmtYMD)
        gmtTime = adjustTimeStr(dateYMD,timeHMS,timedelta6H)
        
        latitude    = row[4]
        longitude   = row[5]
        
        #
        # skip rows if Latitude or Longitude are 0
        #
        if (len(latitude)<1) or (len(longitude)<1):
            rowsll0 = rowsll0 + 1
            continue
        if (float(latitude)== 0) or (float(longitude)==0):
            rowsll0 = rowsll0 + 1
            continue

        # Output each row in _analysis format
        csvfile.write('%i,%i,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n' % \
                      (fixNo, 1, gmtMDY, gmtTime, dateMDY, timeHMS, \
                       PackID, WolfID,empty_str, latitude, longitude, empty_str,\
                       empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str,empty_str))

    csvfile.close()

    if rowsll0 != 0:
        print '>>>>>>'
        print '>>>>>>      WARNING Suppressed (%i) Rows with Lat/Long == 0' % rowsll0
        print '>>>>>>'

    return

def processLotek(fname):
    
    print '   >>> Lotek Format'

    # Clear rowdata - contains all of the fixes which are good plus any calcuated fields
    rowdata = []

    # open file, read all lines and trim off the first one (column headers)
    try:
    # Old way linesa = open(fname).readlines()
        with open(fname,'rU') as f:
            linesa = list(f)
    except IOError:
        print 'Error opening/reading "%s"...' % fname
        return

    nlinesa = len(linesa)
    print '   >>> Total Lines     = %i ' % nlinesa

    # Split files into
    #   Junk            (j)
    #   Header          (h)
    #   Data            (d)
    #   Succeeded Fixes (f)
    
    linesj  = linesa[0:1]
    linesh  = linesa[0:1]
    linesd  = linesa[1:]
    linesf  = []

    expected = ('Device Name', 'Device ID', 'Date & Time [GMT]', 'Date & Time [Local]', 'Latitude', 'Longitude', 'Altitude','Fix Status', 'DOP', 'Temp [C]', 'Main [V]', 'Back [V]')
    # returns true if there is a header error
    tstru       = linesh[0]
    tstr        = filter(onlyascii, tstru)
    if checkHeader(expected,tstr,True):
        return rowdata
    
    if debug_flag:
        print '>>> Header = %s ' % linesh
    
    if debug_flag:
        for line in linesd:
            print line

    nlinesf = len(linesd)
    print '   >>> Succeeded Fixes = %i ' % nlinesf
    
    # Parse All Fixes as CSV
    csvinput = iter(linesd)
    csv1 = csv.reader(csvinput,delimiter=',')

    # counters
    #   all rows
    #   total succeeded fix rows

    count  = 0
    
    # Lotek Format
    # Device Name,Device ID,Date & Time [GMT],Date & Time [Local],Latitude,Longitude,Altitude,Fix Status,DOP,Temp [C],Main [V],Back [V]
    #
    # Fields:
    # [00] DeviceName
    # [01] DeviceID
    # [02] DateTime1 "02-13-2017 21:01:42" (GMT) 
    # [03] DateTime2 "02-13-2017 15:01:42" (LMT) 
    # [04] Latitude
    # [05] Longitude
    # [06] Altitude
    # [07] FixStatus 
    # [08] DOP
    # [09] TempC
    # [10] MainV
    # [11] BackV
    #
    # Calculated:
    # [12] Date1 (YYYY-MM-DD)   (LMT)
    # [13] Time1 (HH:MM:SS)     (LMT)
    
    for row in csv1:

        count = count + 1
        lrow = len(row)
        

        # Lotek has lots of extraneous blanks so first remove them for consistent parsing
        
        # check length of row against length of expected headers
        # if row is shorter this is an error so return null data
        # if row is greater then trim (just noise)

        lexpected = len(expected)

        if lrow < lexpected:
            rowdata = []
            return rowdata

        # build new matrix of all data including new strings appended to the end of each row
        row = row[0:lexpected]
        lrow = len(row)
        row1 = []
        
        for i in range(lrow):
            tstr = re.sub(' +',' ',row[i])
            row1.append(tstr.strip(' '))

        lrow = len(row1)
        if debug_flag:
            print '*** Row %i Length %i ' % (count,lrow)
            if count > 0:    #  count == 1 for only first row  count > 0 for all rows
                print '*** First Row'
                frow  = row1
                lfrow = len(frow)
                for i in range(lfrow):
                    print '   Item %i "%s" ' % (i,frow[i])

        # 2021-02-17  Handle No Sat case
        if row1[7] == "No Sats":
            row1[4] = "0.0"
            row1[5] = "0.0"

        # create YYYY-MM-DD (LMT)
        dateMDY = row1[3][0:10]
        dateM   = dateMDY[0:2]
        dateD   = dateMDY[3:5]
        dateY   = dateMDY[6:10]
        dateYMD = dateY + "-" + dateM + "-" + dateD

        timeHMS = row1[3][11:19]
        timeH   = timeHMS[0:2]
        timeM   = timeHMS[3:5]
        timeS   = timeHMS[6:8]
        timeHMS = timeH + ":" + timeM + ":" + timeS

        row1.append(dateYMD)
        row1.append(timeHMS)
        
        rowdata.append(row1)
            
    if debug_flag:
        nrows = len(rowdata)
        print '*** Total Rows = %i' % nrows
        print '*** Last Row'
        frow  = rowdata[nrows-1]
        lfrow = len(frow)
        for i in range(lfrow):
            print '   Item %i "%s" ' % (i,frow[i])

    return rowdata
     

def processFile1(fname):

    # First check if this is a file that needs cleaning
    print ' '
    print 'Collar Cleaning Activated'
    print '>>> Processing "%s"' % fname

    lfn = fname.lower()

    if 'analysis' in lfn:
        print '    Skip - cluster input file'
        return

    if 'location' in lfn:
        print '    Skip - location file'
        return

    if 'mikecluster' in lfn:
        print '    Skip - cluster output file'
        return

    if 'telonics' in lfn:
        rowdata = processTelonics(fname)
        iDate   = 28
        iTime   = 29
        iLat    = 7
        iLong   = 8
        if len(rowdata) > 0 :
            print '   >>> Selection Date Range   Start: "%s"  End: "%s" ' % (daterange_start,daterange_end)
            rowdata1 = check_n_sort(rowdata,iDate,iTime,iLat,iLong)
            if len(rowdata1) > 0:
                genTelonicsLocation(fname,rowdata1)
                genTelonicsAnalysis(fname,rowdata1)
        return

    if 'sirtrack' in lfn:
        rowdata = processSirtrack(fname)
        iDate   = 10
        iTime   = 11
        iLat    = 3
        iLong   = 4
        if len(rowdata) > 0 :
            print '   >>> Selection Date Range   Start: "%s"  End: "%s" ' % (daterange_start,daterange_end)
            rowdata1 = check_n_sort(rowdata,iDate,iTime,iLat,iLong)
            if len(rowdata1) > 0:
                genSirtrackLocation(fname,rowdata1)
                genSirtrackAnalysis(fname,rowdata1)
        return
    
    if 'ats' in lfn:
        rowdata = processATS(fname)
        iDate   = 14
        iTime   = 16
        iLat    = 7
        iLong   = 8
        if len(rowdata) > 0 :
            print '   >>> Selection Date Range   Start: "%s"  End: "%s" ' % (daterange_start,daterange_end)
            rowdata1 = check_n_sort(rowdata,iDate,iTime,iLat,iLong)
            if len(rowdata1) > 0:
                genATSLocation(fname,rowdata1)
                genATSAnalysis(fname,rowdata1)
        return
    
    if 'televilt' in lfn:
        rowdata = processTelevilt(fname)
        iDate   = 16
        iTime   = 17
        iLat    = 2
        iLong   = 5
        if len(rowdata) > 0 :
            print '   >>> Selection Date Range   Start: "%s"  End: "%s" ' % (daterange_start,daterange_end)
            rowdata1 = check_n_sort(rowdata,iDate,iTime,iLat,iLong)
            if len(rowdata1) > 0:
                genTeleviltLocation(fname,rowdata1)
                genTeleviltAnalysis(fname,rowdata1)
        return
    
    if 'lotek' in lfn:
        rowdata = processLotek(fname)
        iDate   = 12
        iTime   = 13
        iLat    = 4
        iLong   = 5
        if len(rowdata) > 0 :
            print '   >>> Selection Date Range   Start: "%s"  End: "%s" ' % (daterange_start,daterange_end)
            rowdata1 = check_n_sort(rowdata,iDate,iTime,iLat,iLong)
            if len(rowdata1) > 0:
                genLotekLocation(fname,rowdata1)
                genLotekAnalysis(fname,rowdata1)
        return

    if 'daterange' in lfn:
        # skip if already parsed
        if daterange_parsed: return
        
        if processDateRange(fname):
            print '>>>>>>'
            print '>>>>>> Error in Date Range File: "%s"' % fname
            print '>>>>>>'
        else:
            print '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
            print '>>>'
            print '>>> Date Range File: "%s" ' % fname
            print '>>> Selection Date Range   Start: "%s"  End: "%s" ' % (daterange_start,daterange_end)
            print '>>>'
            print '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
        return

    print '>>>>>>'
    print '>>>>>> WARNING Unknown File Type - File NOT Processed <<<<<<'
    print '>>>>>>'
    
    return


if __name__ == "__main__":
    ## process all files or folders from the command-line
    ## assume it is a file if it contains '.csv' otherwise a folder
    if len(sys.argv) > 1:
        print ">>> Arguments Provided"
        print ' '
        for a in sys.argv[1:]:
            if '.csv' in a:
                x = a
                if not 'skip' in x:
                    try:
                        processFile1(x)
                    except:
                        print "ERROR processing",x
                        for y in traceback.format_tb(sys.exc_info()[2]):
                            print y,
                        print "CONTINUING"
                else:
                    print x,"looks like a skip-file; skipping"
            else:
                        # find the date range file
                for x in os.listdir(a):
                    if 'daterange' in x:
                        processFile1(x)
                for x in os.listdir(a):
                    if os.path.splitext(x)[1] == '.csv':
                        if not 'skip' in x:
                            try:
                                processFile1(a+"\\"+x)
                            except:
                                print "ERROR processing",x
                                for y in traceback.format_tb(sys.exc_info()[2]):
                                    print y,
                                print "CONTINUING"
                        else:
                            print x,"looks like a skip-file"

                    else:
                        print 'Skipping "%s" because it doesn\'t end in .csv' % x

    else:
        print ">>> NO Arguments Provided"
        print ' '
        # find the date range file
        for x in os.listdir('.'):
            if 'daterange' in x:
                processFile1(x)
                
        for x in os.listdir('.'):
            print ' '
            print ">>> Checking File %s " % x
            if os.path.splitext(x)[1] == '.csv':
                if not 'skip' in x:
                    try:
                        print 'file name %s ' % x
                        processFile1(x)
                    except:
                        print "ERROR processing",x
                        for y in traceback.format_tb(sys.exc_info()[2]):
                            print y,
                        print "CONTINUING"
                else:
                    print x,"looks like a skip-file"

            else:
                print '>>> Skipping "%s" because it doesn\'t end in .csv' % x

        raw_input('press return')

