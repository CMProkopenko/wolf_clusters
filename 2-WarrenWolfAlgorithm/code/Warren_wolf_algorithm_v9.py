#!/usr/bin/env python

import traceback
import csv
import math
import time
import sys
import os
import os.path

##     GPS-data clustering software
##     Copyright (C) 2008 mike warren (mike@mike-warren.com)
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

##     Modifications to the original program have been made under: https://github.com/CMProkopenko/wolf_clusters

## Program Version
##
## v6 - baseline version
##
## v7 - development version
##
## v8 - 2019-06-11
##    - add optional parameter file
##    - add CluPar to output file
##    - fix method "all"
##    - fix method "centroid"
##
## v9 - 2024-04-20
##    - add GitHub info
##    

## Update WWA Version whenever a new version is released 
##
WWA_VERSION = "V9"

## Warren Wolf Algorithm (WWA) takes as input a CSV file of GPS-collar fixes.
##
## A typical such file looks like this:
##
## FIX_#,CASE,GMT_DATE,GMT_TIME,LMT_DATE,LMT_TIME,N7,N8,N9,LATITUDE,LONGITUDE,HEIGHT,N13,N14,N15,N16,N17,N18,N19,N20,N21,N22,N23,N24,N25,N26,N27,N28,N29,N30,N31,N32,N33,N34,N35,N36,N37,N38,N39,N40,N41,N42,N43,N44,N45,N46
## 2,1,2/18/2006,22:04:18,2/18/2006,22:04:18,-1710094,-3498899,5035956,52.47025,-116.04717,1396.71,6.4,3D,Yes,5,15,41,16,41,21,35,26,38,29,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.42,3.56,11,N/A,N/A,N/A
## 4,1,2/19/2006,4:01:24,2/19/2006,4:01:24,-1710121,-3498898,5035938,52.47007,-116.04753,1389.11,4,3D,Yes,5,1,41,11,41,14,38,20,38,25,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.42,3.62,19,N/A,N/A,N/A
##
## The file-reading code presumes the first line of the file is
## column-headers and discards it.
##
## If you wish to change how the data is parsed, see the constructor
## for FixPoint, which gets a tuple containing all the data from one
## line. Currently, it only parses out the following columns:
##
##    0: fix number
##    1: "case"
##    4 + 5: time of the fix
## CP 6: PackID
## CP 7: WoldID
##    8: Not Used (column ID is N9)
##    9 + 10: latitude and longitude
##    11: height (actually not used currently)
##
## There are a couple of options you may want to change; see "OPTIONS" below.
##

## A CluPar string will be created and added to the output file
## The string will contain: RmmmHttAaaaVn
##
##		CLUSTER_RADIUS m   - in meters
##		CLUSTER_TIME   t   - in hours
##		CLUSTER_METHOD a   - text string (any, all, centroid)
##		WWA_VERSION    n
##
## An optional parameter file can be placed in the same directory as the analysis files
##
##    Name: ...WWAparameters...   (file name must contain the string WWAparamaters)
##    Type: .csv
##
##    Rows 2..n are optional and can be in any order
##
##    Algorithm Paramaters
##    Row 1 (header)  = "Parameter",     		"Value"
##    Row 2 (data)    = "CLUSTER_RADIUS", 		mmm
##    Row 3 (data)    = "CLUSTER_TIME",   		ttt
##    Row 4 (data)    = "CLUSTER_METHOD", 		"string"
##    Debug Parameters (default is 0 (off))
##    Row 5 (data)    = "GENERATE_ORIGINAL",    0 or 1 (generate original file format)
##    Row 6 (data)    = "GENERATE_PREFIX",    	0 or 1 (prefix generated file with Parameter string)


##
## DEFAULT OPTIONS you might want to change (or set in the WWAParameters file)
##

# approximate target cluster radius, in meters (depends a little on the method selected below)
# value used to checkif distance being compared is < CLUSTER_RADIUS
#
CLUSTER_RADIUS = 300

# approximate target cluster time, in hours (depends a little on the method selected below)
# check if delta time <= CLUSTER_TIME
#
CLUSTER_TIME   = 4*24.0

# available clustering methods; see pointInCluster for more details
#
# all methods check time: 
#   is new point within CLUSTER_TIME   (hours)  of earliest(first) or latest (last) point in cluster
#
# Method: any
#	is new point within CLUSTER_RADIUS (meters) of ANY point currently in the cluster 
#
# Method: all
#	is new point within CLUSTER_RADIUS (meters) of ALL points currently in the cluster 
#
# Method: centroid
#	is new point within CLUSTER_RADIUS (meters) of current centroid (centroid changes as new points are added) 
#
CLUSTER_METHODS = ['any', 'all', 'centroid']
CLUSTER_METHOD = CLUSTER_METHODS[0]

# default Cluster Parameters to just the 
CluPar = WWA_VERSION


# debug parameters settable from WWAParameters File
GENERATE_ORIGINAL   	= False			# generate original Mike Cluster format output file
GENERATE_PREFIX     	= False			# generate prefix for Mike Cluster2 output file
GENERATE_DEBUG    		= False			# generate debug output for clustering steps

# debugging stuff - change values here
GOOGLE_MAP 				= False         # output google-map data (to plot on a google-map)
DEBUG_DISTANCE_MATRIX 	= False         # output a matrix of all points with the distance to all others
EXPORT_RAW 				= False         # pickle the clusters to "clusters.pickle"
LOAD_PICKLED_CLUSTERS 	= False         # don't re-cluster; just load whatever is in "clusters.pickle"


################ code follows ###################

#hash table of FixPoint objects indexed by fix number
dataByFixNumber = {}

def greatCircleDistance(a,b):
	""" from http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/393241
	much more info at: http://en.wikipedia.org/wiki/Great_circle_distance

	this is using the haversine arcsin method (which won't work
	well for points on opposite sides of the earth, but that
	shouldn't be a problem for this data)

	this will return the distance in METERS

	"other" can be another FixPoint or a 2-tuple of (lat,lng)

	"""

	(alat,alng) = a
	(blat,blng) = b

	lngdist = math.radians(blng - alng)
	latdist = math.radians(blat - alat)

	alat = math.radians(alat)
	blat = math.radians(blat)
	alng = math.radians(alng)
	blng = math.radians(blng)
	
	a = (math.sin(latdist / 2))**2 + math.cos(alat) * math.cos(blat) * (math.sin(lngdist / 2))**2
	c = 2 * math.asin(min(1, math.sqrt(a)))
	# dist = 6399.592 * c  # magic number for poles
	# dist = 6335.437 * c  # magic number for equator
	dist = 6372.795 * c # average arcradius of Earth (in km)
	return int(dist * 1000)              # return meters, not kilometers (don't return decimals; up to 0.5% error anyway)


##
## each of the points in the file are read as one of these
## if there was no data for that point, self.latlng will be None
##

class FixPoint:
	def __init__(self, *args):
		self.number = int(args[0])
		self.case = args[1]
		
		# excel seems to do this:
		self.time = time.mktime(time.strptime(args[4] + ' ' + args[5], "%m/%d/%Y %H:%M:%S"))
		
		# gnumeric seems to do this:
		if 0:
			thetm = args[4] + ' ' + args[5]
			if thetm[-7:] == '.000000':
				thetm = thetm[:-7]
			self.time = time.mktime(time.strptime(thetm, "%Y/%m/%d %H:%M:%S"))

		self.lmt = args[4] + ' ' + args[5]

		## CP: PackID and WolfID

		self.packID = args[6]
		self.wolfID = args[7]

		temptime = time.localtime(self.time)

		self.year   = temptime[0]
		self.mon    = temptime[1]
		self.day    = temptime[2]
		self.hour   = temptime[3]
		self.min    = temptime[4]
		self.sec    = temptime[5]
		
		if args[9] == 'N/A' or args[10] == 'N/A':
			self.latlng = None
		else:
			self.latlng = (float(args[9]), float(args[10]))
		#self.height = float(args[11])


	def distance(self,other):
		"""distance from me to the other point, either a (lat,lng)
		tuple or another FixPoint"""
		
		if self.latlng is None:
			return None
		if isinstance(other,FixPoint) and other.latlng is None:
			return None

		if not isinstance(other,FixPoint):
			(olat,olng) = other
		else:
			(olat,olng) = other.latlng
			

		return greatCircleDistance( self.latlng, (olat,olng) )


	def timedelta(self,other):
		"""
		return the (absolute) difference in time between this and the other point in HOURS
		"""
		# the if portion can be used if fix number can be mapped to relative time
		if 0:
			# return time difference based on fix number (3 hours apart)
			return math.fabs( (self.number*3) - (other.number*3) )

		else:
			# actual seconds-based time difference
			seconds = other.time - self.time
			seconds = math.fabs(seconds)
			return seconds / (60.0*60.0)           # return hours

	def __str__(self):
		if self.latlng:
			return "<FixPoint #%d (%f,%f) %f seconds>" % (self.number,self.latlng[0],self.latlng[1],self.time)
		return "<FixPoint #%d (N/A) %f seconds>" % (self.number,self.time)

	def __repr__(self):
		return self.__str__()

##
## This represents a cluster, obviously
##

class Cluster:
	def __init__(self):
		self.points = []
		self.centroidcache = None

	def clusterInCluster(self,c):
		"""returns True if the other cluster c is "related to" this one
		(i.e. if they can be merged); centroids must be within CLUSTER_RADIUS
		and the first/last points of one must be within CLUSTER_TIME of the other"""
		
		if greatCircleDistance(self.centroid(),c.centroid()) < CLUSTER_RADIUS:
			# earliest point in c
			a = c.points[0]
			# latest point in c
			b = c.points[-1]
			if (self.points[-1].timedelta(a) <= CLUSTER_TIME) or (self.points[0].timedelta(a) <= CLUSTER_TIME):
				return True
			if (self.points[-1].timedelta(b) <= CLUSTER_TIME) or (self.points[0].timedelta(b) <= CLUSTER_TIME):
				return True
		return False
	
	def allFixNumbers(self):
		#
		# return: string containg an ordered list of fix numbers
		#
		rtn = ''
		for x in self.points:
			rtn = rtn + '%d ' % x.number
		return rtn

	def pointInCluster(self,p):
		#
		# Three methods of checking if a point is in the primary new cluster when two clusters are being merged
		#
		# Method: any
		#   is new point within CLUSTER_TIME   (hours)  of earliest(first) or latest (last) point in cluster
		#	is new point within CLUSTER_RADIUS (meters) of ANY point currently in the cluster 
		#
		# Method: all
		#   is new point within CLUSTER_TIME   (hours)  of earliest(first) or latest (last) point in cluster
		#	is new point within CLUSTER_RADIUS (meters) of ALL points currently in the cluster 
		#
		# Method: centroid
		#   is new point within CLUSTER_TIME   (hours)  of earliest(first) or latest (last) point in cluster
		#	is new point within CLUSTER_RADIUS (meters) of current centroid (centroid changes as new points are added) 
		#
		#	note: ORIGINAL CODE: CLUSTER_TIME is NOT used - this is likely a bug and needs further investigation
		#
		if p.latlng is None:
			return False
		
		if len(self.points) == 0:
			return False
		elif len(self.points) == 1:
			# ORIGINAL CODE: the following statement is correct though redundant in that for an array of length 1
			# the first point (point[0]) and the last point (point[-1]) are the same
			timeok = (self.points[-1].timedelta(p) <= CLUSTER_TIME) or (self.points[0].timedelta(p) <= CLUSTER_TIME)
			return (self.points[0].distance(p) < CLUSTER_RADIUS) and timeok
		else:
			# points are in time order - so only the first and last points need to be checked
			timeok = (self.points[-1].timedelta(p) <= CLUSTER_TIME) or (self.points[0].timedelta(p) <= CLUSTER_TIME)
			
			#
			# any
			#
			if CLUSTER_METHOD == 'any':
				# method "is new point within CLUSTER_RADIUS m of ANY point currently in the cluster"
				# check all points AND check that the time is OK (within CLUSTER_TIME)
				for x in self.points:
					if p.distance(x) <= CLUSTER_RADIUS:
						if timeok:
							return True
				return False

			#
			# all
			#
			if CLUSTER_METHOD == 'all':
				# method "is new point within CLUSTER_RADIUS m of ALL points currently in the cluster"?
				
				#
				# ORIGINAL CODE: is incorrect: points outside of time window would always be added
				#
				# for x in self.points:
				# 	if p.distance(x) > CLUSTER_RADIUS:
				#		if timeok:
				# 			return False
				# return True
				
				#
				# UPDATED CODE
				#
				if timeok == False:
					return False

				for x in self.points:
					if p.distance(x) > CLUSTER_RADIUS:
							return False
				#
				# at this point time is ok AND all points are within CLUSTER_RADIUS
				#
				return True
			#
			# centroid
			#
			if CLUSTER_METHOD == 'centroid':
				# method "is new point within CLUSTER_RADIUS m of current centroid of cluster"?
				# note: ORIGINAL CODE: time check is commented out
				# return p.distance(self.centroid()) < CLUSTER_RADIUS     #and self.points[-1].timedelta(p) < CLUSTER_TIME
				
				if timeok == False:
					return False
				return p.distance(self.centroid()) < CLUSTER_RADIUS 

		# default Unknown CLUSTER_METHOD
		return False
		
	def addPoint(self,p):
		#
		# add points with a lat long that are not already in set - then sort based on fix number
		#
		if p.latlng is None:
			print ">>> ERROR misisng latlong",p
			return
		if p not in self.points:
			self.points.append(p)
		self.points.sort( lambda x,y: cmp(x.number,y.number) )

	def centroid(self):
		#
		# there are many ways to calculate a centroid 
		# simple average of latitude and longnitude is accurate enough when the points aren't too far apart
		# the link states 400km, for wolf clusters it will ok: http://www.geomidpoint.com/calculation.html
		#
		# if the cache of the centroid exists and no points have been added then use the cached value
		#
		if self.centroidcache is not None and self.centroidcache[1] == len(self.points):
			return self.centroidcache[0]
		avglat = 0.0
		avglng = 0.0
		for p in self.points:
			(lat,lng) = p.latlng
			avglat = avglat + lat
			avglng = avglng + lng
		rtn = (avglat/len(self.points), avglng/len(self.points))
		self.centroidcache = (rtn,len(self.points))
		return rtn

	def averageDistanceFromCentroid(self):
		#
		# average: distance of all points from the cluster
		#
		avgdist = 0.0
		for x in self.points:
			avgdist = avgdist + x.distance(self.centroid())
		return avgdist / len(self.points)

	def maxDistanceFromCentroid(self):
		#
		# maximum: distance of all points from the cluster
		#
		max = 0.0
		for x in self.points:
			dist = x.distance(self.centroid())
			if dist > max:
				max = dist
		return max
		

	def totalTime(self):
		#
		# time spread of cluster in hours
		#
		# assertion is checking that list of points is in order of fixes
		#
		# return: time of last point - time of first pointInCluster#
		#
		for x in range(1,len(self.points)):
			assert self.points[x].number > self.points[x-1].number
		return self.points[-1].timedelta(self.points[0])

	def totalTheoreticalPoints(self):
		"""
		return the number of points which "should" be in the cluster; this is the difference between the biggest and
		smallest fix-number in the cluster
		note: when rarifying data, for example from 1 hour to 2 hours, fixes need to renumbered
			  otherwise it will look like there should be twice as many points
		"""
		
		assert len(self.points) > 1
		return self.points[-1].number - self.points[0].number + 1

	def missingPoints(self):
		"""how many points are "missing" from the cluster?
		that is, if the first point is #10 and the last is
		#20, there "should" be 11 points -- but if, say,
		#12, #13 and #14 aren't in this cluster [and they might be missing
		totally -- i.e. "N/A" fixes] but the rest are, then this method should return 3.
		"""
		
		last = self.points[0]
		missing = 0
		for x in self.points[1:]:
			if last.number != x.number-1:
				assert (x.number-last.number-1) > 0
				missing = missing + (x.number-last.number-1)
			last = x
		return missing

	def fixesAwayFromCluster(self,returnListOfMissing=False):
		"""
		similar to above, but say that #13 in the
		above example was a "successful" fix (i.e.
		not "N/A" in the file) then this method
		should return 1.
		"""

		last = self.points[0]
		away = 0
		bonus = []
		for x in self.points[1:]:
			if last.number != x.number-1:
				for n in range(last.number+1, x.number):
					if dataByFixNumber.has_key(n):
						assert(n not in self.points)
						fixn = dataByFixNumber[n]
						if fixn.latlng != None:
							away = away + 1
							bonus.append(n)
			last = x
		if returnListOfMissing:
			return (away,bonus)
		return away

	def numberOf24HourPeriods(self):
		"""
		Kyle: The number of 24 hour periods where a fix was obtained
		at the cluster.  For example, a cluster might have 8 points
		and span 4 days, but 7 of the points were obtained on the
		first day and one was obtained on the last day.  This cluster
		has two 24 hour periods where there was a fix. A similar 8
		point cluster has 2 points obtained on each of 4 days and so
		has a score of 4 in this output.

		Mike: Algorithm: starting with the first point, we "quantize"
		everything into 24-hour-long buckets, starting with the hour
		of the first fix (i.e. if there was a fix at 11:30pm and two
		more at 2am, they'd only be on one"day")
		"""

		daystarts = []
		for x in self.points:
			if len(daystarts) == 0 or x.time > daystarts[-1] + (24*60*60):
				daystarts.append(x.time)
		return len(daystarts)
			

	def __eq__(self,other):
		"""return True if this cluster is equal to "other" -- has all the same points"""
		
		if len(self.points) == 0 or len(other.points) == 0:
			return False
		if len(self.points) != len(other.points):
			return False
		for x in self.points:
			if not x in other.points:
				return False
		return True

	def __str__(self):
		if True:
			pnt = '['
			for x in self.points:
				pnt = pnt + str(x.number) + ', '
			pnt = pnt[:-2] + ']'
			return 'Cluster of %d points (%d hours spread, missing %d points, %dm avg dist to centroid).' % (len(self.points), int(self.totalTime()), self.missingPoints(),int(self.averageDistanceFromCentroid())) + pnt

		else:
			rtn = 'Cluster of %d points (%f hours spread, missing %d points, %dm average distance):\n' % (len(self.points), self.totalTime(), self.missingPoints(),int(avgdist))
			start = self.points[0]
			avgdist = self.averageDistanceFromCentroid()
			for x in self.points:
				rtn = rtn +  "   #%d (%f,%f) timedelta %f hours\n" % (x.number,x.latlng[0],x.latlng[1],x.timedelta(start))
			rtn = rtn + '   average distance from centroid: %dm\n' % int(avgdist)
			return rtn

	def html(self):
		rtn = 'Cluster of %d points<br/>%f hours spread.<br/>missing %d points<br/>' % (len(self.points), self.totalTime(), self.missingPoints())
		rtn = rtn + 'avg dist from centroid: %dm' % int(self.averageDistanceFromCentroid())
		return rtn


def CluParStr(p1,p2,p3,p4):
	# 
	# return paramater string
	#
	retstr = "R"+str(p1)+"H"+str(int(p2))+"A"+str(p3)+p4
	return retstr
	
def readParam(fname):

	# Update WWA Parameters from file
	global CLUSTER_RADIUS
	global CLUSTER_TIME
	global CLUSTER_METHOD
	global GENERATE_ORIGINAL
	global GENERATE_PREFIX
	global GENERATE_DEBUG

	print '>>> Processing WWA Parameters "%s"' % fname

	import csv

	csv.register_dialect('myDialect',
	delimiter = ',',
	skipinitialspace=True)

	with open(fname, 'r') as csvFile:
		reader = csv.reader(csvFile, dialect='myDialect')
		for row in reader:
			print(row)
			if (row[0] == "CLUSTER_RADIUS"): 	CLUSTER_RADIUS 		= int(row[1])
			if (row[0] == "CLUSTER_TIME"):   	CLUSTER_TIME   		= int(row[1])
			if (row[0] == "CLUSTER_METHOD"): 	CLUSTER_METHOD 		= row[1]
			if (row[0] == "GENERATE_ORIGINAL"): GENERATE_ORIGINAL 	= (row[1] != 0)
			if (row[0] == "GENERATE_PREFIX"): 	GENERATE_PREFIX		= (row[1] != 0)
			if (row[0] == "GENERATE_DEBUG"): 	GENERATE_DEBUG		= (row[1] != 0)

	csvFile.close()

	paramFlag = True
	return paramFlag

def processParam(flist):
	#
	# Output defaults
	# Check for file name containing WWAparameters
	# If found display file name, read parameters, display new values
	# Else indicate that defaults are being used
	#
	# Return True if parameters are in legal range
	#
	print '>>> Processing Parameters'
	#
	# default Cluster Parameter String
	global CluPar
	#
	CluPar = CluParStr(CLUSTER_RADIUS,CLUSTER_TIME,CLUSTER_METHOD,WWA_VERSION)
	#
	# Report Parameter Defaults
	#
	print '   Default Parameters: "%s"' % CluPar
	print '    CLUSTER_RADIUS:    "%s"' % CLUSTER_RADIUS
	print '    CLUSTER_TIME:      "%s"' % CLUSTER_TIME
	print '    CLUSTER_METHOD:    "%s"' % CLUSTER_METHOD
	print '   WWA Version:        "%s"' % WWA_VERSION
	#
	# Look for file
	#
	print '>>> Looking for Algorithm Parameters (WWAparameters)'
	pfound = False
	for x in flist:
		#print ">>> Checking File %s " % x
		if os.path.splitext(x)[1] == '.csv':
			# look for unique name
			if ('WWAparameters' in x) :
				try:
					pfound = readParam(x)
				except:
					print ">>> ERROR processing paramter file ",x
					for y in traceback.format_tb(sys.exc_info()[2]):
						print y,
					print "CONTINUING"
	#
	# Range Check Parameters
	#
	pflag = True
	CluPar = CluParStr(CLUSTER_RADIUS,CLUSTER_TIME,CLUSTER_METHOD,WWA_VERSION)
	if (CLUSTER_RADIUS < 1 or CLUSTER_RADIUS > 10000): pflag = False
	if (CLUSTER_TIME   < 1 or CLUSTER_TIME   > 1000 ): pflag = False
	if (CLUSTER_METHOD in CLUSTER_METHODS == False  ): pflag = False
	#
	if not pflag:
		print '>>> Bad Parameters: "%s"' % CluPar
		return False
	print '   Parameter Range Check OK: "%s"' % CluPar
	if (pfound):
		print '  '
		print '   Updated Parameters: "%s"' % CluPar
		print '    CLUSTER_RADIUS:    "%s"' % CLUSTER_RADIUS
		print '    CLUSTER_TIME:      "%s"' % CLUSTER_TIME
		print '    CLUSTER_METHOD:    "%s"' % CLUSTER_METHOD
		print '   WWA Version:        "%s"' % WWA_VERSION
		print '  '
	return True


def processFile(fname):
	print '>>> Processing "%s"' % fname
	
	# open file, read all lines and trim off the first one (column headers)
	try:
		lines = open(fname).readlines()[1:]
	except IOError:
		print '>>> Error opening/reading "%s"...' % fname
		return

	# parse it as a CSV file
	input = csv.reader(lines)

	# "data" is any data with actual coordinates
	# "dead" is "N/A" points
	data = []
	dead = []

	try:
		last = None
		while True:
			foo = input.next()
			pt = FixPoint(*foo)
			dist = 0
			if last:
				dist = last.distance(pt)
				if dist is None: dist = -1
			if 1:
				print "  loaded fix #%d (%dm away from previous fix)" % (pt.number,dist)
			last = pt
			if pt.latlng:
				data.append(pt)
			else:
				dead.append(pt)
			dataByFixNumber[pt.number] = pt
	except StopIteration:
		pass
	print ">>>"
	print ">>> loaded %d points: %d good (with latlong), %d missing (no latlong)." % (len(data)+len(dead),len(data),len(dead))
	print ">>>"


	if True:
		print '>>>'
		print '>>> start hierarchical clustering <<<'
		print ">>>"
		##
		## do "hierarchical clusering"
		##
		## this works by making a "cluster" for each point containing just that point.
		## then, an attempt is made to merge each cluster with all other clusters until
		## no more merging can be done.
		##
		clusters = []

		## data now pre-filtered to only have valid fixes
		##
		## points with no latlong can be left in the input file or
		##    skip the fixNum or
		##    determining post-WWA based on time
		
		## make all points into a "cluster" of one point
		for x in data:
			c = Cluster()
			c.addPoint(x)
			clusters.append(c)

		## merge clusters which match distance criterion (e.g. centers
		## are within CLUSTER_RADIUS m of each other)
		print "  merge clusters - check existing clusters against all other clusters"
		sys.stdout.flush()

		#
		# Nested Function: merge - returns True if there is a successful merge of two clusters into 1 (or 2) new clusters
		#
		def merge(clusters):

			for c in clusters:
				for n in clusters:
					if c is n:
						continue
					#
					# this check determines at a coarse level which clusters can be combined
					#
					# clusterInCluster uses time and distance between centroids
					if c.clusterInCluster(n):
						print "   making new cluster from:"
						print "  ",c
						print "  ",n
						print "-----------"
						foo = Cluster()
						bar = Cluster()
						for y in c.points:
							foo.addPoint(y)
						#
						# final check will be point by point based on pointInCluster
						#
						for y in n.points:
							if foo.pointInCluster(y):
								foo.addPoint(y)
							else:
								bar.addPoint(y)
								
						if ( (c == bar) and (n == foo)) or (( (c == foo) and (n == bar))):
							print "--new clusters same as old; not replacing"
							continue
						
						clusters.remove(c)
						clusters.remove(n)
						clusters.append(foo)
						if len(bar.points) > 0:
							print "  2 new clusters - primary and secondary (points not passing pointInCluster check):"
							print foo
							print "-----------"
							print bar
							print "-----------"
							clusters.append(bar)
						else:
							print "  single new cluster (all points passed pointInCluster check):"
							print foo
							print "-----------"
						return True
			return False

		if not LOAD_PICKLED_CLUSTERS:
			last = time.time()
			while merge(clusters):
				now = time.time()
				if now - last > 2:
					#print '    \b\b\b\b\b%04d' % len(clusters),
					sys.stdout.write('.')
					sys.stdout.flush()
					last = now
			print 'done\n  filtering:'

		else:
			import pickle
			clusters = pickle.load(open('clusters.pickle','r'))

		## from the list of all clusters, filter out the
		## ones we don't want

		# only keep clusters with > 1 point
		before = len(clusters)
		clusters = filter(lambda x: len(x.points) > 1, clusters)
		print "    %d 1-point clusters removed" % (before-len(clusters),)

		print "  found %d clusters:" % len(clusters)

		if EXPORT_RAW:
			import pickle
			pickle.dump(clusters,open('clusters.pickle','w'))

		if 0:
			## debugging; if we can still merge here, something is wrong
			print "  more merging?",merge(clusters)

		if GENERATE_ORIGINAL:
			## write cluster data to a .csv file
			csvfile = open(os.path.splitext(fname)[0] + '-mikeclusters.csv','w')
			csvfile.write('''"first fix number","first fix LMT","last fix number","theoretical total points","actual number of points","points away","fix success","time span (hours)","day-periods","average distance (m)","cluster radius (m)","centroid latitude","centroid longitude","all fix numbers in cluster","all away points"\n''')
			for x in clusters:
				totalpoints = x.totalTheoreticalPoints()
				assert (x.missingPoints() >= 0)
				awayfixes, awaypoints = x.fixesAwayFromCluster(True)
				# make awaypoints into a space-separated string for output
				awaypoints = ' '.join(map(str,awaypoints))
				
				fixsuccess = ((float(totalpoints) - (totalpoints - (awayfixes+len(x.points)))) / float(totalpoints)) * 100.0
				days = x.numberOf24HourPeriods()
				csvfile.write('%d,%s,%d,%d,%d,%d,%f,%d,%d,%d,%d,%f,%f,%s,%s\n' % (x.points[0].number,x.points[0].lmt,x.points[-1].number,totalpoints,len(x.points),awayfixes,fixsuccess,x.totalTime(),days,int(x.averageDistanceFromCentroid()),int(x.maxDistanceFromCentroid()),x.centroid()[0],x.centroid()[1],x.allFixNumbers(),awaypoints))
			csvfile.close()

		if True:
			## CP: New Output Format for Cluster File revision 2
			##     ...mikeclusters2...
			##
			## write cluster data to a .csv file
			##
			##  CollarID
			##  PackID
			##
			## totalpoints   = totalTheoreticalPoints (last fix number - first fix number + 1)
			##
			## actual        = fixes that are in cluster
			##
			## away          = fixes not in cluster
			##
			## missing       = points that are not in cluster and (do not exist or have N/A lat/long)
			##  
			##
			mc2filename = os.path.splitext(fname)[0] + '-mikeclusters2.csv'
			if GENERATE_PREFIX:
				mc2filename = CluPar + '-' + mc2filename
			csvfile = open(mc2filename,'w')
			csvfile.write('''"CollarID","PackID","First_date","First_time","Last_date","Last_time","Theo_fixes","Act_fixes","Fixes_away","Fix_success","Total_hours","Total_days","Ave_dist_m","Clus_rad_m","Latitude","Longitude","Y","X","Status","CentID","FfixNum","FfixY","FfixM","FfixD","LfixNum","LfixY","LfixM","LfixD","all fix numbers in cluster","all away points","CluPar"\n''')
			for x in clusters:
				totalpoints = x.totalTheoreticalPoints()
				assert (x.missingPoints() >= 0)
				awayfixes, awaypoints = x.fixesAwayFromCluster(True)
				# make awaypoints into a space-separated string for output
				awaypoints = ' '.join(map(str,awaypoints))
				fixsuccess = ((float(totalpoints) - (totalpoints - (awayfixes+len(x.points)))) / float(totalpoints)) * 100.0
				days = x.numberOf24HourPeriods()

				## CP: calculate new fields
				firstFix = x.points[0]
				lastFix  = x.points[-1]
				centID = firstFix.packID + "_" + firstFix.wolfID + "_" + str(firstFix.year) + str(firstFix.mon).zfill(2) + str(firstFix.day).zfill(2) + "_" + str(firstFix.hour).zfill(2) + str(firstFix.min).zfill(2) + str(firstFix.sec).zfill(2)

				## FfixMDY FfixHMS FfixY FfixM FfixD
				FfixDMY    = str(firstFix.year) + "-" + str(firstFix.mon).zfill(2)  + "-" + str(firstFix.day).zfill(2)
				FfixHMS    = str(firstFix.hour).zfill(2) + ":" + str(firstFix.min).zfill(2) + ":" + str(firstFix.sec).zfill(2)
				FfixY      = str(firstFix.year)
				FfixM      = str(firstFix.mon).zfill(2)
				FfixD      = str(firstFix.day).zfill(2)
				LfixDMY    = str(lastFix.year) + "-" + str(lastFix.mon).zfill(2)  + "-" + str(lastFix.day).zfill(2)
				LfixHMS    = str(lastFix.hour).zfill(2) + ":" + str(lastFix.min).zfill(2) + ":" + str(lastFix.sec).zfill(2)
				LfixY      = str(lastFix.year)
				LfixM      = str(lastFix.mon).zfill(2)
				LfixD      = str(lastFix.day).zfill(2)
				
				empty_str  = " "
				##
				## 01 s CollarID
				## 02 s PackID
				## 03 s First_date
				## 04 s First_time
				## 05 s Last_date
				##
				## 06 s Last_time
				## 07 d Theo_fixes  (number of theoretical fixes = last number - first number + 1)
				## 08 d Act_fixes   (number of fixes in cluster)
				## 09 d Fixes_Away  (number of fixes not in cluster)
				## 10 f Fix_Success (this is number of points with lat,long data wether in cluster or not in cluster - it will be 100% if N/A's are not included)
				##
				## 11 d Total_hours
				## 12 d Total_days
				## 13 d Ave_dist_m
				## 14 d Clus_rad_m
				## 15 f Latitude
				##
				## 16 f Longitude
				## 17 s Y
				## 18 s X
				## 19 s Status
				## 20 s CentID"
				##
				## 21 d FfixNum
				## 22 s FfixY
				## 23 s FfixM
				## 24 s FfixD"
				## 25 d LfixNum
				##
				## 26 s LfixY
				## 27 s LfixM
				## 28 s LfixD
				## 29 s all fix numbers in cluster  (space separated list of fix numbers)
				## 30 s all away points             (space separated list of fix numbers)
				##
				## 31 s CluPar (the parameters used for this run)
				##
				## Format      01 02 03 04 05 06 07 08 09 10   11 12 13 14 15   16   17 18 19 20 21 22 23 24 25 26 27 28 29 30
				csvfile.write('%s,%s,%s,%s,%s,%s,%d,%d,%d,%.4f,%d,%d,%d,%d,%.6f,%.6f,%s,%s,%s,%s,%d,%s,%s,%s,%d,%s,%s,%s,%s,%s, %s\n' % \
							  (firstFix.wolfID, firstFix.packID, FfixDMY, FfixHMS, LfixDMY, \
							   LfixHMS, totalpoints,len(x.points),awayfixes,fixsuccess, \
							   x.totalTime(),days,int(x.averageDistanceFromCentroid()),int(x.maxDistanceFromCentroid()),x.centroid()[0], \
							   x.centroid()[1],empty_str, empty_str,  empty_str, centID,\
							   firstFix.number,FfixY,FfixM,FfixD,lastFix.number, \
							   LfixY,LfixM,LfixD,x.allFixNumbers(),awaypoints,CluPar))

			csvfile.close()



		if GOOGLE_MAP:
			## write data to produce the google-map
			file = open('coords','w')
			last = None
			for p in data:
				if p.latlng:
					if last and last.number != p.number-1:
						file.write('geo:lat=%f geo:lon=%f end\n' % p.latlng)
					else:
						file.write('geo:lat=%f geo:lon=%f waypoint\n' % p.latlng)
				last = p

			for x in clusters:
				print "  ",x
				lat,lng = x.centroid()
				file.write('geo:lat=%f geo:lon=%f %s\n' % (lat,lng,x.html(),))

			file.close()

		return clusters
		


	if DEBUG_DISTANCE_MATRIX:
		## this outputs a NxN matrix of each point and the distance to all other points
		## (for debugging)
		print 'writing "matrix.csv"'
		f = open('matrix.csv','w')
		for x in data:
			for y in data:
				dist = x.distance(y)
				if dist is None: dist = -1
				f.write('%d,' % dist)
			f.write('\n')
		f.close()



if __name__ == "__main__":
	## process all files from the command-line
	if len(sys.argv) > 1:
		print ">>> Arguments Provided"
		for x in sys.argv[1:]:
			if 'mikeclusters' in x:
				continue
			processFile(x)

	else:
		print ">>> NO Arguments Provided"
		#
		# first check if there is a parameter file which will override defaults
		#
		dlist = os.listdir('.')
		if processParam(dlist) == False:
			raw_input('press return to exit')
			exit()
		#
		# look for input files which have "analysis" as part of the name
		# and do not have "mikecluster" as part of the name
		#
		# note: this naming convention restricts the name of any folders where the files are located
		#
		for x in os.listdir('.'):
			print ">>> Checking File %s " % x
			if os.path.splitext(x)[1] == '.csv':
				# don't process cluster files and only process analysis files as input
				if (not 'mikecluster' in x) and ('analysis' in x) :
					try:
						processFile(x)
					except:
						print "ERROR processing",x
						for y in traceback.format_tb(sys.exc_info()[2]):
							print y,
						print "CONTINUING"
				else:
					print x,"looks like a cluster-file or not an analysis file; skipping"

			else:
				print 'Skipping "%s" because it doesn\'t end in .csv' % x

		raw_input('press return to exit')

