#!/usr/bin/env python

##     CSV File Copy Utility
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

## For more information refer to the GitHub Project: https://github.com/CMProkopenko/wolf_clusters

import os
import shutil
import glob

# Copies all *.csv files from a source folder (recursively) to a single destination folder

# change this path for ease of use
source_dir = "C:/Wolf-Projects/Data/AllDownloadsMarch2018FINALwEDITSnCLEAN_20180826"
dest_dir   = "C:/Wolf-Projects/Data/AllDownloadsMarch2018FINALwEDITSnCLEAN_20180826_Flat2"

def recursive_copy_files(source_path, destination_path, override=False):

#Recursive copies files from source  to destination directory. All ".csv" files copied to destination.
#:param source_path: source directory
#:param destination_path: destination directory
#:param override if True all files will be overridden otherwise skip if file exist#return: count of copied files
#

	files_count = 0

	if not os.path.exists(destination_path):
		print ">>> Creating Destination"
		os.mkdir(destination_path)
	items=glob.glob(source_path + "/*")

	for item in items:
		if os.path.isdir(item):
			path=os.path.join(destination_path, item.split("\\")[-1])
			files_count += recursive_copy_files(source_path=item, destination_path=destination_path, override=override)
		else:
			file= os.path.join(destination_path, item.split("\\")[-1])
			if not os.path.exists(file) or override:
				if ".csv" in file:
					print "item=", item
					print "file=", file
					shutil.copyfile(item, file)
					files_count += 1
				
	return files_count

print "   "
print ">>> Copying files"
print "    From:  ",source_dir
print "    To:    ",dest_dir

nfiles = recursive_copy_files(source_dir,dest_dir,True)

print "    Count: ",nfiles
print "   "
