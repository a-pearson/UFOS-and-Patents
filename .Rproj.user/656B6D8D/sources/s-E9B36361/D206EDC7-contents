################################################################################

#                                 MAIN SCRIPT                                  #

################################################################################
#
#   This script contains the code to set up the work flow used thorughout the 
#   rest of the project. It also contains housekeeping code.
#
#   Project to use: "UFO Big Data.Rproj"
#
#   The Scripts within this project should be run in the following order:
#                   - MainScript
#                   - cleaning.ufo.data
#                   - Patent_cleaner_script
#                   - AnalysisScript
#
################################################################################
#
#================== All Libraries and Files that Must be Loaded ================
library(openair)
library(tidyverse)
library(ggplot2)
library (ggpubr)

# files that must be loaded:
# had to reset working directory to retreve file...
setwd("2.Clean.Data")
clean.patent.db<- read.csv( "clean.patent.db.csv",)
ufo.freq.table.date.range <- read.csv(paste(path.cd, "UFO.freq.date.range.TV.csv"))
# reset working directory back to original
setwd("~/GitHub/UFOS-and-Patents")
getwd()
ufo <- read.csv ("consolidated_weather_V03.csv")


#================================ Variables ====================================

# Number of UFO sightings required to make it a date of interest.
ufo.crit.val <- 5   # this was determined in the cleaning.ufo.data script, it is
                    # based off the mean frequency of UFO sightings during the
                    # data time period.

# Delay period before sampling of patent frequencies.
delay.period <- 14

# Length of sample period after delay (in days).
sample.period <- 30

# Number of samples the null distribution runs. 
null.run.times <- 10000

#============================ WORK FLOW SET UP =================================
work.d <- getwd()

out.put.folders <- c("1.Raw.Data","2.Clean.Data", "3.Analysis", "4.Graphs", 
                     "4.Graphs/Table")

# check if folders exist and make them if they don't.
# This loop checks goes through the given out.put.folders list and checks to see 
# if they exisit in the working directory. If they don't they print "does not 
# exisit" and creates them, if it does exist it prints "does exist"

for(i in 1:length(out.put.folders)){
  if(!file.exists(out.put.folders[i])){
    print(paste(i, "does not exist"))
    dir.create(out.put.folders[i])
  }else{
    print(paste(i,"does exist")) 
  }
}
#---- Pathways---------

#path to 1.RawData folder
path.rd <- paste(work.d,"/",out.put.folders[1], "/", sep="")
#test the pathway...
# x.t <- c(5)
# write.csv(x.t,paste(path.rd, "test.x.csv"), row.names=FALSE)
# to remove row names from saved files       ^^^

#Path to 2.Clean.Data
path.cd <- paste(work.d,"/",out.put.folders[2], "/", sep="")

# Path to 3.Analysis
path.a <- paste(work.d,"/",out.put.folders[3], "/", sep="")

# Path to 4.Graphs
path.g <- paste(work.d,"/",out.put.folders[4], "/", sep="")

# Path to Table folder in 4.Graphs folder
path.t <- paste(work.d,"/",out.put.folders[5], "/", sep="")

# write.csv(x.t,paste(path.t, "test.x.csv"), row.names=FALSE)
