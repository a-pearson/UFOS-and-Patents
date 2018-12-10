################################################################################
#                                                                              #
#                             Data Analysis                                    #
#                                                                              #
################################################################################
#
#
################################################################################
#
#   This script contains the code used to analysis the cleaned UFO and patent
#   data.
#
################################################################################
#
install.packages("ggpubr")

#--------- Load Libraries and Files --------------------------------------------
library(openair)
library(tidyverse)
library(ggplot2)
library (ggpubr)

# files that must be loaded:
# had to reset workign directory to retreve file...
setwd("2.Clean.Data")
clean.patent.db<- read.csv( "clean.patent.db.csv",)
ufo.freq.table.date.range <- read.csv(paste(path.cd, "UFO.freq.date.range.TV.csv"))
# reset working directory back to original
setwd("~/GitHub/UFOS-and-Patents")
getwd()

################ Create a Null Distribution For Patent Frequency ###############
#
# In this we aim to create a null distribution for our patent frequencies. We 
# are using a sample size of 30 days to allow for differences in time from idea
# to patent application.
#
#========= Make a Frequency Table of Pantent For Entire Date Range =============
#---- Create Frequncy Table with Pantent Data ----------------------------------

# use table function to make frequency table
table.pf <- table(clean.patent.db$actiondate)
# make into a data frame
patent.freq.df <- data.frame(table.pf)
# rename column
colnames(patent.freq.df) <- c("action_date","Frequency")
# make the actiondate into a date instead of a factor
patent.freq.df$action_date <- as.Date(patent.freq.df$action_date)
class(patent.freq.df$action_date)

#---- Insert Missing dates -----------------------------------------------------

# make vector with all dates within the range 
all.dates.p <- seq(as.Date("2006-01-01"), as.Date("2015-12-31"), "day")

# make into a dataframe
missing.dates.p.df <- as.data.frame(c(all.dates.p))

# add another column for frequency 
missing.dates.p.df$frequency <- rep(NA, length(all.dates.p))

# Rename the columns
colnames(missing.dates.p.df) <- c("action_date", "Frequency")

#---- Merge Missing Date and Patent Frequency Table ----------------------------
# Merge missing dates with patent frequency table.
patent.freq.p.df <- merge(patent.freq.df, missing.dates.p.df, 
                          by.x= "action_date", by.y="action_date", all=TRUE)
                          
# check to confirm merge worked by looking for NA in the Frequency Data from the
# Patent dataframe
any(is.na(patent.freq.p.df$Frequency.x))

# cut the Frequency.y column
final.patent.freq <- patent.freq.p.df[, c("action_date", "Frequency.x")]
# Rename Frequency.x to Frequency
colnames(final.patent.freq) <- c("action_date", "Frequency")
# replace all NA values with 0
final.patent.freq$Frequency[is.na(final.patent.freq$Frequency)] <- 0
# check to see if it worked
any(is.na(final.patent.freq$Frequency))
# save as csv
write.csv(final.patent.freq, paste(path.cd, "final.patent.freq.csv"))




#========= Create the Null Distribution Sampling ===============================

#---- Pulling Samples ----------------------------------------------------------
# we want to pull random samples of 30 from our patent frequency table

# these two pieces of code create a matrix in which each row has 30 columns, and
# each column is an observation form 1 sample. We have 100 rows, so we then have
# 100000 samples of 30 randomly selected values form our patent frequency table.
test.t <- sample(final.patent.freq$Frequency, size = 100000*30, replace = TRUE)
test <- matrix(test.t, 100000)   # run these two line together
# now we want to get the mean of each of the samples so...
# we apply the mean function to each row of the matrix
sample.mean <- apply(test, 1, mean)

sample.means <- as.data.frame(sample.mean)
#---- PLotting the Null Distribution -------------------------------------------
# we want the means displayed in frequencies in order
null.dis <- ggplot(data=sample.means, aes(sample.means$sample.mean)) + 
         geom_histogram(col ="blue", bins = 100, fill="light blue") +
  labs(x= "Sample Mean", y= "Frequency") + 
  theme(panel.grid.major = element_blank(), 
     panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))
# save the graph
pdf(paste(path.g, "Null.Dis.Patents.pdf"))
plot(null.dis)
dev.off()

##################### The Observed Sampling Distibution ########################
#
# We need to isolate the days in our UFO data where there were >5 sightings.
# Then we need to determine the date 2 weeks after each of the days where UFO 
# sightings were >5. This is our date of interest. From there we need to 
# determine the mean frequency of patents for 30 days staring at the date of 
# interest. Once this is done for each of the days of interest, we can plot the
# means on a histogram.
#

#---- Find Dates where UFO sightings >5 --------------------------------------

# pull UFO data for days with more than 5 sightings
head(ufo.freq.table.date.range)
ufo.freq.critical <- ufo.freq.table.date.range[
  ufo.freq.table.date.range$Frequency>5,]  
nrow(ufo.freq.critical) # count number of days.

#---- Add 2 Week Period to Each Date ------------------------------------------
# This is our delay period to account for time required to produce patent 
# application. Our dates if interest are 14 days after the sighting of >5.

# change the Date variable to be recognized as class "Date"
class(ufo.freq.critical$Date)
ufo.freq.critical$Date<- as.Date(ufo.freq.critical$Date)
class(ufo.freq.critical$Date)

ufo.freq.critical$Delayed
# add 14 days to each date in the data frame

# Create for loop to add 14 ays to each date and then give these dates in a new
# column in our data frame.

ufo.freq.critical$Delayed <- ufo.freq.critical$Date + 14
head(ufo.freq.critical)

# check to ensure it ran correctly
head(ufo.freq.critical)

class(ufo.freq.critical[1,2])

str(ufo.freq.critical)
class(ufo.freq.critical$Date)
#============== Combining UFO Sighting Data and Patent Frequency ===============
#
#  In this section we want to make a new data frame with just the dates of 
# interest and an indicator column. We will then merge with the Patenet 
# Frequency data frame. This will give us our dates of interest, patent 
# frequency as well as our indicator column. The indicator coulmn will = 1 for 
# dates of interest and NA for dates of non-interest. This will allow us to 
# easily query for patent frequencies on dates of interest.

#---- Create New Data Frame ----------------------------------------------------
# create data frame with Delayed dates and Indicator column.
d.dates.in <- data.frame(ufo.freq.critical$Delayed)  # make new dataframe with
# the Delayed date
d.dates.in$indicator <- rep(1, length(d.dates.in))  #made indicator column with
# value = 1

# rename the columns to: date_intrest and indicator
colnames(d.dates.in) <-  c("date_interest", "indicator")

#---- Merge with Patent Data ---------------------------------------------------

# merging 
patent.dates.intrest <- merge(final.patent.freq, d.dates.in, 
                          by.x= "action_date", by.y="date_interest", all=TRUE)
# We not have our dates of interest connected to our patent frequencies. We also
# have an indicator column so we can easily query only rows that contain dates 
# of interest

#=============== Sampleing 30 Days Following a Day of Interest =================
#
# In this section we take the patent frequencies from the 30 days following a 
# date of interest and find the mean. We will do this for all of the days of 
# interest.

#---- For Loop to Take Mean of 30 Days After Date of Interest ------------------

# Build empty matrix for the means

ob.samples <- NULL

# build a for loop to take mean of frequencies for each day of interest and the 
# following 29 days. (this is 30 dya including the day of interest)

for(i in 1:nrow(patent.dates.intrest)){
  if(is.na(patent.dates.intrest[i,3])==FALSE){   # saying if indicator not NA...
    t.sum <- 0   # them create a variable that = 0
    for(j in 1:30){     # then count from 1-30 starting at date of interest
      patent.row.no <- i + j   # assign row number 
      t.sum <- t.sum + patent.dates.intrest$Frequency[patent.row.no]
    }   # add up the patent frequencies for each of the 30 rows following a date
        # of interest
    t.mean <- t.sum/30  # devide this by 0 to get the mean
    row.no <- nrow(ob.samples) + 1 # assign new row number
     ob.samples <- rbind(ob.samples, data.frame(t.mean))  #put mean in dataframe

    #ob.samples[row.no,1:30] <- patent.dates.intrest[i:i+29,2]
  }
}
View(patent.dates.intrest)
head(ob.samples)


#================= Plotting The Observed Distribution ==========================

# plot a histogram of the means of pantent applications
Sighting.dis <- ggplot(data=ob.samples, aes(ob.samples$t.mean)) + 
  geom_histogram(col ="dark red", bins = 100, fill="maroon") +
  labs(x= "Mean", y= "Frequency") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# save the graph
pdf(paste(path.g, "Sighting.Dis.Patents.1.pdf"))
plot(Sighting.dis)
dev.off()


######################### Running Statistical Tests ############################
#
################################################################################
# We will use the one sample Wilcox test to determine if the mean of our
# observed distruibution is significantly different form our null distribution.
###############################################################################
#
#
#---- Find the Means of Both Null and Observed Distribution --------------------

# the mean of the null sample distribution
null.mean <- mean(sample.means[,1])
