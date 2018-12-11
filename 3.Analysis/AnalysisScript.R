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
#
################################################################################
#
# For our null distribution we will be looking at the mean of the sum of patents
# for 2547-31 samples of 30 days. We will randomly collect 2516 samples of 30 
# days periods. We will find the sum of patents for each of the 30 days then we
# will take the mean of these sums. We will repeat this entire process 100 times 
# to determine out null distribution.

#=========== Find UFO Sighting Dates ===========================================

#---- Find Dates where UFO sightings > ufo.crit.val ----------------------------

# pull UFO data for days with more than critical value of sightings
head(ufo.freq.table.date.range)
ufo.freq.critical <- ufo.freq.table.date.range[
  ufo.freq.table.date.range$Frequency>ufo.crit.val,]  

# save the number of critical days to variable for later use
num.crit.days <- nrow(ufo.freq.critical)



#======== Pulling Desired Number of Samples of sample.period days ==============
#
# create an empty data frame to store all of the means
null.dis.means <- as.data.frame(NULL)

#---- Determining Number of sample.periods to Call -------------------------------------
# To determine the number of sample periods to specify in our null loop we need 
# to know how many sample periods will be taken from ufo critical sighting dates.
# This is equal to the number of critical dates identified minus any critical
# dates that fall within a sample.period of the last date in our patent 
# frequency data set. We can find this by running one our loop once, with the
# number of sampling periods pulled equaling the number of critical dates. Then
# we can count the number of NA values returned by that loop (these are sum 
# values for which our critical date was within one sample period of the end 
# date), and subtract this from the total number of crtical values. This gives 
# us the total number of critical dates for which we will be able to run our 
# test.


# first we run the for loop with the number of critical dates. 
for(i in 1:1){
  pull.samples <- sample(final.patent.freq$Frequency, size = num.crit.days*sample.period, 
                         replace = TRUE) #pull a number of day = sample.period 
                                        # repeat x= num.crit.days times
  pull.sample <- matrix(pull.samples, num.crit.days) # putting those into a matrix
  
  sum.sample <- apply(pull.sample,1,sum)  # sum values within a sample.period
  # day sample.
  sample.sums <- as.data.frame(sum.sample) # storeing sums into a data frame
  
}
# now we count the number of NA values produced
num.na <- sum(is.na(sample.sums))
tail(sample.sums)

# this for loop will collect 2516 samples of 30 days, sum the patent number for 
# each of the 30 day periods and then find the mean number of patent sums within
# the 2516 samples. It will then run this 10000 times.
for(i in 1:10000){
  pull.samples <- sample(final.patent.freq$Frequency, size = 2516*sample.period, 
                         replace = TRUE) #pull a sample of , 2516 times
  pull.sample <- matrix(pull.samples, 2516) # putting those into a matrix
  
  sum.sample <- apply(pull.sample,1,sum)  # sum values within a sample.period
  # day sample.
  sample.sums <- as.data.frame(sum.sample) # storeing sums into a data frame
  null.dis.means[i,1] <- mean(sample.sums[,1])  #taking the mean of the sums and
  # storing it into a new data frame.
  
}

# rename coloumn to mean
colnames(null.dis.means) <- c("Mean")
head(null.dis.means)

# save as a csv
write.csv(null.dis.means, paste(path.cd, "null.dis.means.csv"), row.names = FALSE)

#---- *Pulling Samples (original code for sample dis. NO LONGER APPLICABLE)*----
# we want to pull random samples of 30 from our patent frequency table

# these two pieces of code create a matrix in which each row has 30 columns, and
# each column is an observation form 1 sample. We have 100 rows, so we then have
# 100000 samples of 30 randomly selected values form our patent frequency table.

#test.t <- sample(final.patent.freq$Frequency, size = 100000*30, replace = TRUE)
#test <- matrix(test.t, 100000)   # run these two line together

# now we want to get the mean of each of the samples so...
# we apply the mean function to each row of the matrix
#sample.mean <- apply(test, 1, mean)

#sample.means <- as.data.frame(sample.mean)



#---- PLotting the Null Distribution -------------------------------------------
# we want the means displayed in frequencies in order
null.dis.mean.sum <- ggplot(data=null.dis.means, aes(null.dis.means$Mean)) + 
         geom_histogram(col ="blue", bins = 100, fill="light blue") +
  labs(x= "Sample Mean", y= "Frequency") + 
  theme(panel.grid.major = element_blank(), 
     panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
         axis.line = element_line(colour = "black"))
# save the graph
pdf(paste(path.g, "Null.Dis.Patents.pdf"))
plot(null.dis.mean.sum)
dev.off()

########################## The Observed Mean ###################################
#
# We need to isolate the days in our UFO data where there were > ufo.crit.val 
# sightings. Then we need to determine the date 2 weeks after each of the days 
# where UFO sightings were > ufo.crit.val. This is our date of interest. From 
# there we need to determine the sum patents for the sample.period staring at 
# the date of interest.Then we will take the mean of these sums. This is our 
# observed mean. 
#


#============== Combining UFO Sighting Data and Patent Frequency ===============
#
#  In this section we want to make a new data frame with just the dates of 
# interest and an indicator column. We will then merge with the Patenet 
# Frequency data frame. This will give us our dates of interest, patent 
# frequency as well as our indicator column. The indicator coulmn will = 1 for 
# dates of interest and NA for dates of non-interest. This will allow us to 
# easily query for patent frequencies on dates of interest.

#---- Add Delay Period to Each Critical Date -----------------------------------
# This is our delay period to account for time required to produce patent 
# application. Our dates if interest = the delay.period of days after the 
# sighting of > ufo.crit.val.

# change the Date variable to be recognized as class "Date"
class(ufo.freq.critical$Date)
ufo.freq.critical$Date<- as.Date(ufo.freq.critical$Date)
class(ufo.freq.critical$Date)

ufo.freq.critical$Delayed
# add delay.period days to each date in the data frame

# Create for loop to add delay.period days to each date and then give these 
# dates in a new column in our data frame.

ufo.freq.critical$Delayed <- ufo.freq.critical$Date + delay.period
head(ufo.freq.critical)

# check to ensure it ran correctly
head(ufo.freq.critical)
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

#---- For Loop to Take SUM of 30 Days After Date of Interest ------------------

# create empty vector
ob.sum <- NULL


for(i in 1:nrow(patent.dates.intrest)){
  if(is.na(patent.dates.intrest[i,3])==FALSE){   # saying if indicator not NA...
    t.sum <- 0   # them create a variable that = 0
    for(j in 1:30){     # then count from 1-30 starting at date of interest
      patent.row.no <- i + j   # assign row number 
      t.sum <- t.sum + patent.dates.intrest$Frequency[patent.row.no]
    }   # add up the patent frequencies for each of the 30 rows following a date
    # of interest
    row.no <- nrow(ob.samples) + 1 # assign new row number
    ob.sum <- rbind(ob.sum, data.frame(t.sum))  #put sum in dataframe

  }
}
tail(ob.sum) # the last 31 samples are NA values as the data set does not 
# contain enough data to look for the whole 30 days following this.
# excludes all the NA values
ob.sums <- na.exclude(ob.sum)

#----- Find The Observed Mean --------------------------------------------------
# find the mean of the patents sums in the 30 days following a day of interest.
ob.mean <- mean(ob.sums[,1])

#-------------- OG For Loop * no longer applicable* ----------------------------------------------------
# Build empty matrix for the means

#ob.samples <- NULL

# build a for loop to take mean of frequencies for each day of interest and the 
# following 29 days. (this is 30 dya including the day of interest)

#for(i in 1:nrow(patent.dates.intrest)){
  #if(is.na(patent.dates.intrest[i,3])==FALSE){   # saying if indicator not NA...
   # t.sum <- 0   # them create a variable that = 0
    #for(j in 1:30){     # then count from 1-30 starting at date of interest
     # patent.row.no <- i + j   # assign row number 
      #t.sum <- t.sum + patent.dates.intrest$Frequency[patent.row.no]
#    }    add up the patent frequencies for each of the 30 rows following a date
        # of interest
   # t.mean <- t.sum/30  # devide this by 0 to get the mean
    #row.no <- nrow(ob.samples) + 1 # assign new row number
     #ob.samples <- rbind(ob.samples, data.frame(t.mean))  #put mean in dataframe

    #ob.samples[row.no,1:30] <- patent.dates.intrest[i:i+29,2]
#  }
#}
#View(patent.dates.intrest)
#head(ob.samples)
#tail(ob.samples)

#sum(is.na (ob.samples$t.mean))


#================= Plotting The Observed Distribution ==========================

# plot the null distribution with a line where our observed mean sits
null.dis.ob.mean <- ggplot(data=null.dis.means, aes(null.dis.means$Mean)) + 
  geom_histogram(col ="blue", bins = 100, fill="light blue") +
  labs(x= "Sample Mean", y= "Frequency") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  geom_vline(xintercept = ob.mean, color= "maroon")

# save the graph.
pdf(paste(path.g, "Null.dis.ob.mean.pdf"))
plot(null.dis.ob.mean)
dev.off()

#-------------- Old Plotting of Observed Distribution not Applicable -----------
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
# 
# For our statistical test we are looking at the the probability of our sample
# mean occuring given the null distribution created.
#
###############################################################################
#
#
#---- Pull Number of Means Grater than Observed Mean from Null Dis -------------

# count the number of values in our null distribution higher than our observed 
# mean
 sum(null.dis.means[null.dis.means$Mean>ob.mean,1])
