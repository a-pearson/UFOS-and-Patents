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
#   In the analysis of this data we are testing the following hypothesis:
#   Null: There is no relationship between UFO sightings and the frequency of 
#   patent applications.
#   Alternative: There is a relationship between UFO sightings and the 
#   frequency of patent applications.
#
#   For our analysis our test statistic will be the mean of the sum of n periods
#   of x days. Where n = "number.sample.p" (defined in this script) and 
#   x= "sample.period" (defined in the Main Script"). We will use a two-tailed 
#   p-vaule to determine significance, with alpha = 0.05.
#
################################################################################



###################### Assembling Data For Test Statistic ######################
#
# In this section we do the following:
#     - Assemble Patent Data into Frequency per Day, Including all Dates
#     - ID Date in which UFO sightings > ufo.crit.val
#     - Add the delay.period to each Date of which the above is true
#     - Merge the New Dates of Intrest with the Patent Frequency Data
#

################################################################################
#========= Make a Frequency Table of Pantent For Entire Date Range =============
#---- Create Frequncy Table with Pantent Data ----------------------------------
# This code places the cleaned patent data into a frequency of patents per day

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
# Not all dates have patent application, however we want all dates within our 
# range. Therefore we must identify the dates not in our frequency table and 
# insert these date with frequency values of 0. 
# First we will make a data frame with all the dates within our range (2006-16)

# make vector with all dates within the range 
all.dates.p <- seq(as.Date("2006-01-01"), as.Date("2015-12-31"), "day")

# make into a dataframe
missing.dates.p.df <- as.data.frame(c(all.dates.p))

# add another column for frequency 
missing.dates.p.df$frequency <- rep(NA, length(all.dates.p))

# Rename the columns
colnames(missing.dates.p.df) <- c("action_date", "Frequency")

#---- Merge Missing Date and Patent Frequency Table ----------------------------
# We will merge our data frame with all dates within range to the our patent 
# frequency data frame. We merge the columns so that where dates are missing new
# rows are inserted.

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



#=========== Find  Critical UFO Sighting Dates =================================

#---- Find Dates where UFO sightings > ufo.crit.val ----------------------------

# pull UFO data for days with more than critical value of sightings
head(ufo.freq.table.date.range)
ufo.freq.critical <- ufo.freq.table.date.range[
  ufo.freq.table.date.range$Frequency>ufo.crit.val,]  

# save the number of critical days to variable for later use
num.crit.days <- nrow(ufo.freq.critical)


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
#============== Combining UFO Sighting Data and Patent Frequency ===============
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








########################## The Observed Mean ###################################
#
# In this next section we will used the data generated on Days of interest to 
# calculate our test statistic, which will be a mean of the sum of patent
# frequencies during the sample.period following the delay.period of every date
# on which the observed UFO sightings was > ufo.crit.val. We will also determine
# the number of sample.periods within our sample, which we will later use to 
# generate our null distribution.
#
################################################################################

#=============== Sampleing 30 Days Following a Day of Interest =================
#
# In this section we take the patent frequencies from the 30 days following a 
# date of interest and find the mean. We will do this for all of the days of 
# interest.

#---- For Loop to Take SUM of 30 Days After Date of Interest ------------------

# create empty vector in which to store the sum of each sample.period.
ob.sum <- NULL

# create a for loop which will go through our data frame contianing patent 
# frequency, date of interest and indicator varibles. It will identify rows 
# containing dates of interest via the indicator coulmn, then selection the 
# patent frequency for that date and the following 29 days, take the sum of 
# these frequencies, store them in a data frame and move on to the next date of
# interest.
for(i in 1:nrow(patent.dates.intrest)){
  if(is.na(patent.dates.intrest[i,3])==FALSE){   # saying if indicator not NA...
    t.sum <- 0   # them create a variable that = 0
    for(j in 1:30){     # then count from 1-30 starting at date of interest
      patent.row.no <- i + j   # assign row number 
      t.sum <- t.sum + patent.dates.intrest$Frequency[patent.row.no]
    }   # add up the patent frequencies for each of the 30 rows following a date
    # of interest
    row.no <- nrow(ob.sum) + 1 # assign new row number
    ob.sum <- rbind(ob.sum, data.frame(t.sum))  #put sum in dataframe
    
  }
}
tail(ob.sum) # the last 31 samples are NA values as the data set does not 
# contain enough data to look for the whole 30 days following this.

# *** WE WILL USE THESE NA VALUES TO DETERMINE THE NUMBER OF sample.periods TO
#                           CALL IN OUR NULL DISTRIBUTIONS*********

#=-=-=-=-=-=-=- Determining Number of sample.periods to Call -=-=-=-=-=-=-=-=-=-
#
# To determine the number of sample periods to specify in our null loop we need 
# to know how many sample periods were taken from in our observed sample.
# This is equal to the number of critical dates identified minus any critical
# dates that fall within a "sample.period" of the last date in our patent 
# frequency data set. We can find this by counting the number of NA values 
# returned by the above loop (these are sum values for which our critical date 
# was within one sample period of the end date), and subtract this from the 
# total number of crtical values. This gives us the total number of critical 
# dates for which we will be able to run our test.

# number of critical dates minus the number of NA values returned.
number.sample.p <- num.crit.days-(sum(is.na(ob.sum)))
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# once we have determined the "number.sample.p" we need to remove the NA values 
# from our observed sums so that we can calculate the mean.

ob.sums <- na.exclude(ob.sum)# excludes all the NA values

#----- Find The Observed Mean --------------------------------------------------
# find the mean of the patents sums in the 30 days following a day of interest.
ob.mean <- mean(ob.sums[,1])





######################### CREATING A NULL DISTRIBUTION #########################
#
# Our Test Statistic is the mean of the sum of patent frequencies from a sample 
# of 2516 30-day sample periods. The sample period is defined in the variable 
# " sample.period" in the MainScript, and the number of sample periods is 
# defined in the code above as the variable "number.sample.p". To create the 
# null distribution we need to run the sample of 2516 30-day periods many times
# and then plot the means of each of these samples into a histogram. We can then
# see where on our null distribution our Observed Mean falls and from there we
# can determine the probability of this mean occuring given the null hypothesis:
# there is no relationship between pantent application frequencies and sightings
# of more than 5 UFOs.

#======= Creating the Null Distribution Loop ===================================

# create an empty data frame to store all of the means
null.dis.means <- as.data.frame(NULL)


# this for loop will collect "number.sample.p" samples of "sample.period" days. 
# It will then sum the patent frequencies for each "sample.period" and then find 
# the mean number of patent sums within "number.sample.p" samples. 
# It will then run this loop "null.run.times" times to create a distribution of 
# means.
for(i in 1:null.run.times){
  pull.samples <- sample(final.patent.freq$Frequency, size = number.sample.p*sample.period, 
                         replace = TRUE) #pull a sample of , 2516 times
  pull.sample <- matrix(pull.samples, number.sample.p) # putting those into a matrix
  
  sum.sample <- apply(pull.sample,1,sum)  # sum values within a sample.period
  # day sample.
  sample.sums <- as.data.frame(sum.sample) # storeing sums into a data frame
  null.dis.means[i,1] <- mean(sample.sums[,1])  #taking the mean of the sums and
  # storing it into a new data frame.
  
}

# rename coloumn to "Mean"
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





#======================== PLotting the Null Distribution =======================
#
# In this section we plot the means generated for our null distribution in a 
# histogram. We also add a red line depicting out Observed Mean.

# plotting the null distribution and Observed Mean.
null.dis.mean.sum <- ggplot(data=null.dis.means, aes(null.dis.means$Mean)) + 
         geom_histogram(col ="blue", bins = 100, fill="light blue") +
  labs(x= "Sample Mean", y= "Frequency") + 
  theme(panel.grid.major = element_blank(), 
     panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
         axis.line = element_line(colour = "black")) + 
  geom_vline(xintercept = ob.mean, color= "maroon")
# save the graph
pdf(paste(path.g, "Null.Dis.Patents.pdf"))
plot(null.dis.mean.sum)
dev.off()


#-------------- OG For Loop * NO LONGER APPLICABLE * ---------------------------
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



#-------------- Old Observed Distribution *NO LONGER APPLICABLE*  -----------
# plot a histogram of the means of pantent applications
#Sighting.dis <- ggplot(data=ob.samples, aes(ob.samples$t.mean)) + 
  #geom_histogram(col ="dark red", bins = 100, fill="maroon") +
  #labs(x= "Mean", y= "Frequency") + 
  #theme(panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #panel.background = element_blank(), 
        #axis.line = element_line(colour = "black"))

# save the graph
#pdf(paste(path.g, "Sighting.Dis.Patents.1.pdf"))
#plot(Sighting.dis)
#dev.off()




######################### Running Statistical Tests ############################
#
################################################################################
# 
# For our statistical test we are looking at the the probability of our sample
# mean occuring given the null distribution created. We are looking for a two-
# tailed p-value.
#
###############################################################################
#
#
#---- Pull Number of Means > or = Observed Mean from Null Dis -------------

# count the number of values in our null distribution higher than or equal to 
# our observed mean.
 num.grater.equal <- sum(null.dis.means[null.dis.means$Mean>=ob.mean,1])
# we will then multiply this by two, add 1 and divide by the "null.run.times".
# This will give us the two-tailed p-value associated with our Observed Mean
# given our null distribution.
p.value <- ((num.grater.equal*2)+1)/(null.run.times)

# our p-value is 1e-04