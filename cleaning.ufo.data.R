################################################################################
#                           Cleaning The UFO Data                              #
################################################################################

# Raw data import using the most recent file available from the kaggle 
# Downloaded Data Contained:
      #  Consolidated UFO and Weather Data

# -------- Packages & Libraries Used -------------------------------------------
install.packages("tidyverse")
install.packages("openair")
library(openair)
library(tidyverse)

#--------- Importing Data ------------------------------------------------------

# Confirm working directory
getwd()
#import the raw data downloaded from Kaggle
# raw data also save to 1.RawData folder in workflow.
ufo <- read.csv ("consolidated_weather_V03.csv")



#========================= Exploring Data ======================================
# Examine structure and variables in data set, summarize variables to ID ones we
# will require.
str(mydata)
head(mydata)

summary(ufo)
# All weather related variables aren't of use.
# colour and shape varibles not useful
# Varibles of interest:
      # state
      # mday
      # month
      # year

# look at all the columns, whats the min and max 


#--------- Exploring State Varible  --------------------------------------------
table(ufo$state) # frequency table 
prop.table(table(ufo$state)) # proportion table
barplot(table(ufo$state)) # visualize the data 
# want to arrange the frequency counts or proportions

ufo %>% count(state) # visualize like a frequency table
ufo %>% count(state) %>% arrange(-n) # tells us top 10 states for sightings
# took ufo and took number of states and arranged in descending order of n

# A tibble: 51 x 2
# state     n
# <fct> <int>
# 1 CA     6558
# 2 FL     3374
# 3 WA     2854
# 4 NY     2394
# 5 TX     2374
# 6 PA     2040
# 7 IL     1835
# 8 OH     1756
# 9 AZ     1753
# 10 NC     1593
# ... with 41 more rows

#--------- Exploring Month/Year/Day Variables ----------------------------------
#MONTH
hist(ufo$month) 

# YEAR
hist(ufo$year) # hard to see so look at 
ufo %>% count(year) %>% arrange(-n) # after 2010 here is a spike in the data, 
# what happened in terms of patents in those years, were there lots of patents 
# after that?

# A tibble: 21 x 2
#  year     n
# <int> <int>
# 1  2014  5058
# 2  2012  4665
# 3  2013  4618
# 4  2015  3939
# 5  2016  3299
# 6  2011  3186
# 7  2008  2727
# 8  2009  2538
# 9  2010  2514
# 0  2007  2404
# ... with 11 more rows

# Day
hist(ufo$mday, breaks = 31)
ufo %>% count(mday) %>% arrange(-n)



##################### Summary of Observed Trends Present #######################
# most sightings in july and august
# most sightings in California
# more sightings recently, 2010 onward




#======================= Cleaning Data Set =====================================
################################################################################
#
# In this section we have the code used to clean the raw data file into the 
# files used for analysis. In this section of code our goal is to:
#       - Remove the unnessicary variables from the data set, keeping only the 
#         "state", "year", "month" and "day" variables.
#       - Consolidate the time varibles into a single date varible. 
#       - Determine the frequency of each date in the data set, and therfore the
#         frequency of sightings on each day.
#       - Create a continous Date variable by inserting days on which there were
#         no sightings and assigning them a frequency of 0.
#       - Subsetting this data to look at the date range of 2006-2016.
#  * not nessicarily in this order *
################################################################################



#--------- Removing unwanted varibles ------------------------------------------

head(ufo)
ufo.cut <- ufo[c("state", "mday", "month", "year")] #select just desired columns
head(ufo.cut) # chekc to see if it worked

#--------- Merging time variables into single Date variable --------------------

ufo.cut$Date   # create new date column
# for loop to merge the date columns into one w/ universal format.
for(i in 1:4){
  ufo.cut$Date <- as.Date(paste(ufo.cut$year,ufo.cut$month,
                                    ufo.cut$mday,sep="-"), format = "%Y-%m-%d")
  
}

#--------- Creating Frequency Table with all dates -----------------------------
# create the freq. table for sightings per day.

table.1.t <- table(ufo.cut$Date) # use table to find frequencies

View(ufo.cut$Date)

ufo.freq.table.all.dates <- as.data.frame(table.1.t) # change to dataframe.


#--------- Make sub set of data w/ frequencies, from 2006-2016 -----------------
    # order the ufo.cut dataframe by date 
ufo.cut <- ufo.cut[order(as.Date(ufo.cut$Date, format="%Y-%m-%d")),]
class(ufo.cut$Date)

# select all rows from the year range >2005 but <2016
ufo.cut.dates <- ufo.cut[ufo.cut$year>2005 & ufo.cut$year<2016,] 
write.csv(ufo.cut.dates, paste(path.cd, "ufo.cut.dates.csv"))
# save a final file w/ just "Dates" and "state"
ufo.cut.final <-ufo.cut.dates[,c("state", "Date")]                     
write.csv(ufo.cut.final, paste(path.cd, "ufo.dates_range.states.csv"))


#--------- Create another Frequency Table for the date range -------------------
table2.t <- table(ufo.cut.final$Date)  # find frequency of each date
ufo.freq.table.date.range <- as.data.frame(table2.t)  # make the table into data
# frame.
colnames(ufo.freq.table.date.range) <- c("Date", "Frequency")
write.csv(ufo.freq.table.date.range, paste(path.cd, "UFO.freq.date.range"))
# write as a csv. and save



#=-=-=-=-=-=- Creating master Frequency Table w/ all Dates -=-=-=-=-=-=-=-=-=-=-

#----------- Making Dataframe with all dates in range --------------------------

 # use sequence function to make all dates, range 2006< x < 2016
all.dates <- seq(as.Date("2006-01-01"), as.Date("2015-12-31"), "day")

# make into a dataframe
missing.dates.df <- as.data.frame(c(all.dates))

# add another column for frequency 
missing.dates.df$frequency <- rep(NA, length(all.dates))

# Rename the columns
colnames(missing.dates.df) <- c("Date", "Frequency")

#----------- Merging the missing dates w/ UFO dates ----------------------------

final.ufo.freq.m <- merge(ufo.freq.table.date.range, missing.dates.df, 
                        by.x= "Date", by.y="Date", all=TRUE)
# check to confirm merge worked by looking for NA in the Frequency Data from the
# UFO dataframe
any(is.na(final.ufo.freq.m$Frequency.x))

# cut the Frequency.y column
final.ufo.freq <- final.ufo.freq.m[, c("Date", "Frequency.x")]
# Rename Frequency.x to Frequency
colnames(final.ufo.freq) <- c("Date", "Frequency")
# replace all NA values with 0
final.ufo.freq$Frequency[is.na(final.ufo.freq$Frequency)] <- 0
 # check to see if it worked
any(is.na(final.ufo.freq$Frequency))
# save as csv
write.csv(final.ufo.freq, paste(path.cd, "final.ufo.freq.csv"))

#==================== Determine Critical Sighting Value ========================

# Use the mean of the sighting frequency to determine the critical value for 
# sightings. This value will be used during analysis as the treshold for a day
# to count as having a sighting.

round(mean(final.ufo.freq$Frequency), digits = 0) #we want it rounded to the 
# nearest whole number.    

# Critical Value = 5
