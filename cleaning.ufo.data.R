# Raw data import using the most recent file available from the kaggle 
# Consolidated UFO and Weather Data
install.packages("tidyverse")
install.packages("openair")
library(openair)
library(tidyverse)
getwd()
ufo <- read.csv ("consolidated_weather_V03.csv")

str(mydata)
head(mydata)

summary(ufo)
# colour not useful
# shape frequency counts
# look at all the columns, whats the min and max 
# state, city, shape, colour, date


table(ufo$state) # frequency table 
prop.table(table(ufo$state)) # proportion table
barplot(table(ufo$state)) # visualize the data 
# want to arrange the frequency counts or proportions

ufo %>% count(state) # visualize like a frequency table pipe operater %>% (and then)
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

# 8000 values in city, could do the same thing with the city
ufo %>% count(city) %>% arrange(-n) 
# city            n
# <fct>       <int>
# 1 Phoenix       342
# 2 Seattle       318
# 3 Portland      317
# 4 Las Vegas     280
# 5 Los Angeles   257
# 6 San Diego     243
# 7 Chicago       212
# 8 Houston       195
# 9 Orlando       188
# 10 Tucson        188

ufo %>% count(city) %>% arrange(-n) %>% print(n=30) # gives you a table of 30

# move on to colour, mostly missing values so it can be skipped move on to shape
ufo %>% count(shape) %>% arrange(-n) 
# shape         n
# <fct>     <int>
# 1 Light     15493
#2 Circle     7680
#3 Triangle   6770
#4 Fireball   6193
#5 Sphere     4799
#6 Disk       3470
#7 Oval       3171
#8 Formation  2275
#9 Changing   1856
#10 Other        79
#11 Rectangle    15
#12 Diamond      14
#13 Cigar        12
#14 Cylinder     12
#15 Chevron      11
#16 Flash        11
#17 VARIOUS       4
#18 EGG           3
#19 ""            2


#MONTH
hist(ufo$month)
# peak in July, why is there a peak in July, 
# data set ond outdoor activity 

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

# what about hours?  Seems to be sightings when people are out and about and 
# awake
hist(ufo$hour)
ufo %>% count(hour) %>% arrange(-n)

hist(ufo$mday, breaks = 50)
ufo %>% count(mday) %>% arrange(-n)

############TRENDS#########
# most sightings in july and august
# more sightings at certain times of day, 8, 9, 10pm
# more sightings recently, 2010 onwards


# other datasets
# was the sun shining when there are more UFO sightings, is it daylight hours is 
# it evening hours? Was it evening / dusk 
# does the angle of the sun make us see more UFO's 

# trying to find trends or things in this data set, which can be correlated in 
# other data sets


#------------ make a subset of data from 2000 - 2010 -----------------------

# isolate data
ufo.isolated <- ufo[c("state", "year")]
rm(ufo.isolated)

#--------- merge year, month, day columns with paste -------------------------
# then turn into date format with merge 
# seq.date(as date("")from, to, )

head(ufo)
ufo.cut <- ufo[c("state", "mday", "month", "year")]
head(ufo.cut)
ufo.cut$Date
# for loop to merge the date columns into one w/ universal format.
for(i in 1:4){
  ufo.cut$Date <- as.Date(paste(ufo.cut$year,ufo.cut$month,
                                       ufo.cut$mday,sep="-"), format = "%Y-%m-%d")
  
}



#--------- Creating Frequency Table with all dates -----------
# create the freq. table for sightings per day.


table.1 <- table(ufo.cut$Date) # use table to find frequencies

View(ufo.cut$Date)

ufo.freq.table.all.dates <- as.data.frame(table.1) # change to dataframe.


#------------------ make sub set of data w/ frequencies, from 2006-2016 --------
    # order the ufo.cut dataframe by date 
ufo.cut <- ufo.cut[order(as.Date(ufo.cut$Date, format="%Y-%m-%d")),]
class(ufo.cut$Date)

# select all rows from the year range >2005 but <2016
ufo.cut.dates <- ufo.cut[ufo.cut$year>2005 & ufo.cut$year<2016,] 
write.csv(ufo.cut.dates, paste(path.cd, "ufo.cut.dates.csv"))
# save a final file w/ just "Dates" and "state"
ufo.cut.final <-ufo.cut.dates[,c("state", "Date")]                     
write.csv(ufo.cut.final, paste(path.cd, "ufo.dates_range.states.csv"))


#---------- Create another Frequency Table for the date range --------
table2.t <- table(ufo.cut.final$Date)  # find frequency of each date
ufo.freq.table.date.range <- as.data.frame(table2.t)  # make the table into data
# frame.
colnames(ufo.freq.table.date.range) <- c("Date", "Frequency")
write.csv(ufo.freq.table.date.range, paste(path.cd, "UFO.freq.date.range"))
# write as a csv. and save


#================= Creating master Frequency Table w/ all Dates ===============

#----------- Making Dataframe with all dates in range --------------------------

 # use sequence function to make all dates, range 2006< x < 2016
all.dates <- seq(as.Date("2006-01-01"), as.Date("2015-12-31"), "day")

# make into a dataframe
missing.dates.df <- as.data.frame(c(all.dates))

# add another column for frequency 
missing.dates.df$frequency <- rep(NA, length(all.dates))

# Rename the columns
colnames(missing.dates.df) <- c("Date", "Frequency")

#-------------------- Merging the missing dates w/ UFO dates -------------------

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
any(is.na(final.ufo.freq.n$Frequency))
# save as csv
write.csv(final.ufo.freq, paste(path.cd, "final.ufo.freq.csv"))

#-------------------- 

