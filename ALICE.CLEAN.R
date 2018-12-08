# Raw data import using the most recent file available from the kaggle 
# Consolidated UFO and Weather Data

library(tidyverse)
getwd()
ufo <- read.csv ("consolidated_weather_V03.csv")

str(ufo)
head(ufo)

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


# make a subset of data from 2000 - 2010

# isolate data
# ufo.isolated <- ufo[c("state", "year")]
# rm(ufo.isolated)

# merge with paste
# then turn into date format with merge 
# seq.date(as date("")from, to, )

head(ufo)
ufo.cut <- ufo[c("state", "mday", "month", "year")]

write.csv(ufo.cut,"ufo.cut.csv")
head(ufo.cut)

ufo.cut$Date
# for loop to merge the date columns into one w/ universal format.
for(i in 1:4){
  ufo.cut$Date <- as.Date(paste(ufo.cut$year,ufo.cut$month,
                                       ufo.cut$mday,sep="-"))
  
}

# make a data frame with all the missing dates, then use rbind(a,b) to combine 
# the two.  Then sort by data

#--------- Creating Frequency Table -----------
# create the freq. table for sightings per day.


# create a vector to be used in the frequency table
freq <- as.character(ufo.cut$Date)


# make a frequency table 
table.1 <- table(ufo.cut$Date)


# convert frequency table to data frame for easy viewing 
frq <- as.data.frame(table.1)

#----------------- select dates of interest in the script

ufo.cut.dates <- ufo.cut[ufo.cut$year> 2005 & ufo.cut$year <2016, ]

# now we need to make a frequency table for the section of data we are 
# interested in

selected.table <- table(ufo.cut.dates$Date)

freq.df.UFO <- as.data.frame(selected.table)
colnames(freq.df.UFO) <- c("Date", "frequency)

#---------- Days with no observations ---------------

# create a frequency table with all dates to be merged with the dataset to make 
# the data continuous for analysis

# using the sequence function, the date table can be created
all.dates <- seq(as.Date("2006-01-01"), as.Date("2016-01-01"), "day")



# make a dataframe by combing the two vectors
missing.dates.df <- as.data.frame(c(all.dates))

# add a variable to the dataframe with nothing in it

missing.dates.df$frequency <- rep(NA, length(all.dates))
colnames(missing.dates.df) <- c("date", "frequency")

# Now we have a data frame with two variables, "date" and "frequency" 

missing.dates.df



# ------------- MERGING OBSERVATION DATA WITH NO OBSERVATION DATA --------------

merged.data <- merge(freq.df.UFO, missing.dates.df, by.x = "Date", 
                     by.y = "date", all = TRUE)





