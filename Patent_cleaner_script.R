################################################################################
#                                                                              #
#                        Patent Cleaning Script                                #
#                                                                              #
################################################################################

################################################################################
#
#   This script details the cleaning and sorting of the patent data.
#
################################################################################

#-=-=-=-=-=-=-=-==-=-=-=-=-=-= NOTE -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

#The raw data used for this script was too large to be pushed to the repo.
#But they can all be accessed here, and must be placed in thec ~root folder.
#Main DB: 
# http://s3.amazonaws.com/data-patentsview-org/20180528/download/ipcr.tsv.zip

#Location DB: 
# http://s3.amazonaws.com/data-patentsview-org/20180528/download/location.tsv.zip

#Location_A DB: 
# http://s3.amazonaws.com/data-patentsview-org/20180528/download/location_assignee.tsv.zip

#Assignee_MD_DN: 
# http://s3.amazonaws.com/data-patentsview-org/20180528/download/assignee.tsv.zip


#-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


#================= Cleaning the Raw Patent Data ================================

#---- Main Patent Database -----------------------------------------------------

#Import the rawdata for the patents database
#International Patent Classification data for all patents
#13,051,297 rows, 2GB in size

raw.ipcr <- read.delim("ipcr.tsv") #read as delim because of tsv format

#Check the import
head(raw.ipcr) #check first six rows
tail(raw.ipcr) #check last six rows
str(raw.ipcr) #check structure
class(raw.ipcr) #check the elements

#The dataset contains a lot of missing values, we want to remove all of these

raw.ipcr[raw.ipcr == "NULL"] = NA
raw.ipcr.a <- na.omit(raw.ipcr) #Remove all rows containing NULL values

#I initially attempted to clean it all from the one table,
#but the data was too large, so I chose to
#save it to files and data frames in betweeen the process
write.csv(raw.ipcr.a, paste(path.rd, "clean.ipcr.csv"))

#Because of the limitations of working memory and using R, saving the 
#intermediate files in between cleaning saved memory and also allowed
#for faster processing 

#Write it to a csv to make data cleaning more efficent, tsv is difficult

# clean.icpr <- read.csv("clean.ipcr.csv") #initial attempted code

#There are issues with reading this file. The RStudio Connect process runs
#as the root user. It needs escalated privileges to allow binding to protected
#ports and to create “unshare” environments where content processes are run.
#Manually load the csv to overcome Sandboxing issue

#Turns out the problem was actually a typo, R was saving files with a space in
#front of them. Resolved this by checking what code the IDE was using to import
#it, then used it myself 

setwd(path.rd)

clean.ipcr <- read.csv(" cleanipcr.csv")

clean.icpr2 <- data.frame(clean.ipcr$patent_id, clean.ipcr$section,
                                                clean.ipcr$action_date)

#Order the action_date, as a date, then sort
clean.icpr2[order(as.Date(clean.icpr2$clean.ipcr.action_date)),]

#The patent data is now completely clean and can be written into the CD folder
write.csv(clean.icpr2, paste(path.cd,"clean.patent.data.csv")

#The code here is a bit messy because of how the colnames were randomly assigned, 
#but they are assigned at the end. Though not the most aesthetic, these colnames
#allowed the distinction between the many data-frames in the envir. 
          
################################################################################         
          
#================ Cleaning Location & Assignee Data ============================
          
#Now begin to link location to the patent ID. We must use two different
#metadata table assosciate with the assignee_id

setwd(work.d) #Go back to the root directory

#Read the location metadata file (Metadata table for many-to-many relationships)
location.metadata <- read.delim("location_assignee.tsv") #Read as delim (table)
  location.metadata[complete.cases(location.metadata), ] #Drop the NULL rows

patent.metadata <- read.delim("patent_assignee.tsv") #Read as delim (table)
  patent.metadata[complete.cases(location.metadata), ] #Drop the NULL rows

#The tables are too large for our working memory to handle a merge
#Take a random sample of 100,000 from the location and patent metadate tables
#Merge these two together, then eliminate rows that do not match together

sample.patent <- patent.metadata[sample(1:nrow(patent.metadata), 100000,
                                        replace=FALSE),]

sample.location <- location.metadata[sample(1:nrow(location.metadata), 100000,
                                            replace=FALSE),]

#Merge the two frames
patent.id.location <- merge(sample.location, sample.patent, by = "assignee_id")

#Set path to clean data
setwd(path.cd)

#Read the main database
main.patentdb <- read.csv(" clean.patent.data.csv")

#Drop the unrequired collumn
main.patentdb$X <- NULL

#take a random sample of the main database as well
sample.main <- main.patentdb[sample(1:nrow(main.patentdb), 100000,
                                    replace=FALSE),]

#================ Merging the data frames ======================================

#merge the two databases to form the final database
final.db <- merge(patent.id.location, sample.main, by.x = "patent_id",
                                                   by.y = "clean.ipcr.patent_id")

final.db$X <- NULL #Drop the unrequired collumn

#Convert to a character string to remove strings we don't need
final.db$clean.ipcr.section <- as.character(final.db$clean.ipcr.section)

#Remove rows containg strings A,D or E in section data

clean.final <- final.db[!final.db$clean.ipcr.section == "A", ]
gc() #clear memory
clean.final2 <- clean.final[!clean.final$clean.ipcr.section == "D", ]
gc() #clear memory
clean.final3 <- clean.final2[!clean.final2$clean.ipcr.section == "E", ]

clean.final3$X <- NULL #Drop the unrequired collumn

#Convert the actiondate collum from a factor to a date
clean.final3$clean.ipcr.action_date <- as.Date(clean.final3$clean.ipcr.action_date)

#Order the action date in asencding order
clean.final4 <- clean.final3[order(clean.final3$clean.ipcr.action_date,
                                   decreasing = FALSE),]

#Remove within selected date range
complete.db <- clean.final4[clean.final4$clean.ipcr.action_date >= "2006-01-01"
             & clean.final4$clean.ipcr.action_date <= "2015-12-31",]

complete.db$X <- NULL #Remove the unwanted collumn

#Correct the collumn names
names <- c("patent_id", "assignee_id", "location_id", "section", "actiondate")

colnames(complete.db) <- names

#Write to disk
write.csv(complete.db, "clean.patent.db.csv")
##############################################################################
