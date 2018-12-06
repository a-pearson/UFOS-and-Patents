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
