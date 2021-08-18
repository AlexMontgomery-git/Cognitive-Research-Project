##########################################################
##                         MERGE                        ##
##########################################################

# get the file names 
fileNames = list.files(paste(inputFolder, "tasks", sep = "/"), full.names = TRUE)

# read files
data = lapply(fileNames, read.csv, header = TRUE, sep = ",", stringsAsFactors = FALSE)

# merge list of data.frames into single data.frame
data = bind_rows(data)

# write data
write.table(data, file = paste(outputFolder, "mergedData.csv", sep = "/"), sep = ",", row.names = FALSE)


