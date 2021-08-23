##########################################################
##                      COMBINE                         ##
##########################################################

#load the data from the questionnaire
rawq = read.csv("questionnaire/overwatch_project.csv")

#delete unneeded columns
cleanq <- rawq %>% select(7, 18:32, 58:60)  

#delete unneeded rows
cleanq <- cleanq[-c(1, 2),]

#delete incomplete forms
cleanq <- cleanq[!(cleanq$Finished == FALSE),]

#rename sessiontoken to match aggShiftingData
colnames(cleanq)[colnames(cleanq) == "sessiontoken"] <- "sessionToken"

#combine data from questionnaires and tasks
df <- merge(cleanq, aggInhibitionData, by = "sessionToken", all = TRUE)
df1 <- merge(df, aggShiftingData, by = "sessionToken", all = TRUE)
preclean <- merge(df1, aggUpdatingData, by = "sessionToken", all = TRUE)

preclean <- preclean[, !(names(preclean) %in% c("userCode.y", "userCode.x"))]
