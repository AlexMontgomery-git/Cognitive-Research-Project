##########################################################
##                      CLEAN                           ##
##########################################################

#remove participants with missing data
preclean <- na.omit(preclean)

#ensure all numeric columns are numeric
preclean$Q17 <- as.numeric(preclean$Q17)
preclean$QID19 <- as.numeric(preclean$QID19)
preclean$Q2 <- as.numeric(preclean$Q2)
preclean$Q4 <- as.numeric(preclean$Q4)
preclean$Q5 <- as.numeric(preclean$Q5)
preclean$Q7 <- as.numeric(preclean$Q7)
preclean$Q8 <- as.numeric(preclean$Q8)


## REMOVE FRAUD RESPONSES ##
############################

#remove participants with reaction times lower than 150ms
cleandf <- subset(preclean, speed > 150)
cleandf <- subset(cleandf, congruent > 150)
cleandf <- subset(cleandf, incongruent > 150)

#remove participants with SR under 500 
cleandf <- subset(cleandf, Q7 > 500)

#remove participants with weekly play time more than total
cleandf <- subset(cleandf, Q4 > Q5)

#remove participants with current SR higher than peak SR
cleandf <- subset(cleandf, Q7 >= Q8)

#remove responses indicating having started playing overwatch 7 or more years ago (the game is less than 6 years old)
cleandf <- subset(cleandf, (Q17 - Q2) <= 6)


#rename columns
cleandf <- cleandf %>% 
  rename(
    gender = Q22,
    age = Q17,
    country = Q20,
    race = Q26,
    education = Q24,
    SES = QID19,
    ageoverwatch = Q2,
    accounts = Q3,
    totalhours = Q4,
    weeklyhours = Q5,
    class = Q6,
    peaksr = Q7,
    currentsr = Q8,
    team = Q9,
    aids = Q10,
    level = Q16,
    league = Q17.1
  )

#remove specifically examined participants (removing participants with periods of rapid responses less than 50ms) and remove missing data (i.e. prefer not to say) - for loop does not work
cleandf <- subset(cleandf, userCode != 137329)
cleandf <- subset(cleandf, userCode != 138216)
cleandf <- subset(cleandf, userCode != 138241)
cleandf <- subset(cleandf, userCode != 138290)
cleandf <- subset(cleandf, userCode != 138368)
cleandf <- subset(cleandf, userCode != 138380)
cleandf <- subset(cleandf, userCode != 138520)
cleandf <- subset(cleandf, userCode != 138563)
cleandf <- subset(cleandf, userCode != 138694)
cleandf <- subset(cleandf, userCode != 138695)


#calculate mahalanobis distance
MD <- mahalanobis(cleandf[, c(22, 25, 27)], colMeans(cleandf[,c(22, 25, 27)]), cov(cleandf[, c(22, 25, 27)]))

#add mahalanobis distance to the data frame
cleandf$MD <- round(MD, 3)

#set threshhold to 10
cleandf$outlier_maha <- FALSE

cleandf$outlier_maha[cleandf$MD > 10] <- TRUE

#remove outliers
cleandf <- subset(cleandf, outlier_maha != TRUE)

#rank order education
cleandf$educationcode <- as.numeric(factor(cleandf$education, levels = c("No schooling completed", "Less than a high school diploma (GCSEs)", "High school degree or equivalent (A levels)", "Bachelor's degree", "Master's degree", "Doctorate degree")))


