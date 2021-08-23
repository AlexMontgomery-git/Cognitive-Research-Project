##########################################################
##                      AGGREGATE                       ##
##########################################################

## TASK SWITCHING ##
####################

# include only completed sessions and exclude practice trials
shiftingData = data %>%
  filter(session.complete == 1 & (executableId %in% c("figural_single_1_1", "figural_single_2_1", "figural_mixed", "figural_single_1_2", "figural_single_2_2"))) %>%
  select(-c(extId, moduleId, sessionId, session.complete, session.condition, session.forceExit, trialNumber, responseNumber, nSubstitutions, nRepetitions, nSteps))
  
# trim data
trimmedData = trimRTs(shiftingData)

# compute switching and mixing costs
aggShiftingData = trimmedData %>%
  group_by(userCode, sessionToken, shiftingType) %>%
  summarise(reactionTime = mean(reactionTime, na.rm = TRUE)) %>%
  spread(key = shiftingType, value = reactionTime, drop = TRUE) %>%
  group_by(userCode, sessionToken) %>%
  mutate(speed = single, 
         switchCosts = switch - repetition,
         mixingCosts = repetition - speed) %>%
  select(userCode, sessionToken, speed, switchCosts, mixingCosts) %>%
  gather(measure, score, speed:mixingCosts) %>%
  unite(var, measure, sep = "_") %>%
  spread(key = var, value = score, drop = TRUE)

# write data
write.table(aggShiftingData, file = paste(outputFolder, "aggShiftingData.csv", sep = "/"), sep = ",", row.names = FALSE)



##   INHIBITION   ##  SIMON TASK  proportional RT interference scores - subtracting RT in congruent trials from incongruent and dividing by congruent RT
####################

# include only completed sessions and exclude practice trials
inhibitionData = data %>%
  filter(session.complete == 1 & (executableId %in% "simon_test")) %>%
  select(-c(extId, moduleId, sessionId, session.complete, session.condition, session.forceExit, trialNumber, responseNumber, nSubstitutions, nRepetitions, nSteps))

# trim data
trimmedInhibition = trimRTs(inhibitionData)

# compute proportional RT inference scores
aggInhibitionData = trimmedInhibition %>%
  group_by(userCode, sessionToken, stimulusType) %>%
  summarise(reactionTime = mean(reactionTime, na.rm = TRUE)) %>%
  spread(key = stimulusType, value = reactionTime, drop = TRUE) %>%
  group_by(userCode, sessionToken) %>%
  mutate(RTinterference = (incongruent - congruent)/congruent)

# write data
write.table(aggInhibitionData, file = paste(outputFolder, "aggInhibitionData.csv", sep = "/"), sep = ",", row.names = FALSE)
  


##    UPDATING    ##  DIGIT KEEP TRACK   accuracy is dependent variable (number of correctly solved items divided by total number of items)
####################

# include only completed sessions and exclude practice trials
updatingData = data %>%
  filter(session.complete == 1 & (executableId %in% "number_test")) %>%
  select(-c(extId, moduleId, sessionId, session.complete, session.condition, session.forceExit))

#compute accuracy
aggUpdatingData = updatingData %>%
  group_by(userCode, sessionToken) %>%
  summarise(updatingaccuracy = (sum(score, na.rm = TRUE))/100) 

# write data
write.table(aggUpdatingData, file = paste(outputFolder, "aggUpdatingData.csv", sep = "/"), sep = ",", row.names = FALSE)



