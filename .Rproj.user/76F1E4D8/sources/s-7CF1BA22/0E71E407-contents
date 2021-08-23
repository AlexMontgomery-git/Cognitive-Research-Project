# ------------------ RT Trimming ------------------ 
trimRTs = function(data) {
  madRTs = data %>%
    filter(score == 1 & shiftingType != "start") %>%
    group_by(userCode, shiftingType, stimulusType) %>%
    summarise(median = median(reactionTime), mad = mad(reactionTime), 
              cutoff1 = (median(reactionTime) + 3*mad(reactionTime)), cutoff2 = (median(reactionTime) - 3*mad(reactionTime)))
  
  trimmedData = inner_join(data, madRTs, by = c("userCode", "shiftingType", "stimulusType")) %>%
    filter(score == 1) %>%
    filter(reactionTime < cutoff1 & reactionTime > cutoff2) %>%
    select(-c(cutoff1, cutoff2, median, mad))
  
  return(trimmedData)
}
