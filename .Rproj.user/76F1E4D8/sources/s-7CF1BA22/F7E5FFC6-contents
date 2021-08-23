##########################################################
##                      ANALYSIS                        ##
##########################################################

#descriptive statistics
stat.desc(cleandf)
table(cleandf$level)
table(cleandf$league)
table(cleandf$gender)
table(cleandf$country)
table(cleandf$race)
table(cleandf$education)


#correlation matrix
cormat <- cleandf %>% select(3, 8, 11, 12, 14, 15, 22, 25, 27, 30)

source("http://www.sthda.com/upload/rquery_cormat.r")

rquery.cormat(cormat, type="upper")

#z transform predictor variables
cleandf$zCSR <- (cleandf$currentsr - mean(cleandf$currentsr))/sd(cleandf$currentsr)

cleandf$zAGE <- (cleandf$age - mean(cleandf$age))/sd(cleandf$age)

cleandf$zSES <- (cleandf$SES - mean(cleandf$SES))/sd(cleandf$SES)

cleandf$zEDU <- (cleandf$educationcode - mean(cleandf$educationcode))/sd(cleandf$educationcode)

cleandf$zTH <- (cleandf$totalhours - mean(cleandf$totalhours))/sd(cleandf$totalhours)

cleandf$zWH <- (cleandf$weeklyhours - mean(cleandf$weeklyhours))/sd(cleandf$weeklyhours)

#write cleandf
write.table(cleandf, file = paste(outputFolder, "cleanData.csv", sep = "/"), sep = ",", row.names = FALSE)

#proficiency evidence regression
PROFlm <- lm(currentsr ~ zTH + level + league, data=cleandf)

summary(PROFlm)


## CURRENT SR ##
################

#updating regression
UPDlm <- lm(updatingaccuracy ~ zCSR + zAGE + zSES + zEDU, data=cleandf)

summary(UPDlm)

#inhibition regression
INHlm <- lm(RTinterference ~ zCSR + zAGE + zSES + zEDU, data=cleandf)

summary(INHlm)

#shifting regression
SHIlm <- lm(switchCosts ~ zCSR + zAGE + zSES + zEDU, data=cleandf)

summary(SHIlm)


## TOTAL HOURS ##
#################

#updating regression
THUPDlm <- lm(updatingaccuracy ~ zTH + zAGE + zSES + zEDU, data=cleandf)

summary(THUPDlm)

#inhibition regression
THINHlm <- lm(RTinterference ~ zTH + zAGE + zSES + zEDU, data=cleandf)

summary(THINHlm)

#shifting regression
THSHIlm <- lm(switchCosts ~ zTH + zAGE + zSES + zEDU, data=cleandf)

summary(THSHIlm)


#subset different classes
tankdf <- subset(cleandf, class == "Tank")
write.table(tankdf, file = paste(outputFolder, "tankData.csv", sep = "/"), sep = ",", row.names = FALSE)

supportdf <- subset(cleandf, class == "Support")
write.table(supportdf, file = paste(outputFolder, "supportData.csv", sep = "/"), sep = ",", row.names = FALSE)

damagedf <- subset(cleandf, class == "Damage")
write.table(damagedf, file = paste(outputFolder, "damageData.csv", sep = "/"), sep = ",", row.names = FALSE)

## DAMAGE ##
############

#updating regression
DUPDlm <- lm(updatingaccuracy ~ zCSR + zAGE + zSES + zEDU, data=damagedf)

summary(DUPDlm)

#inhibition regression
DINHlm <- lm(RTinterference ~ zCSR + zAGE + zSES + zEDU, data=damagedf)

summary(DINHlm)

#shifting regression
DSHIlm <- lm(switchCosts ~ zCSR + zAGE + zSES + zEDU, data=damagedf)

summary(DSHIlm)

## SUPPORT ##
#############

#updating regression
SUPDlm <- lm(updatingaccuracy ~ zCSR + zAGE + zSES + zEDU, data=supportdf)

summary(SUPDlm)

#inhibition regression
SINHlm <- lm(RTinterference ~ zCSR + zAGE + zSES + zEDU, data=supportdf)

summary(SINHlm)

#shifting regression
SSHIlm <- lm(switchCosts ~ zCSR + zAGE + zSES + zEDU, data=supportdf)

summary(SSHIlm)

## TANK ##
##########

#updating regression
TUPDlm <- lm(updatingaccuracy ~ zCSR + zAGE + zSES + zEDU, data=tankdf)

summary(TUPDlm)

#inhibition regression
TINHlm <- lm(RTinterference ~ zCSR + zAGE + zSES + zEDU, data=tankdf)

summary(TINHlm)

#shifting regression
TSHIlm <- lm(switchCosts ~ zCSR + zAGE + zSES + zEDU, data=tankdf)

summary(TSHIlm)




