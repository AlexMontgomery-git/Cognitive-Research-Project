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

#save output
stargazer(PROFlm, type="text",
          dep.var.labels=c("Current SR"),
          covariate.labels=c("Total Hours", "Level", "League"), out="Proficiency Regression.txt")


## CURRENT SR ##
################

#updating regression
UPDlm <- lm(updatingaccuracy ~ zCSR + zAGE + zSES + zEDU, data=cleandf)

summary(UPDlm)

#save output
stargazer(UPDlm, type="text",
          dep.var.labels=c("Updating Accuracy"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Current SR - Updating Regression.txt")

#inhibition regression
INHlm <- lm(RTinterference ~ zCSR + zAGE + zSES + zEDU, data=cleandf)

summary(INHlm)

#save output
stargazer(INHlm, type="text",
          dep.var.labels=c("RT Interference"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Current SR - Inhibition Regression.txt")

#shifting regression
SHIlm <- lm(switchCosts ~ zCSR + zAGE + zSES + zEDU, data=cleandf)

summary(SHIlm)

#save output
stargazer(SHIlm, type="text",
          dep.var.labels=c("Switch Costs"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Current SR - Shifting Regression.txt")

## TOTAL HOURS ##
#################

#updating regression
THUPDlm <- lm(updatingaccuracy ~ zTH + zAGE + zSES + zEDU, data=cleandf)

summary(THUPDlm)

#save output
stargazer(THUPDlm, type="text",
          dep.var.labels=c("Updating Accuracy"),
          covariate.labels=c("Total Hours", "Age", "SES", "Education"), out="Total Hours - Updating Regression.txt")

#inhibition regression
THINHlm <- lm(RTinterference ~ zTH + zAGE + zSES + zEDU, data=cleandf)

summary(THINHlm)

#save output
stargazer(THINHlm, type="text",
          dep.var.labels=c("RT Interference"),
          covariate.labels=c("Total Hours", "Age", "SES", "Education"), out="Total Hours - Inhibition Regression.txt")

#shifting regression
THSHIlm <- lm(switchCosts ~ zTH + zAGE + zSES + zEDU, data=cleandf)

summary(THSHIlm)

#save output
stargazer(THSHIlm, type="text",
          dep.var.labels=c("Switch Costs"),
          covariate.labels=c("Total Hours", "Age", "SES", "Education"), out="Total Hours - Shifting Regression.txt")


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

#save output
stargazer(DUPDlm, type="text",
          dep.var.labels=c("Updating Accuracy"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Damage - Updating Regression.txt")

#inhibition regression
DINHlm <- lm(RTinterference ~ zCSR + zAGE + zSES + zEDU, data=damagedf)

summary(DINHlm)

#save output
stargazer(DINHlm, type="text",
          dep.var.labels=c("RT Interference"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Damage - Inhibition Regression.txt")

#shifting regression
DSHIlm <- lm(switchCosts ~ zCSR + zAGE + zSES + zEDU, data=damagedf)

summary(DSHIlm)

#save output
stargazer(DSHIlm, type="text",
          dep.var.labels=c("Switch Costs"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Damage - Shifting Regression.txt")

## SUPPORT ##
#############

#updating regression
SUPDlm <- lm(updatingaccuracy ~ zCSR + zAGE + zSES + zEDU, data=supportdf)

summary(SUPDlm)

#save output
stargazer(SUPDlm, type="text",
          dep.var.labels=c("Updating Accuracy"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Support - Updating Regression.txt")

#inhibition regression
SINHlm <- lm(RTinterference ~ zCSR + zAGE + zSES + zEDU, data=supportdf)

summary(SINHlm)

#save output
stargazer(SINHlm, type="text",
          dep.var.labels=c("RT Interference"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Support - Inhibition Regression.txt")

#shifting regression
SSHIlm <- lm(switchCosts ~ zCSR + zAGE + zSES + zEDU, data=supportdf)

summary(SSHIlm)

#save output
stargazer(SSHIlm, type="text",
          dep.var.labels=c("Switch Costs"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Support - Shifting Regression.txt")

## TANK ##
##########

#updating regression
TUPDlm <- lm(updatingaccuracy ~ zCSR + zAGE + zSES + zEDU, data=tankdf)

summary(TUPDlm)

#save output
stargazer(TUPDlm, type="text",
          dep.var.labels=c("Updating Accuracy"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Tank - Updating Regression.txt")

#inhibition regression
TINHlm <- lm(RTinterference ~ zCSR + zAGE + zSES + zEDU, data=tankdf)

summary(TINHlm)

#save output
stargazer(TINHlm, type="text",
          dep.var.labels=c("RT Interference"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Tank - Inhibition Regression.txt")

#shifting regression
TSHIlm <- lm(switchCosts ~ zCSR + zAGE + zSES + zEDU, data=tankdf)

summary(TSHIlm)

#save output
stargazer(TSHIlm, type="text",
          dep.var.labels=c("Switch Costs"),
          covariate.labels=c("Current SR", "Age", "SES", "Education"), out="Tank - Shifting Regression.txt")


