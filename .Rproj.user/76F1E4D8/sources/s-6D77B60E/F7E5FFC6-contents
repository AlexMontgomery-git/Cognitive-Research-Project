##########################################################
##                      ANALYSIS                        ##
##########################################################

#descriptive statistics
stat.desc(cleandf)
table(cleandf$gender)


#correlation matrix
cormat <- cleandf %>% select(3, 8, 9, 11, 12, 14, 15, 22, 25, 27, 30)

source("http://www.sthda.com/upload/rquery_cormat.r")

rquery.cormat(cormat, type="upper")

#z transform predictor variables
cleandf$zCSR <- (cleandf$currentsr - mean(cleandf$currentsr))/sd(cleandf$currentsr)

cleandf$zAGE <- (cleandf$age - mean(cleandf$age))/sd(cleandf$age)

cleandf$zSES <- (cleandf$SES - mean(cleandf$SES))/sd(cleandf$SES)

cleandf$zEDU <- (cleandf$educationcode - mean(cleandf$educationcode))/sd(cleandf$educationcode)

cleandf$zTH <- (cleandf$totalhours - mean(cleandf$totalhours))/sd(cleandf$totalhours)



#proficiency evidence regression
PROFlm <- lm(currentsr ~ zTH + level + league, data=cleandf)

summary(PROFlm)


## ALL ##
#########

#updating regression
UPDlm <- lm(updatingaccuracy ~ zCSR + zAGE + zSES + zEDU, data=cleandf)

summary(UPDlm)

#inhibition regression
INHlm <- lm(RTinterference ~ zCSR + zAGE + zSES + zEDU, data=cleandf)

summary(INHlm)

#shifting regression
SHIlm <- lm(switchCosts ~ zCSR + zAGE + zSES + zEDU, data=cleandf)

summary(SHIlm)

#standardised effect sizes or scale variables 


## WITHOUT CONTROLS ##
######################

#updating regression
NOCUPDlm <- lm(updatingaccuracy ~ zCSR, data=cleandf)

summary(NOCUPDlm)

#inhibition regression
NOCINHlm <- lm(RTinterference ~ zCSR, data=cleandf)

summary(NOCINHlm)

#shifting regression
NOCSHIlm <- lm(switchCosts ~ zCSR, data=cleandf)

summary(NOCSHIlm)


#subset different classes
tankdf <- subset(cleandf, class == "Tank")

supportdf <- subset(cleandf, class == "Support")

damagedf <- subset(cleandf, class == "Damage")

## DAMAGE ###########################################################################    Write other data
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




