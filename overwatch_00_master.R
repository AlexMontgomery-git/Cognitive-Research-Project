##########################################################
##                      PRELIMINARIES                   ##
##########################################################
set.seed(1234321)
library(tidyverse)
library(corrplot)
library(pastecs)

# clear workspace
rm(list=ls()) 


##########################################################
##                  SET WORK ENVIRONMENT                ##
##########################################################

# set general folder
workspace = getwd()

# set name of script with functions
funScript = "overwatch_funs.R"

# load functions into workspace
source(paste(workspace, funScript, sep = "/"))

# set names of folders for input and output files, set output filename
inputFolder = paste(workspace, "input", sep = "/")
outputFolder = paste(workspace, "output", sep = "/")

# set data input folder as default location of input files
setwd(inputFolder)


##########################################################
##                        PREPARE                       ##
##########################################################

# unzip and merge all data
source(paste(workspace, "overwatch_01_merge.R", sep = "/"))

# aggregate data
source(paste(workspace, "overwatch_02_aggregate.R", sep = "/"))

#combine questionnaire and task data
source(paste(workspace, "overwatch_03_combine.R", sep = "/"))

#clean dataframe
source(paste(workspace, "overwatch_04_clean.R", sep = "/"))

#run analysis
source(paste(workspace, "overwatch_05_analysis.R", sep = "/"))


###################################### add the summarys for the regressions at the bottom here

