# ~ -----------------------------------------------------------------------
# ProjectName: PackID
# Purpose:     Setup the Directories
# Programmer:  Jieqiong Yu
# Date:        07-13-2020
# ~ -----------------------------------------------------------------------

## setup the directories
system("mkdir 01_Background 02_Inputs 03_Outputs 04_Codes 05_Internal_Review
       06_Deliveries")


##load library
library(openxlsx)
library(plyr)
library(tidyverse)
library(randomForest)
library(caret)
library(doParallel)
library(feather)
library(xgboost)
library(data.table)
library(glmnet)
library(elasticnet)
library(ggplot2)
library(stringdist)
library(stringi)
library(stringr)
library(foreach)
