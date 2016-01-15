# load("CCSS_exampledat.rda") ##the true BOLD data
load("CCSS_exampledat.RData")
source("my_functions.R",verbose=F)
library(shiny,verbose=F)
library(rCharts,verbose=F) #for dPlot
library(data.table,verbose=F) #for summary data
library(reshape2,verbose=F) #for summary data
library(SKAT,verbose=F) #for manhattan plot
library(glmnet,verbose=F) #for penalized regression\
library(MASS,verbose=F) #for glm.nb
library(rjson,verbose=F)

