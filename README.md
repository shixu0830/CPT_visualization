### CPT_visualization
A Shiny app for hypothesis testing, penalized regression, and visualization for comparison of count data (Current Procedural Terminology codes) that are potentially rare, unbalanced, over-dispersed, and zero-inflated.
#### Download and run the following lines to generate a shiny app ####

#### (0) Install packages
install.packages("shiny",verbose=F)

install.packages("devtools");require(devtools);install_github('rCharts', 'ramnathv')

install.packages("data.table",verbose=F)

install.packages("reshape2",verbose=F)

install.packages("SKAT",verbose=F)

install.packages("glmnet",verbose=F)

install.packages("rjson",verbose=F)

#### (1) Set the working directory to the one that contains the "CPT_visualization" folder
setwd("")

#### (2) Run it!
library(shiny)

runApp("CPT_visualization")
