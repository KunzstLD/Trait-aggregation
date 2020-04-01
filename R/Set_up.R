# libraries
library(dplyr) # basic data transformation
library(purrr)
library(data.table) # data transformation
library(ggplot2) # plotting
library(ggsci) # plotting
library(cowplot) # arranging plots
library(xtable) # convert R output to Latex output
library(Hmisc)
library(readxl)
library(forcats)
library(testthat) # unit tests
library(plotly) # interactive graphics
library(zeallot) # %<-% operator
library(ade4) # trait analysis
library(vegan) # trait analysis

# source script with used functions
source(file = file.path(".", "R", "used_functions.R"))

# data output path
data_out <- "./Output"