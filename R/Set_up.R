# libraries
# data transformation & data handling
library(dplyr) 
library(data.table)
library(tidyr)
library(broom)
library(forcats)
library(purrr)
library(readxl)
library(zeallot)

# plotting & arranging plots
library(ggplot2) 
library(ggsci) 
library(cowplot)  
# library(plotly)

# data summaries
library(Hmisc)
# library(gtsummary)

# unit tests
library(testthat) 

# trait analysis
# library(ade4) 
# library(vegan) 

# convert R output to Latex output
# library(xtable) 

# simulation
library(truncnorm)

# source script with used functions
source(file = file.path(".", "R", "used_functions.R"))

# data output path
data_out <- "./Output"

# output for paper
data_paper <- "/home/kunz/Dokumente/Projects/Trait_DB/Invertebrate_traits/Paper/Figures"