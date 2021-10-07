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
library(gridGraphics)
library(ggpubr)
library(ggrepel)
library(patchwork)
library(ggthemes)
# library(plotly)

# data summaries
library(Hmisc)
# library(gtsummary)

# unit tests
library(testthat) 

# trait analysis & functional analysis
library(ade4) 
library(vegan) 
library(FD)
library(ade4)
library(adegraphics)

# convert R output to Latex output
library(xtable) 

# simulation
library(truncnorm)

# source script with used functions
source(file = file.path(".", "R", "used_functions.R"))

# intermediate data
data_cache <- "./Cache"

# data output path
data_out <- "./Output"

# output for paper
data_paper <- "/home/kunzst/Dokumente/Projects/Trait_DB/Invertebrate_traits/Paper/Submission_new/Figures"

# to r scripts
path_src <- "./R"
