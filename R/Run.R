# __________________________________________________________________________________________________
# Scripts to run analysis ----
# Follow this script along to reproduce the analysis
# __________________________________________________________________________________________________

# Packages, paths and functions ----
# This script needs must run in the beginning to reproduce the analysis
source("./R/Set_up.R")



# Preprocessing comparison of trait aggregation ----
# This scripts needs to be run before the comparison of aggregated and assigned traits
source("./R/01_preprocessing_trait_agg.R")

## Completeness of information & taxonomic coverage ----
# Contains R-Code to generate table 2 and table 3 in the publication
source("./R/01_01_basic_analysis_trait_data.R")



# Comparison between aggregation methods ----
# Results not used in the paper 
# source("./R/02_comparing_agg_methods.R")
# source("./R/02_01_analyse_agg_methods.R")
# source("./R/02_02_check_deviating_examples.R")



# Comparison traits aggregated to family level with traits assigned at family level ----

## Australia ----
source("./R/03_comparing_species_traits_family_assigned_traits.R")

## North America ----
source("./R/04_comparing_species_traits_family_assigned_traits_NOA.R")



# Re-analysis Szöcs et al. 2014 ----
# requires the following scripts :
source("./R/Reanalysis_salinization/01_data_processing.R")
# For graphical display of species and site scores (Figure 4 in the publication) 
# script 03_plots_species_scores_comp.R is required (needs to be uncommented in the following script)  
source("./R/Reanalysis_salinization/02_trait_response_aggregated.R")

## FD analysis ----
# data preparation & analysis
source("./R/Reanalysis_salinization/04_FD.R")
# Analyse results
source("./R/Reanalysis_salinization/04_01_FD_analysis.R")

## Fourth corner analysis ----
source("./R/Reanalysis_salinization/05_fourth_corner_analysis.R")