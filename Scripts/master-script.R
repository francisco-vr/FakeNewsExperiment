########################################################################################################
### Master Script                                                                                ####          
### Septiember 2021                                                                                 ####
### Francisco Villarroel                                                                            ####
### Thesis Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la        ####
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo. ####
########################################################################################################

# Load packages

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse","dplyr","haven","ggplot2","readxl","summarytools", "patchwork","stringr",
              "tidyr","kableExtra","psych", "MASS", "foreign","wesanderson","ggpubr", "Rmisc",
              "data,table","gtools","lubridate","AER","xtable","pBrackets","Hmisc","ri","ggpubr")
ipak(packages)


# Part 1: processing data frame 

source("Scripts/processing.R")

# Part 2: Creating Tables and other outcomes

source("Scripts/analysis.R")


# Part 3: Creating Graphics

source("Scripts/graphics.R")


rm(list=ls())

