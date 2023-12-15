### Psychometric properties from IRBS (Kaakinen et.al, 2020) 
## Using Jose Sallam instructions: https://jmsallan.netlify.app/blog/a-workflow-for-exploratory-factor-analysis-in-r/

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse","dplyr","haven","ggplot2","readxl","summarytools", "patchwork","stringr",
              "tidyr","kableExtra","psych", "MASS", "foreign", "data.table","gtools","lubridate","AER",
              "xtable","pBrackets","Hmisc","ggpubr", "stargazer", "Rmisc","wesanderson", "gridExtra","ggmosaic",
              "vcd", "plyr", "corrr", "lavaan")
ipak

## Load data ##

eco <-readRDS("Data/echo_chamber_psy.RDS")


echo <-lapply(echo, as.numeric)
echo <-as.data.frame(echo)


### crombach alpha

alpha(echo)

#matrix correlations

cor_tb <- correlate(echo)

cor_tb |>
  rearrange() |>
  rplot(colors = c("red", "white", "blue"))

cor_mat <- cor(echo)

# Kaiser–Meyer–Olkin (KMO) 

KMO(cor_mat)

cortest.bartlett(R = cor_mat, n = 693)

## nUMBER OF FACTORS EXTRACT



