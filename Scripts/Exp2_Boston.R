## Experiment 2 - Belief in fake news

remotes::install_github("mattcowgill/ggannotate")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage

packages <- c("tidyverse","dplyr","haven","ggplot2","readxl","summarytools", "patchwork","stringr",
              "tidyr","kableExtra","psych", "MASS", "foreign", "data.table","gtools","lubridate","AER",
              "xtable","pBrackets","Hmisc","ri","ggpubr", "stargazer", "Rmisc","wesanderson", "gridExtra","ggmosaic",
              "vcd", "plyr", "ggannotate","scales", "fastDummies","gt")
ipak(packages)

# READ DF

df <-readRDS("Data/DF-final.RDS")

# make some dummies

df$SexDum <- if_else(df$GenRecod == "Femenino",
                     true = 1, false = 0)

df <-fastDummies::dummy_cols(df, select_columns = 'Age')
df <-fastDummies::dummy_cols(df, select_columns = 'Educ')
df <-fastDummies::dummy_cols(df, select_columns = 'E2Treat')
df <-fastDummies::dummy_cols(df, select_columns = 'ideologia')
df <-fastDummies::dummy_cols(df, select_columns = 'NivEco')
df <-dplyr::select(df, -Age_1, -Educ_1, -E2Treat_Control, -ideologia_centro, -NivEco_1)


# Regresion with balanced covariates

df$SC0 <-as.numeric(df$SC0)

Model1 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto, data = df)
Model2 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex, data = df)
Model3 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex, data = df)
Model4 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum, data = df)
Model5 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4, data = df)
Model6 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4 + Educ_2
             + Educ_3 + Educ_4 + Educ_5, data = df)
Model7 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4 + Educ_2
             + Educ_3 + Educ_4 + Educ_5 + ideologia_Izquierda + ideologia_Derecha + ideologia_Ninguno,
             data = df)
Model8 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4 + Educ_2
             + Educ_3 + Educ_4 + Educ_5 + ideologia_Izquierda + ideologia_Derecha + ideologia_Ninguno
             + NivEco_2 + NivEco_3 + NivEco_4 + NivEco_5, data = df)

cm <-c('(Intercept)' = 'Constant', 'E2Treat_Afin' = 'T1 Like-Minded', 'E2Treat_Opuesto' = 'T2 Opposite',
       'HomoIndex' = 'High Eco Chamber', 'DigitIndex' = 'High Digital Citizenship')

cap <-'regression table with balanced variables'


Reg1Bal <-modelsummary::msummary(list(Model1, Model2, Model3),
                                 output = "gt", coef_map = cm, title = cap,
                                 starts = TRUE, statistic = 'p.value')
Reh1Bal <-Reg1Bal%>%
  tab_footnote(footnote = md("A very important variable."))

Reg1Bal

## Not balanced regression

ct <-c('(Intercept)' = 'Constant', 'E2Treat_Afin' = 'T1 Like-Minded', 'E2Treat_Opuesto' = 'T2 Opposite',
       'HomoIndex' = 'High Eco Chamber', 'DigitIndex' = 'High Digital Citizenship', 'SexDum' = 'Women', 'Age_2' = '30 to 40 years',
       'Age_3' = '41 to 65 years', 'Age_4' = '66+ years', 'Educ_2' = 'Elemental School', 'Educ_3' = 'Secondary School',
       'Educ_4' = 'Graduate', 'Educ_5' = 'Postgraduate', 'ideologia_Izquierda' = 'left-wing', 'ideologia_Derecha' = 'right-wing',
       'ideologia_Ninguno' = 'Without Ideology', 'NivEco_2' = '$224.001 - $448-000', 'NivEco_3' = '$448.001 - $1.000.000',
       'NivEco_4' = '$1.000.001 - $3.000.000', 'NivEco_5' = '$3.000,000+')

titulo <-'Regression with not-balanced variables'

Reg1NoBal <-modelsummary::msummary(list(Model1, Model2, Model3, Model4, Model5, Model6, Model7, Model8),
                                   output = "gt", title = titulo, coef_map = 'ct', Starts = TRUE)

Reg1NoBal
library(xaringanBuilder)

build_pdf("beamer_presentation/beamer_presentation.Rmd")

