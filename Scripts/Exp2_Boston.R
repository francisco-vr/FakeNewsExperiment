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


Reg1Bal <-stargazer::stargazer(Model1, Model2, Model3,
                               title = "Success score distinguishing fake news from real news",
                               dep.var.labels = "Success score",
                               covariate.labels = c("T1 Like-minded", "T2 Opposite", "High Eco Chamber",
                               "High Digital citizenship","Constant"),
                               star.cutoffs = c(0.05, 0.01, 0.001),
                               column.sep.width = "1pt",
                               notes.label = "Significance levels",
                               type = "html",
                               out = "beamer_presentation/beamer_presentation_files/regression_balanced.html")

## Not balanced regression


Reg1NoBal <-stargazer::stargazer(Model1, Model2, Model3, Model4, Model5, Model6, Model7, Model8,
                                 title = "Success score distinguishing fake news from real news. Non-balanced variables",
                                 dep.var.labels = "Success score",
                                 covariate.labels = c("T1 Like-minded", "T2 Opposite", "High Eco Chamber",
                                                      "High Digital citizenship","Female", "30 to 40 years", "41 to 65 years",
                                                      "66+ years", "Elemental School", "Secondary School", "Graduate",
                                                      "Postgraduate","Left-wing", "Right-wing","Without Ideology", "$224.001 - $448-000",
                                                      "$448.001 - $1.000.000", "$1.000.001 - $3.000.000", "$3.000,000+","Constant"),
                                 star.cutoffs = c(0.05, 0.01, 0.001),
                                 column.sep.width = "1pt",
                                 notes.label = "Significance levels",
                                 type = "html",
                                 out = "beamer_presentation/beamer_presentation_files/regression-non-balanced.html")

#Baseline Variables: T0-Control Group, Low Eco Chamber membership and Digital Citizenship,
#\n Gender: Men, Age: 18-29 years, Education: No-Education, Ideology: Center, Income: >$224.001

library(xaringanBuilder)

build_pdf("beamer_presentation/beamer_presentation.Rmd")

