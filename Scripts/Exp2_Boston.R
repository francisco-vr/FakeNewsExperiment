## Experiment 2 - Belief in fake news

remotes::install_github("mattcowgill/ggannotate")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse","dplyr", "patchwork","kableExtra","MASS","ggpubr","fastDummies")
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
df <-dplyr::select(df, -Age_1, -Educ_1, -E2Treat_Control, -ideologia_centro)


# Regresion with balanced covariates

df$SC0 <-as.numeric(df$SC0)

Model1 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto, data = df)
Model2 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex, data = df)
Model3 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex, data = df)
Model4 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + Age_2 + Age_3 + Age_4,
              data = df)
Model5 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + Age_2 + Age_3 + Age_4 + Educ_2
             + Educ_3 + Educ_4 + Educ_5, data = df)
Model6 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + Age_2 + Age_3 + Age_4 + Educ_2
             + Educ_3 + Educ_4 + Educ_5 + ideologia_Izquierda + ideologia_Derecha + ideologia_Ninguno,
             data = df)

Reg1Bal <-stargazer::stargazer(Model1, Model2, Model3)
Reg1Bal


# Regresion with unbalanced covariates


Reg1NoBa <-modelsummary::msummary(list(Model1, Model2, Model3, Model4, Model5, Model6), statistic = 'p.value', stars = TRUE)
Reg1NoBa

