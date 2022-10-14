#######
#
# Pruebas para ir incorporando en el script de Exp2_Boston
#
###########
setwd("~/GitHub/FakeNewsExperiment")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage

packages <- c("tidyverse","dplyr","haven","ggplot2","readxl","summarytools", "patchwork","stringr",
              "tidyr","kableExtra","psych", "MASS", "foreign", "data.table","gtools","lubridate","AER",
              "xtable","pBrackets","Hmisc","ri2","ggpubr", "stargazer", "Rmisc","wesanderson", "gridExtra","ggmosaic",
              "vcd", "plyr", "ggannotate","scales", "fastDummies","gt", "MASS", "modelsummary")
ipak(packages)


# READ DF

df <-readRDS("Data/FNDF.RDS")



names(df)
plot(density(df$SC0))  


### vars

# make some dummies for regression tables

df$SexDum <- if_else(df$GenRecod == "Femenino",
                     true = 1, false = 0)

df <-fastDummies::dummy_cols(df, select_columns = 'Age')
df <-fastDummies::dummy_cols(df, select_columns = 'Educ')
df <-fastDummies::dummy_cols(df, select_columns = 'E2Treat')
df <-fastDummies::dummy_cols(df, select_columns = 'ideologia')
df <-fastDummies::dummy_cols(df, select_columns = 'NivEco')
df <-dplyr::select(df, -Age_1, -Educ_1, -E2Treat_Control, -ideologia_centro, -NivEco_1)


# Relevel
df$ideologia<- factor(df$ideologia, levels = c("Ninguno", "centro",  "Derecha", "Izquierda" ))
df$AgeRecod<- factor(df$AgeRecod, levels = c("18 a 29 años", "30 a 40 años", "41 a 65 años", "+66 años"))
df$IncomeRecod<- factor(df$IncomeRecod, levels = c("Menos de $224.000",  "Entre $224.001 - $448.000", "Ente $448.001 y $1.000.000", "Entre $1.000.001 - $3.000.000" , "Más de $3.000.000"))






### Reduced linear model

lm1<-lm(SC0 ~ E2Treat, data=df)
summary(lm1)

lm2<-lm(SC0 ~ E2Treat+ SexDum + Age_2 + Age_3 + Age_4 + Educ_2 + Educ_3 + Educ_4 + Educ_5 + ideologia + NivEco_2 +
      NivEco_3 + NivEco_4 + NivEco_5 + DigitIndex, data=df) # Poner ideología "ninguno" como base
summary(lm2)

lm<-lm(SC0 ~ E2Treat+ SexDum +AgeRecod + EducRec + ideologia + IncomeRecod, data=df)
summary(lm)

lm<-lm(SC0 ~E2Treat +AgeRecod  , data=df) # Base personas de menor edad
summary(lm)

lm<-lm(SC0 ~ E2Treat + DigitIndex , data=df)
summary(lm)

lm<-lm(SC0 ~ E2Treat  + ideologia , data=df) ### recodificar y poner el ninguno de base
summary(lm)

lm<-lm(SC0 ~ E2Treat  + IncomeRecod, data=df) ## recodificar base a ingreso mas bajo
summary(lm)




### False responses
summary(ol1<-polr(as.factor(SC0) ~ E2Treat+ SexDum +AgeRecod + EducRec + ideologia + IncomeRecod + DigitIndex, data = df, Hess=TRUE))
summary(ol2<-polr(as.factor(SC0) ~ E2Treat+ AgeRecod + ideologia + IncomeRecod + HomoIndex , data = df, Hess=TRUE))
summary(ol3<-polr(as.factor(SC0) ~ E2Treat+ ideologia, data = df, Hess=TRUE))
summary(ol4<-polr(as.factor(SC0) ~ E2Treat+ ideologia, data = df, Hess=TRUE))
summary(ol5<-polr(as.factor(SC0) ~ E2Treat+ IncomeRecod, data = df, Hess=TRUE))




## crosstable to echo chambers


datasummary_crosstab(HomoIndex ~ ideologia, data = df)
