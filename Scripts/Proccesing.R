
setwd("~/GitHub/FakeNewsExperiment")

## Experiment 2 - Belief in fake news


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
              "vcd", "plyr", "ggannotate","scales", "fastDummies","gt", "MASS", "FindIt","modelsummary")
ipak(packages)


# READ DF

df <-readRDS("Data/FNDF.RDS")

# create true headlines scores

Tmp1 <-df%>%
  dplyr::select(E2TC_1, E2TC_9, E2TC_12, E2T1a_1, E2T1a_6, E2T1a_7, E2T1b_1, E2T1b_6, E2T1b_7, E2T1c_1, E2T1c_5, E2T1c_7,
                E2T1d_1, E2T1d_5, E2T1d_7, E2T2a_1, E2T2a_6, E2T2a_7, E2T2b_1, E2T2b_6, E2T2b_7, E2T2b_11, E2T2b_12, E2T2c_1,
                E2T2c_6, E2T2c_7)


Tmp1<-ifelse(Tmp1==1, 1, 0)
Tmp1 <-as.data.frame(Tmp1)

Tmp1[is.na(Tmp1)] <- 0
Tmp1$SumTrue <-rowSums(Tmp1)

df$SumTrue<-Tmp1$SumTrue

table(df$SumTrue)
rm(Tmp1)

#Create false headlines scores

Tmp2 <-df%>%
  dplyr::select(E2TC_2:E2TC_8, E2TC_10, E2TC_11, E2T1a_2:E2T1a_5, E2T1b_2:E2T1b_5, E2T1c_2:E2T1c_4, E2T1c_6, E2T1d_2:E2T1d_4,
                E2T1d_6, E2T2a_2:E2T2a_5, E2T2b_2:E2T2b_5, E2T2b_8, E2T2b_9, E2T2b_10, E2T2c_2:E2T2c_5)

Tmp2<-ifelse(Tmp2==1, 1, 0)
Tmp2<-as.data.frame(Tmp2)

Tmp2[is.na(Tmp2)] <- 0
Tmp2$SumFalse<- rowSums(Tmp2)

df$SumFalse<-Tmp2$SumFalse

table(df$SumFalse)

rm(Tmp2)


### create Echochambers*ideology interaction

df$ECHI <-as.factor(ifelse((df$HomoIndex == 1 & df$ideologia == "Izquierda"),'HighECH_Left',
                                    ifelse((df$HomoIndex == 1 & df$ideologia == "Derecha"), 'HighECH_Right',
                                    ifelse((df$HomoIndex == 1 & df$ideologia == "Ninguno"), 'HighECH_Without',
                                    ifelse((df$HomoIndex == 1 & df$ideologia == "centro"), 'HighECH_Center',
                                    ifelse((df$HomoIndex == 0 & df$ideologia == "Izquierda"), 'LowECH_Left',
                                    ifelse((df$HomoIndex == 0 & df$ideologia == "Derecha"), 'LowECH_Right',
                                    ifelse((df$HomoIndex == 0 & df$ideologia == "Ninguno"), 'LowECH_Without',
                                    ifelse((df$HomoIndex == 0 & df$ideologia == "centro"), 'LowECH_Center',NA)))))))))

table(df$ECHI)
# make some dummies for regression tables

df$SexDum <- if_else(df$GenRecod == "Femenino",
                     true = 1, false = 0)

df <-fastDummies::dummy_cols(df, select_columns = 'Age')
df <-fastDummies::dummy_cols(df, select_columns = 'Educ')
df <-fastDummies::dummy_cols(df, select_columns = 'E2Treat')
df <-fastDummies::dummy_cols(df, select_columns = 'ideologia')
df <-fastDummies::dummy_cols(df, select_columns = 'NivEco')
df <-fastDummies::dummy_cols(df, select_columns = 'ECHI')
df <-dplyr::select(df, -Age_1, -Educ_1, -E2Treat_Control, -ideologia_centro, -NivEco_1, -ECHI_LowECH_Left,
                   -ECHI_LowECH_Right, -ECHI_LowECH_Without, -ECHI_LowECH_Center, -ECHI_HighECH_Center)

