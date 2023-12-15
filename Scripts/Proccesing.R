
#setwd("~/GitHub/FakeNewsExperiment")

## Experiment 2 - Belief in fake news. proccesing to SumTrue and SumFalse


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
              "vcd", "plyr","scales", "fastDummies", "MASS", "FindIt", "sjPlot")
ipak(packages)


# READ DF

df <-readRDS("Data/IntermediateData/df_fakenews.RDS")

# create true headlines scores

Tmp1 <-df%>%
  dplyr::select(E2TC_1, E2TC_9, E2TC_12, E2T1a_1, E2T1a_6, E2T1a_7, E2T1b_1, E2T1b_6, E2T1b_7, E2T1c_1, E2T1c_5, E2T1c_7,
                E2T1d_1, E2T1d_5, E2T1d_7, E2T2a_1, E2T2a_6, E2T2a_7, E2T2b_1, E2T2b_6, E2T2b_7, E2T2b_11, E2T2b_12, E2T2c_1,
                E2T2c_6, E2T2c_7)

Tmp1<-ifelse(Tmp1==2, 1,
             ifelse(Tmp1==1,0,NA))

Tmp1 <-as.data.frame(Tmp1)

Tmp1$SumTrue <-rowSums(Tmp1, na.rm = T)
Tmp1$cero <-rowSums(Tmp1 == 0, na.rm = T)

Tmp1$percent_true <-as.numeric(round(Tmp1$SumTrue/rowSums(Tmp1[,c("SumTrue","cero")])*100,2))

df$SumTrue <-Tmp1$SumTrue
df$percent_true <-Tmp1$percent_true

#Create false headlines scores

Tmp2 <-df%>%
  dplyr::select(E2TC_2:E2TC_8, E2TC_10, E2TC_11, E2T1a_2:E2T1a_5, E2T1b_2:E2T1b_5, E2T1c_2:E2T1c_4, E2T1c_6, E2T1d_2:E2T1d_4,
                E2T1d_6, E2T2a_2:E2T2a_5, E2T2b_2:E2T2b_5, E2T2b_8, E2T2b_9, E2T2b_10, E2T2c_2:E2T2c_5)

Tmp2<-ifelse(Tmp2==2, 1,
             ifelse(Tmp2==1,0,NA))

Tmp2 <-as.data.frame(Tmp2)

Tmp2$SumFalse <-rowSums(Tmp2, na.rm = T)
Tmp2$cero <-rowSums(Tmp2 == 0, na.rm = T)

Tmp2$percent_false <-as.numeric(round(Tmp2$SumFalse/rowSums(Tmp2[,c("SumFalse","cero")])*100,2))


df$SumFalse <-Tmp2$SumFalse
df$percent_false <-Tmp2$percent_false


## save data frame with sumTrue and SumFalse percentages


saveRDS(df, file = "Data/FinalData/final_fakenews.RDS")


