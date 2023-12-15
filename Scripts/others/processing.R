########################################################################################################
### Manipulation data                                                                               ####          
### Septiember 2021                                                                                 ####
### Francisco Villarroel                                                                            ####
### Thesis Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la        ####
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo. ####
########################################################################################################

#Load Packages

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
              "vcd", "plyr", "ordinal")
ipak(packages)




## Load data ##

thesis <-read.csv("Data/InputData/test5.csv", na.strings=c("","NA"))


# select just the valid surveys

thesis <-thesis%>%
  dplyr::slice(-c(1,2))

##delete time duration outliers
mean_duration <-mean(as.numeric(thesis$Duration..in.seconds.))
sd_duration <-sd(as.numeric(thesis$Duration..in.seconds.))

threeshold <- sd_duration*3


thesis <-filter(thesis, Consent=='1')%>%
  dplyr::filter(Finished=="1" & abs(as.numeric(Duration..in.seconds.)) < threeshold)


thesis <-dplyr::select(thesis, Age:SC0)



## Create three new variables: Homophily index, digital citizenship and political position

# Eliminate incomplete surveys

thesis <-thesis%>%
  drop_na("DigiCit_14")

# Homophily Index

thesis<- thesis%>% 
  mutate_at(c(6:26), as.numeric)

thesis$count <-rowSums(thesis[6:12], na.rm = T)

thesis$HomoIndex <-ifelse(thesis$count<=39,0,1)


#Digital CitizenShip

thesis$DigiCount <-rowSums(thesis[13:26], na.rm = T)
thesis$DigitIndex <-ifelse(thesis$DigiCount<=62,0,1)

table(thesis$HomoIndex)
table(thesis$DigitIndex)


### Recode personal atributes ####

## Age ##

thesis <-thesis%>%
  drop_na("Age")


thesis <-mutate(thesis, AgeRecod = dplyr::recode(thesis$Age, "1" = "18 a 29 años","2" = "30 a 40 años","3" = "41 a 65 años",
                                                 "4" = "+66 años"))
table(thesis$AgeRecod)

## Education ##

thesis <-mutate(thesis, EducRec = dplyr::recode(thesis$Educ, "1" = "Sin Obligatoria","2" = "Sin Obligatoria","3" = "Obligatoria",
                                                 "4" = "Superior", "5" = "Postgrado"))
table(thesis$EducRec)

## Income ##

thesis <-mutate(thesis, IncomeRecod = dplyr::recode(thesis$NivEco, "1" = "Menos de $224.000", "2" = "Entre $224.001 - $448.000",
                                                 "3" = "Ente $448.001 y $1.000.000", "4" = "Entre $1.000.001 - $3.000.000","5" = "Más de $3.000.000"))
table(thesis$IncomeRecod)

## Genre ##

thesis <-mutate(thesis, GenRecod = dplyr::recode(thesis$Genre, "1" = "Masculino", "2" = "Femenino", "3" = "Otro"))
table(thesis$GenRecod)

# agrupated genres: masculines vs "others" (femenines + others)

# Agrupar "Masculino" y "Femenino" como una categoría y convertir en dicotómica
thesis$GenReBinary <- ifelse(thesis$GenRecod %in% c("Otro", "Femenino"), "Otro", "Masculino")


## Political position ##

thesis <-thesis%>%
  drop_na("IdePol")

# recode self position

thesis <-mutate(thesis, IdeoRec = car::recode(thesis$Ideolo_1, "1:4 = 1; 5:6 = 2; 7:9 = 3; 0 = 3")) 

# Edit in final DF. "20" shouldn't exist
thesis <-mutate(thesis, IdeoRec = dplyr::recode(thesis$IdeoRec, "1" = "Izquierda","2" = "Centro","3" = "Derecha"))
table(thesis$IdeoRec)

thesis<-mutate(thesis, identity = dplyr::recode(thesis$IdePol, "0" = "Ninguno", "1" = ''))

#Merge with "no ideology" column

thesis <-mutate(thesis, ideologia = coalesce(thesis$IdeoRec, thesis$identity))

table(thesis$ideologia)

#Biding variables Experiment 1

#thesis<- thesis%>% 
#  mutate_at(c(38:42), as.numeric)

# Biding outcomes Experiment N°1

thesis <-mutate(thesis, E1 = coalesce(E1TC_1,E1T1a_1,E1T1b_1, E1T2a_1, E1T2b_1))
class(thesis$E1)
thesis$E1 <-as.numeric(thesis$E1)

table(thesis$E1)

thesis$E1Treat <- ifelse(!is.na(thesis$E1T1a_1),'Afin',
                         ifelse(!is.na(thesis$E1T1b_1),'Afin',
                               ifelse(!is.na(thesis$E1T2a_1),'Opuesto',
                                      ifelse(!is.na(thesis$E1T2b_1),'Opuesto',
                                             ifelse(!is.na(thesis$E1TC_1),'Control',NA)))))

table(thesis$E1Treat)


## Biding outcomes Experiment 2

thesis$SC0 <-as.numeric(thesis$SC0)


# Merges all treatments columns






#thesis <-thesis%>%
#  dplyr::mutate(E2Treat = ifelse(E2T1a_1:E2T1a_7, E2T1b_1:E2T1b_7, E2T1c_1E2T1d_1:E2T1d_7 != NA, 'Afin',
#                                 ifelse(E2T2a_1:E2T2a_7, E2T2b_1:E2T2b_12, E2T2c_1:E2T2c_7, E2T2 != NA, 'opuesto',
#                                        ifelse(E2TC_1:E2TC_12 != NA, 'control', NA))))

thesis$E2Treat <- ifelse(!is.na(thesis$E2T1a_DO),'Afin',
                         ifelse(!is.na(thesis$E2T1b_DO),'Afin',
                                ifelse(!is.na(thesis$E2T2c_DO),'Opuesto',
                                ifelse(!is.na(thesis$E2T1c_DO),'Afin',
                                       ifelse(!is.na(thesis$E2T1d_DO),'Afin',
                                              ifelse(!is.na(thesis$E2T2a_DO),'Opuesto',
                                                     ifelse(!is.na(thesis$E2T2b_DO),'Opuesto',
                                                            ifelse(!is.na(thesis$E2T2c_DO),'Opuesto',
                                                                   ifelse(!is.na(thesis$E2TC_DO),'Control',NA)))))))))
table(thesis$E2Treat)

thesis <-thesis%>%
  drop_na("E2Treat")


## Biding outcomes Experiment 3


# Merges all treatments columns : broke or mantain social ties

thesis$E3Treat <- ifelse(!is.na(thesis$E3T1a1_1),'Amigo-validado',
                         ifelse(!is.na(thesis$E3T1b1_1),'Amigo-validado',
                                ifelse(!is.na(thesis$E3T2a1_1),'Amigo-Misinfo',
                                       ifelse(!is.na(thesis$E3T2b1_1),'Amigo-Misinfo',
                                              ifelse(!is.na(thesis$E3T2b2),'Amigo-Misinfo',
                                              ifelse(!is.na(thesis$E3T3a1_1),'Conocido-validado',
                                                     ifelse(!is.na(thesis$E3T3b1_1),'Conocido-validado',
                                                            ifelse(!is.na(thesis$E3T4a1_1),'Conocido-Misinfo',
                                                                   ifelse(!is.na(thesis$E3T4b1_1),'Conocido-Misinfo',NA)))))))))



#Create variables by type of argument and and social tie

thesis$E3TTie <-ifelse(thesis$E3Treat=='Amigo-validado','Lazo fuerte',
                       ifelse(thesis$E3Treat=='Amigo-Misinfo','Lazo fuerte',
                              ifelse(thesis$E3Treat=='Conocido-validado', 'Lazo debil',
                                     ifelse(thesis$E3Treat=='Conocido-Misinfo', 'Lazo debil',NA))))


table(thesis$E3TTie)

thesis$E3TArg <-ifelse(thesis$E3Treat=='Amigo-validado','Arg. Validado',
                       ifelse(thesis$E3Treat=='Conocido-validado', 'Arg. Validado',
                              ifelse(thesis$E3Treat=='Amigo-Misinfo', 'Desinformación',
                                     ifelse(thesis$E3Treat=='Conocido-Misinfo','Desinformación',NA))))



# Merges outcomes: brake or maintain social ties

thesis <-mutate(thesis, E3 = coalesce(thesis$E3T1a2, thesis$E3T1b2, thesis$E3T2a2, thesis$E3T2b2, thesis$E3T3a2,
                                      thesis$E3T3b2, thesis$E3T4a2, thesis$X.E3T4b2))
class(thesis$E3)
thesis$E3 <-as.numeric(thesis$E3)
table(thesis$E3)

thesis <-thesis%>%
  drop_na("E3")

# Merges emotions 

#Anger
thesis <-mutate(thesis, E3Angry = coalesce(thesis$E3T1a1_1, thesis$E3T1b1_1, thesis$E3T2a1_1, thesis$E3T2b1_1,
                                       thesis$E3T3a1_1, thesis$E3T3b1_1, thesis$E3T4a1_1, thesis$E3T4b1_1))
thesis$E3Angry <-as.numeric(thesis$E3Angry)

#Joy

thesis <-mutate(thesis, E3Joy = coalesce(thesis$E3T1a1_2, thesis$E3T1b1_2, thesis$E3T2a1_2, thesis$E3T2b1_2,
                                         thesis$E3T3a1_4, thesis$E3T3b1_2,thesis$E3T4a1_2, thesis$E3T4b1_2))
thesis$E3Joy<-as.numeric(thesis$E3Joy)

#Fear

thesis <-mutate(thesis, E3Fear = coalesce(thesis$E3T1a1_4, thesis$E3T1b1_4, thesis$E3T2a1_4, thesis$E3T2b1_4,
                                          thesis$E3T3a1_5, thesis$E3T3b1_4, thesis$E3T4a1_4, thesis$E3T4b1_4))
thesis$E3Fear<-as.numeric(thesis$E3Fear)

#Sadness

thesis <-mutate(thesis, E3Sad = coalesce(thesis$E3T1a1_5, thesis$E3T1b1_5, thesis$E3T2a1_5, thesis$E3T2b1_5,
                                         thesis$E3T3a1_6, thesis$E3T3b1_5, thesis$E3T4a1_5, thesis$E3T4b1_5))

thesis$E3Sad<-as.numeric(thesis$E3Sad)


## Biding outcomes Experiment 4
thesis$E4Treat <- ifelse(!is.na(thesis$E4T1a2),'Familia-Politico',
                         ifelse(!is.na(thesis$E4T1b2),'Familia-Politico',
                                ifelse(!is.na(thesis$E4T2a2),'Amigo-Politico',
                                       ifelse(!is.na(thesis$E4T2b2),'Amigo-Politico',
                                              ifelse(!is.na(thesis$E4T3a2),'Familia-No-Politico',
                                                            ifelse(!is.na(thesis$E4T4a2),'Amigo-No-Politico',NA))))))

# Create new variables by family/friend or polítical/non-political

thesis$E4TFam <-ifelse(thesis$E4Treat=='Familia-Politico','Familiar',
                       ifelse(thesis$E4Treat=='Familia-No-Politico','Familiar',
                              ifelse(thesis$E4Treat=='Amigo-Politico','Amigo',
                                     ifelse(thesis$E4Treat=='Amigo-No-Politico','Amigo',NA))))

thesis$E4TPol <-ifelse(thesis$E4Treat=='Familia-Politico','Politico',
                       ifelse(thesis$E4Treat=='Familia-No-Politico','No-Politico',
                              ifelse(thesis$E4Treat=='Amigo-Politico','Politico',
                                     ifelse(thesis$E4Treat=='Amigo-No-Politico','No-Politico',NA))))


# Merges all treatments columns : discuss or avoid 

thesis <-mutate(thesis, E4 = coalesce(thesis$E4T1a2, thesis$E4T1b2, thesis$E4T2a2, thesis$E4T2b2,
                                         thesis$E4T3a2,thesis$E4T4a2))

thesis$E4 <-as.numeric(thesis$E4)

thesis<-thesis%>%
  drop_na("E4")


# Merges emotions 


#anger
thesis <-mutate(thesis, E4Angry = coalesce(thesis$E4T1a1_1, thesis$E4T1b1_1, thesis$E4T2a1_1, thesis$E4T2b1_1,
                                           thesis$E4T3a1_1, thesis$E4T4a1_1))
thesis$E4Angry <-as.numeric(thesis$E4Angry)

#Joy
thesis <-mutate(thesis, E4Joy = coalesce(thesis$E4T1a1_2, thesis$E4T1b1_2, thesis$E4T2a1_2, thesis$E4T2b1_2,
                                         thesis$E4T3a1_2, thesis$E4T4a1_2))
thesis$E4Joy<-as.numeric(thesis$E4Joy)

#fear

thesis <-mutate(thesis, E4Fear = coalesce(thesis$E4T1a1_4, thesis$E4T1b1_4, thesis$E4T2a1_4, thesis$E4T2b1_4,
                                         thesis$E4T3a1_4, thesis$E4T4a1_4))
thesis$E4Fear<-as.numeric(thesis$E4Fear)

#sadness

thesis <-mutate(thesis, E4Sad = coalesce(thesis$E4T1a1_5, thesis$E4T1b1_5, thesis$E4T2a1_5, thesis$E4T2b1_5,
                                          thesis$E4T3a1_5, thesis$E4T4a1_4))

thesis$E4Sad<-as.numeric(thesis$E3Sad)

# Create digital skills from digital citizenship scale (choi et.al, 2015, 2017, etc)

digit <-thesis%>%
  dplyr::select(DigiCit_1:DigiCit_14)

digit <-lapply(digit, as.numeric)

digit <-as.data.frame(digit)

thesis$skill <-digit$DigiCit_1 + digit$DigiCit_2 + digit$DigiCit_3 #digital skills
thesis$global <-digit$DigiCit_4 + digit$DigiCit_5   # global problems consciescioness
thesis$EC <-digit$DigiCit_7 + digit$DigiCit_8 + digit$DigiCit_9 + digit$DigiCit_6  ## critic perspective
thesis$CA <-digit$DigiCit_10 + digit$DigiCit_11   #digital colective actions
thesis$DPol <-digit$DigiCit_12 + digit$DigiCit_13 + digit$DigiCit_14     #political activism


### CREATE BASE VARIABLES

thesis$ideologia <-factor(thesis$ideologia, levels = c("Izquierda", "Centro", "Derecha", "Ninguno"))
thesis$EducRec <-factor(thesis$EducRec, levels = c("Sin Obligatoria", "Obligatoria", "Superior", "Postgrado"))
thesis$AgeRecod <-factor(thesis$AgeRecod, levels = c("18 a 29 años", "30 a 40 años", "41 a 65 años", "+66 años"))
thesis$IncomeRecod <-factor(thesis$IncomeRecod, levels = c("Menos de $224.000", "Entre $224.001 - $448.000",
                                                           "Ente $448.001 y $1.000.000","Entre $1.000.001 - $3.000.000",
                                                           "Más de $3.000.000"))

thesis$GenReBinary<-relevel(factor(thesis$GenReBinary), ref = "Otro")
thesis$AgeRecod <-relevel(factor(thesis$AgeRecod), ref = "18 a 29 años")
thesis$ideologia <-relevel(factor(thesis$ideologia), ref = "Derecha")
thesis$IncomeRecod <-relevel(factor(thesis$IncomeRecod), ref = "Menos de $224.000")
thesis$EducRec <-relevel(factor(thesis$EducRec), ref = "Sin Obligatoria")
thesis$E2Treat <-relevel(factor(thesis$E2Treat), ref = "Control")

## Create new data frame

finalDF <-dplyr::select(thesis,end_4:GenReBinary,ideologia:CA)



#save DF final

saveRDS(finalDF, file = "Data/FinalData/DF-final.RDS")

## save fake news DF, with each column by headline to creat SUmFalse and SUmTrue variables

final_fn <-dplyr::select(thesis, E2TC_1:GenReBinary,E2Treat,ideologia,skill:CA)

saveRDS(final_fn, file="Data/IntermediateData/df_fakenews.RDS")


### save echo chamber psychometric properties 

echo <-thesis%>%
  dplyr::select(Homo_1:Homo_7)

saveRDS(echo, "Data/FinalData/echo_chamber_psy.RDS")






