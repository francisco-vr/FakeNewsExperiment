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
              "tidyr","kableExtra","psych", "MASS", "foreign", "data,table","gtools","lubridate","AER",
              "xtable","pBrackets","Hmisc","ri","ggpubr", "stargazer", "Rmisc","wesanderson", "gridExtra","ggmosaic",
              "vcd", "plyr")
ipak(packages)




## Load data ##

thesis <-read.csv("Data/InputData/test5.csv", na.strings=c("","NA") )


# select just the valid surveys

thesis <-thesis%>%
  slice(-c(1,2))

thesis <-filter(thesis, Consent=='1')%>%
  dplyr::filter(Finished=="1")

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

thesis <-mutate(thesis, EducRec = dplyr::recode(thesis$Educ, "1" = "Sin Estudios","2" = "Básica","3" = "Media",
                                                 "4" = "Superior", "5" = "Postgrado"))
table(thesis$EducRec)

## Income ##

thesis <-mutate(thesis, IncomeRecod = dplyr::recode(thesis$NivEco, "1" = "Menos de $224.000", "2" = "Entre $224.001 - $448.000",
                                                 "3" = "Ente $448.001 y $1.000.000", "4" = "Entre $1.000.001 - $3.000.000","5" = "Más de $3.000.000"))
table(thesis$IncomeRecod)

## Genre ##

thesis <-mutate(thesis, GenRecod = dplyr::recode(thesis$Genre, "1" = "Masculino", "2" = "Femenino", "3" = "Otro"))
table(thesis$GenRecod)

## Political position ##

thesis <-thesis%>%
  drop_na("IdePol")

# recode self position

thesis <-mutate(thesis, IdeoRec = car::recode(thesis$Ideolo_1, "1:4 = 1; 5:6 = 2; 7:9 = 3; 0 = 3")) 

# Edit in final DF. "20" shouldn't exist
thesis <-mutate(thesis, IdeoRec = dplyr::recode(thesis$IdeoRec, "1" = "Izquierda","2" = "centro","3" = "Derecha"))
table(thesis$IdeoRec)

thesis<-mutate(thesis, identity = dplyr::recode(thesis$IdePol, "0" = "Ninguno", "1" = ''))

#Merge with "no ideology" column

thesis <-mutate(thesis, ideologia = coalesce(thesis$IdeoRec, thesis$identity))

table(thesis$Ideologia)

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

as.numeric(df$SC0)


# Merges all treatments columns

thesis$E2Treat <- ifelse(!is.na(thesis$E2T1a_1),'Afin',
                         ifelse(!is.na(thesis$E2T1b_1),'Afin',
                                ifelse(!is.na(thesis$E2T2b_2),'Opuesto',
                                ifelse(!is.na(thesis$E2T1c_1),'Afin',
                                       ifelse(!is.na(thesis$E2T1c_1),'Afin',
                                              ifelse(!is.na(thesis$E2T1d_1),'Afin',
                                              ifelse(!is.na(thesis$E2T2a_1),'Opuesto',
                                                     ifelse(!is.na(thesis$E2T2b_10),'Opuesto',
                                                     ifelse(!is.na(thesis$E2T2b_1),'Opuesto',
                                                            ifelse(!is.na(thesis$E2T2c_1),'Opuesto',
                                                                   ifelse(!is.na(thesis$E2T2b_3),'Opuesto',
                                                                   ifelse(!is.na(thesis$E2TC_12),'Control',
                                                                          ifelse(!is.na(thesis$E2TC_6),'Control',
                                                                          ifelse(!is.na(thesis$E2TC_2),'Control',NA))))))))))))))
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

## Create new data frame

finalDF <-dplyr::select(thesis,end_4:GenRecod,ideologia:E4Sad)



#save DF

saveRDS(finalDF, file = "Data/Analysis-Data/DF-final.RDS")



