########################################################################################################
### Analysis Data                                                                                   ####          
### Septiember 2021                                                                                 ####
### Francisco Villarroel                                                                            ####
### df Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la            ####
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo. ####
########################################################################################################


# load DF

df <-readRDS("Data/Analysis-Data/DF-final.RDS")

### SAMPLE DESCRIPTIVES ###

dfreduct <-dplyr::select(df, AgeRecod:ideologia,HomoIndex,DigitIndex)

view(dfSummary(dfreduct))


### TESTING RANDOMIZATION ###

##################
## Experiment 1 ##
##################

# Treatment by Age #

df$AgeRecod <- ordered(df$AgeRecod, levels = c("18 a 29 años", "30 a 40 años", "41 a 65 años", "+ 66 años"))
AE1Age <-ctable(df$AgeRecod, df$E1Treat, dnn = c('Grupo etáreo', 'Tratamiento'), style = 'rmarkdown', prop = "r", useNA = "no",
                justify = "c", headings = FALSE)


# Treatment by Gender #

df$GenRecod <-ordered(df$GenRecod, levels = c("Femenino", "Masculino","Otro"))
AE1Gen <-ctable(df$GenRecod, df$E1Treat, dnn = c('Género', 'Tratamiento'), style = 'rmarkdown', prop = "r", useNA = "no",
                headings = FALSE)


# Treatment by Income #


df$IncomeRecod <-ordered(df$IncomeRecod, levels = c("Menos de $224.000","Entre $224.001 - $448.000",
                                                    "Ente $448.001 y $1.000.000","Entre $1.000.001 - $3.000.000",
                                                    "Más de $3.000.000"))
AE1Inc <-ctable(df$IncomeRecod, df$E1Treat, dnn = c('Ingreso', 'Tratamiento'), style = 'rmarkdown', prop = "r", useNA = "no",
                headings = FALSE)


# Treatment by political position #
df$ideologia <-ordered(df$ideologia, levels = c("Izquierda","Derecha","centro","Ninguno"))
AE1Pol <-ctable(df$ideologia, df$E1Treat, dnn = c('Posición Política', 'Tratamiento'), style = 'rmarkdown',
                prop = "r", useNA = "no", headings = FALSE)

##################
## Experiment 2 ##
##################


# Treatment by Age #

AE2Age <-ctable(df$AgeRecod, df$E2Treat, dnn = c('Grupo etáreo', 'Tratamiento'), style = 'rmarkdown', prop = "r",
                useNA = "no", headings = FALSE)


# Treatment by Gender #

AE2Gen <-ctable(df$GenRecod, df$E2Treat, dnn = c('Género', 'Tratamiento'), style = 'rmarkdown', prop = "r",
                useNA = "no", headings = FALSE)

# Treatment by Income #

AE2Inc <-ctable(df$IncomeRecod, df$E2Treat,dnn = c('Ingreso', 'Tratamiento'),style = 'rmarkdown', prop = "r",
                useNA = "no", headings = FALSE)

# Treatment by political position #

AE2Pol <-ctable(df$ideologia, df$E2Treat, dnn = c('Posición Política', 'Tratamiento'), style = 'rmarkdown',
                prop = "r", useNA = "no", headings = FALSE)


##################
## Experiment 3 ##
##################

# Treatment by Age #

AE3Age <-ctable(df$Age, df$E3Treat, dnn = c('Grupo etáreo', 'Tratamiento'), style = 'rmarkdown', prop = "r",
                useNA = "no", headings = FALSE)

# Treatment by Gender #

AE3Gen <-ctable(df$GenRecod, df$E3Treat, dnn = c('Género', 'Tratamiento'), style = 'rmarkdown', prop = "r",
                useNA = "no", headings = FALSE)

# Treatment by Income #

AE3Inc <-ctable(df$IncomeRecod, df$E3Treat,dnn = c('Ingreso', 'Tratamiento'), style = 'rmarkdown', prop = "r", useNA = "no",
                headings = FALSE)

# Treatment by political position #

AE3Pol <-ctable(df$ideologia, df$E3Treat, dnn = c('Posición Política', 'Tratamiento'), style = 'rmarkdown', prop = "r",
                useNA = "no", headings = FALSE)

##################
## Experiment 4 ##
##################


# Treatment by Age #

AE4Age <-ctable(df$Age, df$E4Treat, dnn = c('Grupo etáreo', 'Tratamiento'), style = 'rmarkdown', prop = "r", useNA = "no",
                headings = FALSE)


# Treatment by Gender #

AE4Gen <-ctable(df$GenRecod, df$E4Treat, dnn = c('Género', 'Tratamiento'), style = 'rmarkdown', prop = "r", useNA = "no",
                headings = FALSE)

# Treatment by Income #

AE4Inc <-ctable(df$IncomeRecod, df$E4Treat,dnn = c('Ingreso', 'Tratamiento'), style = 'rmarkdown', prop = "r",
                useNA = "no", headings = FALSE)

# Treatment by political position #

AE4Pol <-print(ctable(df$ideologia, df$E4Treat,dnn = c('Posición Política', 'Tratamiento'), style = 'rmarkdown',
                      prop = "r", useNA = "no", headings = FALSE), method = 'render')

### Diferencias entre Medias (cuadros decentes)  ###


######################
### first outcomes ###
######################

## ARREGLAR LOS GROUP_BY DE CASI TODOS (MENOS LOS DOS PRIMEROS EXPERIMENTOS) PORQUE NO FUNCIONAN

## Experiment 1 ##

detach("package:ggpubr", unload = TRUE)

q = c(.25, .5, .75)

E1 <-df%>%
  dplyr::group_by("Tratamiento" = E1Treat)%>%
  dplyr::summarize(Media = mean(E1, na.rm = T), "Desviacion Estandar" = sd(E1, na.rm = T),
                   "Quantil 25%" = quantile(E1, probs = q[1]), "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E1, caption = "Resultados descriptivos de Experimento N°1: Ira por cercanía social")

## Kruskal wallis test ##

kruskal.test(E1 ~ E1Treat, data = df)
pairwise.wilcox.test(df$E1, df$E1Treat)

E1Homo <-df%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex, "Tratamiento" =E1Treat,)%>%
  dplyr::summarize(Media = mean(E1, na.rm = T), desviacion = sd(E1, na.rm = T), "Quantil 25%" = quantile(E1, probs = q[1]),
                   "Quantil 50%" = quantile(E1, probs = q[2]), "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E1Homo, caption = "Resultados descriptivos de Experimento N°1 subdividido por niveles de Membresía a Cámaras de Eco")


E1Digit <-df%>%
  dplyr::group_by("Ciudadanía Digital"=DigitIndex,"Tratamiento"=E1Treat,)%>%
  dplyr::summarize(Media = mean(E1, na.rm = T), desviacion = sd(E1, na.rm = T), "Quantil 25%" = quantile(E1, probs = q[1]),
                   "Quantil 50%" = quantile(E1, probs = q[2]), "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E1Digit, caption = "Resultados descriptivos de Experimento N°1 subdividido por niveles de Ciudadanía Digital")


# Regresion with balanced covariates


# Regresion with unbalanced covariates


## Experiment 2 ##
df$SC0 <-as.numeric(df$SC0)

E2 <-df%>%
  dplyr::group_by("Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
            Desviacion = sd(SC0, na.rm = T),"Quantil 25%" = quantile(E1, probs = q[1]),
            "Quantil 50%" = quantile(E1, probs = q[2]),
            "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E2)


## Kruskal wallis test ##

kruskal.test(SC0 ~ E2Treat, data = df)
pairwise.wilcox.test(df$SC0, df$E2Treat)


E2Homo <-df%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex,"Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T), "Quantil 25%" = quantile(E1, probs = q[1]), "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E2Homo)




E2Digit<-df%>%
  dplyr::group_by("Ciudadanía Digital"=DigitIndex,"Tratamiento"=E2Treat)%>%
  dplyr::summarise(Media = mean(SC0, na.rm = T),
                   Desviacion = sd(SC0, na.rm = T),"Quantil 25%" = quantile(E1, probs = q[1]), "Quantil 50%" = quantile(E1, probs = q[2]),
                   "Quantil 75%" = quantile(E1, probs = q[3]))

xtable(E2Digit)



## Experiment 3 ##

## Mantain or broke ties ##

E3 <-ctable(df$E3, df$E3Treat, dnn = c('Probabilidad de romper lazos', 'Tratamiento'), prop = "c",useNA = "no",
            chisq = T, headings = TRUE, style = 'rmarkdown')

##by echo chambers

E3Homo ->ordered(df$E3, levels = c("Romper lazos", "Mantener Lazos"))%>%
  stby(data = list(x= df$E3, y = df$E3Treat),
       INDICES = df$HomoIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE)

E3Digi <-ordered(df$E3, levels = c("Romper lazos", "Mantener Lazos"))%>%
  stby(data = list(x= df$E3, y = df$E3Treat),
       INDICES = df$DigitIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE)

## by type of social tie and type of argumentation

E3Friend <-ctable(df$E3, df$E3TTie, dnn = c('Probabilidad de romper lazos', 'Tratamiento'), useNA = "no", chisq = T,
                  OR = T, RR = T, headings = FALSE, style = 'rmarkdown')

E3Arg <-ctable(df$E3, df$E3TArg, dnn = c('Probabilidad de romper lazos', 'Tratamiento'), useNA = "no", chisq = T,
               OR = T, RR = T, headings = FALSE, style = 'rmarkdown')

## subdivided by digital citizenship - social ties

levels(df$E3) <-c("Mantener Lazos", "Romper lazos")


E3FrHomo<-ordered(df$E3, levels = c("Romper lazos", "Mantener Lazos"))%>%
  stby(data = list(x= df$E3, y = df$E3TTie),
       INDICES = df$HomoIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)



E3FrDigi <-ordered(df$E3, levels = c("Mantener Lazos", "Romper lazos"))%>%
                     stby(data=list(x = df$E3, y = df$E3TTie),
                          INDICES = df$DigitIndex,
                          FUN = ctable,
                          dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
                          prop = "c",
                          chisq = TRUE,
                          OR = TRUE,
                          RR = TRUE)

## subdivided by argumentation

E3ArgHomo <-ordered(df$E3, levels = c("Romper lazos", "Mantener Lazos"))%>%
  stby(data=list(x = df$E3, y = df$E3TArg),
       INDICES = df$HomoIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)


E3ArgDigi <-ordered(df$E3, levels = c("1", "0"))%>%
  stby(data=list(x = df$E3, y = df$E3TArg),
       INDICES = df$DigitIndex,
       FUN = ctable,
       dnn = c('Probabilidad de romper lazos', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)

# Regresion model

df$E3 <-as.factor(df$E3)
E3M1 <-glm(E3 ~ E3Treat, data = df, family = "binomial")
E3M2 <-glm(E3 ~ E3Treat + HomoIndex, data = df, family = "binomial")
E3M3 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex, data = df, family = "binomial")
E3M4 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod, data = df, family = "binomial")
E3M5 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod + EducRec, data = df, family = "binomial")
E3M6 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod + EducRec + GenRecod, data = df, family = "binomial")
E3M7 <-glm(E3 ~ E3Treat + HomoIndex + DigitIndex + AgeRecod + EducRec + GenRecod + ideologia, data = df, family = "binomial")


# Logistic regression with balanced covariates

summary(E3M1)

E3ReBa <-stargazer(E3M1, E3M2, E3M3,
          title = "Probabilidad de romper lazos sociales por facebook, con covariables balanceadas",
          dep.var.caption = "Probabilidad de ruptura",
          dep.var.labels = c("Modelo 1", "Modelo 2", "Modelo 3"),
          covariate.labels = c("Amigo - Arg. validado", "Conocido- Arg. de fake News", "Conocido - Arg. validado",
                               "Pertenencia a cámaras de eco", "Ciudadanía Digital", "Constante"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          column.sep.width = "1pt",
          notes.label = "Niveles de significancia",
          type = "latex",
          out = "Results/Tables/E3-balanced.html")


# Logistic regression with unbalanced covariates

E3ReUn <-stargazer(E3M1, E3M2, E3M3,E3M4, E3M5, E3M6, E3M7,
          title = "Probabilidad de romper lazos sociales por facebook, con covariables bañanceadas y sin balancear ",
          dep.var.caption = "Probabilidad de ruptura",
          dep.var.labels = c("Modelo 1", "Modelo 2", "Modelo 3"),
          covariate.labels = c("Amigo - Arg. validado", "Conocido- Arg. de fake News", "Conocido - Arg. validado",
                               "Pertenencia a cámaras de eco", "Ciudadanía digital", "18 a 29 años", "30 a 40 años",
                               "41 a 65 años","Educación Media", "Postgrado", "Sin Estudios",
                               "Educación Superior", "Masculino", "Otros géneros", "Derecha", "Izquierda",
                               "Ninguna ideología","Constante"),
          column.sep.width = "1pt",
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes.label = "Niveles de significancia",
          type = "latex",
          out = "Results/Tables/E3-unbalanced.html")




#Emotions #

# Angry
E3Angry <-df%>%
  dplyr::group_by("Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Angry, na.rm = T), "Desviacion Estandar" = sd(E3Angry, na.rm = T),
                   "Quantil 25%" = quantile(E3Angry, probs = q[1]), "Quantil 50%" = quantile(E3Angry, probs = q[2]),
                   "Quantil 75%" = quantile(E3Angry, probs = q[3]))

xtable(E1Digit, caption = "Resultados descriptivos de Experimento N°2: Niveles de Ira según tratamiento")


E3AngryHomo <-df%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex, "Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Angry, na.rm = T), "Desviacion Estandar" = sd(E3Angry, na.rm = T),
                   "Quantil 25%" = quantile(E3Angry, probs = q[1]), "Quantil 50%" = quantile(E3Angry, probs = q[2]),
                   "Quantil 75%" = quantile(E3Angry, probs = q[3]))

E3AngryDigit <-df%>%
  dplyr::group_by("Ciud. digital"=DigitIndex,"Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Angry, na.rm = T), "Desviacion Estandar" = sd(E3Angry, na.rm = T),
                   "Quantil 25%" = quantile(E3Angry, probs = q[1]), "Quantil 50%" = quantile(E3Angry, probs = q[2]),
                   "Quantil 75%" = quantile(E3Angry, probs = q[3]))

kruskal.test(E3Angry ~ E3Treat, data = df)
pairwise.wilcox.test(df$E3Angry, df$E3Treat)

# Joy-Hapiness

E3Joy <-df%>%
  dplyr::group_by("Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Joy, na.rm = T), "Desviacion Estandar" = sd(E3Joy, na.rm = T),
                   "Quantil 25%" = quantile(E3Joy, probs = q[1]), "Quantil 50%" = quantile(E3Joy, probs = q[2]),
                   "Quantil 75%" = quantile(E3Joy, probs = q[3]))

kruskal.test(E3Joy ~ E3Treat, data = df)
pairwise.wilcox.test(df$E3Joy, df$E3Treat)

E3JoyHomo<-df%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex,"Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Joy, na.rm = T), "Desviacion Estandar" = sd(E3Joy, na.rm = T),
                   "Quantil 25%" = quantile(E3Joy, probs = q[1]), "Quantil 50%" = quantile(E3Joy, probs = q[2]),
                   "Quantil 75%" = quantile(E3Joy, probs = q[3]))

E3JoyDigit<-df%>%
  dplyr::group_by("Ciud. digital"=DigitIndex,"Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Joy, na.rm = T), "Desviacion Estandar" = sd(E3Joy, na.rm = T),
                   "Quantil 25%" = quantile(E3Joy, probs = q[1]), "Quantil 50%" = quantile(E3Joy, probs = q[2]),
                   "Quantil 75%" = quantile(E3Joy, probs = q[3]))


# sadness

E3Sad <-df%>%
  dplyr::group_by("Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Sad, na.rm = T), "Desviacion Estandar" = sd(E3Sad, na.rm = T),
                   "Quantil 25%" = quantile(E3Sad, probs = q[1]), "Quantil 50%" = quantile(E3Sad, probs = q[2]),
                   "Quantil 75%" = quantile(E3Sad, probs = q[3]))

kruskal.test(E3Sad ~ E3Treat, data = df)
pairwise.wilcox.test(df$E3Sad, df$E3Treat)

E3SadHomo <-df%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex,"Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Sad, na.rm = T), "Desviacion Estandar" = sd(E3Sad, na.rm = T),
                   "Quantil 25%" = quantile(E3Sad, probs = q[1]), "Quantil 50%" = quantile(E3Sad, probs = q[2]),
                   "Quantil 75%" = quantile(E3Sad, probs = q[3]))


E3SadDigit <-df%>%
  dplyr::group_by("Ciud. digital"=DigitIndex,"Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Sad, na.rm = T), "Desviacion Estandar" = sd(E3Sad, na.rm = T),
                   "Quantil 25%" = quantile(E3Sad, probs = q[1]), "Quantil 50%" = quantile(E3Sad, probs = q[2]),
                   "Quantil 75%" = quantile(E3Sad, probs = q[3]))

# Fear

E3Fear <-df%>%
  dplyr::group_by("Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Fear, na.rm = T), "Desviacion Estandar" = sd(E3Fear, na.rm = T),
                   "Quantil 25%" = quantile(E3Fear, probs = q[1]), "Quantil 50%" = quantile(E3Fear, probs = q[2]),
                   "Quantil 75%" = quantile(E3Fear, probs = q[3]))

kruskal.test(E3Fear ~ E3Treat, data = df)
pairwise.wilcox.test(df$E3Fear, df$E3Treat)

E3FearHomo<-df%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex,"Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Fear, na.rm = T), "Desviacion Estandar" = sd(E3Fear, na.rm = T),
                   "Quantil 25%" = quantile(E3Fear, probs = q[1]), "Quantil 50%" = quantile(E3Fear, probs = q[2]),
                   "Quantil 75%" = quantile(E3Fear, probs = q[3]))

E3FearDigit <-df%>%
  dplyr::group_by("Ciud. digital"=DigitIndex,"Tratamiento"=E3Treat)%>%
  dplyr::summarize(Media = mean(E3Fear, na.rm = T), "Desviacion Estandar" = sd(E3Fear, na.rm = T),
                   "Quantil 25%" = quantile(E3Fear, probs = q[1]), "Quantil 50%" = quantile(E3Fear, probs = q[2]),
                   "Quantil 75%" = quantile(E3Fear, probs = q[3]))
## Experiment 4 ##

# get a talk or avoid conversation

E4 <-ctable(df$E4, df$E4Treat, prop = "c", dnn = c('Probabilidad de discutir', 'tratamiento'),
            chisq = T, useNA = "no", headings = FALSE, style = 'rmarkdown')

# by tie or political/non-political

E4Fam<-ctable(df$E4, df$E4TFam, dnn = c('Discusión o evasión','Tratamiento'),chisq = T, OR=TRUE, RR=TRUE,
                                        useNA = "no", headings = FALSE, style = 'rmarkdown')

E4Pol<-ctable(df$E4, df$E4TPol, dnn = c('Discusión o evasion', 'Tratamiento'), chisq = T, OR=TRUE, RR=TRUE,
                                        useNA = "no", headings = FALSE, style = 'rmarkdown')


#subdivided by digital citizenship

levels(df$E4) <-c("Evadir Discusión", "Discutir abiertamente")


E4FamHomo<-ordered(df$E4, levels = c("1", "0"))%>%
  stby(data = list(x= df$E4, y = df$E4TFam),
       INDICES = df$HomoIndex,
       FUN = ctable,
       dnn = c('Probabilidad de discusión', 'Tratamiento'),
       prop = "c",
       chisq = TRUE,
       OR = TRUE,
       RR = TRUE)

E4FamDigi <-ordered(df$E4, levels = c("1", "0"))%>%
  stby(data = list(x= df$E4, y = df$E4TFam),
                 INDICES = df$DigitIndex,
                 FUN = ctable,
                 dnn = c('Probabilidad de discusión', 'Tratamiento'),
                 prop = "c",
                 chisq = TRUE,
                 OR = TRUE,
                 RR = TRUE)

## subdivided by echo chambers

E4PolHomo <-ordered(df$E4, levels = c("1", "0"))%>%
  stby(data = list(x= df$E4, y = df$E4TPol),
                 INDICES = df$HomoIndex,
                 FUN = ctable,
                 dnn = c('Probabilidad de discusión', 'Tratamiento'),
                 prop = "c",
                 chisq = TRUE,
                 OR = TRUE,
                 RR = TRUE)

E4PolDigi <-ordered(df$E4, levels = c("Discutir abiertamente", "Evadir Discusión"))%>%
  stby(data = list(x= df$E4, y = df$E4TPol),
                 INDICES = df$DigitIndex,
                 FUN = ctable,
                 dnn = c('Probabilidad de discusión', 'Tratamiento'),
                 prop = "C",
                 chisq = TRUE,
                 OR = TRUE,
                 RR = TRUE)


### Regression ###
E4M1 <-glm(E4 ~ E4Treat, data = df, family = "binomial")
E4M2 <-glm(E4 ~ E4Treat + HomoIndex, data = df, family = "binomial")
E4M3 <-glm(E4 ~ E4Treat + HomoIndex + DigitIndex, data = df, family = "binomial")
E4M4 <-glm(E4 ~ E4Treat + HomoIndex + DigitIndex + AgeRecod, data = df, family = "binomial")
E4M5 <-glm(E4 ~ E4Treat + HomoIndex + DigitIndex + AgeRecod + EducRec, data = df, family = "binomial")
E4M6 <-glm(E4 ~ E4Treat + HomoIndex + DigitIndex + AgeRecod + EducRec + GenRecod, data = df, family = "binomial")
E4M7 <-glm(E4 ~ E4Treat + HomoIndex + DigitIndex + AgeRecod + EducRec + GenRecod + ideologia, data = df, family = "binomial")

#regresion with balanced covariates
E4ReBa <-stargazer(E4M1, E4M2, E4M3,
          title = "Probabilidad de discutir con lazos sociales cercanos por opiniones opuestas, con covariables balanceadas",
          dep.var.caption = "Probabilidad de discutir",
          covariate.labels = c("Amigo - Tema político", "Familiar - Tema no político", "Familiar - Tema político",
                               "Pertenencia a cámaras de eco", "Ciudadanía digital", "Constante"),
          notes.label = "Niveles de significancia",
          column.sep.width = "1pt",
          type = "latex",
          out = "Results/Tables/E4-balanced.html")

#regresion with UNbalanced covariates
E4ReUn <-stargazer(E4M1, E4M2, E4M3,E4M4, E4M5, E4M6, E4M7,
          title = "Probabilidad de discutir con lazos sociales cercanos por opiniones opuestas, con covariables balanceadas y sin balancear",
          dep.var.caption = "Probabilidad de discutir",
          covariate.labels = c("Amigo - Tema político", "Familiar - Tema no político", "Familiar - Tema político",
                               "Pertenencia a cámaras de eco", "Ciudadanía digital", "18 a 29 años", "30 a 40 años",
                               "41 a 65 años","Educación Media", "Postgrado", "Sin Estudios",
                               "Educación Superior", "Masculino", "Otros géneros", "Derecha", "Izquierda",
                               "Ninguna ideología","Constante"),
          notes.label = "Niveles de significancia",
          column.sep.width = "1pt",
          type = "latex",
          out = "Results/Tables/E4-unbalanced.html")


# Emotions Experiment 4

# Angry

E4Angry <-df%>%
  dplyr::group_by("Tratamiento"=E4Treat)%>%
  dplyr::summarize(Media = mean(E4Angry, na.rm = T), "Desviacion Estandar" = sd(E4Angry, na.rm = T),
                   "Quantil 25%" = quantile(E4Angry, probs = q[1]), "Quantil 50%" = quantile(E4Angry, probs = q[2]),
                   "Quantil 75%" = quantile(E4Angry, probs = q[3]))

kruskal.test(E4Angry ~ E4Treat, data = df)
pairwise.wilcox.test(df$E4Angry, df$E4Treat)

E4AngryHomo <-df%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex, "Tratamiento"=E4Treat)%>%
  dplyr::summarize(Media = mean(E4Angry, na.rm = T), "Desviacion Estandar" = sd(E4Angry, na.rm = T),
                   "Quantil 25%" = quantile(E4Angry, probs = q[1]), "Quantil 50%" = quantile(E4Angry, probs = q[2]),
                   "Quantil 75%" = quantile(E4Angry, probs = q[3]))

E4AngryDigit <-df%>%
  dplyr::group_by("Ciud. digital"=DigitIndex,"Tratamiento"=E4Treat)%>%
  dplyr::summarize(Media = mean(E4Angry, na.rm = T), "Desviacion Estandar" = sd(E4Angry, na.rm = T),
                   "Quantil 25%" = quantile(E4Angry, probs = q[1]), "Quantil 50%" = quantile(E4Angry, probs = q[2]),
                   "Quantil 75%" = quantile(E4Angry, probs = q[3]))




# Joy-Hapiness

E4Joy <-df%>%
  dplyr::group_by("Tratamiento"=E4Treat)%>%
  dplyr::summarize(Media = mean(E4Joy, na.rm = T), "Desviacion Estandar" = sd(E4Joy, na.rm = T),
                   "Quantil 25%" = quantile(E4Joy, probs = q[1]), "Quantil 50%" = quantile(E4Joy, probs = q[2]),
                   "Quantil 75%" = quantile(E4Joy, probs = q[3]))

kruskal.test(E4Joy ~ E4Treat, data = df)
pairwise.wilcox.test(df$E4Joy, df$E4Treat)

E4JoyHomo<-df%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex,"Tratamientos"=E4Treat)%>%
  dplyr::summarize(Media = mean(E4Joy, na.rm = T), "Desviacion Estandar" = sd(E4Joy, na.rm = T),
                   "Quantil 25%" = quantile(E4Joy, probs = q[1]), "Quantil 50%" = quantile(E4Joy, probs = q[2]),
                   "Quantil 75%" = quantile(E4Joy, probs = q[3]))


E4JoyDigit<-df%>%
  dplyr::group_by("Ciud. digital"=DigitIndex,"Tratamiento"=E4Treat)%>%
  dplyr::summarize(Media = mean(E4Joy, na.rm = T), "Desviacion Estandar" = sd(E4Joy, na.rm = T),
                   "Quantil 25%" = quantile(E4Joy, probs = q[1]), "Quantil 50%" = quantile(E4Joy, probs = q[2]),
                   "Quantil 75%" = quantile(E4Joy, probs = q[3]))


# sadness

E4Sad <-df%>%
  dplyr::group_by("Tratamiento"=E4Treat)%>%
  dplyr::summarize(Media = mean(E4Sad, na.rm = T), "Desviacion Estandar" = sd(E4Sad, na.rm = T),
                   "Quantil 25%" = quantile(E4Sad, probs = q[1]), "Quantil 50%" = quantile(E4Sad, probs = q[2]),
                   "Quantil 75%" = quantile(E4Sad, probs = q[3]))



kruskal.test(E4Sad ~ E4Treat, data = df)
pairwise.wilcox.test(df$E4Sad, df$E4Treat)

E4SadHomo<-df%>%
  dplyr::group_by("Cámaras de eco"=HomoIndex,"Tratamiento" = E4Treat)%>%
  dplyr::summarize(Media = mean(E4Sad, na.rm = T), "Desviacion Estandar" = sd(E4Sad, na.rm = T),
                   "Quantil 25%" = quantile(E4Sad, probs = q[1]), "Quantil 50%" = quantile(E4Sad, probs = q[2]),
                   "Quantil 75%" = quantile(E4Sad, probs = q[3]))


E4SadDigit <-df%>%
  dplyr::group_by("Ciud. digital"=DigitIndex,"Tratamiento"=E4Treat)%>%
  dplyr::summarize(Media = mean(E4Sad, na.rm = T), "Desviacion Estandar" = sd(E4Sad, na.rm = T),
                   "Quantil 25%" = quantile(E4Sad, probs = q[1]), "Quantil 50%" = quantile(E4Sad, probs = q[2]),
                   "Quantil 75%" = quantile(E4Sad, probs = q[3]))

# Fear

E4Fear<-df%>%
  dplyr::group_by("Tratamiento"=E4Treat)%>%
  dplyr::summarize(Media = mean(E4Fear, na.rm = T), "Desviacion Estandar" = sd(E4Fear, na.rm = T),
                   "Quantil 25%" = quantile(E4Fear, probs = q[1]), "Quantil 50%" = quantile(E4Fear, probs = q[2]),
                   "Quantil 75%" = quantile(E4Fear, probs = q[3]))


kruskal.test(E4Fear ~ E4Treat, data = df)
pairwise.wilcox.test(df$E4Fear, df$E4Treat)

E4FearHomo<-df%>%
  dplyr::group_by("Cámaras de eco" = HomoIndex,"Tratamiento" = E4Treat)%>%
  dplyr::summarize(Media = mean(E4Fear, na.rm = T), "Desviacion Estandar" = sd(E4Fear, na.rm = T),
                   "Quantil 25%" = quantile(E4Fear, probs = q[1]), "Quantil 50%" = quantile(E4Fear, probs = q[2]),
                   "Quantil 75%" = quantile(E4Fear, probs = q[3]))
E4FearDigit <-df%>%
  dplyr::group_by("Ciud. digital"=DigitIndex,"Tratamiento"=E4Treat)%>%
  dplyr::summarize(Media = mean(E4Fear, na.rm = T), "Desviacion Estandar" = sd(E4Fear, na.rm = T),
                   "Quantil 25%" = quantile(E4Fear, probs = q[1]), "Quantil 50%" = quantile(E4Fear, probs = q[2]),
                   "Quantil 75%" = quantile(E4Fear, probs = q[3]))


## Create new data frame


finalDF <-saveRDS(finalDF, file = "Data/Analysis-Data/DF-final.RDS")


## Save all tables to use ##


anexo <-list(AE1Age, AE1Gen, AE1Inc, AE1Pol, AE2Age, AE2Gen, AE2Inc, AE2Pol, AE3Age, AE2Gen, AE3Inc, AE3Pol,
             AE4Age, AE4Gen, AE4Inc, AE4Pol)

TablaExperi <-list(E1, E1Homo, E1Digit,E2, E2Homo, E2Digit,E3, E3Friend, E3Arg,E3FrHomo,E3FrDigi, E3ArgHomo, E3ArgDigi,E3ReBa,E3ReUn,E4,
                   E4Fam,E4Pol, E4FamHomo,E4FamDigi,E4PolHomo, E4PolDigi,E4ReBa, E4ReUn)

TablaEmo <-list(E3Angry, E3AngryHomo, E3AngryDigit, E3Joy, E3JoyHomo, E3JoyDigit, E3Sad, E3SadHomo, E3SadDigit,
                E3Fear, E3FearHomo, E3FearDigit, E4Angry, E4AngryHomo, E4AngryDigit, E4Joy, E4JoyHomo, E4JoyDigit,
                E4Sad, E4SadHomo, E4SadDigit, E4Fear, E4FearHomo, E4FearDigit)

#testing block randomization

E1BHomo <-ctable(df$HomoIndex, df$E1Treat, dnn = c('Cámaras de Eco', 'Tratamiento'), style = 'rmarkdown', prop = "r",
                 useNA = "no", headings = FALSE)
E1BDigi <-ctable(df$DigitIndex, df$E1Treat, dnn = c('Ciudadanía Digital','Tratamiento'), style = 'rmarkdown', prop = "r",
                 useNA = "no",headings = FALSE)

E2BHomo <-ctable(df$HomoIndex, df$E2Treat,dnn = c('Cámaras de Eco', 'Tratamiento'), style = 'rmarkdown', prop = "r",useNA = "no")
E2BDigi <-ctable(df$DigitIndex, df$E2Treat, dnn = c('Ciudadanía Digital','Tratamiento'), style = 'rmarkdown', prop = "r",
                 useNA = "no",headings = FALSE)

E3BHomo <-ctable(df$HomoIndex, df$E3Treat,dnn = c('Cámaras de Eco', 'Tratamiento'), style = 'rmarkdown', prop = "r",
                 useNA = "no", headings = FALSE)
E3BDigi <-ctable(df$DigitIndex, df$E3Treat, dnn = c('Ciudadanía Digital','Tratamiento'), style = 'rmarkdown', prop = "r",
                 useNA = "no", headings = FALSE)

E4BHomo <-ctable(df$HomoIndex, df$E4Treat,dnn = c('Cámaras de Eco', 'Tratamiento'), style = 'rmarkdown', prop = "r",
                 useNA = "no", headings = FALSE)
E4BDigi <-ctable(df$DigitIndex, df$E4Treat, dnn = c('Ciudadanía Digital','Tratamiento'), style = 'rmarkdown', prop = "r",
                 useNA = "no", headings = FALSE)

tablaBlock <-list(E1BHomo,E1BDigi,E2BHomo,E2BDigi,E3BHomo,E3BDigi,E4BHomo,E4BDigi)

#Save tables

saveRDS(anexo, file="Results/Tables/Tabla-anexos.rds")
saveRDS(TablaExperi, file = "Results/Tables/Tabla-resultados-experimentos.rds")
saveRDS(TablaEmo, file = "Results/Tables/Tabla-emociones.rds")
saveRDS(tablaBlock, file = "Results/Tables/tabla-bloques.rds")

#############################



