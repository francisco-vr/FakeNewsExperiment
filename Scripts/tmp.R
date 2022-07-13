#######
#
# Pruebas para ir incorporando en el script de Exp2_Boston
#
###########


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

### Negative binomial model

Library(MASS)





