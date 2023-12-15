## Regression models. we test lineal models and ordered logit




df <-readRDS("Data/FinalData/final_fakenews.RDS")


#OLS Estimation

LiModel1 <-lm(SC0 ~ E2Treat, data = df)
LiModelHomo <-lm(SC0 ~ E2Treat*HomoIndex, data = df)

summary(LiModelHomo)

LiModelControls <-lm(SC0 ~ E2Treat*HomoIndex + AgeRecod + IncomeRecod + GenReBinary + ideologia
                     + skill, data = df)

summary(LiModelControls)


### Create table with models


#stargazer::stargazer(MLModel1, LiModel1,
#                     title = "Success score distinguishing fake news from real news",
#                     dep.var.labels = "Success score",
#                     covariate.labels = c("T1 Like-minded", "T2 Opposite","High Eco Chamber", "High Digital Citizenship",
#                                          "Female", "30 to 40 years", "41 to 65 years",
#                                          "66+ years", "Constant"),
#                     star.cutoffs = c(0.05, 0.01, 0.001),
#                     column.sep.width = "1pt",
#                     notes = c("Base variables: Control Group, 18 to 29 years, Lowest Income"),
#                     notes.label = "Significance levels",
#                     type = "html",
#                     out = "beamer_presentation/beamer_presentation_files/E2RegGeneral.html")


## create plot m,odel graphics

plot <- plot_model(
  LiModelControls,
  title = "Fake news accuracy standardized coefficients",
  show.p = TRUE,
  type = "std2",
  show.values = TRUE,
  value.offset = 0.4,
  value.size = 4,
  dot.size = 1.8,
  line.size = 0.5,
  vline.color = "black",
  axis.textsize = 12,
  width = 0.4,
  axis.labels = c(
    "Inconsistent * Echo Chamber",
    "Consistent * Echo Chamber",
    "Digital skills",
    "Without Ideology",
    "Center",
    "Left-wing",
    "Masculine",
    "+3.000.000",
    "$1.000.000 - $3.000.000",
    "$448.001 - $1.000.000",
    "$224.001 - $448.000",
    "+66 years",
    "41 to 65 years",
    "30 - 40 years",
    "Echo Chamber Membership",
    "Ideologically Opposite",
    "Ideologically consistent"
  )
)

# Ajustar el tema para cambiar el tamaño del título
plot + theme(plot.title = element_text(size = 16, face = "bold"))
    
             
#Ordeder Logit


# with Eco Chamber 

#stargazer::stargazer(MLModelHomo, LiModelHomo,
#                     title = "Success score distinguishing fake news from real news",
#                    dep.var.labels = "Success score",
#                     covariate.labels = c("T1 Like-minded", "T2 Opposite", "High Eco Chamber", "Female", "30 to 40 years",
#                                          "41 to 65 years", "66+ years", "Low income", "Low - Mid Income", 
#                                          "Mid-High Income", "Highest Income", "Constant"),
#                     star.cutoffs = c(0.05, 0.01, 0.001),
#                     column.sep.width = "1pt",
#                     notes = c("Base variables: Control Group, 18 to 29 years,", "lowest income"),
#                     notes.label = "Significance levels",
#                     type = "html",
#                     out = "beamer_presentation/beamer_presentation_files/E2RegHomo.html")




########################################
## true or false headlines regression###
########################################


### TRUE HEADLINES ### 

#OLS
LiModel1TRUE <-lm(SumTrue ~ E2Treat, data = df)

summary(LiModel1TRUE)

LiModelHomoTRUE <-lm(SumTrue ~ E2Treat*HomoIndex, data = df)

summary(LiModelHomoTRUE)

LiModelControlsTRUE <-lm(SumTrue ~ E2Treat*HomoIndex + AgeRecod + IncomeRecod + GenReBinary + ideologia
                         + skill, data = df)

summary(LiModelControlsTRUE)

## Regression

#General table

#stargazer::stargazer(MLModel1TRUE, LiModel1TRUE,
#                     title = "Success score distinguishing True headlines",
#                     dep.var.labels = "Success score",
#                     covariate.labels = c("T1 Like-minded", "T2 Opposite","High Eco Chamber", "High Digital Citizenship",
#                                          "Female", "30 to 40 years", "41 to 65 years",
#                                          "66+ years", "Low income", "Low - Mid Income", 
#                                          "Mid-High Income", "Highest Income", "Constant"),
#                     star.cutoffs = c(0.05, 0.01, 0.001),
#                     column.sep.width = "1pt",
#                     notes = c("Base variables: Control Group, 18 to 29 years, Lowest Income"),
#                     notes.label = "Significance levels",
#                     type = "html",
#                     out = "beamer_presentation/beamer_presentation_files/E2RegGeneralTRUE.html")

# with Eco Chamber 

#stargazer::stargazer(MLModelHomoTRUE, LiModelHomoTRUE,
#                     title = "Success score distinguishing true headlines",
#                     dep.var.labels = "Success score",
#                     covariate.labels = c("T1 Like-minded", "T2 Opposite", "High Eco Chamber", "Female", "30 to 40 years",
#                                          "41 to 65 years", "66+ years", "Low income", "Low - Mid Income", 
#                                          "Mid-High Income", "Highest Income", "Constant"),
#                     star.cutoffs = c(0.05, 0.01, 0.001),
#                     column.sep.width = "1pt",
#                     notes = c("Base variables: Control Group, 18 to 29 years,", "lowest income"),
#                     notes.label = "Significance levels",
#                     type = "html",
#                     out = "beamer_presentation/beamer_presentation_files/E2RegHomoTRUE.html")

## sjplot

plot_true <- plot_model(
  LiModelControlsTRUE,
  title = "Fake news accuracy standardized coefficients from true headlines",
  show.p = TRUE,
  type = "std2",
  show.values = TRUE,
  value.offset = 0.4,
  value.size = 4,
  dot.size = 1.8,
  line.size = 0.5,
  vline.color = "black",
  axis.textsize = 12,
  width = 0.4,
  axis.labels = c(
    "Inconsistent * Echo Chamber",
    "Consistent * Echo Chamber",
    "Digital skills",
    "Without Ideology",
    "Center",
    "Left-wing",
    "Masculine",
    "+3.000.000",
    "$1.000.000 - $3.000.000",
    "$448.001 - $1.000.000",
    "$224.001 - $448.000",
    "+66 years",
    "41 to 65 years",
    "30 - 40 years",
    "Echo Chamber Membership",
    "Ideologically Opposite",
    "Ideologically consistent"
  )
)

plot_true + theme(plot.title = element_text(size = 16, face = "bold"))


### FALSE HEADLINES ### 

#OLS

#OLS
LiModel1FALSE <-lm(as.numeric(percent_false) ~ E2Treat, data = df)

summary(LiModel1FALSE)

LiModelHomoFALSE <-lm(as.numeric(percent_false) ~ E2Treat*HomoIndex, data = df)

summary(LiModelHomoFALSE)

LiModelControlsFALSE <-lm(as.numeric(percent_false) ~ E2Treat*HomoIndex + AgeRecod + IncomeRecod + GenReBinary + ideologia
                         + skill, data = df)

summary(LiModelControlsFALSE)

plot_false <- plot_model(
  LiModelControlsFALSE,
  title = "Percentage of Fake news accuracy from false headlines",
  show.p = TRUE,
  #type = "std",
  show.values = TRUE,
  value.offset = 0.4,
  value.size = 4,
  dot.size = 1.8,
  line.size = 0.5,
  vline.color = "black",
  axis.textsize = 12,
  width = 0.4,
  axis.labels = c(
    "Inconsistent * Echo Chamber",
    "Consistent * Echo Chamber",
    "Digital skills",
    "Without Ideology",
    "Center",
    "Left-wing",
    "Masculine",
    "+3.000.000",
    "$1.000.000 - $3.000.000",
    "$448.001 - $1.000.000",
    "$224.001 - $448.000",
    "+66 years",
    "41 to 65 years",
    "30 - 40 years",
    "Echo Chamber Membership",
    "Ideologically Opposite",
    "Ideologically consistent"
  )
)

plot_false + theme(plot.title = element_text(size = 16, face = "bold"))

## Regression

#General table

#stargazer::stargazer(MLModel1FALSE, LiModel1FALSE,
#                     title = "Success score distinguishing false headlines",
#                     dep.var.labels = "Success score",
#                     covariate.labels = c("T1 Like-minded", "T2 Opposite","High Eco Chamber", "High Digital Citizenship",
#                                          "Female", "30 to 40 years", "41 to 65 years",
#                                          "66+ years", "Constant"),
#                     star.cutoffs = c(0.05, 0.01, 0.001),
#                     column.sep.width = "1pt",
#                     notes = c("Base variables: Control Group, 18 to 29 years, Lowest Income"),
#                     notes.label = "Significance levels",
#                     type = "html",
#                     out = "beamer_presentation/beamer_presentation_files/E2RegGeneralFALSE.html")

# with Eco Chamber 

#stargazer::stargazer(MLModelHomoFALSE, LiModelHomoFALSE,
#                     title = "Success score distinguishing false headlines",
#                     dep.var.labels = "Success score",
#                     covariate.labels = c("T1 Like-minded", "T2 Opposite", "High Eco Chamber", "Female", "30 to 40 years",
#                                          "41 to 65 years", "66+ years", "Low income", "Low - Mid Income", 
#                                          "Mid-High Income", "Highest Income", "Constant"),
#                     star.cutoffs = c(0.05, 0.01, 0.001),
#                     column.sep.width = "1pt",
#                     notes = c("Base variables: Control Group, 18 to 29 years,", "lowest income"),
#                     notes.label = "Significance levels",
#                     type = "html",
#                     out = "beamer_presentation/beamer_presentation_files/E2RegHomoFALSE.html")

#Poisson estimation

PoModel1 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto, family = poisson, data = df)
PoModel2 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex, family = poisson, data = df)
PoModel3 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + DigitIndex, family = poisson, data = df)
PoModel4 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex, family = poisson, data = df)
PoModel5 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum, family = poisson, data = df)
PoModel6 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4, family = poisson, data = df)
PoModel7 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4 + Educ_2
               + Educ_3 + Educ_4 + Educ_5,family = poisson, data = df)
PoModel8 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4 + Educ_2
               + Educ_3 + Educ_4 + Educ_5 + ideologia_Izquierda + ideologia_Derecha + ideologia_Ninguno, family = poisson,
               data = df)
PoModel9 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4 + Educ_2
               + Educ_3 + Educ_4 + Educ_5 + ideologia_Izquierda + ideologia_Derecha + ideologia_Ninguno
               + NivEco_2 + NivEco_3 + NivEco_4 + NivEco_5, family = poisson, data = df)


#Create regression tables

#cm <-c('(Intercept)' = 'Constant', 'E2Treat_Afin' = 'T1 Like-Minded', 'E2Treat_Opuesto' = 'T2 Opposite',
#       'HomoIndex' = 'High Eco Chamber', 'DigitIndex' = 'High Digital Citizenship')

#cap <-'regression table with balanced variables'


#Reg1Bal <-stargazer::stargazer(MLModel1,PoModel1, MLModel2, PoModel2, MLModel3, PoModel3,MLModel4, PoModel4,
#                               title = "Success score distinguishing fake news from real news",
#                               dep.var.labels = "Success score",
#                               covariate.labels = c("T1 Like-minded", "T2 Opposite", "High Eco Chamber",
#                                                    "High Digital citizenship","Constant"),
#                               star.cutoffs = c(0.05, 0.01, 0.001),
#                               column.sep.width = "1pt",
#                               notes = "Base variables: Control Group",
#                               notes.label = "Significance levels",
#                               type = "html",
#                               out = "beamer_presentation/beamer_presentation_files/regression_balanced.html")

## Not balanced regression


#Reg1NoBal <-stargazer::stargazer(MLModel4, PoModel4, MLModel5, PoModel5, MLModel6, PoModel6, MLModel7, PoModel7, MLModel8,PoModel8, MLModel9, PoModel9,
#                                 title = "Success score distinguishing fake news from real news. Non-balanced variables",
#                                 dep.var.labels = "Success score",
#                                 covariate.labels = c("T1 Like-minded", "T2 Opposite", "High Eco Chamber",
#                                                      "High Digital citizenship","Female", "30 to 40 years", "41 to 65 years",
#                                                      "66+ years", "Elemental School", "Secondary School", "Graduate",
#                                                      "Postgraduate","Left-wing", "Right-wing","Without Ideology", "$224.001 - $448-000",
#                                                      "$448.001 - $1.000.000", "$1.000.001 - $3.000.000", "$3.000,000+","Constant"),
#                                 star.cutoffs = c(0.05, 0.01, 0.001),
#                                 column.sep.width = "1pt",
#                                 notes.label = "Significance levels",
#                                 type = "html",
#                                 out = "beamer_presentation/beamer_presentation_files/regression-non-balanced.html")

#Baseline Variables: T0-Control Group, Low Eco Chamber membership and Digital Citizenship,
#\n Gender: Men, Age: 18-29 years, Education: No-Education, Ideology: Center, Income: >$224.001


## transform html beamer to pdf 

library(renderthis)

to_pdf("beamer_presentation/beamer_presentation.Rmd")



##### Robustness tests


### Ordered logit

## Transform to factor

df$SC0 <-factor(df$SC0, levels = c("0", "1", "2", "3", "4","5","6","7"))

ol1<-polr(SC0 ~ E2Treat*HomoIndex + AgeRecod + IncomeRecod + GenReBinary + ideologia
                  + skill, data = df, Hess = T)

ol_model <- clm(SC0 ~ E2Treat * HomoIndex + AgeRecod + IncomeRecod + GenReBinary + ideologia + skill, data = df)

# Obtener los intervalos de confianza
conf_int <- confint(ol_model)

print(conf_int)




summary(ol1<-polr(as.factor(SC0) ~ E2Treat+ SexDum +AgeRecod + EducRec + ideologia + IncomeRecod + DigitIndex, data = df, Hess=TRUE))
summary(ol2<-polr(as.factor(SC0) ~ E2Treat+ AgeRecod + ideologia + IncomeRecod , data = df, Hess=TRUE))
summary(ol3<-polr(as.factor(SC0) ~ E2Treat+ ideologia, data = df, Hess=TRUE))
summary(ol4<-polr(as.factor(SC0) ~ E2Treat+ ideologia, data = df, Hess=TRUE))
summary(ol5<-polr(as.factor(SC0) ~ E2Treat+ IncomeRecod, data = df, Hess=TRUE))

### Negative binomial model


summary(nb1 <- glm.nb(SC0 ~ E2Treat+ SexDum +AgeRecod + EducRec + ideologia + IncomeRecod + DigitIndex, data=df))
summary(nb2 <- glm.nb(SC0 ~ E2Treat+ SexDum + DigitIndex, data=df))
summary(nb3 <- glm.nb(SC0 ~ E2Treat+ AgeRecod + ideologia + IncomeRecod, data=df))
summary(nb4 <- glm.nb(SC0 ~ E2Treat+ SexDum +AgeRecod + EducRec + ideologia + IncomeRecod, data=df))
summary(nb5 <- glm.nb(SC0 ~ E2Treat+ SexDum +AgeRecod + EducRec + ideologia + IncomeRecod, data=df))


##################################
# False Responses
##################################

summary(frol1<-polr(as.factor(SumFalse) ~ E2Treat+ SexDum +AgeRecod + EducRec + ideologia + IncomeRecod + DigitIndex, data = df, Hess=TRUE))
summary(frol2<-polr(as.factor(SumFalse) ~ E2Treat+ AgeRecod + ideologia + IncomeRecod + DigitIndex, data = df, Hess=TRUE))
summary(frol3<-polr(as.factor(SumFalse) ~ E2Treat+ ideologia, data = df, Hess=TRUE))
summary(frol4<-polr(as.factor(SumFalse) ~ E2Treat+ ideologia, data = df, Hess=TRUE))
summary(frol5<-polr(as.factor(SumFalse) ~ E2Treat+ IncomeRecod, data = df, Hess=TRUE))
summary(frol5<-polr(as.factor(SumFalse) ~ E2Treat+ DigitIndex, data = df, Hess=TRUE)) ### Non significant on it's own



