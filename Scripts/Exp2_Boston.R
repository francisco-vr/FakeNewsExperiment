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
              "xtable","pBrackets","Hmisc","ri","ggpubr", "stargazer", "Rmisc","wesanderson", "gridExtra","ggmosaic",
              "vcd", "plyr", "ggannotate","scales", "fastDummies","gt")
ipak(packages)


# READ DF

df <-readRDS("Data/FNDF.RDS")

# Create false or true scores


df <-df%>%
  nrow()%>%
  dplyr::mutate(SC0False = as.matrix.data.frame(E2TC_2:E2TC_8, E2TC_10, df$E2TC_11, df$E2T1a_2:df$E2T1a_5, df$E2T1b_2:df$E2T1b_5, df$E2T1c_2:df$E2T1b_4,
      df$E2T1b_6, df$E2T1d_2:df$E2T1d_4, df$E2T1d_6, df$E2T2a_2:df$E2T2a_5, df$E2T2b_2:df$E2T2b_5, df$E2T2b_8:df$E2T2b_10,
      df$E2T2c_2:df$E2T2c_5))%>%
  rowSums(SC0False)


table(df$E2TC_1)

# Plots for descriptive analysis

#####################
#### Experiment 2 ###
#####################

compE2 <-list(c("Control","Afin"), c("Control", "Opuesto"))

df$SC0 <-as.numeric(df$SC0)

E2general<-df%>%
  dplyr::filter(!is.na(E2Treat))%>%
  ggplot(data = df, mapping = aes(x = E2Treat, y = as.numeric(SC0), fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  geom_hline(yintercept = mean(as.numeric(df$SC0)), linetype = 2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=7, vjust = -2) +
  labs(title = "Experimento N°2: Puntajes de  de acertividad ante \n titulares de noticias falsos o verdaderos",
       x = "Tratamiento", y = "Puntaje (max 7)",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 9)

E2homo <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  geom_hline(yintercept = mean(as.numeric(df$SC0)), linetype = 2) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette("Darjeeling1")) +
  labs(title = "Según nivel de membresía a cámaras de eco",
       x = "Tratamiento", y = "Puntaje (max 7)",
       caption = "NS = No Statistical significance; * ≤ .05; ** ≤.01; *** ≤.001") +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 10) +
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

E2digit <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  geom_hline(yintercept = mean(as.numeric(df$SC0)), linetype = 2) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  labs(title = "Según nivel de Ciudadania Digital",
       x = "Tratamiento", y = "Puntaje (max 7)",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  stat_compare_means(comparisons = compE2,label = "p.signif") +
  stat_compare_means(label.y = 10) +
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital")))

PlotE2 <-(E2general | E2homo / E2digit)

ggsave(PlotE2, filename = "beamer_presentation/beamer_presentation_files/Plot1.png",
       dpi = 400, width = 15, height = 9)



# make some dummies for regression tables

df$SexDum <- if_else(df$GenRecod == "Femenino",
                     true = 1, false = 0)

df <-fastDummies::dummy_cols(df, select_columns = 'Age')
df <-fastDummies::dummy_cols(df, select_columns = 'Educ')
df <-fastDummies::dummy_cols(df, select_columns = 'E2Treat')
df <-fastDummies::dummy_cols(df, select_columns = 'ideologia')
df <-fastDummies::dummy_cols(df, select_columns = 'NivEco')
df <-dplyr::select(df, -Age_1, -Educ_1, -E2Treat_Control, -ideologia_centro, -NivEco_1)


## Regresion with balanced covariates

#Maximum likehood 

df$SC0 <-as.numeric(df$SC0)

MLModel1 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto, data = df)
MLModel2 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex, data = df)
MLModel3 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + DigitIndex, data = df)
MLModel4 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex, data = df)
MLModel5 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum, data = df)
MLModel6 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4, data = df)
MLModel7 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4 + Educ_2
             + Educ_3 + Educ_4 + Educ_5, data = df)
MLModel8 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4 + Educ_2
             + Educ_3 + Educ_4 + Educ_5 + ideologia_Izquierda + ideologia_Derecha + ideologia_Ninguno,
             data = df)
MLModel9 <-glm(SC0 ~ E2Treat_Afin + E2Treat_Opuesto + HomoIndex + DigitIndex + SexDum + Age_2 + Age_3 + Age_4 + Educ_2
             + Educ_3 + Educ_4 + Educ_5 + ideologia_Izquierda + ideologia_Derecha + ideologia_Ninguno
             + NivEco_2 + NivEco_3 + NivEco_4 + NivEco_5, data = df)


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

cm <-c('(Intercept)' = 'Constant', 'E2Treat_Afin' = 'T1 Like-Minded', 'E2Treat_Opuesto' = 'T2 Opposite',
       'HomoIndex' = 'High Eco Chamber', 'DigitIndex' = 'High Digital Citizenship')

cap <-'regression table with balanced variables'


Reg1Bal <-stargazer::stargazer(MLModel1,PoModel1, MLModel2, PoModel2, MLModel3, PoModel3,MLModel4, PoModel4,
                               title = "Success score distinguishing fake news from real news",
                               dep.var.labels = "Success score",
                               covariate.labels = c("T1 Like-minded", "T2 Opposite", "High Eco Chamber",
                               "High Digital citizenship","Constant"),
                               star.cutoffs = c(0.05, 0.01, 0.001),
                               column.sep.width = "1pt",
                               notes = "Base variables: Control Group",
                               notes.label = "Significance levels",
                               type = "html",
                               out = "beamer_presentation/beamer_presentation_files/regression_balanced.html")

## Not balanced regression


Reg1NoBal <-stargazer::stargazer(MLModel4, PoModel4, MLModel5, PoModel5, MLModel6, PoModel6, MLModel7, PoModel7, MLModel8,PoModel8, MLModel9, PoModel9,
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

## Surprise levels

compE3 <-list(c("Afin","Control"), c("Control", "Opuesto"))
              
E2Sur <-ggplot(data = df, aes(x = factor(E2Treat), y = as.numeric(end_4), color = as.numeric(end_4))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Surprise levels after show they own results",
       x = "", y = "Surprise levels",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(method = "t.test", label.y = 110)


E2SurDigi <-ggplot(data = df, aes(x = factor(E2Treat), y = as.numeric(end_4), color = as.numeric(end_4))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Surprise levels after show they own results",
       x = "", y = "Surprise levels",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(method = "t.test", label.y = 110) +  
facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital"))) 



E2SurHomo <-ggplot(data = df, aes(x = factor(E2Treat), y = as.numeric(end_4), color = as.numeric(end_4))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Surprise levels after show they own results",
       x = "", y = "Surprise levels",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(method = "t.test", label.y = 110) + 
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE2Sur <-(E2Sur / E2SurDigi | E2SurHomo)
PlotE3Fear<-PlotE3Fear + plot_annotation(title = 'Experimento N°3: Distribución de temor según condición experimental, \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                         theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3Fear, filename = "Results/Plots/E3FearSub.png",
       dpi = 400, width = 14, height = 11)


## transform html beamer to pdf 

#library(renderthis)

#to_pdf("beamer_presentation/beamer_presentation.Rmd")


