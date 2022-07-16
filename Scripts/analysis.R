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


## transform html beamer to pdf 

#library(renderthis)

#to_pdf("beamer_presentation/beamer_presentation.Rmd")



##### Robustness tests


### Ordered logit
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


###################################
#### Ramdomization inference estimations
#########################################



#################### RI ####################

df$SC0n<-as.numeric(df$SC0)

### 
declaration<-declare_ra(N=690, prob_each=c(0.34, 0.66), simple=TRUE)
declaration

#df$above_med_correct<-ifelse(df$SC0n>4, 1, 0)

out <- conduct_ri(SC0n ~ E2Treat_Afin + ideologia + IncomeRecod,
                  declaration = declaration,
                  assignment = "E2Treat_Afin",
                  sharp_hypothesis = 0,
                  data = df)

summary(out)
plot(out)
tidy(out)


### Opuesto
declaration<-declare_ra(N=690, prob_each=c(0.34, 0.66), simple=TRUE)
declaration

#df$above_med_correct<-ifelse(df$SC0n>4, 1, 0)

out2 <- conduct_ri(SC0n ~ E2Treat_Opuesto,
                   declaration = declaration,
                   assignment = "E2Treat_Opuesto",
                   sharp_hypothesis = 0,
                   data = df)

summary(out2)
plot(out2)
tidy(out)




###########################
#### interaction effects
###########################
library(BayesTree)

set.seed(89)

# Data set up including calculating ability rank
df.b <- df


################## Efect de noticias afines e interacción con ecochambers
df.b$treat.het <- as.factor(df.b$E2Treat_Opuesto) #dummy tratment variable

df.b$AgeRecod <- as.factor(df.b$AgeRecod)
df.b$ideologia <- as.factor(df.b$ideologia)
df.b$IncomeRecod <- as.factor(df.b$IncomeRecod)
df.b$HomoIndex <- as.factor(df.b$HomoIndex)


# Define model variables incl. outcome as column 1
vars <- c("SC0n", "treat.het", "AgeRecod", "ideologia", "IncomeRecod",  "HomoIndex" )

df.b <- df.b[,vars]
df.b <- df.b[complete.cases(df.b),]

# Separate outcome and training data
y <- df.b$SC0n
train <- df.b[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:5)], test[test$treat.het==1,c(2:5)])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Descriptive results reported in main text:
mean(CATE_df$CATE)
summary(CATE_df$CATE)

# Proportion of CATEs that are negative:
sum(CATE_df$CATE < 0)/nrow(CATE_df)
sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)

# Ecochamber - Homo prob below mean
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$HomoIndex =="1" )/sum(CATE_df$HomoIndex == "1")

# Echochamber - Hetero prop. below mean
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$HomoIndex == "0" )/sum(CATE_df$HomoIndex =="0")


# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,nrow(train)))
#ggsave(effectsPlot, filename= "test.pdf")
# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=factor(HomoIndex))) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train)))#+
#scale_fill_discrete(name = "", labels = c("0", "Male"))
#scale_fill_manual(name="Mode", values=colours) +
# +
#scale_x_continuous(limits = c(0,5220))

# Combine all plots into one chart
homoindex_het <- ggarrange(effectsPlot, modePlot,
                           ncol = 1, nrow = 2, heights = c(2,2))

#ggsave(homoindex_het_opuesto, filename = "gender_het1_alltreats.pdf", path=fig.path, device = "pdf", height = 8, width = 6, dpi = 300)




################## Efect de noticias afines e interacción con ecochambers

df.b<-df
df.b$treat.het <- as.factor(df.b$E2Treat_Afin) #dummy tratment variable

df.b$AgeRecod <- as.factor(df.b$AgeRecod)
df.b$ideologia <- as.factor(df.b$ideologia)
df.b$IncomeRecod <- as.factor(df.b$IncomeRecod)
df.b$HomoIndex <- as.factor(df.b$HomoIndex)


# Define model variables incl. outcome as column 1
vars <- c("SC0n", "treat.het", "AgeRecod", "ideologia", "IncomeRecod",  "HomoIndex" )

df.b <- df.b[,vars]
df.b <- df.b[complete.cases(df.b),]

# Separate outcome and training data
y <- df.b$SC0n
train <- df.b[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:5)], test[test$treat.het==1,c(2:5)])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Descriptive results reported in main text:
mean(CATE_df$CATE)
summary(CATE_df$CATE)

# Proportion of CATEs that are negative:
sum(CATE_df$CATE < 0)/nrow(CATE_df)
sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)

# Ecochamber - Homo prob below mean
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$HomoIndex =="1" )/sum(CATE_df$HomoIndex == "1")

# Echochamber - Hetero prop. below mean
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$HomoIndex == "0" )/sum(CATE_df$HomoIndex =="0")


# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,nrow(train)))
#ggsave(effectsPlot, filename= "test.pdf")
# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=factor(HomoIndex))) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train)))#+
#scale_fill_discrete(name = "", labels = c("0", "Male"))
#scale_fill_manual(name="Mode", values=colours) +
# +
#scale_x_continuous(limits = c(0,5220))

# Combine all plots into one chart
homoindex_het <- ggarrange(effectsPlot, modePlot,
                           ncol = 1, nrow = 2, heights = c(2,2))

homoindex_het
#ggsave(homoindex_het_afin, filename = "gender_het1_alltreats.pdf", path=fig.path, device = "pdf", height = 8, width = 6, dpi = 300)
