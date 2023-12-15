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


test <-multinom(E2Treat ~ HomoIndex + DigitIndex + SexDum + Age_1 + Age_2 + Age_3 + Age_4 + Educ_1 + Educ_2
                + Educ_3 + Educ_4 + Educ_5 + ideologia_Izquierda + ideologia_Derecha + ideologia_Ninguno + ideologia_centro +
                  + NivEco_1 + NivEco_2 + NivEco_3 + NivEco_4 + NivEco_5, data = df)
summary(test)

stargazer::stargazer(test)

table(df$E2Treat)