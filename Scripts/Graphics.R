# Plots for descriptive analysis



compE2 <-list(c("Control","Afin"), c("Control", "Opuesto"), c("Afin","Opuesto"))

df$SC0 <-as.numeric(df$SC0)

E2general<-df%>%
  dplyr::filter(!is.na(E2Treat))%>%
  ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  geom_hline(yintercept = mean(as.numeric(df$SC0)), linetype = 2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"), guide = "none") +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=7, vjust = -2) +
  labs(title = "Accuracy scores of news headlines \n General results",
       x = "Treatment", y = "Score (max 7)",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 10)
E2general

ggsave(E2general, filename = "beamer_presentation/beamer_presentation_files/Plot1General.png",
       dpi = 400, width = 15, height = 9)


E2homo <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  geom_hline(yintercept = mean(as.numeric(df$SC0)), linetype = 2) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette("Darjeeling1"), guide = "none") +
  labs(title = "By Echo Chamber membership",
       x = "Treatment", y = "Score (max 7)",
       caption = "NS = No Statistical significance; * ≤ .05; ** ≤.01; *** ≤.001") +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 10) +
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Low Echo Chamber membership",
                                                                    '1'="High Echo Chamber membership")))

E2digit <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  geom_boxplot() +
  geom_hline(yintercept = mean(as.numeric(df$SC0)), linetype = 2) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"), guide = "none") +
  labs(title = "By Digital Citizenship",
       x = "Treatment", y = "Score (max 7)",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  stat_compare_means(comparisons = compE2,label = "p.signif") +
  stat_compare_means(label.y = 10) +
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Low digital citizenship",
                                                                      '1'="High digital citizenship")))

PlotE2 <-(E2general | E2homo / E2digit)

ggsave(PlotE2, filename = "beamer_presentation/beamer_presentation_files/Plot1General.png",
       dpi = 400, width = 15, height = 9)


#### Create false and true headlines plots

##TRUE HEADLINES


compE2 <-list(c("Control","Afin"), c("Control", "Opuesto"), c("Afin","Opuesto"))

df$SC0 <-as.numeric(df$SumTrue)

E2generalTRUE<-df%>%
  dplyr::filter(!is.na(E2Treat))%>%
  ggplot(data = df, mapping = aes(x = E2Treat, y = SumTrue, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  geom_hline(yintercept = mean(as.numeric(df$SumTrue)), linetype = 2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"), guide = "none") +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=7, vjust = -2) +
  labs(title = "Accuracy scores of True news headlines \n General results",
       x = "Treatment", y = "Score (max 3)",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 4)
E2generalTRUE

ggsave(E2general, filename = "beamer_presentation/beamer_presentation_files/Plot1General.png",
       dpi = 400, width = 15, height = 9)


E2homoTRUE <-ggplot(data = df, mapping = aes(x = E2Treat, y = SumTrue, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  geom_hline(yintercept = mean(as.numeric(df$SumTrue)), linetype = 2) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette("Darjeeling1"), guide = "none") +
  labs(title = "True headlines By Echo Chamber membership",
       x = "Treatment", y = "Score (max 3)",
       caption = "NS = No Statistical significance; * ≤ .05; ** ≤.01; *** ≤.001") +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 5) +
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Low Echo Chamber membership",
                                                                    '1'="High Echo Chamber membership")))

E2digitTRUE <-ggplot(data = df, mapping = aes(x = E2Treat, y = SumTrue, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  geom_boxplot() +
  geom_hline(yintercept = mean(as.numeric(df$SumTrue)), linetype = 2) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"), guide = "none") +
  labs(title = "True headlines by Digital Citizenship",
       x = "Treatment", y = "Score (max 3)",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  stat_compare_means(comparisons = compE2,label = "p.signif") +
  stat_compare_means(label.y = 5) +
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Low digital citizenship",
                                                                      '1'="High digital citizenship")))

PlotE2TRUE <-(E2generalTRUE | E2homoTRUE / E2digitTRUE)

ggsave(PlotE2TRUE, filename = "beamer_presentation/beamer_presentation_files/Plot1General.png",
       dpi = 400, width = 15, height = 9)

### FALSE HEADLINES

compE2 <-list(c("Control","Afin"), c("Control", "Opuesto"), c("Afin","Opuesto"))

df$SC0 <-as.numeric(df$SumFalse)

E2generalFALSE<-df%>%
  dplyr::filter(!is.na(E2Treat))%>%
  ggplot(data = df, mapping = aes(x = E2Treat, y = SumFalse, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  geom_hline(yintercept = mean(as.numeric(df$SumFalse)), linetype = 2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"), guide = "none") +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=7, vjust = -2) +
  labs(title = "Accuracy scores of False news headlines \n General results",
       x = "Treatment", y = "Score (max 4)",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 6)
E2generalFALSE

ggsave(E2general, filename = "beamer_presentation/beamer_presentation_files/Plot1General.png",
       dpi = 400, width = 15, height = 9)


E2homoFALSE <-ggplot(data = df, mapping = aes(x = E2Treat, y = SumFalse, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  geom_hline(yintercept = mean(as.numeric(df$SumFalse)), linetype = 2) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette("Darjeeling1"), guide = "none") +
  labs(title = "False headlines by Echo Chamber membership",
       x = "Treatment", y = "Score (max 4)",
       caption = "NS = No Statistical significance; * ≤ .05; ** ≤.01; *** ≤.001") +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 6) +
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Low Echo Chamber membership",
                                                                    '1'="High Echo Chamber membership")))

E2digitFALSE <-ggplot(data = df, mapping = aes(x = E2Treat, y = SumFalse, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  geom_boxplot() +
  geom_hline(yintercept = mean(as.numeric(df$SumTrue)), linetype = 2) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"), guide = "none") +
  labs(title = "False headlines by Digital Citizenship",
       x = "Treatment", y = "Score (max 4)",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  stat_compare_means(comparisons = compE2,label = "p.signif") +
  stat_compare_means(label.y = 6) +
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Low digital citizenship",
                                                                      '1'="High digital citizenship")))

PlotE2FALSE <-(E2generalFALSE | E2homoFALSE / E2digitFALSE)

ggsave(PlotE2FALSE, filename = "beamer_presentation/beamer_presentation_files/Plot1General.png",
       dpi = 400, width = 15, height = 9)
