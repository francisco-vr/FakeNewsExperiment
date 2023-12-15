# Plots for descriptive analysis


df$SC0 <-as.numeric(df$SC0)

df$E2Treat <-factor(df$E2Treat, levels = c("Afin", "Control", "Opuesto"))


t.test(df$SC0, conf.level = 0.95)

dt <-df%>%
  dplyr::group_by(E2Treat)%>%
  dplyr::summarise(
    mean = mean(SC0),
    lci = t.test(SC0, conf.level = 0.95)$conf.int[1],
    uci = t.test(SC0, conf.level = 0.95)$conf.int[2]
  )

compE2 <-list(c("Control","Afin"), c("Control", "Opuesto"), c("Afin","Opuesto"))


pl1 <- ggplot(data = dt) +
  geom_line(aes(x = E2Treat, y = mean, fill = ), group = 1, color = viridis(3)[1], size = 1.5) +
  geom_point(aes(x = E2Treat, y = mean), color = viridis(3)[2], size = 3) +
  geom_errorbar(aes(x = E2Treat, ymin = lci, ymax = uci), width = 0.2, color = viridis(3)[2], size = 1.2) +
  geom_text(aes(x = E2Treat, y = lci, label = round(lci, 1)), vjust = 1.5, size = 4, color = "black") +
  geom_text(aes(x = E2Treat, y = uci, label = round(uci, 1)), vjust = -1.5, size = 4, color = "black") +
  scale_x_discrete(labels = c("Afin" = "Consistent", "Control" = "Control", "Opuesto" = "Inconsistent"))+
  theme_classic(base_size = 14) +
  labs(
    title = "Averange fake news accuracy",
    subtitle = "(95% Confidence intervals)",
    x = "Experimental condition",
    y = "Accuracy score CI"
  ) +
  coord_cartesian(ylim = c(4, 6)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

pl1

# another but with echo cham,ber membership

dt_eco <-df%>%
  dplyr::group_by(E2Treat, HomoIndex)%>%
  dplyr::summarise(
    mean = mean(SC0),
    lci = t.test(SC0, conf.level = 0.95)$conf.int[1],
    uci = t.test(SC0, conf.level = 0.95)$conf.int[2]
  )

#same plot but fill with eco chamber


# Establecer un tema más atractivo
library(viridis)

theme_set(theme_minimal(base_size = 14))

pl_homo <- ggplot(data = dt_eco) +
  geom_line(aes(x = E2Treat, y = mean), group = 1, color = viridis(3)[1], size = 1.5) +
  geom_point(aes(x = E2Treat, y = mean), color = viridis(3)[2], size = 3) +
  geom_errorbar(aes(x = E2Treat, ymin = lci, ymax = uci), width = 0.2, color = viridis(3)[2], size = 1.2) +
  geom_text(aes(x = E2Treat, y = lci, label = round(lci, 1)), vjust = 1.5, size = 4, color = "black") +
  geom_text(aes(x = E2Treat, y = uci, label = round(uci, 1)), vjust = -1.5, size = 4, color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Averange fake news accuracy by Echo Chamber membership",
    subtitle = "(95% Confidence intervals)",
    x = "Experimental condition",
    y = "Accuracy score CI"
  ) +
  coord_cartesian(ylim = c(4, 6)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  facet_wrap(~HomoIndex, scales = "free_y", 
             labeller = labeller(HomoIndex = c("0" = "Low Echo Chamber Membership",
                                               "1" = "High Echo Chamber Membership")))+
  scale_x_discrete(labels = c("Afin" = "Consistent", "Control" = "Control", "Opuesto" = "Inconsistent"))

pl_homo



E2general<-df%>%
  dplyr::filter(!is.na(E2Treat))%>%
  ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  scale_x_discrete(labels = c('Like-Minded', 'Control', 'Opposite')) +
  geom_hline(yintercept = mean(as.numeric(df$SC0)), linetype = 2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"), guide = "none") +
  labs(title = "Accuracy scores of news headlines \n General results",
       x = "Treatment", y = "Score (max 7)",
       caption = "NS = No Statistical significance; * ≤.05; ** ≤.01; *** ≤.001") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE2, label = "p.signif") +
  stat_compare_means(label.y = 10) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.6) +
  stat_summary(fun.data = mean_cl_normal, geom = "crossbar", width = 0.2)

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
