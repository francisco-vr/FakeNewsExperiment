########################################################################################################
### Make Graphics                                                                                   ####          
### September 2021                                                                                 ####
### Francisco Villarroel                                                                            ####
### Thesis Name: “Aves del mismo plumaje se emocionan juntas” Encuesta experimental sobre la        ####
### influencia de la homofília en la construcción de conductas políticas en el Chile Contemporáneo. ####
########################################################################################################


df <-readRDS("Data/Analysis-Data/DF-final.RDS")


###############
#### Experiment 1###
#####################

compE1 <-list(c("Control","Afin"), c("Control", "Opuesto"), c("Afin", "Opuesto"))

E1general <-df%>%
  dplyr::filter(!is.na(E1Treat))%>%
  ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.05) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  labs(title = "Experimento N°1: Nivel de ira por cercanía social, \n según tipo de tratamiento",
       x = "Tratamiento", y = "Nivel de ira (Max 7)",
       caption = "Fuente: Elaboración propia") +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, vjust = -1.5) +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
    stat_compare_means(comparisons = compE1) +
  stat_compare_means(label.y = 9)

E1homo <-ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot(aes(colour = )) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"))+
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  labs(title = "Niveles de Ira según membresía a Cámaras de Eco",
       y = "Nivel de ira (Max 7)", x = "Tratamientos",
       caption = "Fuente: Elaboración propia") + 
  facet_wrap(~HomoIndex, nrow = 1, labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                       '1'="Alta membresia a Cámaras de eco"))) +
  stat_compare_means(comparisons = compE1) +
  stat_compare_means(label.y = 9.2)
  

E1digit <-ggplot(data = df, mapping = aes(x = E1Treat, y = E1, fill = E1Treat)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1"))+
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=3.5, vjust = -2) +
  stat_compare_means(comparisons = compE1) +
  stat_compare_means(label.y = 9.2) +
  labs(title = "Niveles de Ira segun nivel de Ciudadanìa Digital",
       y = "Nivel de ira (Max 7)", x = "Tratamientos",
       caption = "Fuente: Elaboración propia") +
  facet_wrap(~DigitIndex, nrow = 1, labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                       '1'="Alta Ciudadanía Digital")))

PlotE1 <-(E1general | E1homo / E1digit)

ggsave(PlotE1, filename = "Results/Plots/Experimento1.png",
       dpi = 400, width = 13, height = 10)



#####################
#### Experiment 2 ###
#####################

compE2 <-list(c("Control","Afin"), c("Control", "Opuesto"), c("Afin", "Opuesto"))

E2general<-df%>%
  dplyr::filter(!is.na(E2Treat))%>%
  ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=7, vjust = -2) +
  labs(title = "Experimento N°2: Puntajes de  de acertividad ante \n titulares de noticias falsos o verdaderos",
       x = "Tratamiento", y = "Puntaje (max 7)",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE2) +
  stat_compare_means(label.y = 10)

E2homo <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette("Darjeeling1")) +
  labs(title = "Según nivel de membresía a cámaras de eco",
       x = "Tratamiento", y = "Puntaje (max 7)",
       caption = "Fuente: Elaboración propia") +
  stat_compare_means(comparisons = compE2) +
  stat_compare_means(label.y = 10) +
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

E2digit <-ggplot(data = df, mapping = aes(x = E2Treat, y = SC0, fill = E2Treat)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=4, vjust = -2) +
  scale_fill_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  labs(title = "Según nivel de Ciudadania Digital",
       x = "Tratamiento", y = "Puntaje (max 7)",
       caption = "Fuente: Elaboración propia") +
  stat_compare_means(comparisons = compE2,) +
  stat_compare_means(label.y = 10) +
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital")))

PlotE2 <-(E2general | E2homo / E2digit)

ggsave(PlotE2, filename = "Results/Plots/Experimento2.png",
       dpi = 400, width = 15, height = 9)


#####################
#### Experiment 3 ###
#####################


### Maintain/Broke ties ###



# Representación gráfica del modelo.



##broke ties  by emotions ##
compE3 <-list(c("Amigo-Misinfo","Amigo-validado"), c("Conocido-Misinfo", "Conocido-validado"),
              c("Amigo-Misinfo", "Conocido-Misinfo"), c("Amigo-validado", "Conocido-validado"))


E3BAngry <-ggplot(data = df, aes(x = factor(E3), y = E3Angry, color = factor(E3))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c('Mantener lazos', 'Romper lazos')) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5,  hjust = 1.8, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Ira",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(method = "wilcox.test", label.y = 108)+
  

E3BJoy <-ggplot(data = df, aes(x = factor(E3), y = E3Joy, color = factor(E3))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c('Mantener lazos', 'Romper lazos')) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(method = "wilcox.test", label.y = 105)


E3BSad <-ggplot(data = df, aes(x = factor(E3), y = E3Sad, color = factor(E3))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c('Mantener lazos', 'Romper lazos')) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Tristeza",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(method = "wilcox.test", label.y = 105)



E3BFear <-ggplot(data = df, aes(x = factor(E3), y = E3Fear, color = factor(E3))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c('Mantener lazos', 'Romper lazos')) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Temor",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(method = "wilcox.test", label.y = 105)



PlotE3B <-((E3BSad / E3BFear) | (E3BJoy / E3BAngry))
PlotE3B <-PlotE3B + plot_annotation(title = 'Experimento N°3: Probabilidad de romper lazos sociales en Facebook, según intensidad de emociones',
                                    theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3B, filename = "Results/Plots/Experimento3-broke-emotions.png",
       dpi = 400, width = 14, height = 11)


## Emotions by treatment ##

# Anger #

E3EAngry <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Angry, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5,  hjust = 1.2, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Ira",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(label.y = 145)


# Fear # 

E3EFear <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Fear, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Temor",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(label.y = 145)


# Joy # 

E3EJoy <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Joy, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(label.y = 145)


# Sadness # 

E3ESad <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Sad, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Tristeza",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3, method = "wilcox.test") +
  stat_compare_means(label.y = 145)

# Mix and create plot #

PlotE3E <-((E3ESad / E3EFear) | (E3EJoy / E3EAngry))
PlotE3E<-PlotE3E + plot_annotation(title = 'Experimento N°3: Nivel de emociones según condiciones experimentales',
                                   theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3E, filename = "Results/Plots/E3_emocion.png",
       dpi = 400, width = 14, height = 11)


## by grouping treatments - Social ties ##
E3EGAngry <-ggplot(data = df, aes(x = factor(E3TTie), y = E3Angry, color = factor(E3TTie))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5,  hjust = 1.2, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Ira",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Fear # 

E3EGFear <-ggplot(data = df, aes(x = factor(E3TTie), y = E3Fear, color = factor(E3TTie))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Temor",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Joy # 

E3EGJoy <-ggplot(data = df, aes(x = factor(E3TTie), y = E3Joy, color = factor(E3TTie))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Sadness # 

E3EGSad <-ggplot(data = df, aes(x = factor(E3TTie), y = E3Sad, color = factor(E3TTie))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Tristeza",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3, method = "wilcox.test") +
  stat_compare_means(label.y = 105, method = "wilcox.test")

# Mix and create plot #

PlotE3GE <-((E3EGSad / E3EGFear) | (E3EGJoy / E3EGAngry))
PlotE3GE<-PlotE3GE + plot_annotation(title = 'Experimento N°3: Nivel de emociones según condiciones experimentales, \n Agrupadas por tipo de Lazo social',
                                     theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3GE, filename = "Results/Plots/E3_emocion-tie.png",
       dpi = 400, width = 14, height = 11)

## by grouping treatments - Type of Arguent  ##

E3EAAngry <-ggplot(data = df, aes(x = factor(E3TArg), y = E3Angry, color = factor(E3TArg))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5,  hjust = 1.2, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Ira",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Fear # 

E3EAFear <-ggplot(data = df, aes(x = factor(E3TArg), y = E3Fear, color = factor(E3TArg))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Temor",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Joy # 

E3EAJoy <-ggplot(data = df, aes(x = factor(E3TArg), y = E3Joy, color = factor(E3TArg))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Sadness # 

E3EASad <-ggplot(data = df, aes(x = factor(E3TArg), y = E3Sad, color = factor(E3TArg))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Tristeza",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3, method = "wilcox.test") +
  stat_compare_means(label.y = 105, method = "wilcox.test")

# Mix and create plot #

PlotE3AE <-((E3EASad / E3EAFear) | (E3EAJoy / E3EAAngry))
PlotE3AE<-PlotE3AE + plot_annotation(title = 'Experimento N°3: Nivel de emociones según condiciones experimentales, \n Agrupadas por tipo de argumentación',
                                     theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3AE, filename = "Results/Plots/E3_emocion-argument.png",
       dpi = 400, width = 14, height = 11)


## Emotions by digital citizenship and Echo Chamber membership ##

# Anger #

E3BAngryDigi <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Angry, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por nivel de Ciudadanía Digital",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~DigitIndex, nrow = 1, labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                       '1'="Alta Ciudadanía Digital")))


E3BAngryHomo <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Angry, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c('Mantener lazos', 'Romper lazos')) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por pertenencia a Cámaras de eco",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE3Angry <-(E3BAngryDigi / E3BAngryHomo)
PlotE3Angry<-PlotE3Angry + plot_annotation(title = 'Experimento N°3: Relación entre nivel de ira y desición de mantener o romper lazos sociales \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                           theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3Angry, filename = "Results/Plots/E3AngrySub.png",
       dpi = 400, width = 14, height = 11)


# Fear # 

E3BFearDigi <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Fear, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por niveles de ciudadanía digital",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital"))) 



E3BFearHomo <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Fear, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por pertenencia a Cámaras de eco",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE3Fear <-(E3BFearDigi / E3BFearHomo)
PlotE3Fear<-PlotE3Fear + plot_annotation(title = 'Experimento N°3: Distribución de temor según condición experimental, \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                           theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3Fear, filename = "Results/Plots/E3FearSub.png",
       dpi = 400, width = 14, height = 11)

# Sadness #

E3BSadDigi <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Sad, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por niveles de ciudadanía digital",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital"))) 



E3BSadHomo <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Sad, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por pertenencia a Cámaras de eco",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE3Sad <-(E3BSadDigi / E3BSadHomo)
PlotE3Sad <-PlotE3Sad + plot_annotation(title = 'Experimento N°3: Distribución de tristeza según condición experimental, \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                         theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3Sad, filename = "Results/Plots/E3SadSub.png",
       dpi = 400, width = 14, height = 11)


# Joy  # 

E3BJoyDigi <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Joy, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital"))) 


E3BJoyHomo <-ggplot(data = df, aes(x = factor(E3Treat), y = E3Joy, color = factor(E3Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE3Joy <-(E3BJoyDigi / E3BJoyHomo)
PlotE3Joy <-PlotE3Joy + plot_annotation(title = 'Experimento N°3: Distribución de felicidad según condición experimental, \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                        theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE3Joy, filename = "Results/Plots/E3JoySub.png",
       dpi = 400, width = 14, height = 11)


#####################
#### Experiment 4 ###
#####################

CompE4 <-list(c("Amigo-No-Politico", "Amigo-Politico"), c("Familia-No-Politico", "Familia-Politico"),
              c("Amigo-No-Politico", "Familia-No-Politico"), c("Amigo-Politico", "Familia-Politico"))

### Discuss or avoid ###

E4DAngry <-ggplot(data = df, aes(x = factor(E4), y = E4Angry, color = factor(E4))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  scale_x_discrete(labels = c('Evadir discusión', 'Discutir abiertamente')) +
  theme_bw() +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 2.3, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Ira",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(method = "wilcox", label.y = 105)

E4DJoy <-ggplot(data = df, aes(x = factor(E4), y = E4Joy, color = factor(E4))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 2.8, vjust= -0.2) +
  scale_x_discrete(labels = c('Evadir discusión', 'Discutir abiertamente')) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(method = "wilcox", label.y = 105)

E4DSad <-ggplot(data = df, aes(x = factor(E4), y = E4Sad, color = factor(E4))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 2.4, vjust= -1.5) +
  scale_x_discrete(labels = c('Evadir discusión', 'Discutir abiertamente')) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Tristeza",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(method = "wilcox", label.y = 105)

E4DFear <-ggplot(data = df, aes(x = factor(E4), y = E4Fear, color = factor(E4))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 2.4, vjust= -0.7) +
  scale_x_discrete(labels = c('Evadir discusión', 'Discutir abiertamente')) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Temor",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(method = "wilcox", label.y = 105)


PlotE4D <-((E4DSad / E4DFear) | (E4DJoy / E4DAngry))
PlotE4D <-PlotE4D + plot_annotation(title = 'Experimento N°4: Probabilidad de discutir abiertamente, según intensidad de emociones',
                                    theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE4D, filename = "Results/Plots/Experimento4-discuss-emotions.png",
       dpi = 400, width = 12, height = 8)

### Emotions by treatment

## Emotions by treatment ##

# Anger #

E4EAngry <-ggplot(data = df, aes(x = factor(E4Treat), y = E4Angry, color = E4Treat)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5,  hjust = 1.2, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Ira",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = CompE4, method = "wilcox.test") +
  stat_compare_means(label.y = 145)

# Fear # 

E4EFear <-ggplot(data = df, aes(x = factor(E4Treat), y = E4Fear, color = factor(E4Treat))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Temor",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = CompE4, method = "wilcox.test") +
  stat_compare_means(label.y = 145)


# Joy # 

E4EJoy <-ggplot(data = df, aes(x = factor(E4Treat), y = E4Joy, color = factor(E4Treat))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = CompE4) +
  stat_compare_means(label.y = 145)


# Sadness # 

E4ESad <-ggplot(data = df, aes(x = factor(E4Treat), y = E4Sad, color = factor(E4Treat))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -1.2) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Tristeza",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = CompE4, method = "wilcox.test") +
  stat_compare_means(label.y = 145)

# Mix and create plot #

PlotE4E <-((E4ESad / E4EFear) | (E4EJoy / E4EAngry))
PlotE4E<-PlotE4E + plot_annotation(title = 'Experimento N°4: Nivel de emociones según condiciones experimentales',
                                   theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE4E, filename = "Results/Plots/E4_emocion.png",
       dpi = 400, width = 14, height = 11)


## by grouping treatments - political or non-politicals ##

E4EPAngry <-ggplot(data = df, aes(x = factor(E4TPol), y = E4Angry, color = factor(E4TPol))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5,  hjust = 1.2, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Ira",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Fear # 

E4EPFear <-ggplot(data = df, aes(x = factor(E4TPol), y = E4Fear, color = factor(E4TPol))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Temor",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Joy # 

E4EPJoy <-ggplot(data = df, aes(x = factor(E4TPol), y = E4Joy, color = factor(E4TPol))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Sadness # 

E4EPSad <-ggplot(data = df, aes(x = factor(E4TPol), y = E4Sad, color = factor(E4TPol))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Tristeza",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(comparisons = compE3, method = "wilcox.test") +
  stat_compare_means(label.y = 105, method = "wilcox.test")

# Mix and create plot #

PlotE4PE <-((E4EPSad / E4EPFear) | (E4EPJoy / E4EPAngry))
PlotE4PE<-PlotE4PE + plot_annotation(title = 'Experimento N°4: Nivel de emociones según condiciones experimentales, \n Agrupadas por tipo conversación Política o No-Política',
                                     theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE4PE, filename = "Results/Plots/E4_emocion-pol.png",
       dpi = 400, width = 11, height = 11)

## by grouping treatments - Type of tie (family or friend)  ##

E4EFAngry <-ggplot(data = df, aes(x = factor(E4TFam), y = E4Angry, color = factor(E4TFam))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5,  hjust = 1.2, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Ira",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Fear # 

E4EFFear <-ggplot(data = df, aes(x = factor(E4TFam), y = E4Fear, color = factor(E4TFam))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust= 1.5, vjust = -1) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Temor",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Joy # 

E4EFJoy <-ggplot(data = df, aes(x = factor(E4TFam), y = E4Joy, color = factor(E4TFam))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust = -1.5) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")


# Sadness # 

E4EFSad <-ggplot(data = df, aes(x = factor(E4TFam), y = E4Sad, color = factor(E4TFam))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme_bw() +
  theme(legend.position = "null") +
  labs(title = "Tristeza",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  stat_compare_means(label.y = 105, method = "wilcox.test")

# Mix and create plot #

PlotE4FE <-((E4EFSad / E4EFFear) | (E4EFJoy / E4EFAngry))
PlotE4FE<-PlotE4FE + plot_annotation(title = 'Experimento N°4: Nivel de emociones según condiciones experimentales, \n Agrupadas por tipo de lazo (Familiar o Amigo)',
                                     theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE4FE, filename = "Results/Plots/E4_emocion-family.png",
       dpi = 400, width = 11, height = 11)


## Emotions by digital citizenship and Echo Chamber membership ##

# Anger #

E4BAngryDigi <-ggplot(data = df, aes(x = factor(E4Treat), y = E4Angry, color = factor(E4Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por nivel de Ciudadanía Digital",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic")) +
  facet_wrap(~DigitIndex, nrow = 1, labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                       '1'="Alta Ciudadanía Digital")))


E4BAngryHomo <-ggplot(data = df, aes(x = factor(E4Treat), y = E4Angry, color = factor(E4Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por pertenencia a Cámaras de eco",
       x = "", y = "Nivel de ira",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE4Angry <-(E4BAngryDigi / E4BAngryHomo)
PlotE4Angry<-PlotE4Angry + plot_annotation(title = 'Experimento N°4: Distribución de temor según condición experimental \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                           theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE4Angry, filename = "Results/Plots/E4AngrySub.png",
       dpi = 400, width = 12, height = 9)


# Fear # 

E4BFearDigi <-ggplot(data = df, aes(x = factor(E4Treat), y = E4Fear, color = factor(E4Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por niveles de ciudadanía digital",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital"))) 



E4BFearHomo <-ggplot(data = df, aes(x = factor(E4Treat), y = E4Fear, color = factor(E4Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por pertenencia a Cámaras de eco",
       x = "", y = "Nivel de temor",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE4Fear <-(E4BFearDigi / E4BFearHomo)
PlotE4Fear<-PlotE4Fear + plot_annotation(title = 'Experimento N°4: Distribución de temor según condición experimental, \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                         theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE4Fear, filename = "Results/Plots/E4FearSub.png",
       dpi = 400, width = 12, height = 9)

# Sadness #

E4BSadDigi <-ggplot(data = df, aes(x = factor(E4Treat), y = E4Sad, color = factor(E4Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por niveles de ciudadanía digital",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital"))) 



E4BSadHomo <-ggplot(data = df, aes(x = factor(E4Treat), y = E3Sad, color = factor(E4Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Por pertenencia a Cámaras de eco",
       x = "", y = "Nivel de tristeza",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE4Sad <-(E4BSadDigi / E4BSadHomo)
PlotE4Sad <-PlotE4Sad + plot_annotation(title = 'Experimento N°4: Distribución de tristeza según condición experimental, \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                        theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE4Sad, filename = "Results/Plots/E4SadSub.png",
       dpi = 400, width = 12, height = 9)


# Joy  # 

E4BJoyDigi <-ggplot(data = df, aes(x = factor(E4Treat), y = E4Joy, color = factor(E4Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~DigitIndex, nrow = 1,labeller = labeller(DigitIndex = c('0'="Baja Ciudanía Digital",
                                                                      '1'="Alta Ciudadanía Digital"))) 


E4BJoyHomo <-ggplot(data = df, aes(x = factor(E4Treat), y = E4Joy, color = factor(E4Treat))) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  geom_jitter(width = 0.1) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(aes(label= round(..y.., 2)), fun=mean, geom="text", size=5, hjust = 1.5, vjust= -0.7) +
  theme(legend.position = "null") +
  labs(title = "Felicidad",
       x = "", y = "Nivel de felicidad",
       caption = "Fuente: Elaboración propia") +
  theme(plot.title = element_text(hjust = .5, size = 14),
        plot.caption = element_text(face = "italic"))+
  facet_wrap(~HomoIndex, nrow = 1,labeller = labeller(HomoIndex = c('0'="Baja membresia a Cámaras de eco",
                                                                    '1'="Alta membresia a Cámaras de eco")))

PlotE4Joy <-(E4BJoyDigi / E4BJoyHomo)
PlotE4Joy <-PlotE4Joy + plot_annotation(title = 'Experimento N°4: Distribución de felicidad según condición experimental, \n subdidido por Ciudadanía digital y membresía a Cámaras de eco',
                                        theme = theme(plot.title = element_text(size = 18,face = 'bold')))

ggsave(PlotE4Joy, filename = "Results/Plots/E4JoySub.png",
       dpi = 400, width = 12, height = 9)

