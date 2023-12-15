library(tidyverse)

# select just the valid surveys

test5 <-test5%>%
  slice(-c(1,2))

test5 <-filter(test5, Consent=='1')%>%
  dplyr::filter(Finished=="1")

names(test5)


# Eliminate incomplete surveys
test5 <-test5%>%
  drop_na("Homo_5")

class(test5$Homo_5)
as.numeric(test5$Homo_5)



hist(test5$Homo5)

glimpse(test5)
