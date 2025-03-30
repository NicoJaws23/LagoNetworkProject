#Combined code
library(tidyverse)
library(lme4)
library(rptR)
c <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupC_allTimeCent.csv")
d <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupD_allTimeCent.csv")
g <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupG_allTimeCent.csv")
p <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupP_allTimeCent.csv")

#add group size and age.sex
c <- c |>
  mutate(GroupSize = 14, sex.age = paste(Sex, ADULT))
d <- d |>
  mutate(GroupSize = 16, sex.age = paste(Sex, ADULT))
g <- g |>
  mutate(GroupSize = 23, sex.age = paste(Sex, ADULT))
p <- p |>
  mutate(GroupSize = 15, sex.age = paste(Sex, ADULT))

allGroups <- bind_rows(c, d, g, p)

allGroups <-allGroups |>
  mutate(GroupSize = as.factor(GroupSize))

m1 <- lm(Eigenvector ~ GroupSize, data = allGroups)
summary(m1)
plot(m1)

m2 <- lmer(Eigenvector ~ (1|Group), data = allGroups)
summary(m2)


null <- lm(Eigenvector ~ 1, data = allGroups)
agesex <- lm(Eigenvector ~ sex.age, data = allGroups)
age <- lm(Eigenvector ~ ADULT, data = allGroups)
sex <- lm(Eigenvector ~ Sex, data = allGroups)

anova(null, agesex, age, sex)
