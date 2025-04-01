#Combined code
library(tidyverse)
library(lme4)
library(rptR)
library(ggplot2)
library(cowplot)
c <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupC_allTimeCent.csv")
d <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupD_allTimeCent.csv")
g <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupG_allTimeCent.csv")
p <- read_csv("C:\\Users\\Jawor\\Desktop\\ANT392J\\LagoNetworkProject\\Exports\\groupP_allTimeCent.csv")

#add group size and age.sex
c <- c |>
  mutate(GroupSize = 14, sex.age = paste(Sex, ADULT, sep = "_"))
d <- d |>
  mutate(GroupSize = 16, sex.age = paste(Sex, ADULT, sep = "_"))
g <- g |>
  mutate(GroupSize = 23, sex.age = paste(Sex, ADULT, sep = "_"))
p <- p |>
  mutate(GroupSize = 15, sex.age = paste(Sex, ADULT, sep = "_"))

allGroups <- bind_rows(c, d, g, p)

allGroups <-allGroups |>
  mutate(GroupSize = as.factor(GroupSize), sex.age = as.factor(sex.age))

m1 <- lm(Eigenvector ~ GroupSize, data = allGroups)
summary(m1)
plot(m1)

m2 <- lmer(Eigenvector ~ (1|Group), data = allGroups)
summary(m2)


null <- lm(Eigenvector ~ 1, data = allGroups)
agesex <- lm(Eigenvector ~ ADULT * Sex, data = allGroups)
age <- lm(Eigenvector ~ ADULT, data = allGroups)
sex <- lm(Eigenvector ~ Sex, data = allGroups)
summary(agesex)
anova(null, agesex, age, sex)
relevel(allGroups$sex.age, ref = "M_N")
summary(age)

a <- ggplot(data = allGroups, mapping = aes(x = ADULT, y = Eigenvector)) +
  geom_boxplot() +
  ggtitle("Age and Eigenvector Centrality") +
  scale_x_discrete(labels = c("N" = "Non-Adult", "Y" = "Adult")) +
  labs(x = "Age Classification", y = "Eigenvector Score")

b <- ggplot(data = allGroups, mapping = aes(x = sex.age, y = Eigenvector)) +
  geom_boxplot() +
  ggtitle("Age-Sex Class and Eigenvector Centrality") +
  scale_x_discrete(labels = c("F_N" = "Non-Adult Female", "M_N" = "Non-Adult Male", "F_Y" = "Adult Female", "M_Y" = "Adult Male")) +
  labs(x = "Age-Sex Classification", y = "Eigenvector Score")
plot_grid(a, b)

ggplot(data = allGroups, mapping = aes(x = Group, y = Eigenvector, color = Individual, shape = Sex)) +
  geom_point() +
  scale_shape_manual(values = c("M" = 17, "F" = 15)) +  # Assigns triangle to males, square to females
  labs(x = "Group ID", y = "Eigenvector Score") +
  theme_minimal()
