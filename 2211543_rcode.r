############## PACKAGES ################

library(tidyverse)
library(ggrepel)
library(vtable)
library(cluster)
library(naniar)
library(Boruta)
library(MASS)
library(knitr)
library(kableExtra)
library(factoextra)
library(gridExtra)
library(caTools)
library(vtable)
library(summarytools)

############## DATA FETCH #################

setwd("C:/Files/UoE/Dissertation/DatasetEmployee")
empcpy <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

names(empcpy)

### Data vis ###

# attrition percent
empcpy %>%
  ggplot(aes(x=Attrition)) + geom_bar()

# bi-variate analysis between age and attrition
empcpy %>%
  ggplot(aes(x=Age)) + geom_bar(fill="darkgreen")

# gender vs attrition
gender_emp <- empcpy %>%
  group_by(Gender) %>%
  summarise(counts = n())

ggplot(gender_emp, aes(x="", y=counts, fill=Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label_repel(aes(x = 1, label = paste0(counts)), nudge_x = 0.8, size = 4, show.legend = FALSE, box.padding = 1) +
  scale_fill_brewer(palette = "Set4") +
  theme_void()

# age vs monthly income
ggplot(empcpy, aes(x = Age, y = MonthlyIncome)) +
  geom_point(color = "darkblue")

# missing values
vis_miss(empcpy)

# overtime vs attrition
empcpy %>%
  ggplot(aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "dodge")

