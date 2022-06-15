library(haven)
library(tidyverse)
library(skimr)

iq <- read_stata("/Users/td/Library/CloudStorage/OneDrive-UNSW/Teaching/SPH Biostatistics Teaching/PHCM9795 - Foundations of Biostatistics/2022/5 - Activities/Datasets/Learning activity data/Activity_S8.2.dta")
skim(iq)

plot(iq$age, iq$iq)
abline(lm(iq$iq ~ iq$age))

ggplot(iq, aes(x=age, y=iq)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE)

cor.test(iq$age, iq$iq)

model <- lm(iq$iq ~ iq$age)
summary(model)
confint(model)
