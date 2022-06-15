library(haven)
library(tidyverse)
library(skimr)
library(summarytools)

# Activity 9.1
vitc <- read_stata("/Users/td/Library/CloudStorage/OneDrive-UNSW/Teaching/SPH Biostatistics Teaching/PHCM9795 - Foundations of Biostatistics/2022/5 - Activities/Datasets/Learning activity data/Activity_S9.1.dta")
skim(vitc)

hist(vitc$ascorb)

vitc_1 <- filter(vitc, dint==1)
vitc_2 <- filter(vitc, dint==2)

hist(vitc_1$ascorb)
hist(vitc_2$ascorb)

boxplot(ascorb ~ dint, data=vitc)


ggplot(vitc, aes(x=ascorb)) +
  facet_wrap(~dint) +
  geom_histogram(boundary=0, binwidth=50, colour="black", fill="white") +
  theme_bw()

ggplot(vitc, aes(x=ascorb, group=dint)) +
  geom_boxplot() +
  theme_bw()

descr(vitc$ascorb)

vitc %>% 
  group_by(dint) %>% 
  descr()

wilcox.test(ascorb ~ dint, data=vitc, correct=FALSE)

# Activity 9.2
hyp <- read_stata("/Users/td/Library/CloudStorage/OneDrive-UNSW/Teaching/SPH Biostatistics Teaching/PHCM9795 - Foundations of Biostatistics/2022/5 - Activities/Datasets/Learning activity data/Activity_S9.2.dta")
skim(hyp)
hyp

hist(hyp$difference)

ggplot(hyp, aes(x=difference)) +
  geom_histogram(boundary=-60, binwidth=10, colour="black", fill="white")

wilcox.test(Pair(bp_before_mmhg, bp_after_mmhg) ~ 1, data=hyp)
wilcox.test(difference ~ 1, data=hyp)
wilcox.test(hyp$bp_before_mmhg, hyp$bp_after_mmhg, paired=TRUE)

descr(hyp)
