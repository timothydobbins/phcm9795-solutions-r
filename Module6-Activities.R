library(haven)
library(tidyverse)
library(summarytools)
library(DescTools)
library(epiR)

# Activity 6.1
alcohol <- read_stata("/Users/td/Library/CloudStorage/OneDrive-UNSW/Teaching/SPH Biostatistics Teaching/PHCM9795 - Foundations of Biostatistics/2022/5 - Activities/Datasets/Learning activity data/Activity_S6.1.dta")

alcohol$Drinking_Status <- factor(alcohol$Drinking_Status, levels=c(0,1), labels=c("Non-drinker", "Drinker"))
table(alcohol$Drinking_Status)
freq(alcohol$Drinking_Status)

binom.test(90, 150, p=0.7)

# BinomCI from DescTools provides alternative CI calculation methods
BinomCI(90, 150, method="wilson")
BinomCI(90, 150, method="wald")
BinomCI(90, 150, method="clopper") # Default from binom.test

# Activity 6.2
asthma <- read_stata("/Users/td/Library/CloudStorage/OneDrive-UNSW/Teaching/SPH Biostatistics Teaching/PHCM9795 - Foundations of Biostatistics/2022/5 - Activities/Datasets/Learning activity data/Activity_S6.2.dta")
asthma$Asthma <- factor(asthma$Asthma, levels=c(1,2), labels=c("Asthma", "No asthma"))
asthma$Gender <- factor(asthma$Gender, levels=c(1,2), labels=c("Male", "Female"))

freq(asthma$Asthma)
freq(asthma$Gender)

tab <- table(asthma$Gender, asthma$Asthma)
tab
epi.2by2(tab) # From epiR


text <- "
               Death, NoDeath
  HeartAttack,    10, 35
  NoHeartAttack,   5, 39"

tab6_3 <- TextToTable(txt, header=TRUE, sep=",", dimnames=c("Exposure", "Outcome"))
tab6_3

epi.2by2(tab6_3)

