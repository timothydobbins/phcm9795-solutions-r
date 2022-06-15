library(haven)
library(tidyverse)
library(summarytools)
library(DescTools)
library(epiR)

# Activity 7.1
asthma <- read_stata("/Users/td/Library/CloudStorage/OneDrive-UNSW/Teaching/SPH Biostatistics Teaching/PHCM9795 - Foundations of Biostatistics/2022/5 - Activities/Datasets/Learning activity data/Activity_S6.2.dta")
asthma$Asthma <- factor(asthma$Asthma, levels=c(1,2), labels=c("Asthma", "No asthma"))
asthma$Gender <- factor(asthma$Gender, levels=c(1,2), labels=c("Male", "Female"))

freq(asthma$Asthma)
freq(asthma$Gender)

tab <- table(asthma$Gender, asthma$Asthma)
chisq.test(tab, correct = FALSE)
chisq.test(tab)$expected

epi.2by2(tab) # From epiR

# Activity 7.2
heart <- read_stata("/Users/td/Library/CloudStorage/OneDrive-UNSW/Teaching/SPH Biostatistics Teaching/PHCM9795 - Foundations of Biostatistics/2022/5 - Activities/Datasets/Learning activity data/Activity_S7.2.dta")
heart$heart_attack <- factor(heart$heart_attack, levels=c(1,0), labels=c("Yes", "No"))
heart$mort <- factor(heart$mort, levels=c(1,0), labels=c("Yes", "No"))

tab_heart <- table(heart$heart_attack, heart$mort)

chisq.test(tab_heart)$expected
chisq.test(tab_heart)
epi.2by2(tab_heart) # From epiR

# Note that epi.2by2 needs data set out as per an epi study, with "yes"/"yes" being cell A


# Activity 7.3
allergen <- read_stata("/Users/td/Library/CloudStorage/OneDrive-UNSW/Teaching/SPH Biostatistics Teaching/PHCM9795 - Foundations of Biostatistics/2022/5 - Activities/Datasets/Learning activity data/Activity_S7.3.dta")
allergentab <- table(allergen$React_Allergen_G, allergen$React_Allergen_B)

mcnemar.test(allergentab)

epibasix::mcNemar(allergentab)$rd
epibasix::mcNemar(allergentab)$rd.CIL
epibasix::mcNemar(allergentab)$rd.CIU

# Activity 7.4
text <- "
               Prem, NotPrem
  Urban,    2, 198
  Rural,   5, 75"

tab7_4 <- TextToTable(text, header=TRUE, sep=",", dimnames=c("Region", "Birth"))
tab7_4

epi.2by2(tab7_4)

chisq.test(tab7_4)$expected
fisher.test(tab7_4)


