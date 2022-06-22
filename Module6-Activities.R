library(DescTools)
library(epiR)
library(jmv)

# Activity 6.1
alcohol <- readRDS("data/activities/Activity_S6.1.rds")

# alcohol$Drinking_Status <- factor(alcohol$Drinking_Status, levels=c(0,1), labels=c("Non-drinker", "Drinker"))
table(alcohol$Drinking_Status)

binom.test(90, 150, p=0.7)

# BinomCI from DescTools provides alternative CI calculation methods
BinomCI(90, 150, method="wilson")
BinomCI(90, 150, method="wald")
BinomCI(90, 150, method="clopper") # Default from binom.test

# Activity 6.2
asthma <- readRDS("data/activities/Activity_S6.2.rds")

asthma$asthma89 <- as.numeric(asthma$Asthma) + 7
asthma$gender89 <- as.numeric(asthma$Gender) + 7

#asthma$Asthma <- factor(asthma$Asthma, levels=c(1,2), labels=c("Asthma", "No asthma"))
#asthma$Gender <- factor(asthma$Gender, levels=c(1,2), labels=c("Male", "Female"))

table(asthma$Asthma)
table(asthma$asthma89)
table(asthma$Gender)
table(asthma$gender89)

tab <- table(asthma$Gender, asthma$Asthma)
tab
epi.2by2(tab) # From epiR

tab89 <- table(asthma$gender89, asthma$asthma89)
tab
epi.2by2(tab) # From epiR


contTables(data=asthma, rows=Gender, cols=Asthma, 
           odds=TRUE, relRisk = TRUE, diffProp = TRUE)

contTables(data=asthma, rows=gender89, cols=asthma89, 
           odds=TRUE, relRisk = TRUE, diffProp = TRUE)


text <- "
               Death, NoDeath
  HeartAttack,    10, 35
  NoHeartAttack,   5, 39"

tab6_3 <- TextToTable(text, header=TRUE, sep=",", dimnames=c("Exposure", "Outcome"))
tab6_3

epi.2by2(tab6_3)

summ <- data.frame(
  heartattack = c(0,0,1,1),
  death = c(0,1,0,1),
  count = c(39, 5, 35, 10)
)

summ

summ$ha_fct <- factor(summ$heartattack, levels=c(1,0), labels=c("Yes", "No"))
summ$death_fct <- factor(summ$death, levels=c(1,0), labels=c("Yes", "No"))

summ

contTables(data=summ, rows=heartattack, cols=death, counts=count,
           pcRow=TRUE, relRisk = TRUE, odds = TRUE)

contTables(data=summ, rows=ha_fct, cols=death_fct, counts=count,
           pcRow=TRUE, relRisk = TRUE, odds = TRUE, diffProp = TRUE)
