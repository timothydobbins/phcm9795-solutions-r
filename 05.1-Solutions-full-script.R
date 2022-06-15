# install.packages("BSDA")
library(BSDA)
library(jmv)
library(broom)

# Activity 5.2

# Calculate difference in means by hand:
19.9 - 13.9

# t-test assuming equal variance
tsum.test(mean.x=19.9, s.x=5.9, n.x=12,
          mean.y=13.9, s.y=6.2, n.y=15,
          mu=0, alternative="two.sided", var.equal = TRUE)

# For information: the Welch's t-test provides similar results:
tsum.test(mean.x=19.9, s.x=5.9, n.x=12,
          mean.y=13.9, s.y=6.2, n.y=15,
          mu=0, alternative="two.sided")


# Activity 5.3
anaemia <- readRDS("data/activities/Activity_S5.3.rds")

descriptives(data=anaemia, vars=hematocrit,
             splitBy = group,
             skew = TRUE,
             hist = TRUE,
             box = TRUE)

# Welch's t-test
t.test(hematocrit ~ group, data=anaemia)
tidy(t.test(hematocrit ~ group, data=anaemia))

# t-test assuming equal variance
t.test(hematocrit ~ group, data=anaemia, var.equal=TRUE)
tidy(t.test(hematocrit ~ group, data=anaemia, var.equal=TRUE))


# Activity 5.4
babies <- readRDS("data/activities/Activity_S5.4.rds")

babies$diff = babies$week_12 - babies$baseline
hist(babies$diff, xlab="Volume (mm3)", main="Difference in haemangioma volume")

t.test(babies$week_12, babies$baseline, paired=TRUE)
