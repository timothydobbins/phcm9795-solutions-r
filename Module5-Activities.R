library(haven)
library(tidyverse)

# Activity 5.2
t.test.grouped <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}

t.test.grouped(m1=19.9, s1=5.9, n1=12, m2=13.9, s2=6.2, n2=15, equal.variance=TRUE)
t.test.grouped(m1=19.9, s1=5.9, n1=12, m2=13.9, s2=6.2, n2=15, equal.variance=FALSE)

# Activity 5.3
anemia <- read_stata("/Users/td/Library/CloudStorage/OneDrive-UNSW/Teaching/SPH Biostatistics Teaching/PHCM9795 - Foundations of Biostatistics/2022/5 - Activities/Datasets/Learning activity data/Activity_S5.3.dta")

hist(anemia$hematocrit)
ggplot(anemia, aes(x=hematocrit)) + 
  geom_histogram(boundary=28, binwidth=1, colour="black", fill="white") +
  scale_x_continuous(breaks=seq(28, 38, 1)) +
  facet_wrap(~group)

ggplot(anemia, aes(group=group, y=hematocrit)) + 
  geom_boxplot()

t.test(hematocrit ~ group, anemia)

broom::tidy(t.test(hematocrit ~ group, anemia))

# Activity 5.4
babies <- read_stata("/Users/td/Library/CloudStorage/OneDrive-UNSW/Teaching/SPH Biostatistics Teaching/PHCM9795 - Foundations of Biostatistics/2022/5 - Activities/Datasets/Learning activity data/Activity_S5.4.dta")
babies

babies$diff <- babies$week_12 - babies$baseline
hist(babies$diff)

t.test(babies$baseline, babies$week_12, paired=TRUE)
