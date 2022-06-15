library(jmv)

# Activity 3.3
bp <- readRDS("data/activities/Activity_S1.4.rds")
head(bp)

hist(bp$diabp, probability = TRUE, ylim=c(0,0.04))
curve(dnorm(x, mean=mean(bp$diabp), sd=sd(bp$diabp)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

descriptives(bp, ci=TRUE)


# Activity 3.4### Copy this section
ci_mean <- function(n, mean, sd, width=0.95, digits=3){
  lcl <- mean - qt(p=(1 - (1-width)/2), df=n-1) * sd/sqrt(n)
  ucl <- mean + qt(p=(1 - (1-width)/2), df=n-1) * sd/sqrt(n)
  
  print(paste0(width*100, "%", " CI: ", format(round(lcl, digits=digits), nsmall = digits),
               " to ", format(round(ucl, digits=digits), nsmall = digits) ))
  
}
### End of copy

ci_mean(n=81, mean=2.7, sd=0.9, width=0.95)
