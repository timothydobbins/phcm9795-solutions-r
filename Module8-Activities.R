iq <- readRDS("data/activities/Activity_S8.2.rds")

plot(iq$age, iq$iq, 
     main="Scatter plot of IQ against age",
     xlab = "Age (years)",
     ylab = "IQ")

abline(lm(iq$iq ~ iq$age))

cor.test(iq$age, iq$iq)

model <- lm(iq$iq ~ iq$age)
summary(model)
confint(model)

resids <- resid(model)

hist(resids, probability =TRUE,
     main = "Histogram of residuals")
curve(dnorm(x,
            mean=mean(resids),
            sd=sd(resids)), add = TRUE)
