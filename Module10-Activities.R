library(epiR)

# Activity 10.2
epi.sscohortc(irexp1=0.4, irexp0=0.1, n=NA, power=0.8)
epi.sscohortc(irexp1=0.4, irexp0=0.1, r=0.5, n=NA, power=0.8)
epi.sscohortc(irexp1=0.4, irexp0=0.2, n=NA, power=0.8)

82/(1-0.15)

epi.sscompb(treat=1.4*0.02065, control=0.02065, n=NA, power=0.9)

# Activity 10.3
epi.sscc(OR=2.5, p0=0.2, n=NA, power=0.9)
epi.sscc(OR=1.5, p0=0.2, n=NA, power=0.9)
epi.sscc(OR=1.5, p0=0.2, n=NA, power=0.9, r=1/3)

# Activity 10.4
epi.sscompc(treat = 4.0, control = 4.4, n = NA, sigma = 1.25, power = 0.8)
epi.sscompc(treat = 4.2, control = 4.4, n = NA, sigma = 1.25, power = 0.8)
