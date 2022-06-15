library(haven)
library(tidyverse)
library(skimr)
library(readxl)

energy <- read_xls("/Users/td/Library/CloudStorage/OneDrive-UNSW/Teaching/SPH Biostatistics Teaching/PHCM9795 - Foundations of Biostatistics/2022/5 - Activities/Datasets/Learning activity data/Activity_S4.3.xls")

hist(energy$Energy)
t.test(energy$Energy, mu=7750)
