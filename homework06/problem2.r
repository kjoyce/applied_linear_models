library(lme4) 
library(faraway)
data(eggprod)
attach(eggprod)

mmod = lmer(eggs ~ treat + (1|block),eggprod)
