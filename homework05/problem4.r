rm(list=ls(all=T))
library(faraway)
data(PlantGrowth)
attach(PlantGrowth)

stripchart(weight~group,data=PlantGrowth,vertical=T,group.names=c("Control","Treatment 1","Treatment 2") ) 

summary(lm(weight~group)) # moderate evidence of significance

reg2 = lm(weight~group-1) 
n = length(resid(reg2))

l = coef(reg2)[1] - .5*(coef(reg2)[2] + coef(reg2)[3])
mse = summary(reg2)$sigma^2
J = tapply(weight,group,length)
lam = c(1,.5,.5)
se = sqrt(mse*sum(lam^2/J))
pt(l/se,reg2$df.residual) # no evidence for treatment effect
