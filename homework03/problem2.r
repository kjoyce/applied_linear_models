library(faraway)
data(teengamb)
attach(teengamb)

mdl = lm(gamble~sex+status+income+verbal)

# a
(summary(mdl))

# c
n = length(gamble)
p = length(mdl$coefficient)
xtx = t(model.matrix(mdl))%*%model.matrix(mdl)
(x_m = c(1,0,colMeans(mdl$model[,c(-1,-2)])) )
(y_m = t(mdl$coefficients)%*%x_m)
(me = qt(.975,n-p)*sqrt(deviance(mdl)/(n-p)*c(x_m)%*%solve(xtx,x_m)))   
sprintf("%.2f",y_m + c(-1,1)*me)

# d
mdl2 = lm(gamble~income)
anova(mdl,mdl2)

