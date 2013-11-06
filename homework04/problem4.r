rm(list = ls())
warming = read.csv('warming2005.txt')
attach(warming)
library(lmtest)

# a
plot(year,temp,xlab="Year",ylab="Temperature deg. C",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Temp. of Northern Hemisphere over Time",cex.main=1.8) 
dev.copy(pdf,'time_plot4.pdf')
dev.off()

reg.out = lm(temp~year) 
beta = reg.out$coefficients
sebeta = sqrt(diag(summary(reg.out)$cov.unscaled))*summary(reg.out)$sigma
tstar = beta[2]/sebeta[2]
1-pt(tstar,length(temp)-2)

# b
qqnorm(resid(reg.out))
dev.copy(pdf,'qqplot4.pdf')
dev.off()
library(nortest)
sf.test(resid(reg.out))

# c
plot(year,reg.out$residuals,xlab="Year",ylab="Model Residuals",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Residuals over Time",cex.main=1.8) 
dev.copy(pdf,'resid4.pdf')
dev.off()
dwtest(reg.out)

# d
library(tseries)
signs = as.factor(sign(reg.out$resid))
runs.test(signs)
