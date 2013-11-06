rm(list = ls())
data(divusa)
library(faraway) 
library(lmtest)
source("find_function.r")
divnew = divusa[,2:7]
rownames(divnew) = divusa$year
attach(divnew)
reg.out = lm(divorce ~ . , data = divnew)

plot(divusa$year,divorce,xlab="Year",ylab="Divorce Rate",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Divorce Rate over Time",cex.main=1.8) 
dev.copy(pdf,'time_plot21.pdf')
dev.off()
plot(divusa$year,reg.out$residuals,xlab="Year",ylab="Divorce Rate",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Divorce Rate over Time",cex.main=1.8) 
abline(h=0,lwd=2)
dev.copy(pdf,'time_plot22.pdf')
dev.off()


qqnorm(resid(reg.out))
dev.copy(pdf,'qqplot2.pdf')
dev.off()
library(nortest)
sf.test(resid(reg.out))
detach(divnew)


