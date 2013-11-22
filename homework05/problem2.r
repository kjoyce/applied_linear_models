rm(list=ls(all=T))
library(faraway)
data(divusa)
attach(divusa)

# a
reg1 = lm(divorce~.-year,data=divusa)
plot(divusa$year,divorce,xlab="Year",ylab="Divorce Rate",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Divorce Rate over Time",cex.main=1.8) 
dev.copy(pdf,'div_time.pdf')
dev.off()
plot(divusa$year,reg1$residuals,xlab="Year",ylab="Residual",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Residuals over Time",cex.main=1.8) 
abline(h=0,lwd=2)
dev.copy(pdf,'div_resid.pdf')
dev.off()
reg2 = lm(divorce~.,data=divusa)
plot(divusa$year,reg2$residuals,xlab="Year",ylab="Residual",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Residuals (With Time) over Time",cex.main=1.8) 
abline(h=0,lwd=2)
dev.copy(pdf,'div_resid2.pdf')
dev.off()

# b
library(nlme)
reg3 = gls(divorce~.-year,data=divusa,correlation=corAR1(form=~year))
reg4 = gls(divorce~.-year,data=divusa,correlation=corAR1(form=~year),method='ML')
summary(reg3)
summary(reg4)

detach(divusa)
