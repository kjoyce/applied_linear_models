sat <- read.csv("Data/satscore.txt")
summary(sat)
pairs(sat[,-1])
satnew <- data.frame(sat[,c(1,2)],logtak=log(sat$takers),
  sat[,4:8])
round(cor(satnew[,-1]),digits=2)

reg.out <- lm(sat~logtak+years+expend,  # Fits SAT score vs. log(takers),
  data=satnew)                          #   years, and expend

# Residual Plot - SAT Data
# ========================
plot(fitted(reg.out),resid(reg.out),
  xlab="Predicted Values",ylab=
  "Residuals",cex.lab=1.6,cex.axis=
  1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),
  main="Residual Plot",cex.main=1.8)
abline(h=0,lwd=2)

summary(lm(abs(resid(reg.out))~fitted(reg.out)))

# Residual vs. Predictor Plots - SAT Data
# =======================================
par(mfrow=c(2,2))
plot(satnew$logtak,resid(reg.out),xlab="Log(Takers)",ylab="Residuals",
  cex.axis=1.3,cex.lab=1.5,cex=1.3,pch=16,mgp=c(2.7,1,0))
plot(satnew$years,resid(reg.out),xlab="Years",ylab="Residuals",
  cex.axis=1.3,cex.lab=1.5,cex=1.3,pch=16,mgp=c(2.7,1,0))
plot(0,0,axes=F,pch=" ",xlab="",ylab="")
plot(satnew$expend,resid(reg.out),xlab="Expenditures",ylab="Residuals",
  cex.axis=1.3,cex.lab=1.5,cex=1.3,pch=16,mgp=c(2.7,1,0))

# Test for variance equality between east and west states
# =======================================================
var.test(resid(reg.out)[satnew$logtak<3],
         resid(reg.out)[satnew$logtak>3])

# Model for SAT on TAKERS with residual plot
# ==========================================
reg2.out <- lm(sat~takers,data=sat)
par(mfrow=c(1,1))
plot(fitted(reg2.out),resid(reg2.out),
  xlab="Predicted Values",ylab=
  "Residuals",cex.lab=1.6,cex.axis=
  1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),
  main="Residual Plot with TAKERS",cex.main=1.8)
abline(h=0,lwd=2)

Durbin-Watson test for sales data
# ===============================
month <- seq(1,14)
sales <- c(6,6.3,6.1,6.8,7.5,8,8.1,8.5,9,8.7,7.9,8.2,8.4,9)
plot(month,sales,xlab="Month",ylab="Sales (millions of $)",pch=16,cex=2,
  cex.axis=1.5,cex.lab=1.6,main="Sales vs. Month",cex.main=1.8,
  mgp=c(2.7,1,0))
reg <- lm(sales ~ month)
plot(month,reg$residuals,xlab="Month",ylab="Residuals",pch=16,cex=2,
  mgp=c(2.7,1,0),cex.axis=1.5,cex.lab=1.6,main="Time Plot of Residuals",
  cex.main=1.8)
abline(h=0)

# Computes Durbin-Watson test statistic, p-value
# ==============================================
library(lmtest)
dwtest(sales~month)

# Computes runs test statistic, p-value
# =====================================
library(tseries)
signs <- as.factor(sign(reg$resid))
runs.test(signs)





