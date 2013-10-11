ozone = read.csv('ozone.txt')
attach(ozone)
library(car)
scatterplot(inhibit~uvb|surface,xlab="UVB",
  ylab="Percent Inhibition",cex.lab=1.6,cex.axis=1.5,
  smooth=FALSE,lwd=2,lty=1,col=c(1,1),pch=c(16,1),cex=1.5,
  legend.coords="topright",legend.title="Depth")
title('Phytoplankton Inhibition vs Ultraviolet Radiation')
dev.copy(pdf,'ozone_eda.pdf')
dev.off()

depth = as.numeric(surface==' D')	     # Make depth variable
mdl.full = lm(inhibit~uvb+depth+(uvb*depth)) # Fit model

(summary(mdl.full)$coefficients[,1:2])
(summary(mdl.full)$coefficients[4,])

plot(mdl.full$fitted,mdl.full$resid,	     # Residual Plot
  xlab="Predicted Percent Inhibition",
  ylab="Residuals",cex.lab=1.6,cex.axis=1.5,main="Residual Plot",
  cex.main=1.8,pch=16,mgp=c(2.7,1,0))
dev.copy(pdf,'ozone_resid.pdf')
dev.off()

qqnorm(mdl.full$resid,cex.lab=1.6,  # Normal quantile plot
  xlab="Standard Normal Quantiles",
  ylab="Residuals",cex.axis=1.5,main="Normal Quantile Plot",
  cex.main=1.8,pch=16,mgp=c(2.7,1,0))
dev.copy(pdf,'ozone_qqplot.pdf')
dev.off()
library(nortest)
sf.test(mdl.full$resid)

mdl2 = lm(asin(inhibit/100)~uvb+depth+(uvb*depth)) 
sf.test(mdl2$resid)

nperm = 4999
F1 = summary(mdl.full)$coefficients[4,3]^2
numerator = sum(replicate(nperm, summary(lm(inhibit~uvb+depth+sample(uvb*depth)))$coefficients[4,3]^2 > F1))
(p = (numerator+1)/(nperm+1))
