library(faraway)
library(ellipse)
data(prostate) 
attach(prostate)
mdl = lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,data=prostate)

# a
print(sprintf("(%.4f,%.4f)",confint(mdl,level=.9)[4,1],confint(mdl,level= .9)[4,2]) ) 
print(sprintf("(%.4f,%.4f)",confint(mdl,level=.95)[4,1],confint(mdl,level= .95)[4,2]) ) 

print(sprintf("(%.4f,%.4f)",confint(mdl,level=1-.1/2)[4,1],confint(mdl,level= 1-.1/2)[4,2]) ) 
print(sprintf("(%.4f,%.4f)",confint(mdl,level=1-.05/2)[4,1],confint(mdl,level=1-.05/2)[4,2]) ) 
# b
plot(ellipse(mdl,c(4,5)),type="l",lwd=2, # Plots the conf. region
      cex.axis=1.5,xlab="Beta3 (Age)",
      ylab="Beta4 (lbph)",cex.lab=1.6,mgp=c(2.7,1,0)#,xlim=c(-.005,.01)
    )
title("Joint Confidence Region for Age and LBPH")
points(0,0,pch=16,cex=2)                 # Plot the origin           
text(0,0,'(0,0)',cex=1.6,pos=2)
dev.copy(pdf,'conf_region_1b.pdf')
dev.off()

# c
n = length(mdl$residuals) 
p = length(mdl$coefficients) 
x_0 = c(1, 1.44692 , 3.62301 , 65.00000 , 0.30010 , 0.00000 , -0.79851 , 7.00000 , 15.00000)
xtx = t(model.matrix(mdl))%*%model.matrix(mdl)
(yhat = (mdl$coefficients%*%x_0))
(me = qt(.975,n-p)*sqrt(deviance(mdl)/(n-p)*t(x_0)%*%solve(xtx,x_0)))   
cat("(",sprintf("%.3f",yhat + c(-1,1)*me),")")

# d
x_1 = c(1, 1.44692 , 3.62301 , 20 , 0.30010 , 0.00000 , -0.79851 , 7.00000 , 15.00000)
(yhat_1 = (mdl$coefficients%*%x_1))
(me_1 = qt(.975,n-p)*sqrt(deviance(mdl)/(n-p)*c(x_1)%*%solve(xtx,x_1)))   
cat("(",sprintf("%.3f",yhat_1 + c(-1,1)*me_1),")\n")
hist(age,cex.axis=1.6,cex.lab=1.5)
dev.copy(pdf,'age_hist.pdf')
dev.off()

# e
nperm = 4999
F1 = summary(mdl)$coefficients[4,3]^2 # Numerator to F test
numerator = sum(replicate(nperm, summary(lm(lpsa~lcavol+lweight+sample(age)+lbph+svi+lcp+gleason+pgg45))$coef[4,3]^2 > F1))
(p = (numerator+1)/(nperm+1))

