library(faraway) 
library(gplots)
data(pipeline) 
attach(pipeline) 

# a
reg1 = lm(Lab ~ Field)
plot(fitted(reg1),resid(reg1),xlab="Predicted Lab Measurement",ylab="Residuals",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Residual Plot",cex.main=1.8)
abline(h=0,lwd=2)
dev.copy(pdf,'nist_resid.pdf')
dev.off()

# b
sort_idx = order(Field) 
grps = factor(ceiling(sort_idx/9))  # this factors into groups of size at most 9
meanfield = tapply(Field,grps,mean)
varlab = tapply(Lab,grps,var)

wls_reg = lm(log(varlab)~log(meanfield))
X = cbind(1,log(Field))
weights = 1 / exp( X%*%coef(wls_reg) )
reg2 = lm(Lab~Field,weight=weights)
summary(reg2)
summary(reg1)
plot(Field,Lab) 
title("Least Squares Fits") 
abline(reg1) 
abline(reg2,lty=5) 
legend(x=49.72192,y=24.26852,c("Regular Least Squares","Weighted Least Squares"),lty=c(1,5))
dev.copy(pdf,"wls_nist.pdf")
dev.off()


# c
labs = list(Lab,sqrt(Lab),log(Lab),1/Lab)
fields = list(Field,sqrt(Field),log(Field),1/Field)
residual_plot = function(reg,resp="Response") {
  plot(fitted(reg),resid(reg),ann=F)
  abline(h=0,lwd=2)
}
par(mfrow=c(5,5),mar=c(1,1,1,1))
ttls = c( 'None'   , 'Sqrt'   , 'Log'    , 'Inverse' )
textplot('Field\\Lab',cex=2)
for( i in 1:4 ) {
  textplot(ttls[i],cex=2.3)
}
for( i in 1:4 ) {
  textplot(ttls[i],cex=2.3)
  for( j in 1:4 ) {
    residual_plot(lm(labs[[i]]~fields[[j]]))
  }
}
dev.copy(pdf,"resid_matrix.pdf")
dev.off()
detach(pipeline)
