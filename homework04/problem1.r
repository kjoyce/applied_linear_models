rm(list = ls())
data(swiss)
attach(swiss)
library(faraway) 
source("find_function.r")
reg.out = lm(Fertility ~ . , data = swiss)

#a
plot(fitted(reg.out),resid(reg.out),xlab="Predicted Fertility",ylab="Residuals",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Residual Plot",cex.main=1.8)
abline(h=0,lwd=2)
dev.copy(pdf,'resid1.pdf')
dev.off()

#b
qqnorm(resid(reg.out))
dev.copy(pdf,'qqplot1.pdf')
dev.off()
library(nortest)
sf.test(resid(reg.out))

#c
lv = find_leverage(reg.out)

#d
outlier = find_outlier(reg.out)
outlier$vals[outlier$idx]

#e
cd = find_cooksd(reg.out)
dff = find_dffits(reg.out)
dfb = find_dfbetas(reg.out)

#idx = Reduce(union, list(lv$idx,dfb$idx,dff$idx,cd$idx)) # this performs the n-union
#idx = idx[idx > 0]  # remove null finds
#idx = sort(idx)
#
#n = length(reg.out$residuals)
#cat(sprintf("%s\n",names(reg.out$residuals)[idx])) 
#print_latex_column(lv$val[idx],match(lv$idx,idx)) 
#print_latex_column(cd$val[idx],match(cd$idx,idx)) 
#print_latex_column(dff$val[idx],match(dff$idx,idx)) 
#print_latex_column(dfb$val[idx,1], dff[idx,1]>=2/sqrt(n) ) 

idx = lv$idx+cd$idx+dff$idx+dfb$idx > 0 
tab = cbind(lv$val,cd$val,dff$val )

stab = matrix(sprintf("%.3f & ",tab),nrow=nrow(tab))
stab[lv$idx,1] = sprintf("\\a{%.3f} & ",tab[lv$idx,1])
stab[cd$idx,2] = sprintf("\\a{%.3f} & ",tab[cd$idx,2])
stab[dff$idx,3] = sprintf("\\a{%.3f} & ",tab[dff$idx,3])

stabdfb = matrix(sprintf("%.3f & ",dfb$val),nrow=nrow(dfb$val))
stabdfb[dfb$ijdx] = sprintf("\\a{%.3f} &",dfb$val[dfb$ijdx])
stab = cbind(stab,stabdfb)

m = ncol(stab)
stab[,m] = sprintf("%s\n",stab[,m]) 
cat(t(stab[idx,c(1:3,5:m)])) # remove intercepts

halfnorm(lm.influence(reg.out)$hat,labs=names(reg.out$residuals),ylab="Leverages",cex.lab=1.6,cex.axis=1.5,mgp=c(2.7,1,0),main="Half-Normal Plot of Leverages",cex.main=1.8)
dev.copy(pdf,'half-normal1.pdf')
dev.off()

#f 
par(mfrow=c(2,3)) 
prplot(reg.out,1) 
prplot(reg.out,2) 
prplot(reg.out,3) 
prplot(reg.out,4) 
prplot(reg.out,5) 
dev.copy(pdf,'partial_residual_plots1.pdf',height=6,width=10) 
dev.off()

detach(swiss)
