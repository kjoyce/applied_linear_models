crab = read.csv("crab.txt")
attach(crab)
library(car)

# b
scatterplot(log10(force)~log10(height)|code,xlab="log propodus height", 
  ylab="log closing force",cex.lab=1.5,cex.axis=1.5,
  smooth=FALSE,
  #lwd=2,lty=1,col=c(1,1,1),pch=c(16,1,2),cex=1.5,
  legend.coords="topleft",legend.title="Species",data=crab)
title('Log Propodus Height vs. Log Closing Force for Various Crab species')
dev.copy(pdf,'crab_scatter.pdf')
dev.off()

# c
lforce = log10(force)
lheight = log10(height)
mdl = lm(lforce~lheight+x2+x3+x2*lheight+x3*lheight) 

summary(mdl)
n = length(lforce)
anov = c(colSums(anova(mdl)[1:5,1:2]), 
         colSums(anova(mdl)[1:5,])[3]/5)
anov = c(anov, anov[3]/summary(mdl)$sigma^2)
anov = c(anov,1-pf(anov[4],anov[1],anova(mdl)[6,1]))
(anov = rbind(anov,anova(mdl)[6,]))
c(n-1,var(lforce)*(n-1),var(lforce))

mdl.red = lm(lforce~lheight+x2+x3)
anova(mdl.red,mdl)

# d
betahat = mdl$coef
lh = log10(11.5)
x0 = c(1,lh,0,1,0,lh)
X = model.matrix(mdl)
se = summary(mdl)$sigma*sqrt(1+t(x0)%*%solve(t(X)%*%X,x0))
(yhat = x0%*%betahat)
(me = qt(.975,n-mdl$rank)*se)
cat("(",round(yhat + c(-1,1)*me,3),")")
10^(c(yhat,me))
cat("(",round(10^yhat + c(-1,1)*10^me,3),")")


# e 
