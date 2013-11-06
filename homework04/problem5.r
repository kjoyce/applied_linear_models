rm(list = ls())
library(MPV)
library(leaps)
big_pollution = read.csv("pollution.txt")
pollution = big_pollution[,c('y','x2','x4','x5','x7')] 
attach(pollution)

# a
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="gray", ...)
}
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <-cor(x, y)
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    if(missing(cex.cor)) cex.cor <- 0.9/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * abs(r))
}
pairs( pollution, upper.panel=panel.smooth,diag.panel=panel.hist, lower.panel=panel.cor)
dev.copy(pdf,'pollution_pairs.pdf')
dev.off()

# b
reg01 = lm(y~.,data=pollution)
reg02 = lm(y~.-x2,data=pollution)
reg03 = lm(y~.-x4,data=pollution)
reg04 = lm(y~.-x5,data=pollution)
reg05 = lm(y~.-x7,data=pollution)
reg06 = lm(y~.-x2-x4,data=pollution)
reg07 = lm(y~.-x2-x7,data=pollution)
reg08 = lm(y~.-x2-x5,data=pollution)
reg09 = lm(y~.-x4-x7,data=pollution)
reg10 = lm(y~.-x4-x5,data=pollution)
reg11 = lm(y~.-x5-x7,data=pollution)
reg12 = lm(y~x2,data=pollution)
reg13 = lm(y~x4,data=pollution)
reg14 = lm(y~x5,data=pollution)
reg15 = lm(y~x7,data=pollution)

tab = data.frame(matrix(nrow=15,ncol=7))
colnames(tab) = c("rsq","adj.rsq","Cp","rtMSE","AIC","BIC","PRESS")
n = length(y)
p = rep(4:1,choose(4,4:1))+1  # clever, eh?
for (i in 1:15) {
  rs = summary(get(sprintf("reg%02d",i)))
  rs1 = summary(get(sprintf("reg%02d",1)))
  rss = rs$sigma^2*(rs$df[2])
  tab[i,'rsq']     = rs$r.squared 
  tab[i,'adj.rsq'] = rs$adj.r.squared
  tab[i,'rtMSE']   = rs$sigma
  tab[i,'AIC']     = n*log(rss/n) + 2*p[i]
  tab[i,'BIC']     = n*log(rss/n) + p[i]*log(n)
  tab[i,'PRESS']   = PRESS(get(sprintf("reg%02d",i)))
  tab[i,'Cp']      = rss/rs1$sigma^2 - (n-2*p[i])
}

stab = matrix(sprintf("%.3g &",as.matrix(tab[,1:6])),nrow=nrow(tab)) 
stab = cbind(stab,sprintf("%.3g \\\\ \n",tab[,7]))
cat(t(stab))

# Not really needed
#subs = regsubsets(y~.,data=pollution)
#rs = summary(subs)

plot(fitted(reg02),resid(reg02),xlab="Predicted Fertility",ylab="Residuals",cex.lab=1.6,cex.axis=1.5,pch=16,cex=1.3,mgp=c(2.7,1,0),main="Residual Plot",cex.main=1.8)
abline(h=0,lwd=2)
dev.copy(pdf,'resid5.pdf')
dev.off()

qqnorm(resid(reg02))
dev.copy(pdf,'qqplot5.pdf')
dev.off()
library(nortest)
sf.test(resid(reg02))

source('find_function.r')
outlier = find_outlier(reg02)
lv = find_leverage(reg02)
cd = find_cooksd(reg02)
dff = find_dffits(reg02)
dfb = find_dfbetas(reg02)

idx = outlier$idx+lv$idx+cd$idx+dff$idx+dfb$idx > 0 
tab = cbind(outlier$val,lv$val,cd$val,dff$val )

stab = matrix(sprintf("%.3f & ",tab),nrow=nrow(tab))
stab[outlier$idx,1] = sprintf("\\a{%.3f} & ",tab[outlier$idx,1])
stab[lv$idx,2] = sprintf("\\a{%.3f} & ",tab[lv$idx,2])
stab[cd$idx,3] = sprintf("\\a{%.3f} & ",tab[cd$idx,3])
stab[dff$idx,4] = sprintf("\\a{%.3f} & ",tab[dff$idx,4])

stabdfb = matrix(sprintf("%.3f & ",dfb$val),nrow=nrow(dfb$val))
stabdfb[dfb$ijdx] = sprintf("\\a{%.3f} &",dfb$val[dfb$ijdx])
stab = cbind(names(reg02$residuals),stab,stabdfb)

m = ncol(stab)
stab[,m] = sprintf("%s\n",stab[,m]) 
cat(t(stab[idx,c(1:3,5:m)])) # remove intercepts
cat("\\\\ \\hline \n")
cat(sprintf("%.3f &",get_cutoffs(reg02)))

detach(pollution) 
