rm(list=ls(all=T))
library(gplots)
library(faraway)
library(MASS)
library(quantreg)
data(stackloss)
pairs(stackloss[,c(4,1,2,3)]) 
pr_plot = function(reg.out,i,data,hlidx=NULL,xlab=NULL,ylab=NULL) {
  if( is.null(xlab) ) { xlab <- attributes(reg.out$terms)$term.labels[i]  }
  if( is.null(ylab) ) { ylab <- paste("beta*", xlab, "+resid", sep = "")}
  x <- model.matrix(reg.out,data=data)[, i + 1]
  plot(x, (reg.out$coeff[i + 1] * x + reg.out$res), xlab = xlab, ylab = ylab,
    pch=20,cex=1.5,
  )
  if( !is.null(hlidx) ) {
    points(x[hlidx],(reg.out$coeff[i + 1] * x + reg.out$res)[hlidx],pch=19,col='red3')
    text(x[hlidx],(reg.out$coeff[i + 1] * x + reg.out$res)[hlidx], labels=as.character(hlidx),pos=4,col='red3')
  }
  abline(0, reg.out$coeff[i + 1])
  invisible()
}

source('../homework04/find_function.r')
reg02 = lm(stack.loss~.,data=stackloss)
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
cat(sprintf("%.3f &",get_cutoffs(reg02)),"\n")

lsreg    =     lm(stack.loss~.,data=stackloss)
lsreg2   =     lm(stack.loss~.,data=stackloss[-which(idx),])
ladreg   =     rq(stack.loss~.,data=stackloss)
huberreg =    rlm(stack.loss~.,data=stackloss)
trimreg  = ltsreg(stack.loss~.,data=stackloss,quantile=(nrow(stackloss)-2))

par(mfrow=c(5,4),mar=c(3.75,2,1,2),oma=c(2,2,3,2))
names = c("LS","LAD","Huber","LTS","LS Out Rem.")
reg = list( lsreg,ladreg,huberreg,trimreg,lsreg2 )
for( i in 1:4 ) {
  textplot(names[i],cex=2)
  for( j in 1:3 ) {
    pr_plot(reg[[i]],j,stackloss,hlidx=which(idx),ylab="Adj. Stack Loss")
  }
}
textplot(names[5],cex=2)
for( j in 1:3 ) {
  pr_plot(reg[[5]],j,stackloss[-which(idx),],ylab="Adj. Stack Loss")
}

title("Partial Residual Plots",outer=T,cex.main=2)
dev.copy(pdf,"robust_matrix.pdf")
dev.off()

# Bootstrap
n <- nrow(stackloss); B <- 5000 # Sample size, No. of bootstraps
X <- model.matrix(stack.loss~.,data=stackloss)   # Constructs X-matrix
bcoef <- matrix(0,B,4)                # Blank Bx2 matrix for boot coefs
for (i in 1:B){                       # Begin bootstrap loop
  samp <- sample(n,n,replace=T)       # Samples 1:67 with replacement
  newy <- predict(trimreg) +          # Computes bootstrap y-values
    residuals(trimreg)[samp]          #   using boot residual sample
  bootreg <- ltsreg(newy~X-1,         # Refits the bootstrap data using
    quantile=(n-1))                   #   LTS removing largest value
  bcoef[i,] <- bootreg$coef           # Writes bootstrap coefs to bcoef
}                                     # End of for loop
(bootse <- apply(bcoef,2,sd))         # Bootstrap SEs
p = 4
alpha = .05
tab = cbind(
  estimate=coef(trimreg),
  lower=sapply(1:4,function(i) {quantile(bcoef[,i],.05/(2*p))}),
  upper=sapply(1:4,function(i) {quantile(bcoef[,i],1-.05/(2*p))})
)
round(tab,3)

round(confint(lsreg,level=(1-.05/p)),3)
round(confint(lsreg2,level=(1-.05/p)),3)

