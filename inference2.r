# Code for Notes on "Hypothesis Tests to Compare Two Models"
# ==========================================================
meadowfoam <- read.csv("meadowfoam.txt")
attach(meadowfoam)
library(car)
scatterplot(flowers~intensity|timing,xlab="Light Intensity",
  ylab="Average Number of Flowers",cex.lab=1.6,cex.axis=1.5,
  smoother=F,lwd=2,lty=1,col=c(1,1),pch=c(16,1),cex=1.5,
  legend.coords="topright",legend.title="Timing")

timing01 <- as.numeric(timing=="At PFI")        # Creates a 0-1 timing variable
x1x2 <- intensity*timing01                      # Defines the interaction term
reg.full <- lm(flowers~intensity+timing01+x1x2) # Fits the linear model
summary(reg.full)                               # Regression summary

n <- length(flowers)                          # Sample size
(TSS <- (n-1)*var(flowers))                   # Total sum of squares
(RSSfull <- deviance(reg.full))               # Full model Residual SS
MSEfull <- RSSfull/df.residual(reg.full)      # Full model MSE
(fstat <- ((TSS-RSSfull)/3)/MSEfull)          # Model Sig. F-statistic
(pval <- 1-pf(fstat,3,df.residual(reg.full))) # Model F-test P-value

reg.012 <- lm(flowers~intensity+timing01)     # Model without interaction
(RSSred <- deviance(reg.012))                 # Reduced model residual SS
(fstat <- ((RSSred-RSSfull)/1)/MSEfull)       # F-statistic to test b3=0
(pval <- 1-pf(fstat,1,df.residual(reg.full))) # F-test p-value
anova(reg.012,reg.full)                       # F-test to compare 2 models

reg.01 <- lm(flowers~intensity)               # Model without timing
anova(reg.01,reg.full)                        # F-test to compare 2 models

library(faraway)                         # Loads the faraway library
data(teengamb)                           # Loads teen gambling dataset
attach(teengamb)                         # Attaches teen gambling dataset
loggamble <- log(gamble+1)               # (Log+1) transform on gamble
reg.full <- lm(loggamble~verbal+status)  # Model of loggamble on V&S
(reg.summ <- summary(reg.full))          # Regression summary info
(factual <- reg.summ$fstat)              # Actual F-statistic from data
nperm <- 4999                            # Number of permutations
fstats <- numeric(nperm)                 # Blank vector of perm. F-values
for (i in 1:nperm){                      # Beginning of permutation loop
  permfit <- lm(sample(loggamble)~       # Permutes the loggamble values
    verbal+status)                       #   and fits a permutation model
  fstats[i] <- summary(permfit)$fstat[1] # ith permutation F-statistic
}                                        # End of loop
perm.pval <- (sum(fstats>factual[1])+1)/ # Computes permutation p-value
 (nperm+1)
perm.pval                                # Prints permutation p-value

hist(fstats,cex.lab=1.6,cex.axis=1.5,    # Histogram of permutation F-
  xlab="Permutation F-Statistics",ylab=  #   statistics
  "Frequency",density=12,angle=45,
  mgp=c(2.7,1,0),main="")
abline(v=factual[1],lwd=3)               # Vertical line at actual F-value

