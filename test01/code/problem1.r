nitriver = read.csv('nitriver.txt')
attach(nitriver)
library(TeachingDemos)

nitdata = cbind(nitconc, discharg, runoff, precip, area, density, dep, nprec, export)
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
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs( nitdata, upper.panel=panel.smooth,diag.panel=panel.hist, lower.panel=panel.cor)
cor(nitdata)
dev.copy(pdf,'eda_problem1.pdf')
dev.off()

# b
alpha = .05
mdl = lm(nitconc~runoff+dep+nprec+export)
(beta_hat = summary(mdl)$coef[,1])
 beta_se  = summary(mdl)$coef[,2]
 beta_t   = qt(1-alpha/(2*mdl$rank),mdl$df.residual)
(beta_me  = beta_t*beta_se)
(beta_ci  = beta_hat%*%t(c(1,1)) + beta_t*beta_se%*%t(c(-1,1)))

# c
nperm = 4999
(f_full = summary(mdl)$coef[4,3]^2)
numerator = sum(replicate(nperm, summary(lm(nitconc~runoff+dep+sample(nprec)+export))$coef[4,3]^2 > f_full))
(p = (numerator+1)/(nperm+1))
