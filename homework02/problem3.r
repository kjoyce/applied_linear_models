library(faraway)
data(prostate)
attach(prostate)

get_r2_rss <- function (i) { 
mdl = lm( lpsa ~ as.matrix(prostate[,idxs[1:i]]) ) 
sm = summary(mdl) 
return( c(sm$r.squared, deviance(mdl)) )
} 

idxs = c(1,2,5,4,3,6,8,7)  # indices of explanatory variables
mat_out = t(sapply(1:length(idxs),get_r2_rss)) 
par(mfrow=(c(1,2)))
plot(mat_out[,1],xlab='Model Index',ylab='R^2')
plot(mat_out[,2],xlab='Model Index',ylab='RSS') 
dev.copy(pdf,'parameter_add_trends.pdf') 
dev.off()
