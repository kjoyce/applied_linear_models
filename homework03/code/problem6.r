shipment = read.csv('shipment.txt')
attach(shipment)
mdl = lm(time~drums+weight)
X = model.matrix(mdl)
y = time

# a
(xtx = t(X)%*%X)
(xty = t(X)%*%y)
(yty = t(y)%*%y)
(xtxi = solve(xtx))
(betahat = solve(xtx,xty))

# b
(betavar = solve(t(X)%*%X)*summary(mdl)$sigma^2

# c
(t = (betahat[3] - 50)/sqrt(betavar[3,3]))

# d
(se_d = sqrt(betavar[2,2] + 4*betavar[3,3] + 4*betavar[2,3]))
(t_d = (betahat[2] + 2*betahat[3])/se_d)

# e
round((est_d = betahat[2] + 2*betahat[3]),3)
round((me_d = qt(.95,17)*se_d),3)
cat("(",sprintf("%.3f",est_d + c(-1,1)*me_d),")\n")
