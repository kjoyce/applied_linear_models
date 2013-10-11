# (a)
tss = 902773
mst = 16718.02
(n = tss/mst + 1)

# (b)
ssreg = 887994
(r_sq = ssreg/tss)

# (c) 
p = 5
(mse = (tss - ssreg)/(n - p))

# (d)
tstar = 1.5554
se_beta2 = 0.4721
beta2 = tstar*se_beta2
bf_alph = 1-.05/(2*p)
(beta2_me = qt(bf_alph,n-p)*se_beta2)
cat("(",sprintf("%.3f",beta2 + c(-1,1)*beta2_me),")\n")

# (f)
beta1 =  0.93209 
se_beta1 = 0.08602
(beta1-1)/se_beta1
(2*pt((beta1-1)/se_beta1,n-p))

# (g)
beta3 = -.4982
se_beta3 = .152
(f3 = (beta3/se_beta3)^2) 

# (h)
(ssreg3_tot = f3*mse)

# (i)
ssreg3 = 3237
ssreg4 = 694
(f12 = (ssreg3 + ssreg4)/2/mse)
1-pf(f12,2,n-p)

# (j)
(rss123 = rss + ssreg4)
(f12_123 = ssreg3/(rss123/(n-p+1)))
1-pf(f12_123,2,n-p+1)

# (l)
se = 2.62
(yhat = c(1,100,25,50,4)%*%c(23.45,0.93209,beta2,beta3,3.486))
(yhat_ci_me = qt(.975,n-p)*se)
yhat_ci = yhat + c(-1,1)*yhat_ci_me
cat("(",sprintf("%.3f",yhat_ci),")\n")
(yhat_pi_me = qt(.975,n-p)*sqrt(mse+se^2))
yhat_pi =  yhat + c(-1,1)*yhat_pi_me

cat("(",sprintf("%.3f",yhat_pi),")\n")

# (m)
(fmod = (tss-rss)/(p-1)/mse)
1-pf(fmod,p-1,n-p)
