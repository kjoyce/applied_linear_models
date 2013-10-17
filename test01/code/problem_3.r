# problem 3
# a
seq_ssreg = c(59256, 20492, 51163, 867  , 21110)
p = 6
rss = 75385
r_sq = .6698
se_resid = 37.36

(ssreg = sum(seq_ssreg))
(msr = ssreg/(p-1))
(tss = ssreg / r_sq)
(mse = se_resid^2)
(n = round(rss/mse + p))
(mst = tss/(n-1))
(f = msr/mse)

beta =c(1000.1021 , 1.3792   , -15.0790 , 3.1602   , -0.1076, NA  )
se_beta = c(92.398, 0.7000, 7.0706, NA    , 0.1359, 0.0914)
(t = beta/se_beta)
t[4] = 5.026
(se_beta[4] = beta[4]/t[4])
(t[6] = sqrt(seq_ssreg[5]/mse))
(pvals = pt(-abs(t),n-p)*2)
(beta[6] = t[6]*se_beta[6])

# b
(f45 = sum(seq_ssreg[4:5])/2/mse)
(p45 = 1-pf(f45,2,n-p))

# c
(f1_123 = sum(seq_ssreg[2:3])/2/(sum(seq_ssreg[4:5]+rss)/(n-4)))
(p1_123 = 1-pf(f1_123,2,n-4))

