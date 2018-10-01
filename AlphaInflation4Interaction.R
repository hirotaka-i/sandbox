library(parallel)
n = 100
b0 = 30
b1 = 1
b2 = 2
slope = -1
slope2 = -0.8
test.func = function(i){
  if (i %% 100 == 0) {print(i)}
  x1 = rnorm(n, 0.7, 9)
  x2 = rnorm(n, 0.2, 14)
  time = rnorm(n, 30, 10)
  e = rbinom(100, n, 0.3)
  y = b0 + b1 * x1 + b2 * x2 + slope * time + slope2 * time^2 + rnorm(n, 0, 12)
  m1= summary(lm(y ~ x1 + x2 + time * e))
  pv_intraction = m1$coefficients[5,4]
  return (pv_intraction)  
}

N_test = 1000
test = mclapply(1:N_test, test.func) # can use mc.cores > 1 if not windows.
pv_sort = sort(do.call(rbind, test))
pv_expc = (1:N_test)/N_test
plot(-log10(pv_expc), -log10(pv_sort))
abline(0, 1)
lambda = qchisq(1-median(pv_sort),1) / qchisq(1-median(0.5),1)
title(sprintf("lambda = %.2f", lambda))