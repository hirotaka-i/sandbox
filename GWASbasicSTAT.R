
# Mean of samples supposedly follows a special distribution no matter how wired the backgound distribution is.
## Creating a wired back ground population 
x = runif(2000, 20, 80)
head(x, n=10)
hist(x)
mean(x) # mean of the destribution
x = ifelse(x>50, x+10, x-10) 
head(x, n=10)
hist(x) 
mean(x) # mean of the destribution

# sample 10 from background destribution and take the mean
x10 = sample(x, 10)
x10
x10_mean = mean(x10)
x10_mean

## SIDE: If we increase the sample, say, 100,
## our guess of the man will be more accurate
x100 = sample(x, 100)
x100_mean = mean(x100)
x100_mean
## Law of large numbers: LLA ###


# take the mean of 10 samples 200 more times
x10_means = x10_mean
for (i in 1:200){
  x10 = sample(x, 10)
  x10_mean = mean(x10)
  x10_means = c(x10_means, x10_mean)
}
length(x10_means)
hist(x10_means, breaks=20)

# do it 20K more times.
for (i in 1:20000){
  x10 = sample(x, 10)
  x10_mean = mean(x10)
  x10_means = c(x10_means, x10_mean)
}
length(x10_means)
hist(x10_means, breaks = 50, xlim = c(20, 80), ylim=c(0,0.1),prob=T, col="blue", density=50)
# this distribution is an approx of a normal distrubiton
####
## Noraml distribution has two paramaters mu and sigma
## and described as N(mu, sigma^2)
####
## The means of n samples follow the normal distribution of
##  mu = mean of the original distribution
##  sigma = standard diviation of the original distribution
##      devided by square root of n (standard error)
MU = mean(x)
SE = sd(x)/sqrt(10)
curve(dnorm(x, mean=MU, sd=SE),add = T)
## Central Limit Theorum: CLT
## If n is enough large (n>30), CLT holds.


# average 40 of x, repeat 20K times will give a narrower distribution.
## distribution of x 
x40_means = c()
for (i in 1:20000){
  x40 = sample(x, 40)
  x40_mean = mean(x40)
  x40_means = c(x40_means, x40_mean)
}
length(x40_means)
head(x40_means, n=10)
par(new=T)
hist(x40_means, breaks = 50, xlim = c(20, 80), ylim=c(0,0.1), prob=T, col="red", density=50)
# This distribution approx,
MU = mean(x)
SE = sd(x)/sqrt(40)
curve(dnorm(x, mean=MU, sd=SE),add = T)


####
# SIDE: what is sd? #####
a=1:5
a
mean(a)
sd(a)
a-mean(a)
(a-mean(a))^2
sum((a-mean(a))^2)
sum((a-mean(a))^2)/4 # variance
sqrt(sum((a-mean(a))^2)/4) # standard deviation
# why devide by 4, not 5? 
## Because we use mean(a) instead of true population mean.
####






# Back to the origial problem
sample=c(52, 56, 48, 52, 56, 54, 49, 42, 56, 57)
mean_10 = mean(sample)

# Can we use CLT to judge whether x10_mean is different from 50
## Supppose x10_means follows N(mu, sigma^2/sqrt(10)) where mu=50, 
## (null hypothesis:H0), we can tell how likely we would get
## x10_mean (obs) or more extreme. 
## The problem: how can we find sigma?
## We substitute sigma by sd(sample), but sd(sample) is always,
## a bit smaller than sd(x), so need to be corrected -> t distribution
curve(dnorm(x, mean=0, sd=1), xlim=c(-3,3))
curve(dt(x, df=3),col="red", add=T)
# When t is large enough, it's almost the same as a normal distribution
curve(dt(x, df=30),col="blue", add=T)




## The t-distribution of x10_mean under the null hypothesis
## (x-50)/{sd(sample)/sqrt(10) follows t-dist with df of 9
curve(dt((x-50)/{sd(sample)/sqrt(10)}, df=9),col="red", xlim=c(45, 55)) 
abline(v=mean_10)
T_statistic = (mean_10-50)/{sd(sample)/sqrt(10)}
pt(T_statistic, df=9, lower.tail = F) # one-way
pt(T_statistic, df=9, lower.tail = F)*2 # two-way
t.test(sample-50) # Confirm it is the same as the t.test results

## SIDE: Color the area
curve(dt((x-50)/{sd(sample)/sqrt(10)}, df=9),col="red", xlim=c(45, 55)) 
dots=mean_10 + 0:300/100
vals=dt((dots-50)/{sd(sample)/sqrt(10)}, df=9)
polygon(c(dots,rev(dots)),
        c(rep(0,301),rev(vals)),col="gray")

# SIDE: simulation of T statistics
# Dist.of  t-stats calculated by 4 samples from N(0,1)
Tstats=c()
for (i in 1:20000){
  x4 = rnorm(4)
  x4_mean = sum(x4)/4
  x4_sd = sd(x4)
  x4_se = x4_sd / sqrt(4)
  Tstat =( x4_mean - 0 ) / x4_se
  Tstats=c(Tstats, Tstat)
}
hist(Tstats, xlim=c(-4, 4), breaks=400, prob=T)
curve(dnorm(x), type = "l", add=T) # doesn't fits well.
# t-dist of 3 df fits.
curve(dt(x, df=3), col="red", add=T)













# Number of alpha errors
x = runif(20000, 20, 80)
hist(x)
mean(x)
MU = mean(x)
SAMPLE=sample(x, 10)
SAMPLE
t.test(SAMPLE - MU) # test whether this is significantly different from 0.05
Tstat=t.test(SAMPLE - MU)
Tstat$p.value

# repeat the same test 10K times
P=c()
for (i in 1:10000){
  SAMPLE=sample(x, 10)
  Tstat=t.test(SAMPLE - MU)
  P = c(P, Tstat$p.value)
}
head(P, 10)
hist(P)
min(P)
sum(P<0.05)
# the number of alpha error (false positives)
# This is a bit larger than 500.
# The deviation from expectation happens because 
# CLT is not a normal dist. but an approx.

# QQplot is a good way to detect structural error
## From above example
Psort=sort(P)
Pexpc=(1:10000)/10000
Psort[1:10]
Pexpc[1:10]
plot(-log10(Pexpc), -log10(Psort))
abline(0,1)
# Lambda
Pmedian=median(P)
Pmedian
Mchisq = qchisq(1-Pmedian,1)
Mchisq/qchisq(0.5,1) 


## Increase the sample size to 1000
P=c()
for (i in 1:10000){
  SAMPLE=sample(x, 400)
  Tstat=t.test(SAMPLE - MU)
  P = c(P, Tstat$p.value)
}
sum(P<0.05)
Psort=sort(P)
plot(-log10(Pexpc), -log10(Psort))
abline(0,1)

# If x is normally distributed (run above "SIDE")
P2=c()
for (i in 1:10000){
  SAMPLE=rnorm(10, mean=50, sd=15)
  Tstat=t.test(SAMPLE-50)
  P2 = c(P2, Tstat$p.value)
}
sum(P2<0.05)
P2sort=sort(P2)
plot(-log10(Pexpc), -log10(P2sort))
abline(0,1)


# Multiple testing correcion
## Bonferroni correction 0.05 / number of tests
P=c()
for (i in 1:10000){
  SAMPLE=sample(x, 10)
  Tstat=t.test(SAMPLE - 46)
  P = c(P, Tstat$p.value)
}
Psort=sort(P)
df = data.frame(P=Psort)
df$BONF_thre_to_rawP = 0.05/10000
df$P_BONF = df$P*10000
df$BONF_rjct = ifelse(df$P_BONF < 0.05, 1, 0)


# False discovery rate of 0.05
df$FDR_thre_to_rawP = 0.05/10000*(1:10000)
df$P_FDR = df$P*10000/(1:10000)
df$FDR_rjct = ifelse(df$P_FDR < 0.05, 1, 0) 










# Power
## H0 distribution and alpha
curve(dnorm(x,0, 1), xlim=c(-3, 5))
# Alpha (0ne-way (alpha = 0.025))
dots=1.96 + 0:300/100
vals=dnorm(dots)
polygon(c(dots,rev(dots)),
        c(rep(0,301),rev(vals)),col="blue")
## H1 distribution and beta
curve(dnorm(x,3, 1), xlim=c(-3, 5), add=T)
dots=1.96 - 0:300/50
vals=dnorm(dots-3)
polygon(c(dots,rev(dots)),
        c(rep(0,301),rev(vals)),col="red")

# Power (1-Beta)
dots=1.96 + 0:300/100
vals=dnorm(dots-3)
polygon(c(dots,rev(dots)),
        c(rep(0,301),rev(vals)),col="grey")

## two ways to increase power
# larger effect
curve(dnorm(x, 4, 1), xlim=c(-3, 5), add=T)

# Larger sample size (or better precision)
curve(dnorm(x, 0, 0.5), xlim=c(-3, 5))
curve(dnorm(x, 3, 0.5), xlim=c(-3, 5), add=T)
dots=1.96/2 + 0:300/100
vals=dnorm(dots, 0, 0.5)
polygon(c(dots,rev(dots)),
        c(rep(0,301),rev(vals)),col="red")
##








# Linear regression for original example
## Y1s are scooped usign "A" scoop, and Y2s are scooped by "B" scoop.
Y1=c(56, 49, 55, 56, 48, 52, 57, 42, 53, 58)
Y2=c(45, 50, 53, 41, 51, 50, 45, 48)
t.test(Y1, Y2)

# combine Y1 and Y2
Y = c(Y1, Y2)

# Linear regression with only intercept
lm(Y~1)
summary(lm(Y ~ 1))
# intercept is the mean of Y
mean(Y)
# plot
plot(Y)
abline(h=mean(Y))


# Give the information regarding spoon A(==0) and spoon B(==1)
X1 = c(rep(0,10), rep(1, 8))
# Add information of X1 in the model
summary(lm(Y ~ 1 + X1))
# Intercept is the mean of Y1
mean(Y1)
# beta for X1 is the same as the following;
mean(Y2) - mean(Y1)
# plot
plot(X1,Y)
abline(a=52.6, b=-4.725)

# Further more, three people scooped in turns.
# Person A == 0, Person B == 1, Person C == 2
X2 = rep(c(0, 1, 2), 6) 
summary(lm(Y ~ 1 + X1 + X2))

# A == (0,0), B == (1,0), C == (0, 1)
X2_1 = ifelse(X2==1, 1, 0)
X2_2 = ifelse(X2==2, 1, 0)
data.frame(X2, X2_1, X2_2)
summary(lm(Y ~ 1 + X1 + X2_1 + X2_2))
# The above is equivalent to 
summary(lm(Y ~ 1 + X1 + as.factor(X2)))
# Predict if person A uses spoon B...


# Try add the age of the three people
X3 = rep(c(25, 30, 35), 6)
summary(lm(Y ~ 1 + X1 + as.factor(X2) + X3))


# Give new value of X3 as the number of people in the shop
set.seed(25)
X3 = rpois(18, 4)
summary(lm(Y ~ 1 + X1 + as.factor(X2) + X3))




# Revisit the previous slide for permutation
## Pemutation test
obs = data.frame(Y, X1, X2)
testOBS = lm(Y ~ X1 + as.factro(X2), data = obs)
summary(testOBS)
BETA1 = testOBS$coefficients[2]
BETA1

BETA_perm = c()
for (i in 1:10000){
  obs$Yperm=sample(obs$Y)
  testPERM = lm(Yperm ~ X1 + as.factor(X2), data = obs)
  BETA_perm = c(BETA_perm, testPERM$coefficients[2])
}
hist(BETA_perm, breaks=100)
abline(v=c(BETA1, -BETA1))
(sum(BETA_perm < -abs(BETA1), BETA_perm > abs(BETA1)) + 1) / 10001

## Multiple test adjustment for a permutation test
X1 # consider this is a SNP
X1c = X1
X1c[2] = 1
X1c # This is a very close SNP to X1
summary(lm(X1~X1c)) # Their correlation is 0.8

summary(lm(Y ~ X1 + as.factor(X2)))
summary(lm(Y ~ X1c + as.factor(X2)))

# get p-value
P1 = summary(lm(Y ~ X1  + as.factor(X2)))$coefficient[2,4]
P1
P2 = summary(lm(Y ~ X1c + as.factor(X2)))$coefficient[2,4]
P2

# Permutation
P1_PERM=c()
P2_PERM=c()
for (i in 1:10000){
  Yperm = sample(Y, 18)
  P_m1 = summary(lm(Yperm ~ X1  + as.factor(X2)))$coefficient[2,4]
  P_m2 = summary(lm(Yperm ~ X1c + as.factor(X2)))$coefficient[2,4]
  P1_PERM=c(P1_PERM, min(P_m1, P_m2))
  P2_PERM=c(P2_PERM, max(P_m1, P_m2))
}
(sum(P1_PERM < P1) + 1)/10001 # P1 adjusted
(sum(P2_PERM < P2) + 1)/10001 # P2 adjusted
## BONF:P1=P2 = 0.05/2
## FDR: P1=0.05/2, P2=0.05
## Permutation can adjust the relatedness between predictors.






###################################################
# 2nd half
###################################################
# PCA
dPCA=subset(iris, Species!="setosa")
head(dPCA)
plot(dPCA$Sepal.Length, dPCA$Sepal.Width, col=dPCA$Species)
m1 = lm(dPCA$Sepal.Width~dPCA$Sepal.Length)
abline(m1)
summary(m1)
PCA = prcomp(dPCA[,c("Sepal.Length", "Sepal.Width")])
head(PCA$x)
plot(-PCA$x[,1], -PCA$x[,2], col=dPCA$Species)
abline(h=0) # PC1
abline(v=0) # PC2
summary(PCA)

## Use information of 4 variables
head(dPCA)
PCA2 = prcomp(dPCA[,1:4])
head(PCA2$x)
summary(PCA2)
plot(-PCA2$x[,1], -PCA2$x[,2], col=dPCA$Species)
plot(-PCA2$x[,1], -PCA2$x[,3], col=dPCA$Species)





# Scree plot to decide number of components to use
EV = PCA2$sdev^2 # eigen value. plink output a file for eigen values
EV
plot(EV, typ="o")
summary(PCA2)$importance[2,]
plot(summary(PCA2)$importance[2,], typ="o")


# model using 4 or 2
m_PC4 = glm(I(dPCA$Species=="virginica") ~ PCA2$x[,1] + PCA2$x[,2] + PCA2$x[,3] + PCA2$x[,4], family = binomial())
m_PC2 = glm(I(dPCA$Species=="virginica") ~ PCA2$x[,1] + PCA2$x[,2], family = binomial())
summary(m_PC4)
summary(m_PC2)
anova(m_PC2, m_PC4, test = "LRT")





# Meta analysis
## We are intersted in the association of petal Width on sepal length 
## after adjusting for the petal length and the species.
IRIS = iris # Load data this time use "setosa" species too.
# One model analysis
mAll  = lm(Sepal.Length ~ Petal.Width + Petal.Length + as.factor(Species), data = IRIS)
# Stratified analysis
mSeto = lm(Sepal.Length ~ Petal.Width + Petal.Length, data = subset(IRIS, Species=="setosa"))
mVers = lm(Sepal.Length ~ Petal.Width + Petal.Length, data = subset(IRIS, Species=="versicolor"))
mVirg = lm(Sepal.Length ~ Petal.Width + Petal.Length, data = subset(IRIS, Species=="virginica"))
# Results
summary(mAll)
summary(mSeto)
summary(mVers)
summary(mVirg)
# Take BETAs and SEs for Petal.Width
seto=summary(mSeto)$coefficient[2,1:2]
vers=summary(mVers)$coefficient[2,1:2]
virg=summary(mVirg)$coefficient[2,1:2]
RES = data.frame(rbind(seto, vers, virg))
RES

## Forest-plot like plot
RES$lcl=RES$Estimate - 1.96 * RES$Std..Error
RES$ucl=RES$Estimate + 1.96 * RES$Std..Error
plot(RES$Estimate, ylim = c(min(RES$lcl-0.5), max(RES$ucl+0.5)))
points(1:3, RES$lcl, pch=24)
points(1:3, RES$ucl, pch=25)
abline(h=0)



# Meta-analysis these results
### The sum of the products of weight x Beta devided by sum of the weights is
### a valid estimator of the overall effect.
RES$W = c(1/3, 1/3, 1/3) # Weight can be anything
RES$WxB= RES$W * RES$Estimate 
sum(RES$WxB)/sum(RES$W)

## The above is valid but not efficient because it doesn't use the information
## of how precise the betas are (SE)
## Inverse variance method gives the weight of inverse of variance
RES$W = 1 / (RES$Std..Error^2)
RES$WxB= RES$W * RES$Estimate 
meta_beta = sum(RES$WxB)/sum(RES$W) # This is the beta of meta-analysis
meta_beta
meta_se = sqrt(1/sum(RES$W))
meta_se
Z = meta_beta/meta_se
p_meta = 2 * pnorm(-Z)
p_meta






# Another example





result = data.frame(
  strata=c("A", "B"),
  beta = c(-0.041, -0.136),
  se = c(0.044, 0.06),
  P = c(0.35, 0.024)
)
result

result$W = 1/(result$se^2)
result$WxB = result$W * result$beta
meta_beta = sum(result$WxB) / sum(result$W)
meta_beta
meta_se = sqrt(1 /sum(result$W))
meta_se
Z = meta_beta/meta_se
Z
p_meta = 2 * pnorm(-abs(Z))
p_meta





# Causal inference
x = rep(c(0,1), 100) # gene 0, 1, 0, 1
bp = rnorm(200, mean=130, sd = 20) + x * 20 # Blood pressure affected by x
or = (bp-100)^2/5000 # OR of outcome is associated with blood pressure
y = rbinom(200, 1, or/(1+or)) # Get 0, 1 outcome based on the OR.
m1 = glm(y ~ x, family = binomial()) # Do analysis
summary(m1) # the valid analysis

# conditioning on a intermediate variable
m2 = glm(y ~ x + I(bp-100), family = binomial())
summary(m2)

# selection bias
x = rep(c(0,1),1000) 
y = sample(c(0, 0, 0, 1),2000, replace = T) # 25% had outcome.
m1 = glm(y ~ x, family = binomial()) # valid analysis
summary(m1)
s = rbinom(2000, 1, p=(0.1 + 0.4*x + 0.3 * y)) # s is affected both by x and y
m2 = glm(y ~ x + s, family = binomial()) # analysis inviting a selection bias
summary(m2)
## people within s==1
m_s1 = glm(y[s==1] ~ x[s==1], family = binomial())
summary(m_s1)
## people with s==0 
m_s0 = glm(y[s==0] ~ x[s==0], family = binomial())
summary(m_s0)
### s is often selection criteria, thus, "selection bias".