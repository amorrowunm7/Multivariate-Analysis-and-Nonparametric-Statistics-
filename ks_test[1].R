## non parametric tests ks test - lets you compare if the two test are the same. There are 2 ways to run it. 
# one sample- you are comparing your data distribution that has an empirical cdf to some specified distribution, like .01, specified outside the data . The test statistic then is the maximum distance between the empirical CDF and the true CDF . This will be pnorm or pbeta or something the reference distribution is a distribution that you know.  your going to use pnorm to plot that and pcdf to plot that and then look at the maxium differnece. 

# the two sample does not have a reference distribution. You have two data sets, the samples do not have to be the same size . we dont know if the data is from the same distribution or not, that is the hypothesis that we are testing.  The test statistic is the same, except now we are subtracting 2 empirical CDFs. 1 based on the first sample, one based on the second. ks.test~  having a big n is good because this test gets better and better as the sample size increases. 
# we are going to find the x , where these are furthest apart. 

## 1) (45 points) The attached spreadsheet contains 5 samples of size n = 50. The first sample x1 represents 50 realizations of random variable X1; x2 contains 50 realizations of random variable X2; etc. Use the one-sample version of the KS test to test the following hypotheses:

# H0 :X1 ∼Unif(min=0,max=1) 
# H0 :X2 ∼N(μ=5,σ=3)
# H0 :X3 ∼Exp(λ=1)
# H0 :X4 ∼Beta(α=1,β=1)
# H0 :X5 ∼N(μ=0,σ=1)

# Choose a significance level α and use it for all five tests. Build a data.frame in R to summarize your results. Your data.frame should have one row for each test. The number of columns is up to you, but don’t leave out anything important.


# α = 0.05

ks.dat1 <- read.csv("ks.dat1.csv",header = T)
head(ks.dat1)

x1 <- (ks.test(ks.dat1$x1, 'punif', min=0, max=1))$p.value
x2 <- (ks.test(ks.dat1$x2,'pnorm',mean = 5, sd=3))$p.value
x3 <- (ks.test(ks.dat1$x3,'pexp', rate=1))$p.value
x4 <- (ks.test(ks.dat1$x4,'pbeta',1,1))$p.value
x5 <- (ks.test(ks.dat1$x5,'pnorm',mean=0, sd=1))$p.value

results <-data.frame(x1,x2,x3,x4,x5)

t(results)

# x1 5.432900e-02
# x2 1.706719e-01
# x3 5.906025e-05
# x4 7.051899e-07
# x5 4.494544e-01

# The same spreadsheet contains 5 samples of size n = 30. Select your value for α and use the two-sample version of KS to test whether each pair of samples come from the same distribution. That means you’ll test H0 : Xi and Xj have the same distribution for every distinct pair i and j. Build a data.frame to summarize your results as in part 1 above.

ks.dat2 <- read.csv("ks.dat2.csv",header = T)

head(ks.dat2)

pairs <- expand.grid(colnames(ks.dat2), colnames(ks.dat2))

pairs <- pairs[pairs$Var1 != pairs$Var2,]

compute.pvalue <- function(x, y) {
  ks.test(ks.dat2[[x]], ks.dat2[[y]])$p.value
}

pairs$pvalues <-mapply(
  compute.pvalue,
  pairs$Var1,
  pairs$Var2)


pairs$pvalues 

data.frame(pairs$pvalues)





#4) (55 points) Build a simulation study to evaluate the type 1 error performance of the KS test in R. Details are up to you, but it could go roughly like this:

# Select a value for α0 = the nominal type 1 error rate, =2Use KS to test whether your sample came from the reference distribution and record the p-value. Repeat N = a large number of times to yield N realized p-values. Count the type 1 errors (rejections are type 1 errors) and divide by N to 1 get αˆn = an empirical estimate of the true α for a sample of size n.

#Answer: The y parameter of the ks.test command can be a "character string naming a cumulative distribution function or an actual cumulative distribution function such as pnorm.". Hence, if data are generated using a random number generator for a beta distribution with parameters (a, b), in your simulation study the code for the Kolmogorov-Smirnov test should be ks.test(x,'pbeta', a, b).

# Here is the code for evaluating the type 1 error performance of the KS test:

set.seed(4321)
n <- 50
reps <- 10000
par1 <- 1
par2 <- 4
pv <- matrix(0,reps)
for (k in 1:reps) {
  x <- rbeta(20,par1,par2)
  ksout <- ks.test(x,'pbeta',par1,par2)
  pv[k] <- ksout$p.value
}
sum(pv<0.05)/reps

# [1] 0.0507

# Repeat all of that for a range of n values and plot αˆn versus n. Add a horizontal line using abline(h=α0) to compare αˆn to α0. Looking at the plot might suggest running more simulations at different values of n


set.seed(4321)
n <- 5
reps <- 100
par1 <- 1
par2 <- 4
pv <- matrix(0,reps)
for (k in 1:reps) {
  x <- rbeta(20,par1,par2)
  ks <- ks.test(x,'pbeta',par1,par2)
  pv[k] <- ks$p.value
}
sum(pv<0.05)/reps

# [1] 0.05

count(pv)

xvec <- seq(0,6, by= 1)
yvec <- seq(1,2000, by= 500)





