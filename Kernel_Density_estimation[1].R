# Kernel density estimation

# Run these R commands to get started. Run them all together as a group.
set.seed(12345)
x0 <- rbeta( 100, 2, 3.5)
x1 <- rbeta(  20, 2, 3.5)
x2 <- rbeta( 100, 2, 3.5)
x3 <- rbeta( 500, 2, 3.5)
x4 <- rbeta(2500, 2, 3.5)
# 1. This part uses x0. Use density() to produce a kde for x0. According to R, what is the optimal bandwidth?
set.seed(12345)
x0 <- rbeta( 100, 2, 3.5)
x1 <- rbeta(  20, 2, 3.5)
x2 <- rbeta( 100, 2, 3.5)
x3 <- rbeta( 500, 2, 3.5)
x4 <- rbeta(2500, 2, 3.5)
x0.kde <- density(x0)
x0.kde$bw
# [1] 0.07626762   ### is the optimal bandwidth

# 2. Use the adjust argument to the density() function to produce three kde’s for x0: one undersmoothed, one optimally smoothed, and one oversmoothed. Produce a plot for each kde which includes the pdf of the true beta density.

x0.kde.under <- density(x0, adj=.25) 
x0.kde.opt <- x0.kde
x0.kde.over <- density(x0, adj=2) 
par(mfrow=c(2,2))

plot(x0.kde.under, col=4, main="I feel nervous, distracted by the littlest things", xlim = c(-.1, 1.1), ylim=c(0,2.5), cex.main=.8)
lines(x0.kde$x, dbeta(x0.kde$x, 2, 3.5), col=5, lwd=2) ### adding true density lwd=2) 

rug(x0); grid()

plot(x0.kde.opt, col=4, main="I feel GOOD!", xlim = c(-.1, 1.1), ylim=c(0,2.5), cex.main=.8)
lines(x0.kde$x, dbeta(x0.kde$x, 2, 3.5), col=5, lwd=2)
rug(x0); grid()

plot(x0.kde.over, col=4, main="I feel so S L E E P Y all the time", xlim = c(-.1, 1.1), ylim=c(0,2.5), cex.main=.8) 
lines(x0.kde$x, dbeta(x0.kde$x, 2, 3.5), col=5, lwd=2)

rug(x0); grid()
par(mfrow=c(1,1))


# 3. Produce the same three plots again, but this time use the bw argument instead of adjust to control the smoothing. Plots should look exactly the same as in previous question.
### specify bandwidth directly as bw =  (adjust.parameter * optimal bw)
### adjust param is whatever you chose above
### optimal bandwidth = 0.07626762 is stored as x0.kde$bw

x0.kde.under <- density(x0, bw = .25 * x0.kde$bw)
x0.kde.opt <- x0.kde
x0.kde.over <- density(x0, bw = 2 * x0.kde$bw)
### plot commands can be exactly as above

# 4. Use x1, x2, x3, x4 for this part. Make a kde for each sample. Produce a scatter plot of optimal bandwidth versus sample size.
### build vector of sample sizes
nvec <- c(20, 100, 500, 2500)
### construct 4 kde’s and grab optimal bw for each one, combine these in a vector
bw1 <-  density(x1)$bw
bw2 <-  density(x2)$bw
bw3 <-  density(x3)$bw
bw4 <-  density(x4)$bw
bwvec <- c(bw1,bw2, bw3, bw4)

plot(nvec, bwvec, main = "Optimal bandwidth versus sample size")
grid()
