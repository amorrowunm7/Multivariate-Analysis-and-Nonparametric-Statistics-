## Just submit your R code.   It's good to run this several times, experiment with different priors.  

## You want to get a feel for what happens when prior is basically correct, definitely wrong, and non-informative (very flat).  

## Plot comparisons will be more meaningful if all four plots have the same y-axis scaling.  Choose a good value for ymax the include in ylim = c(0, ymax) in your plot() commands. 

## You might want to annotate plots to include prior mean and posterior mean.  
## Friday:  Rockets 117 Pelicans 107,  JH free throws:  n=11,  x = 9, n-x = 2


#################

xvec <- seq(0, 1, .001)
priorflat <- rep(1,length(xvec))

J<- dbinom(9, 12, xvec)

## Non-informative (very flat) prior.
plot(xvec, priorflat, type="l", main="Prior",col='blue',lwd=4)
plot(xvec , J, type="l", main="Posterior",col='red',lwd=4)


prior2 <- dbeta(xvec, 96, 40)
plot(xvec, prior2, type="l", main="Prior2",col='blue',lwd=4)

prior3 <- dbeta(xvec, 96,20)
plot(xvec,prior3, type="l", main="Prior3",col='Red',lwd=4)

prior4 <- dbeta(xvec, 96, 80)
J2 <- dbinom(9, 11, xvec)

plot(xvec,prior4, type="l", main="Prior3",col='Black',lwd=4)

plot(xvec,J2, type="l", main="Actual",col='Black',lwd=4)


posterior <- prior3*J2/sum(prior3*J2)
plot(xvec, posterior, type="l", main="Posterior",col='Green',lwd=4)


(EX = sum(prob.range*posterior))
(SDX = sqrt(sum((prob.range-EX)^2*posterior)))


