
#### Demonstrate Stein's paradox using simulation

## k=3 case

### set 3 theta values and pick a (known) value for V

theta = c(0,1,2)
V=1

k=3
## approximate expectation of the sum of squares by averaging the observed sum of squares
## for a large number of simulated Y1,Y2,Y3 values using the given thetai's as mean values.
nsim=10000
# create storate sums of squares using Yi's and using the Stein estimate
ss0 = ss1 = rep(0,nsim)


## conduct nsim simulations
for(iter in 1:nsim){
# generate y|theta,V ~ N(theta,V)
 y =theta +  sqrt(V)*rnorm(k)
 # y is a k-vector, with E(yi)=thetai, i=1,..,k
# save sum of squares for yi's
ss0[iter]=sum((y-theta)^2)
#
# compute James-Stein estimates, k=3 so shrink towards mu=0 (e.g. - mu can be any fixed value)
Bhat = (k-2)*V/sum(y^2)
thetahat = Bhat*0 +  (1-Bhat)*y 
#
# store sum of squares for thetahats
ss1[iter]=sum((thetahat-theta)^2)
#
# print out every 1000th iteration
if(iter/1000==round(iter/1000)) cat(iter,", ")
}


### Compare average sum of squares values
mean(ss0); mean(ss1)

# compute ratio
mean(ss0)/mean(ss1)

## ss1 should be smaller, so the ratio should always be  > 1


## I got a ration of 1.09 with theta = c(0,1,2), meaning the expected 
## sum of squares using the yi's is about 9% larger than using the 
## Stein estimate and shrinking towards mu=0.



## k= 4 case
# With k>3 we can spare a degree of freedom (so k-3 instead of k-2) to estimate 
# mu by the average of the yi's (a random variable) rather than specifying mu


### set 4 theta values and pick a (known) value for V

theta = c(-1,0,1,2)
V=1

k=4
## approximate expectation of the sum of squares by averaging the observed sum of squares
## for a large number of simulated Y1,Y2,Y3 values using the given thetai's as mean values.
nsim=10000
# create storate for sums of squares using Yi's and using the Stein estimate
ss0 = ss1 = rep(0,nsim)


## conduct nsim simulations
for(iter in 1:nsim){
# generate y|theta,V ~ N(theta,V)
 y =theta +  sqrt(V)*rnorm(k)
 # y is a k-vector, with E(yi)=thetai, i=1,..,k
# save sum of squares for yi's
ss0[iter]=sum((y-theta)^2)
#
# compute James-Stein estimates: 
ybar = mean(y)
Bhat = (k-3)*V/sum((y-ybar)^2)
thetahat = Bhat*ybar +  (1-Bhat)*y 
#
# store sum of squares for thetahats
ss1[iter]=sum((thetahat-theta)^2)
#
# print out every 1000th iteration
if(iter/1000==round(iter/1000)) cat(iter,", ")
}


# compute ratio
mean(ss0)/mean(ss1)

# I got a ratio of "expected" sums of squares of 1.07 fot theta = c(-1,0,1,2)

## improvement isn't much in small k problems and there is
## less improvement the more variable the yi's are relative to V
## and the further the thetai's are from the assumed value mu
## but it should always show *some* improvement, no matter the thetai's
## (you may need a larger simulation size to detect the small improvement
## for some sets of thetai's)


## example with yi = average height for k=4 positions 
k=4
theta = c(71.3,70.7,72.8,70.1)

## assume n=5 players in each group; set V = MSE/n
## larger V means more shrinkage
V = 6.0/5


nsim=10000
ss0 = ss1 = rep(0,nsim)


## conduct nsim simulations
for(iter in 1:nsim){
# generate y|theta,V ~ N(theta,V)
 y =theta +  sqrt(V)*rnorm(k)
 # y is a k-vector, with E(yi)=thetai, i=1,..,k
# save sum of squares for yi's
ss0[iter]=sum((y-theta)^2)
#
# compute James-Stein estimates: 
ybar = mean(y)
Bhat = (k-3)*V/sum((y-ybar)^2)
thetahat = Bhat*ybar +  (1-Bhat)*y 
#
# store sum of squares for thetahats
ss1[iter]=sum((thetahat-theta)^2)
#
# print out every 1000th iteration
if(iter/1000==round(iter/1000)) cat(iter,", ")
}


# compute ratio
mean(ss0)/mean(ss1)

## I got about a 10% improvement