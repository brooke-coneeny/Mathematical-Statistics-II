#Notes from office hours with professor reviewing key information for presentation 

x1=c(1,1,2,2,3,3,4,4)

set.seed(123)
x2=rnorm(8)

y=2+3*x1+x2+rnorm(8,0,.5)


out = lm(y~x1+x2)
summary(out)
Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   1.7669     0.4100   4.310  0.00764
x1            3.1507     0.1491  21.135 4.39e-06
x2            0.9723     0.1765   5.508  0.00270
               
(Intercept) ** 
x1          ***
x2          ** 
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4714 on 5 degrees of freedom

## construct nxp (8x3) X matrix

X=cbind(1,x1,x2)

XXinv = solve(t(X)%*%X)

bhat = XXinv%*%t(X)%*%y

yhat = X%*%bhat
ehat = y-yhat

# compute estimate of sigma
rmse = sqrt( sum(ehat^2)/(8-3))

# compute standard errors

rmse*sqrt(diag(XXinv))
