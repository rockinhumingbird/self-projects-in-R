#Jacknife and Bootstrap Demonstration

> library(bootstrap)
# Create Normally-distributed values with a mean 10 10 and a standard deviaton of 2
> x<-rnorm(10,10,2)
> x
[1]  8.175660 10.696655  9.491267 13.548126  9.450175  9.634850 12.654800  7.805769 10.089044  9.047655

#Display the distribution of X

> hist(x)
> plot(density(x))
# Build a function for a statistic of interest
>  theta <- function(x){mean(x)}

# Do the Jacknife; do N samples of N-1 each.  
# This gave us the re-sampled means, and the SE of values                            
>    results <- jackknife(x,theta)   
> results
$jack.se
[1] 0.5758029

$jack.bias
[1] 0

$jack.values
[1] 10.268704  9.988594 10.122526  9.671764 10.127092 10.106572  9.771022 10.309803 10.056106 10.171816

$call
jackknife(x = x, theta = theta)

# Here is how we get the jackknife values
> mean(x[-1])
[1] 10.2687
> mean(x[-19])
[1] 10.0594
# etc.




# Obtain the normal distribution values of X

> t.test(x)

One Sample t-test

data:  x
t = 17.47, df = 9, p-value = 2.982e-08
alternative hypothesis: true mean is not equal to 0
95 percent confidence interval:
  8.756843 11.361957
sample estimates:
  mean of x 
10.0594 

> quantile(x,c(.025,.975))
2.5%     97.5% 
  7.888995 13.347127 
> quantile(results$jack.values,c(.025,.975))
2.5%     97.5% 
  9.694097 10.300556 

> library(psych)
> describe(x)
vars  n  mean   sd median trimmed  mad  min   max range skew kurtosis   se
X1    1 10 10.06 1.82   9.56    9.91 1.23 7.81 13.55  5.74 0.67    -0.88 0.58

# Notice how the Jackknife SE is close to the normal (asymptotic) 

> mean(x[-1])
[1] 10.2687
> mean(x[-19])
[1] 10.0594

# Here's the sd of the jackknife values

> sd(results$jack.values)
[1] 0.2023165

> sd(results$jack.values)*sqrt(9)
[1] 0.6069495

> describe(results$jack.values)
vars  n  mean  sd median trimmed  mad  min   max range  skew kurtosis   se
X1    1 10 10.06 0.2  10.11   10.08 0.14 9.67 10.31  0.64 -0.67    -0.88 0.06
> sd(results$jack.values)*sqrt(10)
[1] 0.639781
> sd(results$jack.values)*sqrt(9)
[1] 0.6069495

# Bootstrap

>  results2 <- bootstrap(x,100,theta)     
> results2
$thetastar
[1]  9.592172 10.102238 10.101180  9.916909 10.818061 10.156167 10.038662 10.935922 10.371827  9.750850 11.209827 10.441229 10.892881  9.872003  9.419927
[16]  9.886374 10.267283 10.366164  9.863586  9.805627  9.665806 10.100343 10.355789 10.266227  9.419624  9.181718 10.602574  9.707271  9.641866  9.908115
[31] 10.018603 10.467677 10.093276  9.390283  9.921376  9.703621 10.098146  9.456235  9.741741 10.029544  9.846937 10.784322 10.490298  9.615595 10.016114
[46] 10.438739  9.933531 11.127952 10.821479 10.010018  9.120728 10.589723  9.309085 10.225526  9.044163  9.326339 10.356919 10.491826 10.045042 10.318119
[61]  9.795524 10.570208 10.235333  9.413162 10.257256 10.610232  9.914859 10.796112 10.533575 10.344849  9.300739 10.920300 10.140292  9.852180 11.008708
[76] 10.244809 10.221109 11.163970 10.231141 10.347505  9.173947 10.600507  9.003911  9.616512 10.434328  9.951611 10.121769  9.880752 10.032896  9.531954
[91]  9.794541  9.429248 10.654059  9.995513 10.242308 10.469195  9.975402 10.072318  9.273317 10.245948

$func.thetastar
NULL

$jack.boot.val
NULL

$jack.boot.se
NULL

$call
bootstrap(x = x, nboot = 100, theta = theta)


> sd(results2$thetastar)
[1] 0.4988623

quantile(results2$thetastar,c(.025,.975) )
2.5%     97.5% 
  9.146007 11.071312 


