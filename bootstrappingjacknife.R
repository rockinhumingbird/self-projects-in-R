#Jacknife and Bootstrap Demonstration

library(bootstrap)
# Create Normally-distributed values with a mean 10 10 and a standard deviaton of 2
x<-rnorm(10,10,2)
x

#Display the distribution of X

hist(x)
plot(density(x))
# Build a function for a statistic of interest
theta <- function(x){mean(x)}

# Do the Jacknife; do N samples of N-1 each.  
# This gave us the re-sampled means, and the SE of values                            
results <- jackknife(x,theta)   
results
$jack.se

$jack.bias
[1] 0

$jack.values
[1] 10.268704  9.988594 10.122526  9.671764 10.127092 10.106572  9.771022 10.309803 10.056106 10.171816

$call
jackknife(x = x, theta = theta)

# Here is how we get the jackknife values
mean(x[-1])
mean(x[-19])

# Obtain the normal distribution values of X

t.test(x)


quantile(x,c(.025,.975))

quantile(results$jack.values,c(.025,.975))

library(psych)
describe(x)

# Notice how the Jackknife SE is close to the normal (asymptotic) 

mean(x[-1])

mean(x[-19])

# Here's the sd of the jackknife values

sd(results$jack.values)

sd(results$jack.values)*sqrt(9)

describe(results$jack.values)

sd(results$jack.values)*sqrt(10)

sd(results$jack.values)*sqrt(9)

# Bootstrap

results2 <- bootstrap(x,100,theta)     
results2
$thetastar
$func.thetastar
$jack.boot.val

$jack.boot.se


$call
bootstrap(x = x, nboot = 100, theta = theta)


sd(results2$thetastar)

quantile(results2$thetastar,c(.025,.975) )


