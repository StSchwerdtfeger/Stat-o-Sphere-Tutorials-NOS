#############################################################
##---------------------------------------------------------##
##               Inferential Statistics III  ─             ##
##     Evaluating the Conditional Linear Relations of a    ##
##                     Linear Model in R                   ##
##             by Steffen Schwerdtfeger 12.2022            ##
##---------------------------------------------------------##
#############################################################


##################
# 1 Introduction #
##################

### Second synthetic data set from Inf. Stat. II

# Lady Meow drinking tea ^^ 

# Cups of Tea: 
cups = c(0,  1, 2,   3.5, 4.8, 5.2, 6,   6.9, 8.5, 9.1, 9.9, # run 1
         0, .6, 2.6, 3.1, 4.8, 6.9, 7.1, 7.5, 8.5, 9.9, 9.9, #     2
         0, .2, 2.9, 2.9, 4.9, 5.2, 6.9, 7.1, 7.3, 9,   9.8) #     3

# Time passed for each run of measurements (cups), abbreviated:
time = c(0:10,0:10,0:10)

# Summary(lm(y~x))
summary(lm(cups~time))

# Residual quantiles:
summary(residuals(lm(cups~time)))
# reproduce via mean(+/-)sd (upper and lower quantile)...

# lm(dependent~independent)
lm(cups~time) 


#### Replication of the essential parts of the lm() function:

# Go-go-gadgeto linear_least_square!!!!
# Replication of the lm() function:
linear_least_square = function (x,y){ ### Start of function
  # Here we define and operate with our guessed input 
  # function.
  lmRandom = function(x) (x) 
  
  # Linear least square method:
  
  # Next up we will optimize our guessed function,
  # by looking for the minimum of the values of a (α) 
  # in relation to b (β).
  
  # Our PDE goes as follows:
  # Error2(a,b) = sum(y-f(x))^2 = sum(y-a-bx)^2  
  # We have been to the part where we rearranged 
  # the whole equation to resemble a system of 
  # equations, such that we can abbreviate the code. 
  # To do so we define a and b, due to an abstraction 
  # of linear algebra in R, when using the function solve().
  a = 1; b = 1 
  
  # Below you will find our rearranged system of equations 
  # in the form of a matrix (left side) and a vector (right 
  # side of the equation). We will use these objects as input 
  # for our solve(left,right) function. 
  
  # From (DO NOT EXECUTE, just formally):
  #   sum(a*length(x)) + b*sum(x)     = sum(y)
  #       a*sum(x)     + b*sum(x^2)   = sum(x*y)
  
  # To get the above set for solving a system of linear 
  # equations we will create objects of the parts of the 
  # above and then fit them into a matrix and a vector.
  
  left_a_r1c1 = sum(a*length(x)); left_b_r1c2 = b*sum(x) ; 
  left_a_r2c1 = a*sum(x)        ; left_b_r2c2 = b*sum(x^2) 
  
  right_r1c1 = sum(y)
  right_r2c1 = sum(x*y)
  
  # Now we will set up the above as matrix (left) and 
  # vector (right) objects:
  left = matrix(c(left_a_r1c1  ,  left_b_r1c2,
                  left_a_r2c1  ,  left_b_r2c2), 
                ncol=2, nrow=2, byrow = TRUE)
  
  right = c(right_r1c1, right_r2c1)
  
  # Now we can solve our system of equations via:
  Result = solve(left,right)
  
  # Fitted function:
  lmFitted = function(x) (Result[1]+Result[2]*x)
  SumError2fitted =  sum((y-lmFitted(x))^2)
  
  # In order to get a nice looking output, we will use the cat() function. 
  # The cat() function concatenates elements of any
  # kind, such as Text and our result values. Use "\n"
  # for a line break, and "\t" for the tab seperator.
  # We also have to apply a trick, to get the
  # name of the input object into our console output
  # using deparse(substitute(x)):
  
  cat(" Linear least square method in R","\n","\n",
      "Independent variable:", "\t", deparse(substitute(x)),"\n", 
      "Dependent   variable:", "\t", deparse(substitute(y)),"\n","\n",
      "alpha", "\t",Result[1],"\n",
      "beta","\t",Result[2], "\t","SumR2", "\t", SumError2fitted) 
  
  # Let us also plot our results:
  # We will also use deparse(substitute(x)) for the 
  # labels of our plot axes.
  plot(x=x,y=y, ylab = deparse(substitute(y)),       
                xlab = deparse(substitute(x)))
  abline(a=Result[1], b=Result[2], col = "darkblue")
  
} #### End of function

# linear_least_square(independent,dependent)
linear_least_square(time,cups) 



###############################################################
# 3 Descriptive Statistics and Conditionally Linear Relations #
#     ─ Mean, Sum of Squares, Variance and Covariance         #
###############################################################

###################################################################
# 3.1 Arithmetic Mean / Weighted Arithmetic Mean / Expected Value #         
###################################################################

x = time
y = cups

# Arithmetic mean for the sample x and y of our second synth. data set,
# i.e., the sample mean. Recall: the formula for the population mean
# is the same as for the sample mean, just with a different size
# of n, i.e., a different (greater!) length of x than in the sample, 
# since N>n for the population (N).

n = length(x)
mean = sum(x)/n
mean = (1/n)*sum(x)

# Using the built in R function mean(x):
mean = mean(x)

# Note that in the case of x = c(0:10,0:10,0:10)
# the mean is equivalent to the median:
mean(x) == median(x)
# [1] TRUE

#####################################################
# 3.1.1 Probabilistic and Non-Probabilistic weights #
#####################################################

#### Weighted arithmetic mean - (modified) Wikipedia example: 

# Link to example ( 29.10.2022):
# https://en.wikipedia.org/wiki/Weighted_arithmetic_mean

# Samples:
grades_class_1 = c(62, 67, 71, 74, 76, 77, 78, 79, 79, 80, 80, 81, 81, 
                   82, 83, 84, 86, 89, 93, 98)
grades_class_2 = c(81, 82, 83, 84, 85, 86, 87, 87, 88, 88, 89, 89, 89, 
                   90, 90, 90, 90, 91, 91, 91, 92, 92, 93, 93, 94, 95, 
                   96, 97, 98, 99)

# Regular arithemtic mean of each sample:
mean_class_1 = mean(grades_class_1)   
# [1] 80
mean_class_2 = mean(grades_class_2)  
# [1] 90

# Regular arithmetic mean of the grades of both classes:
mean_grades = (mean_class_1 + mean_class_2)/2
# [1] 85

# Weights (here weight == n):
n_class_1 = length(grades_class_1)
n_class_2 = length(grades_class_2)

# Numeric example Wikipedia that weights the mean of the samples:
# (still open question if this is a legit method; comment or contact us,
# if you have knowledge on this topic!). 
w_m_numer = (mean_class_1*n_class_1)+(mean_class_2*n_class_2)
w_m_denom = sum(n_class_1+n_class_2)
weighted_mean = w_m_numer/w_m_denom
# [1] 86

# Following the formula of the (general) weighted arithemtic mean:
# Goal: Evaluate overall mean of grades, but include weight 
#       on the fact that n differs in the two samples. Now we will
#       weight each grade by the n of its respective class: 

# For this we will turn the above weights and grades into a vector 
# of weights and grades. To do so we can use the rep() function that
# repeats a value up to a defined length:
weights = c(rep(n_class_1,length(grades_class_1)), 
            rep(n_class_2,length(grades_class_2)))
grades  = c(grades_class_1,grades_class_2)

# Evaluate weighted mean of both samples(!!):
weighted_mean = sum(weights*grades)/sum(weights)
# [1] 86.92308

# We can also normalize weights (that they sum to 1):
norm_weights = weights/sum(weights)
norm_weighted_mean = sum(norm_weights*grades)/sum(norm_weights)

# Alternative via R basic stat. functions:
R_basic = weighted.mean(grades,weights)

# Check:
all.equal(weighted_mean, norm_weighted_mean, R_basic)
# [1] TRUE


##### Expected value 

# Expected value as weighting a regular distribution of data
# points (here dice values) by a uniform probability distribution:

# Dice values of a fair dice weighted by the prob. of 
# occurence of each dice value:
dice_prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6) # prob. vector
1 == sum(dice_prob)
# [1] TRUE
dice_value = c(1:6)

EX_dice     = sum(dice_prob*dice_value)
EX_dice_alt = sum(dice_prob*dice_value)/sum(dice_prob)
# [1] 3.5
all.equal(EX_dice, mean(dice_value),EX_dice_alt) 
# [1] TRUE

#### Same can be done with x of our 2. synth. data set.
x = c(0:10) # hours; we will use 0:10 only once

# Obtain prob. value by calculating 1/n (a kind of normalization):
probX = 1/length(x)          # 1/n == uniform probability distribution
probXvec = c(rep(1/length(x),length(x))) # alternatively represented as 
                                         # probability vector that 
                                         # sums to 1
1 == sum(probXvec)
# [1] TRUE

# Actual weighted values of x:
weighted_x     = x*(1/length(x)) 
weighted_x_vec = x*probXvec  # alt. via prob. vector
weighted_x == weighted_x_vec 
# [1] TRUE

# weighted_x (uniform weight):
# [1] 0.09090909 0.09090909 0.09090909 0.09090909 0.09090909 0.09090909
# [7] 0.09090909 0.09090909 0.09090909 0.09090909 0.09090909

# Expected value as the sum of weighted values of x:
EX = sum(x*(1/length(x))) # sum of weighted_x is in this special case...
EX = sum(probXvec*x)
# [1] 5
EX == mean(x)             # ... equivalent to the unweighted arith.  
                          # mean of x



######################
# 3.2 Sum of Squares #
######################


# Using the second synthetic data set from Inf. Stat. II again:
x=time
y=cups


# Mean (same formula for population, sample mean and expected value).
# The difference lays in the concept. The expected value can be considered
# as the mean of previous studies (overall mean):
n = length(x)
mean = sum(x) / n
mean = mean(x)


### (Total) Sum of Squares + Population and Sample Variance ###

# SumSqXX = ∑(x_i-E[X])^2, 
# where E[X] = 〈x〉 = μ_x = ∑(x_i)/n = expected value 
SumSqXX = sum((x-(sum(x)/length(x)))^2)
SumSqXX = sum((x-mean(x))^2)

# SumSqYY = ∑(y_i-E[Y])^2
SumSqYY = sum((y-(sum(y)/length(y)))^2)
SumSqYY = sum((y-mean(y))^2)

# SumSqXY = ∑(x_i - E[X])*(y_i - E[y])
# We will dig into this one deeper later:
SumSqXY = sum((x-mean(x))*(y-mean(y)))

# Pure deviation from the mean (without summing!):
dev_of_mean_X = x-mean(x)

# Plot of dev_of_mean:
plot(x = x, y = dev_of_mean_X) 
abline(h=0)

# Pure deviation from the mean (without summing and squaring!):
dev_of_mean_X = x-mean(x)

# Plot using absolute values via function abs():
plot(x = x, y = abs(dev_of_mean_X)) 
# Only 0 deviation when x_i = 5 = mean(x)

# Plot using the square, which smoothes the graph:
plot(x = x, y = dev_of_mean_X^2) 


# OPTIONAL SIDE QUEST: Testing for normality of x = c(0:10,0:10,0:10):
shapiro.test(x)
# OUTPUT
# Shapiro-Wilk normality test
# data:  x
# W = 0.94191, p-value = 0.07723

# IMPORTANT: A p-value above 0.05 indicates that normality is given!!
#            Here the null-hypothesis is that the data is normal. We
#            cannot discard the null hypothesis, so normality is given.


################
# 3.3 Variance #
################

# Variance := σ^2 = SS/n := the variance is the mean of the sum of squares

# Variance of x:
sampVarX = SumSqXX/(length(x)-1)    # := sample variance == var(x)
sampVarX = var(x)                   # default var(x) == sample variance
popVarX  = SumSqXX/(length(x))      # := population variance

# Variance of y:
sampVarY = SumSqYY/(length(y)-1)    # := sample variance == var(y)
sampVarY = var(y)                   # default var(y) == sample variance
popVarY  = SumSqYY/(length(y))      # := population variance


##################
# 3.4 Covariance #
##################
##################################################################
# 3.5 Conditional Probabilistic and Conditional Linear Relations #
##################################################################

# SumSqXY = ∑(x_i - E[X])*(y_i - E[y])
SumSqXY = sum((x-mean(x))*(y-mean(y)))

# Covariance:
covarXYsamp = SumSqXY / (length(x)-1) # sample covariance
# [1] 10.39687
covarXYpop  = SumSqXY / length(x)     # population covariance
# [1] 10.08182
covarXYsamp = cov(x,y)                # samp cov := cov(x,y)
# [1] 10.39687

# Example scatter plot for cov(x,y) > 0:
plot(x,y)
cov(x,y) > 0
# [1]  TRUE

# Simple vertical mirroring of our data as an 
# example scatter plot for cov(x,y) < 0
plot(x,-y)
cov(x,-y) < 0
# [1] TRUE

# Here another (synthetic) example for cov(x,y) = 0:
cross_x = c(5, 5.5, 4.5, 5  ,  5 ,  5.25, 4.75, 5   , 5 )
cross_y = c(5, 5  , 5  , 4.5, 5.5,  5   , 5   , 4.75, 5.25)

# Plot data:
plot(cross_x,cross_y, xlim = c(0,10), ylim = c(0,10))
cov(cross_x, cross_y) == 0
# [1] TRUE

# Model for the last data set (cross):
linear_least_square(cross_x, cross_y)
# Linear least square method in R 

# Independent variable: 	 cross_x 
# Dependent   variable: 	 cross_y 

# alpha 	 5 
# beta 	 0 	 SumR2 	 0.625

# Using lm(y~x) only:
lm(cross_y~cross_x)  
plot(cross_x, cross_y)
abline(lm(cross_y~cross_x))

### Alt. universe where where Lady Meow is drinking no tea at all :'(
# Setting up a respective data set. The rep() function is used
# to obtain a vector c() with 11 times the value 0. We need
# 11 values, as x goes from 0h to 10h, resulting in 11 numbers:
data = cbind(x=c(0:10),y=c(rep(0,11)))
data = as.data.frame(data)
cov(x=c(0:10), y=c(rep(0,11)))

# Plot data:
plot(data)
# Or use:
plot(x=c(0:10), y=c(rep(0,11)))
lm(data$y~data$x)

# Model of that data set:
# Call:
# lm(formula = data$y ~ data$x)
#
# Coefficients:
# (Intercept)       data$x  
#           0            0 


# Estimate of the optimal slope of our model function 
# from the second synthetic data set via:
# beta = cov(y,x)/var(x):
beta_meow = cov(y,x)/var(x)
# [1] 1.008182

# beta = SumSqXY / SumSqXX
beta_meow = SumSqXY/SumSqXX
# [1] 1.008182


# Deriving the intercept from the slope-intercept formula:
# To do so we will start with obtaining two
# points that lay on our model function. We 
# will take x = 1 and the second will be the y at
# x = 0, our intercept itself. For the first point we 
# need the value of alpha already, so we will quickly  
# calculate it via our second approach:
alpha_meow = mean(y)-beta_meow*mean(x) # Details later below.

# Our fitted function:
lmFitted = function(x) (alpha_meow+beta_meow*x)

# Value of y of our fitted model function,
# given that x = 1:
y1 = lmFitted(1) 

# Alternatively we can also use the predict function.
# However, it outputs a list of values where the first element 
# is our alpha and the second relates to x=1. Therefore we use [2]
# to only get the second element in the output vector of 
# predict(lm(y~x)):
y1 = predict(lm(y~x))[2]  

# Check for equality:
all.equal(lmFitted(1),as.numeric(predict(lm(y~x))[2]))

# Calculating beta with the points:
beta_meow = (alpha_meow-lmFitted(1))/(0-1)

# Rearrange formula by multiplying beta_meow by (0-1):
beta_meow*(0-1) == alpha_meow-lmFitted(1)

# We will invert the upper line and do some rearrangements:
alpha_meow-lmFitted(1) == beta_meow*(0-1) 
alpha_meow = lmFitted(1) + beta_meow*(0-1)
#[1] 0.2318182

# We can also simplify the formula by taking the mean of 
# x and y instead of a specific point from our fitted function.
# This comes in handy when we do not know the intercept yet!
# The reason we can use the mean is:
alpha_meow-lmFitted(mean(y)) == -beta_meow*lmFitted(mean(x))
# [1] TRUE # Meaning: -5.315868 == -5.315868 
alpha_meow == -beta_meow*lmFitted(mean(x))+lmFitted(mean(y))
# [1] TRUE # alpha_meow = 0.2318182

# We can simplify the above by just using the respective means
# and do some rearrangements:
alpha_meow-mean(y) == -beta_meow*mean(x)
# alpha = mean(y)-beta*mean(x)
alpha_meow = (sum(y)-(beta_meow*sum(x)))/(length(x))
alpha_meow = mean(y)-beta_meow*(0-mean(x)) # x = 0

# The simplest form is:
alpha_meow = mean(y)-beta_meow*mean(x)

# Checking for equality:
all.equal((alpha_meow-mean(y)),(-beta_meow*mean(x)),
          (beta_meow*(0-mean(x)))) 


################################################
# Optional : Evaluating the numerical gradient #
################################################

# Replication of the Matlab style gradient function in R
# for calcualting the 1D numerical gradient.  
# Alternatively use function with same name via library("pracma"):
gradient = function (x) {
  step = length(x)
  upLim  = step-1 # needed for differences on interior points
  
  # List for result
  result = vector("list",1*length(x))
  dim(result) = c(1,length(x))
  for (i in 1:step){
    if (i == 1){
      result[1] = (x[1+1]-x[1])/1  # Take forward differences on left... 
    }
    if (i == step){                # ....... and right edges.
      result[i] = (x[step]-x[step-1])/1
    }
    for (i in 2:upLim) { # needed for differences on interior points
      result[i] = (x[i+1]-x[i-1])/2
    }
  }
  # Print
  return(result)
} # End of function

gradient(lmFitted(0:10))
#          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]    
# [1,] 1.008182 1.008182 1.008182 1.008182 1.008182 1.008182 1.008182 1.008182
#          [,9]    [,10]    [,11]
# [1,] 1.008182 1.008182 1.008182


###########################
# 3.6  Standard deviation #
###########################

# Standard deviation := sqrt(variance), where sqrt() == square root:
# Sd for x given the sample variance
sdXsamp = sqrt((sum((x-mean)^2))/(length(x)-1))
sdXsamp = sqrt(sampVarX)
sdXsamp = sqrt(var(x))
sdXsamp = sd(x)

# Sd for x given population variance:
sdXpop = sqrt((sum((x-mean)^2))/(length(x)))
sdXpop = sqrt(popVarX)

# Sd of y given sample variance:
sdYsamp = sqrt((sum((y-mean(y))^2))/(length(y)-1))
sdYsamp = sqrt(sampVarY)
sdYsamp = sqrt(var(y))
sdYsamp = sd(y)

# Sd for y given population variance:
sdYpop = sqrt((sum((y-mean)^2))/(length(y)))
sdYpop = sqrt(popVarY)



