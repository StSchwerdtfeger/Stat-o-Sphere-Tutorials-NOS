#############################################################
##---------------------------------------------------------##
##                Inferential Statistics IV:               ##
##    Probabilistically Evaluating a Linear Model in R ─   ##
##            (Standard) Normal Distributions and          ## 
##            + Z-Test / Gauß-Test and the T-Test          ## 
##              by Steffen Schwerdtfeger 01.2023           ##
##---------------------------------------------------------##
#############################################################


####################################################################
#    3 Uniform PMFs, PDFs and CDFs ─ Uniform Probability Mass      #
# Functions (Discrete), Probability Density Functions (Continuous) #
#      and the Cumulative Distribution Function (Continuous)       #
####################################################################

####################################################
# 3.1 Uniform Probability Mass Function (Discrete) #
####################################################


# Probability mass function for p(x_i) = 1/6:
dice = c(1:6)
px = 1/length(dice)  # 1/n == for uniform probability distr.

# Written as vector of length(dice_value_x):
px = c(rep(1/length(dice),length(dice))) 

# Plot discrete probabilities:
plot(x=dice,y=px, ylim= c(0,1), xlim = c(0,7))


# Probability mass function for fair dice with n_sides:
dice_px = function(x,n_sides){ # x = sequence / range of possible values
  dice_val = c(1:n_sides)
  px = x # initialize vector for loop with 1:length(x)
  
  for(i in 1:length(x)){ # := for every object of the vector x
    if(x[i] < min(dice_val)){
      px[i] = 0
    }
    else if(x[i] >= min(dice_val) & x[i] <= max(dice_val)){
      px[i] = 1/n_sides
    }
    else if(x[i] > max(dice_val)){
      px[i] = 0
    }
  } # End for i
  
  # Plot probability mass function:
  plot(x=x,y=px, ylim = c(0:1))
  for(i in 1:n_sides){
    segments(x0=dice_val[i],y0=0,x1=dice_val[i],y1=1/n_sides, col = "blue")
  }
  
  # Return px in console:
  return(px)
} # End of function

# Let us test our function:
seq = c(0:7)
px = dice_px(x=seq,n_sides=6)
# 0.1666667 == 1/6
# [1] 0.0000000 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667
# [8] 0.0000000

# Plot discrete probabilities only:
plot(x=seq,y=px, ylim = c(0:1))

# Probability of rolling a 3, 4 or 5, i.e., a range so to speak:
px_3to5 = px[3]+px[4]+px[5]
px_3to5 = sum(px[3:5])
# [1] 0.5


##########################################################
# 3.2 Uniform Probability Density Functions (continuous) #
##########################################################

# Plot Uniform PDF using basic R functions:
x = seq(0,7, length = 10000) # try length 50 and plot again!

# Uniform density for each x:
y = dunif(x, min = 1, max = 6)

# Plot uniform PDF:
plot(x,y, type = "l", ylim = c(0,1))

# Uniform PDF
unif_PDF = function(a,b,x){
  fx = x # initialize output vector of length(x), so we just use x
  for(i in 1:length(x)){ # := for every object of the vector x
    if(x[i] < a){
      fx[i] = 0
    }
    else if(x[i] >= a & x[i] <= b){
      fx[i] = 1/(b-a)
    }
    else if(x[i] > b){
      fx[i] = 0
    }
  } # End for i
  return(fx) 
} # End of function

# Test:
test = unif_PDF(a = 1,b = 6,x = x)
plot(y = test, x = x, type = "l", ylim = c(0,1)) 

# Integration of area under the curve via multiplication:
upper_bo = 5 # set upper bound
low_bo   = 3 # set lower bound
a_min    = 1 # set minimum
b_max    = 6 # set maximum, else f(x) = 0 anyways...
unif_area = (upper_bo-low_bo)*(1/(b_max-a_min))  
# [1] 0.4

# Integration of a uniform prob. density function via ratio
# area of interest divided by total area:
# P(x_1<=X<=x_2):
P_area = (5-3)/(6-1)
# [1] 0.4


# P(x_1=a<=X<=x_2=b) := area of interest == total area:
P_area = (b_max-a_min)/(b_max-a_min)
P_area = (6-1)/(6-1)
# [1] 1

# P(c<=X<=c) := probability of only one point of, e.g., x =5:
P_area = (5-5)/(6-1)
# Results in 0/5 = 0
# [1] 0

# Mean for uniform distribution:
a = 1
b = 6
# For uniform distribution you may come across this formula:
mean = (a+b)/2
# .. but we can also use our regular old mean:
# mean = sum(dice)/length(dice):
mean = sum(dice)/length(dice)
mean = mean(dice)

# The standard deviation for uniform distributions:
# sd() does not deliver equivalent results:
sd = sqrt(((b-a)^2)/12)

# Add mean and sd to plot:
plot(y = test, x = x, type = "l", ylim = c(0,1)) 
abline(v = mean)
abline(v=mean+sd)
abline(v=mean-sd)


#############################################################
# 3.3 The Cumulative Distribution Function of a Uniform PDF #
#############################################################

unif_CDF = function(a,b,x){
  Fx = x # initialize output vector of length(x), so we just use x again
  for(i in 1:length(x)){ # := for every object of the vector x
    if(x[i] < a){
      Fx[i] = 0
    }
    else if(x[i] >= a & x[i] <= b){
      Fx[i] = (x[i]-a)/(b-a) 
    }
    else if(x[i] > b){
      Fx[i] = 1
    }
  } # End for i
  return(Fx) 
} # End of function

# Initialize a sequence of possible x:
x = seq(0,7,by = 1) 
# Fx:
Fx = unif_CDF(a = 1, b = 6, x = x)

# Plot CDF:
plot(x = x, y = Fx)



####################################################
# 4 The (Standard) Normal Probability Distribution # 
#           ─  Gaussian PDFs and CDFs              #
####################################################

#################################
# 4.1 The Central Limit Theorem # 
#################################

# Initialize data via runif, which will
# give us a random but uniformly distributed
# set of data. Each time you run this line, the result will
# be another set of data points.
# Uses set.seed(0) or other values to make results reproducible
# (we will not get further into the the topic here):
data = runif(n = 1000, min = 1, max = 10)

# We can also plot our data as histogram
hist(data, col = "lightblue")

# Use a for loop to take 5000 random samples of size n = 5 each:
n_samp = 5000
sample_n_5 = c() # create empty vector

# The below function will also take the mean of the sample and 
# will store the output in our vector sample_n_5:
for (i in 1:n_samp){
  # Store mean of each sample with length 5:
  sample_n_5[i] = mean(sample(data, 5, replace=TRUE))
} # End for i

# Look at the histogram of the sample_n_5:
hist(sample_n_5, col = "lightblue")


###### Do the same with a log function:

# Plot of our function: 
plot(x = seq(1,10,by=.0009), y = -log(seq(1,10,by=.0009)))

# We will use y as data, so we do some shuffeling
# in order to be able to use most of the code above:
seq = seq(1,10,by=.0009)
seq = -log(seq)
data = seq

# We can also plot our data as histogram
hist(data, col = "lightblue")

# Use a for loop to take 5000 random samples of size n = 5 each:
n_samp = 5000
sample_n_5 = c() # create empty vector

# The below function will also take the mean of the sample and 
# and stores in our vector sample_n_5:
for (i in 1:n_samp){
  # Store mean of each sample with length 5:
  sample_n_5[i] = mean(sample(data, 5, replace=TRUE))
} # End for i

# Look at the histogram of the sample_n_5:
hist(sample_n_5, col = "lightblue")

####################################
# 4.2 The basic Gaussican function #
####################################

# Gaussian base form:
gaussian_fun = function(x){
  fx = exp(-x^2)
  return(fx)
} # End of function

# Plot of basic Gaussian:
seq = seq(-4,4,by = .1)
fx = gaussian_fun(seq)
plot(x = seq, y = fx)

# For comparison exp(x) only
test_pos = function(x){
  fx = exp(x)
  return(fx)
} # End of function

fx_pos = test_pos(seq)
plot(x = seq, y = fx_pos, type = "l")
abline(v = 0)

# ... as well as exp(-x):
test_neg = function(x){
  fx = exp(-x)
  return(fx)
} # End of function

fx_neg = test_neg(seq)
plot(x = seq, y = fx_neg, type = "l")
abline(v = 0)

# Recall a similar effect when squaring the deviation of 
# x_i-mean(x) and looking at the plot:
x = c(0:10)
plot(x = x, y = (x-mean(x)))    # deviation x_i from mean(x)
abline(h=0)
plot(x = x, y = abs(x-mean(x))) # absolute value of x_i-mean(x)
plot(x = x, y = (x-mean(x))^2)  # plot of squared deviation
# Recall: summing the squared deviation results in 
#         the total sum of squares...

# Plot of exp(-abs(x)):
test_abs = function(x){
  fx = exp(-abs(x))
  return(fx)
} # End of function

# Recall that we will use the absolute value of a sequence
# consisting of value ranging from -4 to 4 (by a step of .1):
plot(x = seq, y = test_abs(seq), typ = "l")  

# It does not even matter that we use the number e as base:
test_gen = function(x){
  fx = 2^(-x^2)
  return(fx)
} # End of function

plot(x = seq, y = test_gen(seq), typ = "l")  


##################################
# 4.2.1 Optional facts on exp(x) #
##################################

###### Some facts on why exp(x) is important:
plot(x = seq, y = fx_pos, type = "l")
# forming coordinate system (+x,-x,+y)
abline(v = 0)

# Point at x = 0, so f(0) = y = e^0 = 1
exp(0) # Now add point to plot:
points(y = 1,x = 0) # P (0|1)

# Point at x = 1
# y = f(x) = e^x; f(1) = e^1 = e
exp(1)

points(exp(1)) # P (e|1)
# Slope of tangent at a point P(f(x)|x) := f'(x) = e^x,
# since f(x) = e^x = f'(x)
# Evaluate y coordinate of P(f(x)|x):
y_coordinate = exp(1)
# [1] 2.718282 = Euler's number e.
# We add the respective tangent at that
# point with a slope = e^x. In case you wonder:
# the linear function x does not need to be defined in curve().
curve(exp(1)*x,-3,3, add = TRUE)

# Integral from -Inf to x = 1
expo_fun = function(x) {exp(x)}
integrate(expo_fun, lower = -Inf, upper = 1)
# 2.718282 with absolute error < 0.00015
# = e again.


##########################################
# 4.3 The Parametrized Gaussian function # 
##########################################

# Gaussian with parametric extension:
seq = seq(100,220,by=.1) # height in cm. 
a = .5                 # height of curve peak  
b = (178.4+164.7)/2    # x of peak = mean = 50%
c = ((178.4-170.8)+(164.7-157.6))/2                   
# width of the bell = sd 
# + turning point of function!
gaussian_fun_para = function(x, a, b, c){
  fx = a*exp(-(((x-b)^2)/(2*c^2)))
  return(fx)
} # End of function

a = 1
fx = gaussian_fun_para(seq,a,b,c)
plot(x = seq, y = fx, xlim=c(135,205), type ="l")
abline(v=b, col = "lightblue")
abline(v=b+c, lty = 2)
abline(v=b-c, lty = 2)

# Parameters
a=1
b=0
c=1

# Sequence
seq = seq(-4,4,by=.1)

# Test 1:5
test_1=exp(-(((seq-b)^2)/(2*c^2)))
test_2=exp(-(((seq-b)^2)/(2*c)))
# Test 1 and Test 2 are actually equal!
# Why? Because we set c to 1, so sd and variance are the same!
all.equal(test_1,test_2) 
# [1] TRUE
test_3=exp(-((seq-b)^2))
test_4=exp(-(((seq-b))))
test_5=exp(-(((seq-b))/(2*c)))

# Plot of Test 1:5
plot(x=seq,y=test_1)
plot(x=seq,y=test_2)
plot(x=seq,y=test_3)
plot(x=seq,y=test_4)
abline(v=0)
plot(x=seq,y=test_5)
abline(v=0)


##############################################################
# 4.4 The Parametrized Gaussian Probability Density Function #
#                (the (standard) normal PDF)                 #
##############################################################

# General formula for a probability density function (PDF).
# Insert mean = 0 and var = 1 to obtain the values for input x
# given a so-called standard normal distribution:
prob_dens = function(x,mean,var){
  fx = (1/sqrt(2*pi*var))*exp((-((x-mean)^2))/(2*var))
  return(fx)
} # End of Function

# In case placing () in the above becomes an issue when
# writing a function, you can also decompose the formula:
prob_dens_decomp = function(x,mean,var){
  fx1 = 1/sqrt(2*pi*var)
  fx2 = (-((x-mean)^2))/(2*var)
  fx = fx1*exp(fx2)
  return(fx)
} # End of function

# Initialize seq:
x = seq(-4,4,by=.01)

# Given standard normal distribution:
norm_PDF     = prob_dens(x, mean = 0, var = 1)

# Using basic R function dnorm():
norm_PDF_alt = dnorm(x,mean = 0, sd = 1)

#Check for equality:
all.equal(norm_PDF,norm_PDF_alt)
# [1] TRUE

# Plot
plot(x=x,y=norm_PDF, type = "l")
plot(x=x,y=norm_PDF_alt,type = "l")

# Plot with var = 1 and var = 3
plot(x=x,y=prob_dens(x,mean = 0,var = 1), type="l", col="blue")

# The lines function adds another cure to existing plot:
lines(x=x,y=prob_dens(x,mean = 0,var = 3),col="red")


######################################
# 4.5 Integrating Gaussian Functions #
######################################

# Gaussian base form:
gaussian_fun = function(x){
  fx = exp(-x^2)
  return(fx)
} # End of function

# Using integrate function:
gaussian_integral = integrate(gaussian_fun, lower = -Inf, upper = +Inf)

# Use gaussian_integral$value to get value only, as integrate()
# returns a message and a list.
all.equal(gaussian_integral$value,sqrt(pi))
# [1] TRUE

# 3D Plot of a Gaussian:
x = seq(-4,4,by=.1)
y = x

# We will not go into full detail why we need the outer product for z,
# but the result is a matrix and z needs to be related to two 
# coordinates: x and y.
# However, outer() allows to write the function of our 3D Gaussian 
# within the outer() function:
z = outer(x, y, function(x,y)(exp(-(x^2+y^2))))

# The persp() function lets us plot in 3D; adjust theta and phi
# in order to change perspective: 
persp(x=x,y=y,z=z, theta =20,phi=20)

# From the top:
persp(x=x,y=y,z=z, theta =0,phi=90)


# Integration using our parametrized Gaussian function:
gaussian_fun_para = function(x, a, b, c){
  fx = a*exp(-(((x-b)^2)/(2*c^2)))
  return(fx)
} # End of function

gauss_para_integ = integrate(gaussian_fun_para,a=a,b=b,c=c,lower=-Inf,upper=Inf)
all.equal(gauss_para_integ$value,(a*sqrt(2*pi*c^2)))
# [1] TRUE


###########################################
# 4.6 Integrating the Standard Normal PDF #
###########################################

# Integrate stand. norm. probability density function:
integrate(prob_dens, mean = 0, var =1, -Inf, Inf)
# Result:
# 1 with absolute error < 9.4e-05

# ... or via using dnorm(), now with the parameters "mean =" and "sd =":
integrate(dnorm, mean = 0, sd = 1, -Inf, +Inf)
# Result:
# 1 with absolute error < 9.4e-05

# We could also use the function pnorm():
p_value_Inf = pnorm(Inf,mean = 0,sd = 1)
# [1] 1

# p-value at the mean = 0, 
# given a mean of 0 and sd of 1:
p_value_pnorm = pnorm(0,mean = 0,sd = 1) 
p_value       = integrate(prob_dens, mean = 0, var =1, -Inf, 0)$value
# [1] 0.5

# Test for equality:
all.equal(p_value_pnorm,p_value)
# [1] TRUE


# CDF of a normal PDF:
norm_CDF = function(seq, mean, var){
  # Initialize vector length(density)
  Fx = seq  
  for(i in 1:length(seq)){
    Fx[i] = integrate(prob_dens, mean = 0, var = 1, lower = -Inf, upper = seq[i])$value
  } # End for i
  # Plot result:
  plot(x=seq, y = Fx, type = "l")
  
  return(Fx)
} # End of function

# Test function and plot CDF:
seq = seq(-4,4,by=.1)
norm_CDF(seq,0,1)


#################################################################
# 5 Standardization and Hypothesis Testing ─ Z-Tests, Z-Scores, #
#             Z-Tables (Standard Normal Tables)                 #
#################################################################


###### Special case of pop. mean == sample mean:
pop_cups = c(0:10)

# Population mean, variance and standard deviation:
# Recall that var() and sd() calculates teh sample var/sd
# including Bessel's correction, therefore we use
# the formulas below:
pop_mean = mean(pop_cups)
# [1] 5
pop_var = sum((pop_cups-mean(pop_cups))^2)/length(pop_cups)
# [1] 10
pop_sd = sqrt(pop_var)
# [1] 3.162278

# Function for evaluation the z-value of a 
# non-standardized data set:
z_score = function(x,pop_mean,pop_sd){
  z = (x-pop_mean)/pop_sd
  return(z)
} # End of function

# Standardized Z-Score for a particular value of x:
# Say we want to standardize a value of x that is equal
# to our mean itself as a special where a z-test formula
# is not needed yet:
z_score(pop_mean, pop_mean, pop_sd)
# [1] 0

# Integrating will reasonably result in
# 0.5, i.e., half of the area under the curve:
pnorm(0)
# [1] 0.5
# For a two-tailed test, checking on equality/inequality:
2*pnorm(0)
# [1] 1

# Let us now look at the z-score for our
# value of mean+/-sd in pop_cups:
z_score((pop_mean-pop_sd), pop_mean, pop_sd)
# [1] -1
z_score((pop_mean+pop_sd), pop_mean, pop_sd)
# [1] +1
z_score((pop_mean+2*pop_sd), pop_mean, pop_sd)
# [1] +2
# Worked!


#### Plot standardization of the dependent variable of 
####     second synth. observing Lady Meow

# Second synthetic data set:
# Cups of Tea: 
cups = c(0,  1, 2,   3.5, 4.8, 5.2, 6,   6.9, 8.5, 9.1, 9.9, # run 1
         0, .6, 2.6, 3.1, 4.8, 6.9, 7.1, 7.5, 8.5, 9.9, 9.9, #     2
         0, .2, 2.9, 2.9, 4.9, 5.2, 6.9, 7.1, 7.3, 9,   9.8) #     3

# Time passed for each run of measurements (cups), abbreviated:
time = c(0:10,0:10,0:10)


# Plot corresponding normal and standard norm. PDF:
# Normal PDF (non- standardized) of our data:
seq = seq(-10,20,by=.1)
plot(x=seq,y=prob_dens(seq,mean(cups),pop_var),type ="l")

# Translating all possible values on the x axis into our 
# normal PDF into the format of a standard(!) normal PDF:
z_seq = z_score(seq,mean(cups),pop_var)
plot(x=z_seq,y=prob_dens(seq,mean(cups),pop_var),type ="l")


### Standard normal distribution table / z-table:

# Initilize seq for z-scores (+/-)
z = seq(0,9.99,by = 0.01) 

# Calculate all possible z-scores from 0 to 9.99:
p_z = pnorm(z)

# Plotting the above shows that we calculated the
# upper half of the normal CDF:
plot(x=z,y=p_z)

# Set as matrix/table:
z_table = matrix(p_z,ncol = 10,byrow = TRUE)

# Add row and column names to matrix:
rownames(z_table) = seq(0,9.99,b = .1)
colnames(z_table) = seq(0,.09,by = .01)


# Evaluate z and search table for corresponding p-value:
#       [["col","row"]]
z_table[["3.2","0.03"]] 

# ...or use the following way without having to separate the digits:

# Change class to data_frame:
table = as.data.frame(z_table)

# Define z_score:
z_score = 3.23 # only use value up to two decimal places

# The following lines separates the first 3 digits into the
# [["col","row"]] structure, so you don't need to do yourself:
rowDigit = as.numeric(substr(z_score*100, 1,2))/10 # substr(x,start,stop)
# [1] 3.2
colDigit = as.numeric(substr(z_score*100, 3,3))/100
# [1] 0.03

# Execute the next line to find the respective value
# in the table:
table[which(rownames(table) == rowDigit),which(colnames(table) == colDigit)]
# [1] 0.999381
pnorm(3.23,0,1)
# [1] 0.999381


# Use the qnorm function for the inverse CDF:
# Note that we used a rounded score as example
# before:
qnorm(0.990,0,1) # for a p-value threshold of 1%
# [1] 3.229977

seq = seq(0,1,by=0.0001)
quant_func = c() # empty vector
for(i in 1:length(seq)){
  quant_func[i] = qnorm(seq[i],mean=0,sd=1)
} # End for i

# Plot Quantile function:
plot(x=seq,y=quant_func, type="l")


#######################################################
# 5.1 The Regular Z-Test / Gauß-Test  and its P-Value # 
#######################################################

# Comparing distributions:
# We chose equal variance, so the form will be the same

# Sample:
mean_samp = 5
var_samp = 12

# Population:
mean_pop = 15
var_pop = 12 

# Plot:
seq_samp = seq(-7,40,by =.01)
plot(x = seq_samp, y = prob_dens(seq_samp,mean_samp,var_samp), type = "l", xlim=c(-8,30),lty =2)
seq_pop = seq(-7,40,by =.01)
lines(x = seq_pop,y = prob_dens(seq_pop,mean_pop,var_pop), type = "l")
abline(v=mean_pop,col="lightblue")
abline(v=mean_samp,col="orange")
abline(h=0) # line for x-axis at y = 0

# Quantile function for .05 of pop:
pop_threshold = qnorm(.05,mean=15,sd=sqrt(12))

# Add line to mark lower tail threshold of .05 (5%):
segments(x0=pop_threshold,x1=pop_threshold, y0 = 0, y1 = prob_dens(pop_threshold,mean_pop,var_pop))


##############################################
# 5.1.3 Standard Deviation/Error of the Mean #
##############################################

# Example for SEM:

# Our population:
x = c(rep(c(0:10),100))

# Population variance:
var = sum((x-mean(x))^2)/length(x)

# Evaluate SEM for several different sizes of n:
n_sample = 20
sem_20 = sqrt(var)/sqrt(n_sample) 
# [1] 0.7071068 

n_sample = 200
sem_200 = sqrt(var)/sqrt(n_sample) 
# [1] 0.2236068

n_sample = 1000
sem_1k = sqrt(var)/sqrt(n_sample) 
# [1] 0.1

n_sample = length(x)
sem_N = sqrt(var)/sqrt(n_sample) 
# [1] 0.09534626

n_sample = 10000
sem_10k = sqrt(var)/sqrt(n_sample) 
# [1] 0.0003015113

sem_20 > sem_200 & sem_200 > sem_1k & sem_1k > sem_N & sem_N > sem_10k
# [1] TRUE


# Standard error of the mean including actual sampling:
# How the SEM becomes smaller, the more samples we take:

# First we will use the sample sd and later the population
# sd, which we will set to 2 below

set.seed(1) # for reproducability
data = rnorm(n = 1000, mean = 10, sd = 2)

# We can again plot our data as histogram
hist(data, col = "lightblue")

# Use a for loop to take 5000 random samples of size n = 50 each:
n_samp = 5000
sample_mean = c() # create empty vector

# The below function will also takes samples of length 50
# and then mean of that sample stores in our vector sample_mean.
# Running this loop will always result in different samples:
for (i in 1:n_samp){
  # Store mean of each sample with length 50:
  sample = sample(data, 50, replace=TRUE)
  sample_mean[i] = mean(sample)
} # End for i

# Look at the histogram of the sample_mean:
hist(sample_mean, col = "lightblue")

# Now we compare the SEM of the first 20 sample
# with the SEM of the first 250 sample. Recall
# the vector sample_n_50 contains 5000 means
# with a sample size of 50 each:
sample_mean
length(sample_mean) 
# [1] 5000


#### SEM of the first 20 samples:
samp_n20 = sample_mean[1:20]
length(samp_n20)
# [1] 20

# SEM_n20
SEM_n20 = sd(samp_n20)/sqrt(length(samp_n20))
# [1] 0.05782984


#### SEM of the first 250 samples:
samp_n250 = sample_mean[1:250]
length(samp_n250)
# [1] 250

# SEM_n20
SEM_n250 = sd(samp_n250)/sqrt(length(samp_n250))
# [1] 0.01723958


#### SEM of all 5000 samples:
SEM_n5000 = sd(sample_mean)/sqrt(length(sample_mean))
# [1] 0.003924541

# The numerical values will deviate, each time you run the
# loop, but it will always hold the logic of:
SEM_n20 > SEM_n250 & SEM_n250 > SEM_n5000
# [1] TRUE

# For a z-test we would have to use the population sd,
# which we set to be 2 in rnorm() function above. Given
# the sd we can again just simply adjust a size of n:
# pop_sd/sqrt(n):
2/sqrt(20) > 2/sqrt(250) & 2/sqrt(250) > 2/sqrt(10000) 
# [1] TRUE


#############################################################
# 5.2 Possible Hypotheses of the One- and Two-Sample Z-Test #
#                and their Respective P-Values              #
#############################################################

### Chat Noir One-Sample Z-test:

# We will compare sad cats with a population
# of cats ("all cats"):

# Happy cats like Lady Meow drink 10 cups of tea on average:
pop_cats = c(rep(seq(0,10, length(11)),10))

# Sad cat, which drank only 2 cups of tea in 10 hours:
sample_sad_cats = seq(0,2,length = 11)
# [1] 0.0 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0


# Population var and sd:
var = sum((pop_cats-mean(pop_cats))^2)/length(pop_cats)
sd = sqrt(var)
# SEM:
sem = sd/sqrt(length(sample_sad_cats))

# Z-test:
z_stat = (mean(sample_sad_cats)-mean(pop_cats))/sem
# [1] -4.195235

# P-Value LOWER TAIL
p_val_low = pnorm(z_stat,mean = 0,sd = 1)
# [1] 1.362942e-05
p_val_low < .05
# [1] TRUE

# P-Value UPPER TAIL:
# The below is only theoretically the case and we
# have to pretend to have gotten a positive z-statistics.
# This spares us the time of creating another set of data.
# This time the below actually argues that sad cats drink 
# more tea than happy cats (we made thinks up, but just 
# you know):
p_val_upper = pnorm(abs(z_stat),mean = 0,sd = 1)
1-p_val_upper
# [1] 1.362942e-05
1-p_val_upper < .05
# [1] TRUE

# P-Value TWO-TAIL
p_val_two = 2*pnorm(z_stat,mean = 0,sd = 1)
# [1] 2.725883e-05
p_val_two < .05
# [1] TRUE


#### Chat Noir Independent Two-Sample Z-test:

# We will compare happy cats such as Lady Meow with 
# unfortunate cats that are evidently sad (inclusion 
# criteria will left out for now):

# Happy Cats:
sample_cats = c(0:10)
pop_cats = seq(0,10.5,length = 11)
pop_cats = c(rep(pop_cats,100))

# Sad cats, which drink only 1 cup of tea in 10 hours :'(
sample_sad_cats = seq(0,2.5,length = 11)
# [1] 0.00 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00 2.25 2.50
pop_sad_cats = c(rep(seq(0,2.5,length = 11),100)) 


# SEM of each sample:
happy_var = sum((pop_cats-mean(pop_cats))^2)/length(pop_cats)
happy_pop_sd = sqrt(happy_var)
happy_sem = happy_pop_sd/length(sample_cats)

sad_var_pop = sum((pop_sad_cats-mean(pop_sad_cats))^2)/length(pop_sad_cats)
sad_pop_sd = sqrt(sad_var_pop)
sad_sem = sad_pop_sd/length(sample_sad_cats)

# z-Statistics:
z_statistics_num = (mean(sample_sad_cats)-mean(sample_cats))-(mean(pop_sad_cats)-mean(pop_cats))
z_statistics_denom = sad_sem + happy_sem
z_statistics = z_statistics_num/z_statistics_denom
# [1] -10.03415

# We can simplify the formula and will get the same results:
z_statistics_num = (mean(sample_sad_cats)-mean(sample_cats))
z_statistics_denom = sad_sem + happy_sem
z_statistics = z_statistics_num/z_statistics_denom
# [1] -10.03415  # == -10.034 sd's away from the pop. mean

# P-value:
pnorm(z_statistics)
# [1] 5.394214e-24

#### Chat Noir Dependent/Paired Two-Sample Z-test:

# Sad cats, which drink only 1 cup of tea in 10 hours :'(
# We can also consider this as timestep t_1:
sample_sad_cats = seq(0,2.5,length = 11)
# [1] 0.00 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00 2.25 2.50

# Timestep t_2, i.e., after cat was on catnip:
sample_catnip = c(0:10)
# [1]  0  1  2  3  4  5  6  7  8  9 10

# Evaluate the difference between t_1 and t_2 via
# diff = t_2 - t_1
diff = sample_catnip-sample_sad_cats

# Set up data table:
data = cbind(sample_sad_cats, sample_catnip, diff)
data = as.data.frame(data) # in order to be able to use $

#       sample_sad_cats  sample_catnip diff
#  [1,]            0.00              0 0.00
#  [2,]            0.25              1 0.75
#  [3,]            0.50              2 1.50
#  [4,]            0.75              3 2.25
#  [5,]            1.00              4 3.00
#  [6,]            1.25              5 3.75
#  [7,]            1.50              6 4.50
#  [8,]            1.75              7 5.25
#  [9,]            2.00              8 6.00
# [10,]            2.25              9 6.75
# [11,]            2.50             10 7.50

# Caluclate the mean difference
mean_diff = sum(data$diff)/length(diff)
# Total sum of squared mean differences
sum_sq_diff = sum(((diff-mean_diff)^2))
# Standard error of the mean differences
sd_mean_diff = sqrt(sum_sq_diff/length(diff))/sqrt(length(diff))

# Z_value:
z_value = mean_diff/sd_mean_diff 

# Upper tail z-test, arguing that catnip has the effect that
# sad cats are drinking more tea than before again:
p_value = 1-pnorm(z_value)
# [1] 7.854725e-08


########################
# 6 The Regular T-Test #
########################

# PDF of a T-Distribution:
# df = n-1 # or 2 for two estimates/relations

t_distr = function(x,df){
  t_1 = gamma((df+1)/2)/(sqrt(df*pi)*gamma(df/2))
  t_2 = (1 + (x^2/df))^(-(df+1)/2)
  t_distr = t_1*t_2
  return(t_distr)
} # End of function

# Initialize Sequence: 
seq = seq(-4,4,by = .1)

# Plot standard normal and t-distribution
# with df = 1, df = 2, df = 10, df = 100:
plot(x=seq,y=prob_dens(seq,0,1), type ="l") 
lines(x=seq,y=t_distr(seq,1),col="red")    # df 1
lines(x=seq,y=t_distr(seq,2),col="green")  # df 2
lines(x=seq,y=t_distr(seq,10),col="blue")  # df 10
# Here you can see that given a df = 30 theh t-distribution
# gets very close to standard normal distribution, hence 
# the z-score is used, given n>30:
lines(x=seq,y=t_distr(seq,30),col="lightblue") # df 30

# Moving to df = 100 the light blue line will essentially overlap
# with the standard normal PDF
plot(x=seq,y=prob_dens(seq,0,1), type ="l")
lines(x=seq,y=t_distr(seq,100),col="lightblue") # df 100


####################################################
# 7 Hypothesis Test on the Slope and the Intercept #
#               of a Linear Model                  #
####################################################

# Next we are going to look at a data set where
# a linear relation is not significant. We will
# use the integrated data set "mtcars" for that.
# Let us ignore the meaning of the dependent and
# independent variable and just look at the plot
# that obviously shows no slope above 0:
data("mtcars")
dep = mtcars$disp
indep = c(1:length(dep))
plot(indep,dep)

# The p-value is 0.9047, suggesting no significant
# result, given a slope of -0.2913, a very low
# slope, barely different to 0:
compare = summary(lm(dep~indep))
#  Call:
#  lm(formula = dep ~ indep)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -158.60 -105.75  -36.75   97.03  240.84 

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 235.5282    45.5971   5.165 1.46e-05 ***
#  indep        -0.2913     2.4116  -0.121    0.905    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 126 on 30 degrees of freedom
# Multiple R-squared:  0.0004861,	Adjusted R-squared:  -0.03283 
# F-statistic: 0.01459 on 1 and 30 DF,  p-value: 0.9047


# For more detailed numbers look at:
compare$coefficients
#               Estimate Std. Error    t value     Pr(>|t|)
# (Intercept) 235.528226  45.597118  5.1654192 1.460149e-05
# indep        -0.291294   2.411567 -0.1207903 9.046625e-01

# Our optimal linear model:
beta  =  cov(indep,dep)/var(indep)
# [1] -0.291294
alpha =  mean(dep)-beta*mean(indep)
# [1] 235.5282
fx = alpha+beta*indep

# Plot model:
plot(indep,dep)
abline(a=alpha,b=beta)


#########################
# 7.1 Testing the slope #
#########################

# Sum of square residuals:
SumR2     = sum((dep-fx)^2)
# [1] 475953.3
SumR2_alt = sum(as.numeric(residuals(lm(dep~indep))^2))
SumR2_alt2 = sum((dep-mean(dep))^2) - beta*sum((dep-mean(dep))*(indep-mean(indep)))

all.equal(SumR2,SumR2_alt,SumR2_alt2)
# [1] TRUE

# Residual SE:
residual_standard_error = sqrt(SumR2/(length(indep)-2)) 
# [1] 125.9568
compare
# => Residual standard error: 126
# summary(lm(y~x)) rounds the value in the output,
# but since we will get to equivalent results we can
# savely assume that the output value is just rounded.

# Standard error of the slope for t-distribution:
se_denom = sqrt(sum((indep - mean(indep))^2))
se_slope_t = residual_standard_error/se_denom

# t-value:
t_value = beta/se_slope_t 
# [1] -0.1207903

# p-value for two-tail == to standard output of summary(lm(y~x)):
2*integrate(t_distr, df = length(indep)-2, lower = -Inf, upper = t_value)$value
# [1] 0.9046625
# Alternative via basic R function pt():
2*pt(t_value, df = length(indep)-2) 
# [1] 0.9046625

# t-value and p-value is equivalent with the output of
# summary(lm(y~x)):
compare$coefficients
#               Estimate Std. Error    t value     Pr(>|t|)
# (Intercept) 235.528226  45.597118  5.1654192 1.460149e-05
# indep        -0.291294   2.411567 -0.1207903 9.046625e-01


#############################
# 7.2 Testing the intercept #
#############################

# Example of a model with slope of zero:
x = c(0:10)
y = rep(5,11)
plot(x,y)
abline(h=5)

# Standard error of the intercept:
SE_a = sqrt(SumR2/(length(indep)-2))*sqrt((1/length(indep))+((mean(indep)^2)/sum((indep-mean(indep))^2)))
# [1] 45.59712

# t-value intercept:
t_value = alpha/SE_a
# [1] 5.165419

# p-value intercept:
2*(1-(integrate(t_distr, df = length(indep)-2, lower = -Inf, upper = t_value)$value))
2*(1-pt(t_value,df = length(indep)-2))
# [1] 1.460149e-05


###################################################
# 8 Update on our linear_least_squares() Function #
###################################################

# Updated linear least squares function:
# Go-go-gadgeto linear_least_square!!!!

linear_least_square = function(indep,dep){ # Start of function
  
  # Evaluating coefficients for and optimal linear model
  # given a set of dependent and independent variables:
  beta  =  cov(indep,dep)/var(indep)
  alpha =  mean(dep)-beta*mean(indep)
  fx = alpha+beta*indep
  
  # Sum of Squared Errors/Residuals:
  SumR2     = sum((dep-fx)^2)
  
  # Residual Standard Error/Deviation:
  residual_standard_error = sqrt(SumR2/(length(indep)-2)) 
  
  # Standard error of the slope for t-distribution:
  se_denom = sqrt(sum((indep - mean(indep))^2))
  se_slope_t = residual_standard_error/se_denom
  
  # Standard error of the incercept for t-distribution:
  se_a = sqrt(SumR2/(length(indep)-2))*sqrt((1/length(indep))+((mean(indep)^2)/sum((indep-mean(indep))^2)))
  
  # t-value of the slope:
  t_value_b = beta/se_slope_t 
  
  # t-value of the intercept:
  t_value_a = alpha/se_a
  
  ### p-value of the slope via integrating the PDF of a t-distribution
  # up to the t-value calculated above:
  t_distr = function(x,df){
    t_1 = gamma((df+1)/2)/(sqrt(df*pi)*gamma(df/2))
    t_2 = (1 + (x^2/df))^(-(df+1)/2)
    t_distr = t_1*t_2
    return(t_distr)
  } # End of function
  
  # Two-Tail P(t=T|H_0):
  p_b_2t = 2*integrate(t_distr, df = length(indep)-2, lower = -Inf, upper = t_value_b)$value
  
  ### p-value of the intercept:
  
  # Two-Tail P(t=T|H_0):
  p_a_2t = 2*(1-(integrate(t_distr, df = length(indep)-2, lower = -Inf, upper = t_value_a)$value))
  
  # Results for two tail
  Results_a = c(round(alpha,4), round(se_a,4), round(t_value_a,4), p_a_2t)
  Results_b = c(round(beta,4), round(se_slope_t,4), round(t_value_b,4), p_b_2t)
  Res_data = as.data.frame(rbind(Results_a,Results_b))
  colnames(Res_data) = c("Estimate","Std. Error","t value","Pr(>|t|)")
  rownames(Res_data) = c("Intercept", "Reg. coeff.")
  
  # Nice output using cat() function:
  cat(" Linear least square method in R","\n","\n",
      "Independent variable:", "\t", deparse(substitute(indep)),"\n", 
      "Dependent   variable:", "\t", deparse(substitute(dep)),"\n","\n",
      "alpha", "\t",alpha,"\n",
      "beta","\t",beta, "\t","SumR2", "\t", SumR2, "\n","\n")
  print(Res_data)
  cat("\n","Residual Standard Error:",round(residual_standard_error),"on",
      (length(indep)-2), "degrees of freedom","\n","\n") 
  
  # Let us also plot our results:
  # We will also use deparse(substitute(x)) for the 
  # labels of our plot axes.
  plot(x=indep,y=dep, ylab = deparse(substitute(dep)),       
       xlab = deparse(substitute(indep)))
  abline(a=alpha, b=beta, col = "darkblue")
} # End of function

# Test:
linear_least_square(indep, dep)

# Linear least square method in R 
# 
# Independent variable: 	 indep 
# Dependent   variable: 	 dep 
#
# alpha 	 235.5282 
# beta 	 -0.291294 	 SumR2 	 475953.3 
#
#             Estimate Std. Error t value     Pr(>|t|)
# Intercept   235.5282    45.5971  5.1654 1.460149e-05
# Reg. coeff.  -0.2913     2.4116 -0.1208 9.046625e-01
#
# Residual Standard Error: 126 on 30 degrees of freedom 
