#############################################################
##---------------------------------------------------------##
##                Inferential Statistics IV:               ##
##   Uniform and (Standard) Normal Probability (Density)   ## 
##                        Functions                        ## 
##              by Steffen Schwerdtfeger 01.2023           ##
##---------------------------------------------------------##
#############################################################


####################################################################
#    1 Uniform PMFs, PDFs and CDFs ─ Uniform Probability Mass      #
# Functions (Discrete), Probability Density Functions (Continuous) #
#      and the Cumulative Distribution Function (Continuous)       #
####################################################################

####################################################
# 1.1 Uniform Probability Mass Function (Discrete) #
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
# 1.2 Uniform Probability Density Functions (continuous) #
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
# 1.3 The Cumulative Distribution Function of a Uniform PDF #
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
# 2 The (Standard) Normal Probability Distribution # 
#           ─  Gaussian PDFs and CDFs              #
####################################################

#################################
# 2.1 The Central Limit Theorem # 
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
# 2.2 The basic Gaussican function #
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
# 2.2.1 Optional facts on exp(x) #
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
# 2.3 The Parametrized Gaussian function # 
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
# 2.4 The Parametrized Gaussian Probability Density Function #
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
# 2.5 Integrating Gaussian Functions #
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
# 2.6 Integrating the Standard Normal PDF #
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
# 3 Standardization and Hypothesis Testing ─ Z-Tests, Z-Scores, #
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


