##################################
##################################
#      R-Basic Tutorial II       #
#     CIPom @ Charité Edition    #
#               by               #
#     Steffen Schwerdtfeger      # 
#            10.2023             #
##################################
##################################

##################################################################################
# 8 Einstieg in die Inferenzstatistik: Bedingte Wahrscheinlichkeit / Bayes‘ Rule #
##################################################################################

###### Benfords law example, number between 0 and 10: 
n=seq(0,10,by=.1)
benford = log(n+1)-log(n)
plot(n,benford)

###### Numeric example cond. probability / Bayes' rule:
prior = c(.5,.5)       # Can be set by us!
likelihood = c(.6,.4)  # Can be set by us!
posterior = (prior*likelihood)/sum(prior*likelihood)
# [1] 0.6 0.4

###### Vanessa Holmes example:

# First iteration:
joint = c(.25,.25,.25,.25)*c(.33,.33,.33,0)
model_evidence = sum(c(.25,.25,.25,.25)*c(.33,.33,.33,0))
posterior = joint/model_evidence
posterior

# Second iteration (another suspect ruled out):
joint = c(.33,.33,.33,0)*c(.5,.5,0,0)
model_evidence = sum(c(.33,.33,.33,0)*c(.5,.5,0,0))
posterior = joint/model_evidence
posterior

# Third iteration - let's say all of the suspects are ruled out:
joint = c(.5,.5,0,0)*c(0,0,0,0)
# [1] 0 0 0 0
model_evidence = sum(c(.5,.5,0,0)*c(0,0,0,0))
# [1] 0 
posterior = joint/model_evidence # Note that it says: 0 divided by 0!
posterior

# [1] NaN NaN NaN NaN 
# NaN = Not a number.
# This is due to the fact that 0/0 is not defined. At this point
# Vanessa Holmes would need to find another suspect to be able 
# to do further inference...



#####################################################################################
# 9 Lineare Regression via Methode der kl. Fehlerquadrate + via deskript. Parameter # 
#####################################################################################

cups = c(0:10)
time = c(0:10)
plot(x=time, y=cups)

# lm(y~x) => y under the condition of x
lm(cups~time)
# Add linear model to plot above
abline(lm(cups~time))


# Errors/residuals:
# error = y-f(x); f(x) = x
error = cups - time
# [1] 0 0 0 0 0 0 0 0 0 0 0
error_alt = residuals(lm(cups~time))
#             1             2             3             4             5             6             7 
# -2.766552e-16  6.000627e-16  2.767107e-17 -1.331760e-16 -7.197851e-17 -1.218033e-16 -1.716281e-16 
#             8             9            10            11 
#  2.226363e-16 -2.712777e-16 -3.211025e-16  5.172512e-16 

# Are both approaches equal (enough)?
all.equal(error,as.numeric(error_alt))

# Let us have a look at our dino:
dino = read.csv("dino_csv.csv")
plot(x= dino$x, y = dino$y)
abline(lm(dino$y~dino$x))

### Non idealized Lady Meow Example:
# Cups of Tea:
cups = c(0, 1, 2, 3.5, 4.8, 5.2, 6, 6.9, 8.5, 9.1, 9.9,
         0, .6, 2.6, 3.1, 4.8, 6.9, 7.1, 7.5, 8.5, 9.9, 9.9,
         0, .2, 2.9, 2.9, 4.9, 5.2, 6.9, 7.1, 7.3, 9, 9.8)

# Time passed for each run of measurements (cups), abbreviated:
time = c(0:10,0:10,0:10) # technically 3 days in the life of Lady Meow


# You can add labes to the plot!
plot(x=time, y=cups,
     ylab = "Cups of Tea", xlab= "Hours passed")

lm(cups~time)

### Variance/SD

x = c(0:10)

# Pure deviation from the mean (without summing and squaring!):
dev_of_mean_X = x-mean(x)

# Plot of dev_of_mean:
plot(x = x, y = dev_of_mean_X) 
abline(h=0) # h = 0 adds horizontal line at y = 0

# Pure deviation from the mean (without summing!):
dev_of_mean_X = x-mean(x)

# Plot using absolute values via function abs():
plot(x = x, y = abs(dev_of_mean_X)) 
# Note: Only 0 deviation given when x_i = 5 = mean(x)

# Plot using the square, which smoothes the graph:
plot(x = x, y = dev_of_mean_X^2)


#### Lin. Reg. via deskript. Parameters:

x = time
y = cups


# SumSqXY = ∑(x_i - E[X])*(y_i - E[y])
SumSqXY = sum((x-mean(x))*(y-mean(y)))

# Covariance:
covarXYsamp = SumSqXY / (length(x)-1) # sample covariance
# [1] 10.39687
covarXYpop  = SumSqXY / length(x)     # population covariance
# [1] 10.08182
covarXYsamp = cov(x,y)                # samp cov := cov(x,y)
# [1] 10.39687


##############################################################################
# 10 Z- und T-Test für einfache Verteilung und T-Test für Lineare Regression #
##############################################################################

# Our goal for linear regression (below we will do a regular t-test first!):
summary(lm(cups~time))

# Density via density() function:
plot(density(time))

# Shapiro Wilk Test 
# p-value GREATER .05 == normally distributed!!!!
shapiro.test(cups)  # not normally distributed!


#### Simulation Central Limit Theorem:

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


# Plot norm distribution:
plot(x = seq(-4, 4, by = .1), y = dnorm(seq(-4, 4, by = .1)))
     

### T-Test:

# Basic R-function:
x = c(0:10)
y = c(0:10)*3
t.test(x,y) # Welch test was automatically chosen (t-test for unequal variance)


# PDF of a T-distribution and stand. normal PDF, 
# comparing different df:
seq = seq(-4,4,by=.1)
plot(seq,dnorm(seq, mean = 0, sd = 1), type = "l")
lines(seq,dt(seq, df = 1), type = "l", col = "green")
lines(seq,dt(seq, df = 3), type = "l", col = "blue")
lines(seq,dt(seq, df = 10), type = "l", col = "red")

### Effect size Cohen's d:
# install.packages("effsize")
library(effsize)
x = c(0:10)
y = c(0:10)+3.5
cohen.d(x, y) 
# Here a difference of -1.05529
cohens_d_formular = (mean(x)-mean(y))/sd(y)

# Here you can see how 
plot(x= seq(-7,25, by=.1), y = dnorm(seq(-7,25, by=.1),mean(x), sd = sd(x)), type = "l")
lines(x = seq(-10,35, by=.1), y = dnorm(seq(-10,35, by=.1),mean(y), sd = sd(y)))
abline(v=mean(y)-(1.05529*sd(y)/2)) # adds a vertical line

# Comparison z-stat and effect size:
pop_sd = sum((y-mean(y))^2)/length(y)
sem_x = pop_sd/sqrt(length(x))
z_stat = (mean(x)-mean(y))/sem_x
effect_size = (mean(x)-mean(y))/pop_sd
# Reconstruct mean:
mean(y)-(.35*pop_sd)

# Same with n = 1
sem_x_2 = pop_sd/sqrt(1)
z_stat_2 = (mean(x)-mean(y))/sem_x_2
effect_size_2 = (mean(x)-mean(y))/pop_sd

# Check for equality of z_stat2 and effect_size_2:
all.equal(z_stat_2,effect_size_2)


### Caluclating n give a certain power and conf. interval
# install.packages("pwr")
library(pwr)
x = c(0:10)
y = c(0:10)+3.5
# Calculate needed sample size for one sample t-test, given d =.5
# a power of .8 and a sign.level of .5 == confidence int. of .95:
pwr.t.test(d = .5, sig.level = .05, power = .8, type = "one.sample") 
# n = 33.36713 for One-Sample
# Same for two sample:
pwr.t.test(d = .5, sig.level = .05, power = .8, type = "two.sample") 
# we usually assume .5 medium effect a priori
# n = 63.76561  for each group!





