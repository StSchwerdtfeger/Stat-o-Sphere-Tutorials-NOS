##################################
##################################
#     R-Basic Tutorial II/III    #
#     CIPom @ Charité Edition    #
#               by               #
#     Steffen Schwerdtfeger      # 
#            10.2023             #
##################################
##################################

# install.packages("tidyverse") # needed for caret...
library(tidyverse)
# install.packages("caret") # May demand R 4.4.x 
library(caret)
# install.packages("effsize)
library(effsize)
# install.packages("shiny") # Shiny app
library(shiny)

###### Benfords law example, number between 0 and 10: 
n = seq(0,10,by=.1)
benford = log(n+1)-log(n)
plot(n,benford)

#####################################################################################
# 8 Introduction into Inferential Statistics: Conditional Probability / Bayes‘ Rule #
#####################################################################################
##########################################################
## Numeric Cond. Prob. / Bayes Rule Example and Function #
##########################################################

###### Numeric example cond. probability / Bayes' rule:
prior = c(.5,.5)       # Can be set by us!
likelihood = c(.6,.4)  # Can be set by us!
posterior = (prior*likelihood)/sum(prior*likelihood)
# [1] 0.6 0.4

# The only case where the conditional probabilities (posterior and likelihood)
# are also completely bi-directional, i.e., not only proportionally via the joint, 
# is when prior AND likelihood are distributed uniformly - then every probability 
# in Cond. Prob. / Bayes' rule auto. will be equivalent to each other, 
# in any other case the process of updating a model (essentially the joint prob., 
# since it is calculated by the likelihood weighted by the prior which is the 
# former posterior probability) makes a return to the initial prior
# impossible. In other words, in the above example we moved on, starting from a 
# rather naive uniform perspective as prior assumption, the below just remains
# also numerically bidirectional:

prior = c(.5,.5)       
likelihood = c(.5,.5)  
model_evidence = sum(prior*likelihood) # sum rule!
posterior = (prior*likelihood)/sum(prior*likelihood)
joint1 = likelihood*prior
joint2 = posterior*model_evidence

# Check for equivalence:
all.equal(prior,likelihood,model_evidence,posterior,joint1,joint2)
# [1] TRUE # it says TRUE, even though the joint = [.25 .25] - I can't tell why yet

# Prior and model evidence are also equal from the perspective of the sum rule:
sum(joint1)==sum(joint2) 
# The result of the sum rule, such as the above object "model_evidence=sum(prior*likelihood)"
# is not a prob. vector, but of course ex negativo entails 
# its complement "not-data", as prob. vectors just do by themselves so to speak...


############# BOTH OF THE BELOW FUNCTIONS ONLY WORK WITH PROB. VECTORS EXCEPT WHEN MODEL EVIDENCE IS PROVIDED
############# First simple version of our Bayes_Machine() function:
Bayes_Machine = function(prior,likelihood) {
  joint = prior*likelihood
  # na.rm = TRUE in sum() deletes 0 rows if given
  modelevidence = sum(joint, na.rm = TRUE) 
  posterior = joint/modelevidence
  # Needed for console output and adds text to it
  # using a matrix, which works similar as c()
  postprint = as.data.frame(matrix(c("Posterior",posterior,
                                     "Joint probability", joint,
                                     "Model evidence", modelevidence)))
  print(postprint) # or use return
}  # end of function

# Give it a try with defined prior and likelihood:
prior = c(.5,.5)
likelihood = c(.5,.5)
Bayes_Machine(prior,likelihood)

# Try these inputs and contemplate the results:
prior = c(.1,.9)
likelihood = c(.9,.1)
Bayes_Machine(prior,likelihood)


############## Here is the extended function that can also handle 
############## the model evidence as input (and single prob. values, if given the model evidence!)

Bayes_Machine = function (prior,likelihood,modelevidence) {
  if (missing(modelevidence)){
    joint = prior*likelihood
    # na.rm = TRUE in sum() deletes 0 rows if given
    modelevidence = sum(joint, na.rm = TRUE) 
    posterior = joint/modelevidence
    # Needed for console output
    postprint = as.data.frame(matrix(c("Posterior",posterior,
                                       "Joint probability", joint,
                                       "Model evidence", modelevidence))) 
    print(postprint)
  } # End if
  else {
    joint = prior*likelihood
    posterior = joint/modelevidence  
    postprint = as.data.frame(matrix(c("Posterior",posterior,
                                       "Joint probability", joint,
                                       "Model evidence", modelevidence)))
    print(postprint) 
  } # End Else
} # End of function

Bayes_Machine(likelihood = likelihood, prior = prior)


###########################
## Vanessa Holmes Example #
###########################

# First iteration:
# joint = prior*likelihood
joint = c(.25,.25,.25,.25)*c(.33,.33,.33,0) 
# model_evidence == Sum over A == A and B + not-A and B == sum(joint)
model_evidence = sum(c(.25,.25,.25,.25)*c(.33,.33,.33,0)) 
posterior = joint/model_evidence
posterior

# Second iteration (another suspect ruled out):
joint = c(.33,.33,.33,0)*c(.5,.5,0,0) # prior*likelihood
model_evidence = sum(c(.33,.33,.33,0)*c(.5,.5,0,0)) # sum(joint)
posterior = joint/model_evidence
posterior

# Third iteration - let's say all of the suspects are ruled out:
joint = c(.5,.5,0,0)*c(0,0,0,0) # prior*likelihood
# [1] 0 0 0 0
model_evidence = sum(c(.5,.5,0,0)*c(0,0,0,0)) # sum(joint)
# [1] 0 
posterior = joint/model_evidence # Note that it says: 0 divided by 0!
posterior


# [1] NaN NaN NaN NaN 
# NaN = Not a number.
# This is due to the fact that 0/0 is not defined. At this point
# Vanessa Holmes would need to find another suspect to be able 
# to do further inference... 



###############################
# Example COVID19 Quick Tests #
###############################

# install.packages("caret") # May demand R 4.4.x 
library(caret)

# Contingency table / confusion matrix of our COVID19 test-kit example
true_pos = 144
true_neg = 4
false_pos = 1
false_neg = 499

# Turn the above into a matrix/table:
table = matrix(c(true_pos,true_neg,false_pos,false_neg), nrow=2) # col-wise 
table = matrix(c(true_pos, false_pos,true_neg,false_neg), nrow=2, byrow=TRUE) # row-wise

# table
#      [,1] [,2]
# [1,]  144    1
# [2,]    4  499


# Sensitivity
sensit = true_pos/(true_pos+true_neg)
# [1] 0.972973
TNR = 1-sensit
# [1] 0.02702703
# Check:
sensit+TNR
# [1] 1

# Specificity
specif = false_neg/(false_neg+false_pos)
# [1] 0.998
FPR = 1-specif
# [1] 0.002

# PPV, NPV, FDR and FOR given a prevalence of 14%
prev = .14
PPV = (sensit*prev)/((sensit*prev)+(1-sensit)*(1-prev))
# [1] 0.8542373
FDR = 1-PPV
# [1] 0.1457627

NPV = (specif*prev)/((specif*prev)+(1-specif)*(1-prev))
# [1] 0.9878394
FOR = 1-NPV
# [1] 0.01216063

# Balanced Accuracy
bal_acc = (sensit+specif)/2
# [1] 0.9854875


# Load Caret package for computing confusion matrix
# install.packages("caret") # UNCOMMENT TO INSTALL PACKAGE!!!
library(caret)  
confusionMatrix(table)
?confusionMatrix()
# Confusion Matrix and Statistics

#     A   B
# A 144   1
# B   4 499

# Accuracy : 0.9923          
# 95% CI : (0.9821, 0.9975)
# No Information Rate : 0.7716          
# P-Value [Acc > NIR] : <2e-16          

# Kappa : 0.978           

# Mcnemar's Test P-Value : 0.3711          

#             Sensitivity : 0.9730          
#             Specificity : 0.9980          
#          Pos Pred Value : 0.9931          
#          Neg Pred Value : 0.9920          
#              Prevalence : 0.2284          
#          Detection Rate : 0.2222          
#    Detection Prevalence : 0.2238          
#       Balanced Accuracy : 0.9855          

#        'Positive' Class : A


# How did they obtain a accuracy of 99.2%? The code of the function
# confusionMatrix() reveals it to us (starting line 43 at
# https://github.com/topepo/caret/blob/master/pkg/caret/R/confusionMatrix.R):

# #' The overall accuracy rate is computed along with a 95 percent confidence
# #' interval for this rate (using \code{\link[stats]{binom.test}}) and a
# #' one-sided test to see if the accuracy is better than the "no information
# #' rate," which is taken to be the largest class percentage in the data.


# Binominaltest to check if the true probability of success is equal to bal_acc:
# Number of successes = 643; total number = 648
binom.test(643,648,bal_acc)

# Exact binomial test

# data:  643 and 648
# number of successes = 643, number of trials = 648, p-value = 0.1861
# alternative hypothesis: true probability of success is not equal to 0.9854865
# 95 percent confidence interval:
#  0.9820858 0.9974900
# sample estimates:
# probability of success 
#               0.992284 

###########################
# ----------------------- #
###########################
##########################################################################
# 9 Z- and T-Test for Simple Distributions and Power Analysis for T-Test #
##########################################################################
################
## Variance/SD #
################

# Example vector of numbers 0 to 10:
x = c(0:10)

# Pure deviation from the mean (without summing and squaring!), 
# i.e., centralization:
dev_of_mean_X = x-mean(x)
# [1] -5 -4 -3 -2 -1  0  1  2  3  4  5

# Plot of dev_of_mean:
plot(x = x, y = dev_of_mean_X) 
abline(h=0) # h = 0 adds horizontal line at y = 0

# Sum of de_of_mean_x:
sum(dev_of_mean_X)
# [1] 0

# Plot using absolute values via function abs():
plot(x = x, y = abs(dev_of_mean_X)) 
# Note: Only 0 deviation given when x_i = 5 = mean(x)

# Plot using the square, which smoothes the graph:
plot(x = x, y = dev_of_mean_X^2)


#####################################
## Simulation Central Limit Theorem #
#####################################

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
## Z-/T-Test, Shapiro(), density() #
####################################

# Simple synth. data sets:
x = c(0:10)
y = c(0:10)*3

# Density via density() function (check if distribution is normal):
plot(density(y))

# Shapiro Wilk Test 
# p-value GREATER .05 == normally distributed!!!!
shapiro.test(y)  # y is normally distributed!

# Welch test was automatically chosen (t-test for unequal variance)
t.test(x,y) 

#################################################################################
## Shiny App: Plotting Standardized Difference in Means for a One-Sample Z-Test #
#################################################################################

# Install and load shiny package
# install.packages("shiny")
library(shiny)

# Writing code for a Shiny app entails three components:
# The UI, the Server executing the functions etc., and then you have to run 
# both via the shinyApp(ui,server) function:


####### Code for the User Interface (UI):
ui = fluidPage(
  
  # App title banner: 
  titlePanel("Standardized Difference in Means (One-Sample)"), # End titelPanel()
  # Sidebar layout function, we choose sidebar for the input fields via:
  sidebarLayout(  
    sidebarPanel( 
      # Numeric input for samp/pop. mean and their respective sd!
      # numericInput("object input name for server function", "Label Name", standard value)
      numericInput("sampmean", "Sample Mean:", 
                   value = 120), 
      numericInput("sampsd", "Sample SD:", 
                   value = 5),
      numericInput("popmean", "Population Mean:", 
                   value = 130),
      numericInput("popsd", "Population SD:", 
                   value = 5)
    ), # End sidebarPanel()
    
    mainPanel( # Main space where the plot will be.
      # Output Object name for server will be "curves"
      plotOutput("curves")
    ) # End mainPanel()
    
  ) # End sidebarLayout()
) # End fluidPage()


############ Server entails all the functions and code that is executed in the background
server = function(input, output) {
  
  # Define how the iput is processed and what is plotted: 
  output$curves = renderPlot({ # THIS IS WHERE WE NEED the output name..
    
    # Prob. dens. function (could also use dnorm()):
    prob_dens = function(x,mean,sd){
      fx = (1/sqrt(2*pi*(sd^2)))*exp((-((x-mean)^2))/(2*(sd^2)))
      return(fx)
    } # End of Function
    
    # Effect size diff_mean/po_sd 
    # RECALL: INPUT NAMES WHERE SET VIA numericInput() in code for UI!
    effectsize = (input$sampmean-input$popmean)/input$popsd
    
    # Create sequence of numbers of x (sd's of the x-axis) which then run through
    # the prob_dens function above:
    seq_samp = seq((-abs(effectsize)-3),(abs(effectsize)+3),by =.01) 
    seq_pop  = seq(-5,5,by =.01)
    
    # Check which has the maximum value of prob_dens, to adjust the y-axis limit of the plot:
    yuplim_samp = max(prob_dens(seq_samp,effectsize,(input$sampsd)/input$popsd))
    yuplim_pop = max(prob_dens(seq_pop,0,1))
    yuplim = max(c(yuplim_samp,yuplim_pop))
    
    # Plot of standardized sample distribution:
    plot(x = seq_samp, y = prob_dens(seq_samp,effectsize,(input$sampsd)/input$popsd), 
         type = "l",  # lines instead of points
         lty = 2,     # dotted line
         ylab = "Density",
         xlab = "Difference in Means in Units of SD of the Population",
         ylim = c(0,yuplim)) # Uplim adjusted, so it is not cu off for some cases
    
    # Adds standard normal distribution to plot:
    lines(x = seq_pop,y = prob_dens(seq_pop,0,1), type = "l") # full line, not dotted
    
    # mean 0 for standard normal distribution (representing population):
    abline(v=0,col="lightblue") 
    
    # Effect size diff_mean/sd_pop:
    abline(v=effectsize,col="orange")
    
    # line for x-axis at y = 0:
    abline(h=0) 
    
    # Add Information on effect size to plot:
    # Formula:
    text(grconvertX(0.75, from = "npc", to = "user"), # Makes sure text will always be static!
         grconvertY(0.60, from = "npc", to = "user"),
         expression(frac(bar(x)-mu,sigma) == ""))
    # Result value:
    effectsize = signif(effectsize, digits = 3) # Round to 2 decimal places
    text(grconvertX(0.82, from = "npc", to = "user"), # Makes sure text will always be static!
         grconvertY(0.60, from = "npc", to = "user"),
         effectsize) # Adds value next to the above formula
    
  }) # End renderPlot()
} # End server

##### RUN THE APP (NOTE: A # WAS SET, since otherwise the RStudio RUN button disappears
##### and it then says "Run App" (which just executes the whole script). 
##### Some of you might want to use it, so I set the below as comment...

#shinyApp(ui = ui, server = server) 


# Consider the following cases:

# Pop_mean = 130 		  Sample_mean = 120 	 Pop und Samp_SD = 5
# Pop_mean = 130 		  Sample_mean = 120	   Pop und Samp_SD = 20
# Pop_mean = 129.8  	Sample_mean = 130	   Pop und Samp_SD = 0.1 
# Pop_mean = 130 		  Sample_mean = 120	   Pop_SD = 5	 	          Samp_SD = 20 (makes clear why CI of effect size is important)
# Pop_mean = 130 		  Sample_mean = 120	   Pop_SD = 20	 	        Samp_SD = 21

##############################################################################
## Simulation of how adjusting sample size can lead to synthetic significant #
## p-values and why Bonferroni / alpha-correction or effect size is needed   #
##############################################################################

# Sample sizes from 0 to 1000 in steps of 10:
n = seq(0,1000, by = 10)
# Initialice empty vector for the different t-/z-values, given different sizes of n
t_val = c()

# t_values for effect size 0.1 given different sizes of n:
# The below is just a repetitive calculation of t-/z-values, given different sample sizes!
effsiz = .1
for(i in 1:length(n)){
  t_val[i] = effsiz*sqrt(n[i]) 
} # End for i

# Plot result:
plot(x = n, y = t_val)

##########################
## Effect size Cohen's d #
##########################

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

# Comparison z-statistics/-value and effect size:
pop_sd = sum((y-mean(y))^2)/length(y)
sem_x = pop_sd/sqrt(length(x))
z_stat = (mean(x)-mean(y))/sem_x
effect_size = (mean(x)-mean(y))/pop_sd
# Reconstruct mean(x):
mean(y)-(.35*pop_sd)

# Same for a t-value with n = 1
sem_x_2 = pop_sd/sqrt(1)
z_stat_2 = (mean(x)-mean(y))/sem_x_2
effect_size_2 = (mean(x)-mean(y))/pop_sd

# Check for equality of z_stat2 and effect_size_2:
all.equal(z_stat_2,effect_size_2)


##########################
## Plot of a 3D Gaussian #
##########################
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
persp(x = x,y = y,z = z, theta =20,phi=20)

# From the top:
persp(x = x,y = y,z = z, theta = 0, phi = 90)


###################################################
## PDF of a T-distribution and stand. normal PDF, #
## comparing different degrees of freedom (df)    #
###################################################

# Create sequence to be normalized or turned into t-distribution: 
seq = seq(-4,4,by=.1)

# Plot standard normald PDF_
plot(seq,dnorm(seq, mean = 0, sd = 1), type = "l")

# Add t- distributions with different df:
lines(seq,dt(seq, df = 1), type = "l", col = "green")
lines(seq,dt(seq, df = 3), type = "l", col = "blue")
lines(seq,dt(seq, df = 10), type = "l", col = "red")
lines(seq,dt(seq, df = 30), type = "l", col = "lightblue")


#####################################################################
## Calculating n given a Certain Power and Conf. Interval/Sig.level #
#####################################################################

# install.packages("pwr")
library(pwr)

# Calculate needed sample size for one sample t-test, given d =.5
# a power of .8 and a sign.level of .5 == confidence int. of .95:
pwr.t.test(d = .5, sig.level = .05, power = .8, type = "one.sample") 
# n = 33.36713 for One-Sample
pwr.t.test(d = .5, sig.level = .01, power = .9, type = "one.sample") 
# n = 62.87021 for One-Sample with power .9 and sig.level .01
# Same for two sample:
pwr.t.test(d = .5, sig.level = .05, power = .8, type = "two.sample") 
# we usually assume .5 medium effect a priori
# n = 63.76561  for each group!
?pwr.t.test


#########################
## Power Curve Function #
#########################

# Function to plot a power curve, based on the code from Cinni Patel
# Code was adjusted for readability and uses regular plot() instead of ggplot2()
# https://cinnipatel.medium.com/power-curve-in-r-8a1e67fb2600   

power_curve <- function(sample_size){ # Start of function
  # Vector for the x-axis, representing possible effect sizes ranging from
  # .1 to 1.5 in steps of .1:
  effect_size = seq(from = 0,to = 2.5,by = .1)  
  samp_out = NULL # Empty object
  
  # The following loop is obtaining the power of each of the possible effect sizes
  # within the range of .1 to 1.5 and stores the outpur in a data.frame: 
  for(i in 1:length(effect_size)){
    # Obtaining the power for effect_size[i]
    power <-  power.t.test(d = effect_size[i],n = sample_size, 
                           sig.level = .05, type = "two.sample")$power
    # Storing it in a data.frame shape (only one row)
    power <-  data.frame(effect_size=effect_size[i],power=power)
    # Adds the above row to the previous obtained values recursively:
    samp_out <- rbind(samp_out,power)
  } # End for i
  
  # Define main title using cat() in order to include sample_size (input parameter)
  plot(x = samp_out$effect_size, y = samp_out$power, 
       ylab = "Power", xlab = "Different Effect Sizes", type = "l",
       main = paste0("Power Curve of a t-test with n = ",sample_size ))
  abline(h = .8, lty = 2) # lty = 2 for dotted lines; h-line at power of 80%
} # End of function power_curve  

# Test with sample size = 20, 5, 2000
power_curve(sample_size = 20)
power_curve(sample_size = 5)
power_curve(sample_size = 2000) # small effect size, already high power... 
                                # side effects of uncorrected p-values (issue alpha-correction)...

#################################
## Wikipedia Plot CI-Definition #
#################################

# To recreate the wikipedia plot, we will use only 100 samples
n_sim = 100
n_samp = 10

# Set up initial array:
conf_int_array = array(0, dim = c(n_sim,2))

for(i in 1:n_sim){
  sample = rnorm(n_samp, mean=10,sd=1) # 10 is the set mean of the population!
  t_test = t.test(sample)
  conf_int_array[i,] = c(t_test$conf.int[1],t_test$conf.int[2])
} # End for i

# Recreate plot:
plot(NULL, ylim=c(0,100), xlim = c(min(conf_int_array[,1]),max(conf_int_array[,2])), 
     ylab = "CI of the samples", xlab = "Pop. mean = 10")
abline(v=10, col = "red") # vertical line at 10, since 10 = mean of the population

# The function segments makes it possible to plot lines starting and ending
# at specific points P0(x0,y0) and P1(x1,y1):
for(i in 1:length(conf_int_array[,1])){
  segments(x0=conf_int_array[i,1],y0=i,x1=conf_int_array[i,2], y1=i , col = "blue")
} # End for i


#################################
# ----------------------------- #
#################################
#################################################################################
# 10 Lineare Regression via Lin. Least Square Method + via Descript. Parameters # 
#################################################################################
#######################
## Idealized Data Set #
#######################

# Dep. Var.:
cups = c(0:10)
# Indep. Var.:
time = c(0:10)

# Plot correlation of data points:
plot(x=time, y=cups)

# lm(y~x) => y (dep.) under the condition of x (dep):
lm(cups~time)

# Add linear model to plot above:
abline(lm(cups~time))

# Our goal for linear regression is understanding the below syntax of the summary(lm()) function:
summary(lm(cups~time)) # note that unreliable output, e.g., intercept signif, even though f(x)=0+x

# Call:
#   lm(formula = cups ~ time)

# Residuals:
#   Min         1Q     Median         3Q        Max 
# -3.211e-16 -2.215e-16 -1.218e-16  1.251e-16  6.001e-16 

# Coefficients:
#              Estimate Std. Error   t value Pr(>|t|)    
# (Intercept) 1.071e-15  1.879e-16 5.702e+00 0.000294 ***
# time        1.000e+00  3.176e-17 3.149e+16  < 2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 3.331e-16 on 9 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 9.916e+32 on 1 and 9 DF,  p-value: < 2.2e-16


#####################
## Errors/Residuals #
#####################

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
plot(x = dino$x, y = dino$y)
abline(lm(dino$y~dino$x))

####################################
## Non idealized Lady Meow Example #
####################################

# Cups of Tea (three days in the life of Lady Meow):
cups = c(0, 1, 2, 3.5, 4.8, 5.2, 6, 6.9, 8.5, 9.1, 9.9,
         0, .6, 2.6, 3.1, 4.8, 6.9, 7.1, 7.5, 8.5, 9.9, 9.9,
         0, .2, 2.9, 2.9, 4.9, 5.2, 6.9, 7.1, 7.3, 9, 9.8)
   
# Time passed for each run of measurements (cups), abbreviated:
time = c(0:10,0:10,0:10) # technically 3 days in the life of Lady Meow

# You can add labels to the plot!
plot(x=time, y=cups,
     ylab = "Cups of Tea", xlab= "Hours passed")
# Play around with possible values of a and b (re-run plot() to reset):
abline(a = 0, b = 1)
abline(a = .5, b = 1.2)
abline(a = .23, b = 1.0082, col = "darkgreen")
abline(a = 0 , b = 1, col = "blue")

# Reset plot!
plot(x=time, y=cups,
     ylab = "Cups of Tea", xlab= "Hours passed")

# Add the optimal linear model to the plot via:
abline(lm(cups~time))
# Slope and crossing point with the y-axis is:
lm(cups~time)

# T-Test Reg. Coeff + Intercept:
summary(lm(cups~time))

# The summary function works in various ways depending on the input:
summary(c(0:10))

#######################################
## Lin. Reg. via Descript. Parameters #
#######################################

# We use x and y since it is easier to read the formulas below:
x = time
y = cups

##### Calculating the Regression Coefficient via Covariance and Variance:
# SumSqXY = ∑(x_i - E[X])*(y_i - E[y])
SumSqXY = sum((x-mean(x))*(y-mean(y)))
# [1] 332.7 Sum of squares allows no interpretation without counterweight/division by n

# Covariance (Pop. and Samp.):
covarXYsamp = SumSqXY / (length(x)-1) # sample covariance
# [1] 10.39687
covarXYpop  = SumSqXY / length(x)     # population covariance
# [1] 10.08182
covarXYsamp = cov(x,y)                # samp cov := cov(x,y)
# [1] 10.39687

#### Calculating the intercept:
intercept = mean(y)-(cov(x,y)/var(x))*mean(x)
# [1] 0.2318182

















