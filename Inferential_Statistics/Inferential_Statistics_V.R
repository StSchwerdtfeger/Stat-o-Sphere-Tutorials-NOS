#############################################################
##---------------------------------------------------------##
##                Inferential Statistics V:                ##
##    Probabilistically Evaluating Statistical Models      ##
##         via the Z-Test / Gauß-Test and the T-Test       ## 
##            by Steffen Schwerdtfeger 01.2023             ##
##---------------------------------------------------------##
#############################################################



#######################################################
# 2 The Regular Z-Test / Gauß-Test  and its P-Value # 
#######################################################

# General formula for a probability density function (PDF).
# Insert mean = 0 and var = 1 to obtain the values for input x
# given a so-called standard normal distribution:
prob_dens = function(x,mean,var){
  fx = (1/sqrt(2*pi*var))*exp((-((x-mean)^2))/(2*var))
  return(fx)
} # End of Function

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

# Comparison z-stat and effect size:
x=c(0:10)
y=c(0:10)*3
pop_mean = mean(y)
pop_sd = sum((y-mean(y))^2)/length(y)
sem_x = pop_sd/sqrt(length(x))
z_stat = (mean(x)-mean(y))/sem_x
effect_size = (mean(x)-mean(y))/pop_sd

# Same with n = 1
sem_x_2 = pop_sd/sqrt(1)
z_stat_2 = (mean(x)-mean(y))/sem_x_2
effect_size_2 = (mean(x)-mean(y))/pop_sd

# Check for equality of z_stat2 and effect_size_2:
all.equal(z_stat_2,effect_size_2)


##############################################
# 2.2 Standard Deviation/Error of the Mean #
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

n_sample = length(x) # = 1100
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


###########################################################
# 3 Possible Hypotheses of the One- and Two-Sample Z-Test #
#               and their Respective P-Values             #
###########################################################

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


##########################
# 4 The Student's T-Test #
##########################

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


######################################################
# 5.1 Hypothesis Test on the Slope and the Intercept #
#                of a Linear Model                   #
######################################################

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
# 6 Update on our linear_least_squares() Function #
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
  t_value_b = beta/(se_slope_t+exp(-32)) 
  
  # t-value of the intercept:
  t_value_a = alpha/(se_a+exp(-32))
  
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

