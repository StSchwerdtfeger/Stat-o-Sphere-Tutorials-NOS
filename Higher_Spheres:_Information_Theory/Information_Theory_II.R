#####################################################
#---------------------------------------------------#
#      Higher Spheres: Information Theory II:       #
#     The Relation Between Information Theory /     #
#    Technology and Statistical Thermodynamics      #
#           by Steffen Schwerdtfeger                #
#---------------------------------------------------#
#####################################################



#####################
# 1.3 The Logarithm #
#####################

nat_log_nonzero = function(x) {
x = log(x+exp(-16))
}


# Some remarks on the use of Euler's number:
# Generate sequence for plot:
seq = seq(-4,4, by =.1)
plot(x = seq, y = exp(seq), type = "l")
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

# We can add the respective tangent at exp(y)
# point with a slope = e^x. In case you wonder:
# the linear function x does not need to be defined in curve().
curve(exp(1)*x,-3,3, add = TRUE)

# Integral from -Inf to x = 1
expo_fun = function(x) {exp(x)}
integrate(expo_fun, lower = -Inf, upper = 1)
# 2.718282 with absolute error < 0.00015
# = e again.


#############################################################
# 1.4 Boltzmann’s Entropy Formula and Gibbs’ Generalization #
#############################################################

# Install a package with the install.packages() function. 
# Keep in mind to use " " within ().
install.packages("constants")

# Now load the library you just installed, otherwise
# the functions of the package will not be known by R
library("constants")

# Here you can check out which constants are available.
# The package uses the constants estimated by CODATA, i.e.,     
# the Committee on Data for Science and Technology in Paris
# (CODATA), founded by the International Council for Science. 
# CODATA keeps track of measurements of constants and delivers 
# standardized estimations of constants for science (quality 
# control and improving quality of estimations in general).
View(codata) # View full list of constants

# Use lookup() to search for constants
lookup("boltzmann", ignore.case = TRUE)

# Use with() to use a constant, in our case:
with(syms, syms$k) # can be used as a whole to do math!

# Boltzmann Entropy (syms_with_units adds unit to result)
# For R Version 4.1.0 use k instead of syms$k but both may work:
with(syms_with_units,syms$k)*log(1) # with Omega = 1 possibility
with(syms_with_units,syms$k)*log(2) # with 2 possibilities
with(syms_with_units,syms$k)*log(4) # with 4
with(syms_with_units,syms$k)*log(8) # with 8

# RESULTS:
# > with(syms_with_units,syms$k)*log(1) # with 1 possible states
# [1] 0
# > with(syms_with_units,syms$k)*log(2) # with 2 
# [1] 9.56993e-24
# > with(syms_with_units,syms$k)*log(4) # with 4
# [1] 1.913986e-23
# > with(syms_with_units,syms$k)*log(8) # with 8
# [1] 2.870979e-23

# NOTE: plain log() := natural logarithm := ln.


# Gibbs' generalization of the entropy formula:
# We will try different n with equal prob. of microstates 
# (as in Boltzmann’s equation)
OMEGA1 = c(1) 		# one possible state
OMEGA2 = c(.5,.5)	# two states with equal prob.
OMEGA4 = c(.25,.25,.25,.25)
OMEGA8 = c(.125,.125,.125,.125,.125,.125,.125,.125)
-with(syms_with_units,syms$k)*(sum(OMEGA1*log(OMEGA1)))
-with(syms_with_units,syms$k)*(sum(OMEGA2*log(OMEGA2)))
-with(syms_with_units,syms$k)*(sum(OMEGA4*log(OMEGA4)))
-with(syms_with_units,syms$k)*(sum(OMEGA8*log(OMEGA8))) 

# RESULTS (EQUIVALENT TO THE ABOVE USE OF BOLTZMANN ENTROPY)
# > -with(syms_with_units,syms$k)*(sum(OMEGA1*log(OMEGA1)))
# [1] 0
# > -with(syms_with_units,syms$k)*(sum(OMEGA2*log(OMEGA2)))
# [1] 9.56993e-24
# > -with(syms_with_units,syms$k)*(sum(OMEGA4*log(OMEGA4)))
# [1] 1.913986e-23
# > -with(syms_with_units,syms$k)*(sum(OMEGA8*log(OMEGA8))) 
# [1] 2.870979e-23


# We see that this leads to equivalent results. Now mark 
# and execute only the following part of the code, e.g., 
# OMEGA4*log(OMEGA4), and see how summation is performed, 
# then try just, e.g.,
-log(.5);-log(.25) # etc.
# note that “;” is a delimiter that in this case
# is interpreted by R as a new independent line


################################################
# 1.4.1 The expected Value Notation of Entropy #
################################################

# E[x] = Sum(x*p(x)), where x := dice value
ExpX = 1*1/6 + 2*1/6 + 3*1/6 + 4*1/6 + 5*1/6 + 6*1/6
ExpX   # 3.5 or 7/2
# Or:
x = c(1,2,3,4,5,6)
ExpX = sum(x*(1/6))
# Shortest:
ExpX = sum(1:6*(1/6))

# Note: the function nrow() delivers the number of
# rows/elements in a matrix (counting trick). The parameter 
# "byrow =" adds the values by row, if FALSE, one would end up 
# with 6 columns; default = FALSE; in this case it actually  
# doesn’t matter, as there is only 1 column. 
# Our fair six-sided dice:
dice = matrix(c(1,2,3,4,5,6), byrow = TRUE)
# probs corresponding to the matrix "dice"
weight_dice = c(1/6,1/6,1/6,1/6,1/6,1/6) 

# Arithmetic mean (equal to using equivalent probs./weights)
arith_mean = sum(dice)/nrow(dice)

# The formula of the weighted arithmetic mean is:
weight_mean = sum(weight_dice*dice)/sum(weight_dice)
weight_mean = sum(weight_dice*dice)

# Or use R function from the base package:
weight_mean = weighted.mean(dice, weight_dice)

# Normalizing weights:
normalized_weights = weight_dice/sum(weight_dice)
# [1] 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 
#     0.1666667 #   where 1/6 = 0.1666667

# Weights of a phony dice
phony_dice = matrix(c(1,2,3,4,5,6), byrow = TRUE)
# probs. corresponding to "phony dice"
weights_phony_dice = c(2/6,.5/6,.5/6,1.5/6,.5/6,1/6) 
# Check if probs sum up to 1
sum(weights_phony_dice)

# Normalized weights (again equivalent to our weights)
norm_phony_weights = weights_phony_dice/sum(weights_phony_dice)
# Check on sum again
sum(norm_phony_weights)

# First we check on our weighted mean with our non-normalized 
# weights. Note that the w-mean is actually our expected value:
w_mean_phony_dice = sum(weights_phony_dice*phony_dice) / 
  sum(weights_phony_dice)

# Now we check if we get the same expected value using our
# normalized weights:
n_w_mean_phony_dice = sum(norm_phony_weights*phony_dice) / 
  sum(norm_phony_weights)


#####################
# 2 Shannon Entropy #
#####################

# Surprisal /self-information (information content): 
# I(x) = -log2(p(x)) Bits
# Probability of a sign x = p(x)

# In order to plot a very smooth function, we are using 
# probabilities starting with (x) = 0.0001 and moving towards
# p(x) = 1 in respective steps. This can be seen as forming 
# a sequence. We will start with a probability of
# 0.0001, as -log2(0) = Infinite, i.e., not defined:

# We will use the sequence function seq()
px<- seq(0.0001, 1, by = 0.0001)

# Number of single probabilities:  
length(px) # [1] 10000

# Let us now plot the -log2 for all p(x), i.e., x = 0 to 1
plot(x = px, y = -log2(px), typ = "l", col = "red", 
     ylab = "Amount of Bits = I(x) = -log2(p(x))", xlab = "p(x)",
     panel.first = grid(nx = NULL, ny = NULL, 
                        col = "lightgray", lty = "dotted")) 

# You can also add some points, e.g., for p(x)=.5
# The calculation can be done within the function points():
points(.5,-log2(.5))
points(1,-log2(1))


# Here we will represent two relay switches or two sent signs for 
# a change, i.e., four possible states with equal probability, 
# equivalent to the NAND truth table. 
prior = c(.25,.25,.25,.25)
likelihood = c(1, 0, 0, 0)   
Bayes_Machine(prior,likelihood)

# Calculate surprisal:
Surprisal = -log2(sum(prior*likelihood))
# [1] 2

# Calculate and plot the maximum entropy for 1 sign 
px = seq(.1,1, by = .1)
qx = 1-px

# The entropy is at its maximum for px = .5
MaxEntropy = -(px*log2(px)+qx*log2(qx))

# > MaxEntropy # it reaches maximum of 1.0000000 at px =.5!!
# [1] 0.4689956 0.7219281 0.8812909 0.9709506 1.0000000 0.9709506 0.8812909 0.7219281
# [9] 0.4689956       NaN

# Plot maximum entropy (with new px)
px = seq(0.0001, 1, by = 0.0001)
plot(x = px, y = -(px*log2(px)+((1-px)*log2(1-px))), type = "l")







