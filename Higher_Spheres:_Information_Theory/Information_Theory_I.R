#####################################################
#---------------------------------------------------#
#       Higher Spheres: Information Theory I:       #
#    Introduction into the World of Information     #
#  Processing and its Technological Representation  #  
#            by Steffen Schwerdtfeger               #
#---------------------------------------------------#
#####################################################

################################################################
# 4.1.2 The Formula of Bayes’ rule and Conditional Probability #
################################################################

# Vanessa Holmes Example:
# First iteration:
joint = c(.25,.25,.25,25)*c(.33,.33,.33,0)
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
model_evidence = sum(c(.5,.5,.33,0)*c(0,0,0,0))
# [1] 0 
posterior = joint/model_evidence # Note that it says: 0 divided by 0!
posterior

# [1] NaN NaN NaN NaN 
# NaN = Not a number.
# This is due to the fact that 0/0 is not defined. At this point
# Vanessa Holmes would need to find another suspect to be able 
# to do further inference...


# First simple version of our Bayes_Machine() function:
Bayes_Machine = function (prior,likelihood) {
  joint = prior*likelihood
  # na.rm = TRUE in sum() deletes 0 rows if given
  modelevidence = sum(joint, na.rm = TRUE) 
  posterior = joint/modelevidence
  # Needed for console output and adds text to it
  # using a matrix, which works similar as c()
  postprint = matrix(c("Posterior",posterior,
                       "Joint probability", joint,
                       "Model evidence", modelevidence))
  print(postprint) 
}  # end of function

# Give it a try with defined prior and likelihood:
prior = c(.5,.5)
likelihood = c(.5,.5)
Bayes_Machine(prior,likelihood)

# Try these inputs and contemplate the results:
prior = c(.1,.9)
likelihood = c(.9,.1)
Bayes_Machine(prior,likelihood)


# Here is the extended function that can also handle 
# the model evidence as input:

Bayes_Machine = function (prior,likelihood,modelevidence) {
  if (missing(modelevidence)){
    joint = prior*likelihood
    # na.rm = TRUE in sum() deletes 0 rows if given
    modelevidence = sum(joint, na.rm = TRUE) 
    posterior = joint/modelevidence
    # Needed for console output
    postprint = matrix(c("Posterior",posterior,
                         "Joint probability", joint,
                         "Model evidence", modelevidence)) 
    print(postprint)
  }
  else {
    joint = prior*likelihood
    posterior = joint/modelevidence  
    postprint = matrix(c("Posterior",posterior,
                         "Joint probability", joint,
                         "Model evidence", modelevidence)) 
    print(postprint)
  }
} # End of function

Bayes_Machine(likelihood = likelihood, prior = prior)


#######################################################################
# 4.2 Communication as Bayes’ Inference - the Relation between Sender #
#             and Receiver as a Probabilistic Relation                #
#######################################################################

# Example for sending one binary digit via a noiseless (!) channel:
prior = c(.5,.5)
likelihood = c(1,0)

Bayes_Machine(prior = prior, likelihood = likelihood)
# Posterior = [1,0]


########################################################
# 4.2.1 Noisy Channels (incl. Binominal Distributions) #
########################################################

## David McKay Exercise - Number of flipped bits #

# Use dbinom() for probability distributions
flippedbits = dbinom(x = 0:10000, size = 10000, prob = .1)

# delete [1500]-[10000] for better plot overview 
plotbits = flippedbits[-c(1500:10000)] 
# Plot plotbits
plot(plotbits, typ = "l",
     xlab = "Number of flipped bits", 
     ylab = "Density")


# Number of trials
n = 10000
# P(r|not-s) = f
p = 0.1
# P(r|s) = 1-f
q = 1-p

# Mean
mu = n * p
# [1] 1000

# Variance
sigmaSQ = n * p * q
# [1] 900

# Standard Deviation
sigma = sqrt(sigmaSQ) 
# [1] 30

# Use abline() to add a line to an existing plot.
# "v =": x value(s) of vertical line(s) to be added
# Add mean
abline(v = mu, col = "red") 
# Add standard deviation to plot
abline(v = mu - sigma, col = "blue") # minus sd
abline(v = mu + sigma, col = "blue") # plus sd


