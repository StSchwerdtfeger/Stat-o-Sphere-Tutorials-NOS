#############################################################
##---------------------------------------------------------##
##                Inferential Statistics I -               ##
##             Hypothesis testing in the basic             ##
##      form of conditional probability / Bayes´ rule      ##
##                R - Stat-o-Sphere Script                 ##
##            by Steffen Schwerdtfeger 07.2022             ##
##---------------------------------------------------------##
#############################################################

############################################################
## 3 Computing conditional probability / Bayes' rule in R ##
############################################################

# This is a test, which will also be the name of the ‘object’
test = 2 + 5 # Execute this line!

###### Bayes' rule via single values:

# Define you prior, e.g., .5 for heads.
# Note that R is a case sensitive language (“prior” not same as “Prior”).
prior = .5

# Likelihood
likelihood = .5

# Joint probability:
joint = prior*likelihood

# Model evidence (note that R does not allow spacing within names!):
model_evidence = .25 + .25

# Posterior:
posterior = joint/model_evidence


###### Bayes' rule via probability vectors:

# IMPORTANT NOTE: When executing the lines below
#                 all previous values assigned to,
#                 e.g., the name "prior" will be
#                 overwritten. 

# Define prior
prior = c(.5, .5)

# Likelihood
likelihood = c(.5, .5)

# Joint probability
joint = likelihood*prior

# Model evidence
model_evidence = sum(joint)

# Posterior
posterior = joint / model_evidence


############################################################################
# 4 The Bayesian and the frequentist appraoch to (conditional) probability #
############################################################################

###### Vanessa Holmes example:

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
model_evidence = sum(c(.5,.5,0,0)*c(0,0,0,0))
# [1] 0 
posterior = joint/model_evidence # Note that it says: 0 divided by 0!
posterior

# [1] NaN NaN NaN NaN 
# NaN = Not a number.
# This is due to the fact that 0/0 is not defined. At this point
# Vanessa Holmes would need to find another suspect to be able 
# to do further inference...


