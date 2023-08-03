#####################################################
#---------------------------------------------------#
#       Higher Spheres: Information Theory I:       #
#    Using Markov Chains to Describe Language as    #
#             a Stochastic Process                  #
#           by Steffen Schwerdtfeger                #
#---------------------------------------------------#
#####################################################


###############################################################################
# 1 Markov Chains Describing the Process of Generating and Receiving Messages #
###############################################################################

# Install and load library:
# install.packages("markovchain") # uncomment this row and execute to install
library(markovchain)

# Now we set the possible events (message) and our transition matrix
MessageABCDE = c("A", "B", "C", "D", "E")

MessageTransitionMatrix = matrix(c(.2,.2,.2,.2,.2,
                                   .2,.2,.2,.2,.2,
                                   .2,.2,.2,.2,.2,
                                   .2,.2,.2,.2,.2,
                                   .2,.2,.2,.2,.2),
                                 nrow = 5,
                                 byrow = TRUE,
                                 dimname = list(MessageABCDE, MessageABCDE))

# This will define our actual Markov chain as a relation of 
# states/events to their respective transition probabilities
MCmessage = new("markovchain", states = MessageABCDE,
                byrow = TRUE,
                transitionMatrix = MessageTransitionMatrix,
                name = "WritingMessage")
# With this function you can evaluate a class of an object
class(MCmessage)

# Now let us “walk” on the chain, sample:
markovchainSequence(n = 20, markovchain = MCmessage, t0 = "A")

# My output looked like this (compare with Shannon’s results):
# > markovchainSequence(n = 20, markovchain = MCmessage, t0 = "A")
# [1] "B" "D" "C" "C" "B" "A" "D" "B" "C" "E" "A" "E" "B" "D" "C" "B" 
# [16] "D" "B" "E" "C"

# Plot Markov Chain (very interesting result)
plot(MCmessage, edge.arrow.size = 0.2) 


##### Second chain example:
MessageABCDE = c("A", "B", "C", "D", "E")
MessageTransitionMatrix = matrix(c(.4,.1,.2,.2,.1,
                                   .4,.1,.2,.2,.1,
                                   .4,.1,.2,.2,.1,
                                   .4,.1,.2,.2,.1,
                                   .4,.1,.2,.2,.1),
                                 nrow = 5,
                                 byrow = TRUE,
                                 dimname = list(MessageABCDE, MessageABCDE))

MCmessage = new("markovchain", states = MessageABCDE,
                byrow = TRUE,
                transitionMatrix = MessageTransitionMatrix,
                name = "WritingMessage")
markovchainSequence(n = 20, markovchain = MCmessage, t0 = "A")
# Output Sequence:
# > markovchainSequence(n = 20, markovchain = MCmessage, t0 = "A")
# [1] "A" "D" "C" "A" "C" "C" "C" "D" "D" "A" "A" "B" "D" "A" "A" "E"    
# [17] "C" "A" "E" "A"

# In the case of a MC without Markov property altern. code possible
tokens <- c("A", "B", "C", "D", "E")
probs  <- c(0.4, 0.1, 0.2, 0.2, 0.1)

sample(tokens, size = 20, replace = TRUE, prob = probs)
## [1] "A" "B" "A" "B" "D" "B" "C" "D" "A" "D" "C" "E" "A" "A" "C" "E" "C" "D" "C" "C"


####### Third chain example:
MessageABC = c("A", "B", "C")
MessageABCTransMatrix = matrix(c(.0,.8,.2,
                                 .5,.5,.0,
                                 .5,.4,.1),
                               nrow = 3,
                               byrow = TRUE,
                               dimname = list(MessageABC, MessageABC))

MCmessageABC = new("markovchain", states = MessageABC,
                   byrow = TRUE,
                   transitionMatrix = MessageABCTransMatrix,
                   name = "WritingMessage") 
markovchainSequence(n = 20, markovchain = MCmessageABC, t0 = "A")

# Plot Markov Chain
plot(MCmessage, edge.arrow.size = 0.1) 

################################################
# Steady state distribution - Heuristic method #
################################################

# Calculating steady state distribution:

# "Heuristic" method via the markovchain 
# The respective matrix that entails the 
# eigenvector can be heuristically acquired
# via the markovchain package, simply by 
# “squaring” the chain in R.
MCmessageABC^1  # Initial transition matrix, i.e., phase 1
MCmessageABC^2  # Transition matrix 2. phase
MCmessageABC^10 # approaching steady state 
MCmessageABC^28 # steady state probability (=row (eigen)vector)
# Execute this line of code to directly get the eigenvector
steadyStates(MCmessageABC)



#################################################################################
# 1.1 Basics in Linear Algebra — Method I to Obtain the Stationary Distribution #
#################################################################################

# Set up a function that prints an empty coordinate system
# Note: no input for this function necessary!
grid2D = function() {
  # Let us first plot an empty plot, where x and y
  # range from -5 to 5. For this we will adjust the plot a little:
  
  # These vectors represent a list of tick mark values, see below
  posX = c(-25:25) # “:” states from x to y, abbreviates process
  posY = c(-25:25)
  
  # Plot empty plot (asp = 1 for steady ratio)
  plot(NA, xlim=c(-5,5), ylim=c(-5,5), xlab="X", ylab="Y", asp = 1, 
       axes = TRUE) # axes removes pre-set axes
  abline(h = 0,lty = 1) # adds horizontal line at ylim = 0 
  abline(v = 0,lty = 1) # adds vertical line at xlim = 0
  abline(h = posY, lty=3) # adds grid both h and v
  abline(v = posX, lty=3)
  
} # End of function


# Execute to get blank coordinate system the easy way.
# This is necessary as one cannot delete arrows from a plot,
# one has to re-run all lines of code in such cases
grid2D() 

# Add arrow / vector in terms of (geometric) linear algebra. 
# x0/y0 = origin, x1/y1 = point to draw arrow to
v = arrows(x0 = 0, y0 = 0,x1 = 2, y1 = 2, length = 0.1, lwd = 2)

# Add text. T for transpose, i.e., indicating that the vector was 
# flipped to the side and still represents a column vector, 
# not a row vector.
text(x=2.5,y=1, label="v = [2, 2]T", srt = 5) 


#### Adding vectors together:
grid2D() # get fresh grid
# Calculation of v1+v2 = v3
v1 = c(2, 2); v2 =  c(2, -2); v3 = v1+v2

# Plot result
v1 = arrows(x0 = 0, y0 = 0,x1 = 2, y1 = 2, length = 0.1, lwd = 2)
v2 = arrows(x0 = 0, y0 = 0,x1 = 2, y1 = -2, length = 0.1, lwd = 2)
v3 = arrows(x0 = 0, y0 = 0,x1 = 4, y1 = 0, length = 0.1, lwd = 2)
text(x=1,y=2, label="v[1]", srt = 5) 
text(x=1,y=-2, label="v[2]", srt = 5) 
text(x=3,y=.5, label="v[1]+v[2]=v[3]", srt = 5)

# Execute the following lines; adds to the previous plot
v2add = arrows(x0 = 2, y0 = 2,x1 = 2+2, y1 = 2+(-2), length = 0.1, lwd = 2,
               col = "lightblue")
text(x=3.5,y=1.5, label="v[2]", srt = 5, col = "lightblue")


#### Scalar multiplication
v4 = arrows(x0 = 0, y0 = 0,x1 = 2*2, y1 = 2*(-2), length = 0.1,   
            lwd = 2, col = "lightgreen")
text(x=3,y=-4, label="v[4]", srt = 5, col = "lightgreen")      


# Function to plot vectors as arrows(x), including optional color option (y).
plotvec = function(x,y){
  if (missing(y)){
    vector = arrows(x0 = 0, y0 = 0,x1 = x[[1]], y1 = x[[2]], 
                    length = 0.1, lwd = 2)
  }
  else{
    vector = arrows(x0 = 0, y0 = 0,x1 = x[[1]], y1 = x[[2]], 
                    length = 0.1, lwd = 2, col = y)  
  }
} # End of function

# Vector for each quadrant of the coordinate system
v1 = c(2,2);v2 = c(2,-2);v3 = c(-2,-2); v4 = c(-2,2)
grid2D()
plotvec(v1);plotvec(v2);plotvec(v3);plotvec(v4)

#### Base vectors
ibase = c(1,0); jbase = c(0,1)
plotvec(ibase); plotvec(jbase)


#### Identity matrix
matrixI = matrix(c(1,0,
                   0,1), ncol = 2)
# Alternative shortcut function for the identity matrix: 
matrixI = diag(2) # where 2 refers to a 2x2 (square) matrix dimension

# v1 = c(2,2)
vTrans = v1%*%matrixI


##### Changed base vectors
ibase = c(2,0); jbase = c(0,2)
# Here is a function doing the transformation for us.
# The function adds the base vectors together to a matrix.  
vectrans = function(vector,ibase,jbase){
  matrixTrans = matrix(c(ibase,jbase),ncol=2)
  vecOUT=vector%*%matrixTrans
  print(vecOUT) 
} #End of function

v1trans = vectrans(v1,ibase,jbase)
v2trans = vectrans(v2,ibase,jbase)
v3trans = vectrans(v3,ibase,jbase)
v4trans = vectrans(v4,ibase,jbase)
# Add to plot:
plotvec(v1trans);plotvec(v2trans);plotvec(v3trans);plotvec(v4trans)


#### "Pseudo-Rotating" the vector field
grid2D()
# original ibase/jbase
ibase = c(1,0); jbase = c(0,1)
plotvec(ibase," lightblue");plotvec(jbase," lightblue")
v1trans = vectrans(v1,ibase,jbase)
v2trans = vectrans(v2,ibase,jbase)
v3trans = vectrans(v3,ibase,jbase)
v4trans = vectrans(v4,ibase,jbase)
plotvec(v1trans,"orange");plotvec(v2trans,"orange");
plotvec(v3trans,"orange");plotvec(v4trans,"orange")

# "Pseudo-Rotating" both base vectors 45° 
ibase = c(1,-1); jbase = c(1,1)
plotvec(ibase,"blue");plotvec(jbase,"blue")
v1trans = vectrans(v1,ibase,jbase)
v2trans = vectrans(v2,ibase,jbase)
v3trans = vectrans(v3,ibase,jbase)
v4trans = vectrans(v4,ibase,jbase)
plotvec(v1trans,"green");plotvec(v2trans,"green");
plotvec(v3trans,"green");plotvec(v4trans,"green")


#### Matrix matrix multiplication
# Matrices:
matrix_A = matrix(c(1,2,3,4,5,6), 
                  ncol=3, nrow = 2, byrow = TRUE)
matrix_B = matrix(c(1,2,3,4,5,6), 
                  ncol=2, nrow = 3, byrow = FALSE)
# Use %*% for matrix multiplication instead of *
matrix_A%*%matrix_B


##### System of linear equations:
# Example with Mr. Midnight and Jurassica Parker
# Set up two elements, so that we can solve an
# equation left = right, where a is the left and b is the 
# right-hand side of our system of equations
left = matrix(c(1,1,
                -1,1), ncol = 2, byrow = TRUE)
right = matrix(c(15,7), ncol = 1, byrow = TRUE)

# Use the solve function to solve the system of
# equations. It outputs a vector list = c(x,y)
ResultXY = solve(left,right)



###################################################
# 1.1.1 Calculating the Steady State Distribution #
###################################################


# Quick check if vA=lv is true:
vA = steadyStates(MCmessageABC)%*%MessageABCTransMatrix
lv = 1*steadyStates(MCmessageABC)
# Test for near equality (floating point comparison)
all.equal(vA,lv) 


##### Re-arranging matrix, to solve it:
# We will use a simplified form of our transition matrix
# without renamed columns and rows c("A", "B", "C")
MessageABCTransMatrix = matrix(c(.0,.8,.2,
                                 .5,.5,.0,
                                 .5,.4,.1),
                               nrow = 3,
                               byrow = TRUE)

# Here we will add the fact that the rows of the matrix sum up to 1,
# which in the case of our transposed matrix is done
# by adding another row (!) via rbind() which combines rows.
# Also note that the function t() := transpose, and diag(x) = I
A = rbind(t(MessageABCTransMatrix-diag(3)), c(1,1,1))

# The transposed rows, i.e., columns will not actually 
# sum to 1, as we have subtracted I from A,
# resulting in a sum of zero on the columns
# and a sum of 1 on the rows in the below matrix
# > A
#      [,1] [,2] [,3]
# [1,] -1.0  0.5  0.5
# [2,]  0.8 -0.5  0.4
# [3,]  0.2  0.0 -0.9
# [4,]  1.0  1.0  1.0

# We will also represent our b, which, recall b = 0
# As our vector has to have the same number of
# rows as the matrix, we will also add a 1, which, again,
# just represents our extra information of sum(row)=1 ─
# even though the transposed row, i.e., column of A
# sums to zero, due to the subtracted identity 
b = c(0,0,0,1)

# Our whole system of equations will
# look like the following, using the
# cbind() function (combines by columns)
cbind(A,b)

# > cbind(A,b) 
#                     b
# [1,] -1.0  0.5  0.5 0
# [2,]  0.8 -0.5  0.4 0
# [3,]  0.2  0.0 -0.9 0
# [4,]  1.0  1.0  1.0 1

# Use QR-algorithm to solve the system of equations
qr.solve(A,b)
# > qr.solve(A,b)
# [1] 0.33333333 0.59259259 0.07407407

# Let us see if our steadyStates(x) and qr.solve(a,b) is equal
all.equal(as.vector(steadyStates(MCmessageABC)),qr.solve(A,b))
# [1] TRUE




#### Relative probabilities (joint probability matrix) function:
relProbs = function(Steady,TransMatrix){
  # This will set up an empty matrix list:
  relProbsMatrix = vector("list",   
                          nrow(TransMatrix)*ncol(TransMatrix))
  dim(relProbsMatrix) = matrix(c(nrow(TransMatrix), 
                                 ncol(TransMatrix)))
  # Nested for-loop (loop within a loop)
  # 1:length means: sequence of 1 to max length (=nr of elements):
  for (i in 1:length(Steady)){ 
    for (j in 1:ncol(TransMatrix)){
      relProbsMatrix[[i,j]] = Steady[[i]]*TransMatrix[[i,j]]
    }
  }
  # Set output as matrix (otherwise problems occur for colSums())
  # as.numeric(), as.matrix() etc. changes the class of an object:
  relProbsMatrix = as.numeric(relProbsMatrix) 
  # Output is a vector. dim() adjusts dimension back to a matrix
  # with the square root "sqrt()" of the length as nr. of c(rows,cols)
  # (can be done this way due to the fact that a square matrix is given)
  dim(relProbsMatrix) = matrix(c(sqrt(length(relProbsMatrix)),
                                 sqrt(length(relProbsMatrix))))
  print(relProbsMatrix)
} # End of function

# We can now use both qr.solve(A,b), or steadyStates(MCmessageABC) 
# for the first input “Steady”:
jointMatrix = relProbs(qr.solve(A,b),MessageABCTransMatrix)



############################################################
# 1.2 Summary and Shannon’s (Bayesian) formalism in AMTC - #
#    Method II to Obtain the Stationary Distribution       #
############################################################


# Row and column sums for p(i) := model evidence and p(j) := prior.
# They are equivalent to our steady state (can be seen as its 
# definition in the language of probabilities, as it entails the 
# notion of steadiness, i.e., “not changing” or “no difference  
# between past, present or future”):
pi = rowSums(jointMatrix)
pj = colSums(jointMatrix)

# Corresponding to the formula:
pi = rowSums(pi*MessageABCTransMatrix)

# Looking back at initial heuristic approach:
MCmessageABC^28
# Result:
#           A       B       C
# A 0.3333333 0.5925926 0.07407407
# B 0.3333333 0.5925926 0.07407407
# C 0.3333333 0.5925926 0.07407407

# Using linear algebra again, now with the tranisiton matrix of 
# phase 5. I didn’t find a way to change the class of the 
# output of: 
MCmessageABC^5

# ... so I had to create the matrix myself with the respective 
# values, taken from the console output (a little meassy but still worked!):
phase5 = matrix(c(0.31250, 0.60648, 0.08102,
                  0.34375, 0.58565, 0.07060,
                  0.34375, 0.58564, 0.07061), ncol = 3, byrow= TRUE)

A = rbind(t(phase5-diag(3)), c(1,1,1))
b = c(0,0,0,1)
cbind(A,b)
qr.solve(A,b)

# Let us check if our assumption vA=v holds for phase 1 and the 
# exemplary phase 5, using a shortcut. The shortcut relies on a 
# shortcut to build a joint probability matrix by just multiplying
# TransMatrix*eigenvector, which does the same as relProbs(), 
# though previous output classes requested some additional coding 

# Set eigenvector, here I retrieved it from the previous 
# jointMatrix: 
eigenvector = rowSums(jointMatrix)
# Check if v=vA holds for different phases:
INITphase1 = rowSums(MessageABCTransMatrix*eigenvector)
AFTERphase5 = rowSums(phase5*eigenvector)
# Note that we could also implement diag(length(eigenvector)) from 
# the actual formulas above, but it does not make a difference.


##################################
# 2 Different Forms of “Entropy” #
##################################

# AVERAGE JOINT ENTROPY H/x,y)
# Calculating the entropy just via the following will
# result in NaN, i.e., “Not a Number”, as we have 0 values
# within our TransMatrix (recall log(0) := undefined/Inf.!),
# see for yourself:
EntropyXY = -sum(jointMatrix*log2(jointMatrix))
# [1] NaN

# We will quickly write a function adding a tiny
# value to our inputs:
bit_log_nonzero = function(x) {
  nonzerolog = log2(x+2^(-16))
} # End of function

# Let's try again using our Markov chain example:
EntropyXY = -sum(jointMatrix*bit_log_nonzero(jointMatrix))


# CONDITIONAL ENTROPY H(y|x) (AMTC p. 11)
# Below we will work with the numbers of our last Markov chain example:
EntropyPOST = -sum(jointMatrix*bit_log_nonzero(MessageABCTransMatrix))
# [1] 0.9340018 = H(y|x)

# ENTROPY OF A SINGLE EVENT OF A JOINT:
EntropyX = -sum(jointMatrix*bit_log_nonzero(rowSums(jointMatrix)))
EntropyY = -sum(jointMatrix*bit_log_nonzero(colSums(jointMatrix)))

# Alternative formula for EntropyXY
EntropyXY = EntropyX + EntropyPOST

# The operator “<=” checks on "less than or equal"
EntropyXY <= EntropyX + EntropyY

# Alternative to obtain EntropyPOST
EntropyPOST = EntropyXY-EntropyX

# (AVERAGE) ENTROPY OF THE SOURCE, i.e.,  PER POSSIBLE SYMBOL OF TEXT
EntropySOURCE = -sum(eigenvector*MessageABCTransMatrix*bit_log_nonzero(MessageABCTransMatrix))












