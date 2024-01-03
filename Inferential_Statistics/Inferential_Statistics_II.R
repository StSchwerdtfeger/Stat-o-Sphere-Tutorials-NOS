  #############################################################
  ##---------------------------------------------------------##
  ##                Inferential statistics II -              ##
  ##         Linear regression via the linear least          ##
  ##                      square method                      ##
  ##                        R - Script                       ##
  ##            by Steffen Schwerdtfeger 05.2022             ##
  ##---------------------------------------------------------##
  #############################################################
  
  #######################################################################
  # 2.1 Linear functions - idealized linear relations between variables #
  #######################################################################
  
  ### Synthetic Data Set I - Tea/Time
  
  # Variables 
  
  # time_passed = indep. = y-axis 
  time_passed = c(0:10)    # 0:10 = 0 to 10 = c(1,2,3,4 … 10)
  
  # cups_of_tea = dep. = x-axis
  cups_of_tea = c(0:10)
  
  # Let us plot our data points 
  plot(x = time_passed, y = cups_of_tea)
  
  # Calculating the slope, directly via the objects of the variables
  # that we used above already. The [] are used to call a certain value of the 
  # vector, i.e., element 1 of an object is object[1] etc. If this is too 
  # abstract, check on the whole object again and play around a little by 
  # adding [] with and without values to the object.
  beta = (cups_of_tea[2]-cups_of_tea[1])/(time_passed[2]-time_passed[1]) 
  
  # Alternative via defining the points represented as 
  # vectors P = c(x,y), first.           # row x y
  P1 = c(time_passed[1],cups_of_tea[1])  # [1] 0 0
  P2 = c(time_passed[2],cups_of_tea[2])  # [1] 1 1
  
  # Note that here [] refers to the vector list element of P now, not the 
  # vector list of our data for x and y!
  beta = (P2[2]-P1[2])/(P2[1]-P1[1])  
  
  # We can add a line representing y = a + bx
  # to our plot. The line crosses the y-axis 
  # at 0 (a = 0), as we are starting with 
  # y = 0 at x = 0.
  # a(lpha) = 0, b(eta) = 1
  abline(a = 0, b = 1)
  
  # Define function f(x) = 0 + x * 1 = x.
  f = function(x)(x)
  
  plot(y = f(0:10), x = 0:10, type = "l", ylab = "Cups", xlab = "Time")
  points(0:10,0:10) # add data points to plot
  
  # Example parabola function:
  g = function(x)(x^2)
  g(2) # = 4
  
  # Using lm() to evaluate alpha (intercept) and beta of x.
  # Formula: lm(y~x), saying: a linear model of y (dep.) in 
  # conditional relation (~) to x (indep.).Note that ~ can 
  # also be read as proportional which we will get into in 
  # Inf. Stat. III:
  lm(cups_of_tea~time_passed)
  
  # The result in the console will look like this:
  # CONSOLE OUTPUT (DO NOT EXECUTE)
  
  # Call:
  # lm(formula = cups_of_tea ~ time_passed)
  
  # Coefficients:
  # (Intercept)  cups_of_tea  
  #  1.071e-15    1.000e+00 
  #       α		       β
  
  # Note that one can now also add the line to a plot of data points via:
  plot(x = time_passed, y = cups_of_tea) 
  abline(lm(cups_of_tea~time_passed)) # does not work with plot(f(0:10)...)
  
  
  #############################################################################
  # 2.2 Obtaining a linear model from non-idealized data sets using lm() only #
  #############################################################################
  
  ### Synthetic Data Set II -- Tea/Time
  
  # Cups of tea each h, from 0h:10h 
  # These are just random points I chose, this time slightly 
  # deviating from our linear sequence of 0:10, where the 
  # numerical gradient, difference between each step of time, 
  # was always 1 - linear by itself. The data will represent three
  # measurements of 0h to 10h each. No worries, you will soon 
  # get a proper overview over the data below:
  
  # Cups of Tea: 
  cups = c(0,  1, 2,   3.5, 4.8, 5.2, 6,   6.9, 8.5, 9.1, 9.9, # run 1
           0, .6, 2.6, 3.1, 4.8, 6.9, 7.1, 7.5, 8.5, 9.9, 9.9, #     2
           0, .2, 2.9, 2.9, 4.9, 5.2, 6.9, 7.1, 7.3, 9,   9.8) #     3
  # Time passed for each run of measurements (cups), abbreviated:
  time = c(0:10,0:10,0:10)
  
  # With the cbind(), i.e., the column bind function,
  # we can combine our two vectors, resulting in 
  # something more of a data table (class of
  # that object is actually a combination of a 
  # matrix and an array; see BEM R-Basic Tutorial).
  # We will then convert the result of cbind()
  # and turn it into an object of the class 
  # data frame. There are also other ways, such 
  # as just using y = cups and x = time within the 
  # function lm(). I chose the following, as a proper
  # data table should be a more familiar and less 
  # abstract format to you. It is also a good chance 
  # to learn about the circumstance that not every 
  # object class is compatible with every function,
  # often resulting in enigmatic errors in the console.
  # You can evaluate the class of an object via class(object).
  
  # What we get below is a data table, where each
  # row represents a possible relation between
  # the number of cups and the amount of time 
  # that has passed. The number on the left
  # just represents the number of elements as row number.
  data_table = as.data.frame(cbind(cups,time))
  data_table

    # Let's plot our data points
  plot(x=data_table$time, y=data_table$cups, 
       ylab = "Cups of Tea", xlab= "Hours passed")
  
  # We can use the lm() function again to gain 
  # a simple fitted linear model between time and cups.
  # As mentioned, we could just use:
  linear_model = lm(cups ~ time)
  
  # or the following, using $ to refer to a
  # name of a column:
  linear_model = lm(data_table$cups ~ data_table$time)
  
  # OUTPUT CONSOLE – Do note execute
  # lm(formula = cups ~ time)
  
  # Coefficients:
  # (Intercept)         cups  
  #    0.2318          1.0082
  #      α		            β
  
  # Add linear model to plot:
  abline(linear_model, col = "blue")
  # or again via:
  abline(a = 0.2318, b = 1.0082, col = "blue")
 
  

  ##########################
  # 2.3 Errors / Residuals #
  ##########################
  
  
  # Example residuals plot:
  # x = indep. var.; y = dep. var.
  x = c(0:5); y = c(0,1.5,1.8,3.4,4.4,5.9)
  
  # Our guessed function:
  f = function(x)(x)
  
  # Calculate errors (here x and y refer to the above data vectors!):
  error = y-f(x) 
  # Console output of the object "error", entailing the elements 0-5:
  # [1]  0.0  0.5 -0.2  0.4  0.4  0.9
  
  # Plot of our data points:
  plot(x=x,y=y)
  
  # Plot of our guessed function:
  abline(a=0,b=1)
  
  # Adds lines representing the residuals, i.e., the error between our 
  # "guessed function" and the actual data points, drawn along the y-axis.
  # x0 is the x-coordinate of the start of the line and x1 the x-coordinate 
  # of the end of the dotted line (lty=3):
  segments(x0=x,y0=f(x), x1= x, y1=y, lty =3, col = "blue")
  
  ###########################################################
  # Let us check on the residuals of our idealized
  # model. Following the plain math:
  error = (cups_of_tea-time_passed) # here f(x) = x = time_passed
  sumError  = sum(error)
  sumError2 = sum(error^2)
  
  # Using lm() and residuals() of our idealized model:
  idealized = lm(cups_of_tea~time_passed) 
  residuals_fun = residuals(idealized) # all basically 0 values
  
  # Checks for (near) equality of the result of the function
  # residuals() with our object "error" we got via the plain math 
  # (change object class of residuals_fun to make it work):
  all.equal(error, as.numeric(residuals_fun))
  
  # Let us plot our error / residuals
  plot(error) 
  
  
  ######################################################
  # The same just with our second synthetica data set:
  # Our idealized function will be our first random
  # guess for a possible function for the points of 
  # our second synthetic data set. From there we will 
  # evaluate the errors between the actual points and 
  # the y-value of our random function:
  lmRandom = function(x) (x) 
  
  # These are the errors we still have to minimize
  errorRandom = (data_table$cups-lmRandom(data_table$time))
  sumErrorRan  = sum(errorRandom)
  sumErrorRan2 = sum(errorRandom^2)
  plot(errorRandom)
  # adds (h)orizontal line at y-axis = error = 0 (:= no error)
  abline(h=0)
  
  # This is our fitted model function
  lmFitted = function(x) (0.2318+1.0082*x)
  
  # These are the errors / residuals that are already fitted
  # In other words: this is where we want to get to.
  errorFitted = (data_table$cups-lmFitted(data_table$time))
  
  # The function residuals() does the same with our model
  residualsFitted = residuals(lm(data_table$cups~data_table$time))
  sumErrorFit  = sum(errorFitted)
  sumErrorFit2 = sum(errorFitted^2)
  plot(residualsFitted)
  abline(h=0)
  
  # Let us check if our method delivers same results as 
  # the function residual():
  all.equal(errorFitted, as.numeric(residualsFitted)) 
  # [1] "Mean relative difference: 0.0001735146" = neglectable difference
  
  
  ###########################################################################
  # 2.4.1 Brief excurse on evaluating the min./max. in a parabola function  #
  ###########################################################################
  
  # Defining our function g(x) = x^2 + x + 2
  g = function(x) (x^2+x+2)
  
  # Plot g(x)
  plot(g, xlim = c(-5,5), ylim=c(-1,10), asp=1 )
  abline(h=0, v=0)
  
  # Derivative g'(x) of g(x)
  gdev = function(x) (2*x + 1)
  
  # Evaluate if minimum or maximum is given (none if below = 0)
  # a) Input neg. and pos. test values 
  # b) Output: changes from negativ to positive => maximum is given,
  #            else a minimum is given (i.e., when input == output)
  gdev(-Inf);gdev(+Inf) # Minimum is given
  
  # Add derivative function (it crosses x at our xMin!)
  # Here we graphically see that x refers to both g’(x) and g(x)
  # g’(x)=2*x+1 
  abline(a=1,b=2) # adds our linear function to plot
  
  # Setting g'(x) to zero, to get the x coordinate of that minimum
  # which has a slope of zero.
  # g'(x) = 2*x + 1
  #   0   = 2*x + 1
  # -2x   =     1
  xMin = solve(-2,1)
  #   x   =   -.5
  
  # Let's check if this is true, since g'(.5) = 0
  # In other words: the slope at g(-.5) is zero
  gdev(xMin) == 0   # [1] TRUE 
  
  g(xMin)# y of minimum
  points(x = xMin, y = g(xMin))
  # P(-.5| 1.75)
  
  # Add vertical line at x = -.5
  segments(x0=-.5,y0=0, x1=-.5,y1=g(-.5), col="darkgreen", lty = 3)
  

  #################################################################################
  # 2.4.2 How to obtain the partial deriv. and the diff. between del, d and delta #
  #################################################################################

  # Let us check for the slope at our 
  # previously evaluated minimum P(-.5|1.75) using
  # a differential equation of the ordinary kind:
  
  # Intuitively the below does not work when approaching
  # limit to 0 difference, as it always means 0 divided by 0.
  # We therefore have to add a tiny term into our calculation:
  
  # Output using different x  =>     x  = x +.1       x = x +.001
  betaNumerator = (g(-.5+.1)-g(-.5)) # 0.01           1e-06 
  betaDenominator = ((-.5+.1)+.5)    # 0.1            1e-03 = 0.001
  betaNumerator/betaDenominator      # 0.1            1e-03 = 0.001
  
  # This could be coincidental, so let us check with any
  # other value of x we choose:
  
  # Choose a value for x
  xTest = 1
  
  # With an added value of 1e-7 to our chosen x, R outputs
  # a rounded value for the slope, which is here eqivalent 
  # to gedev(x). Using 1e-12 leads to false values and at 
  # some point you will get a NaN (Not a Number), as the 
  # precision is not high enough (there are ways, but it
  # is not important for us now).
  # Using g(x) from above:
  g = function(x) (x^2+x+2)
  betaTest = (g(xTest+1e-7)-g(xTest))/((xTest+1e-7)-xTest)
  betaTest # xTest = 1 => beta = 3
  
  # There is still a slight difference left though 
  # (not the case with, e.g., xTest = 9):
  gdev = function(x) (2*x + 1)
  all.equal(betaTest,gdev(xTest))
  
  
  
  ###################################################################################################################
  # 2.4.3 Setting partial derivatives to zero and how to rearrange its equations to represent a system of equations #
  ###################################################################################################################
  
  
  # For a better overview, we will rename our data_table objects
  x = data_table$time; y = data_table$cups
  
  # Define a and b as 1 each, to hold for the abstraction we
  # need for the solve() function: 
  # Note ";" marks a new line (delimitter)
  a = 1; b = 1
  
  # Left side of the system of equations:
  left_a_r1c1 = sum(a*length(x)); left_b_r1c2 = b*sum(x) ; 
  left_a_r2c1 = a*sum(x)        ; left_b_r2c2 = b*sum(x^2) 
  
  # Right-hand side of our system of equations: 
  right_r1c1 = sum(y)
  right_r2c1 = sum(x*y)
  
  
  ##############################################################
  # 2.4.4 Approaching results: Solving our system of equations #
  ##############################################################
  
  
  # Bringing the factor outside of the sum: 
  2*sum(1:6) == sum(2*1:6)
  
  #### System of equations: FURRY EXERCISE 
  
  # Set up two elements, so that we can solve an
  # equation in the form: left = right
  left = matrix(c(1,1,
                  -1,1), ncol = 2, byrow = TRUE)
  right = matrix(c(15,7), ncol = 1, byrow = TRUE)
  
  # Use the solve function to solve the system of
  # equations. It outputs a vector list = c(x,y)
  ResultXY = solve(left,right)
  
  
  ###############################################################################
  # 3 Replicating the math in R and how to write your own linear model function #
  ###############################################################################
  
  # Go-go-gadgeto linear_least_square!!!!
  # Replication of the lm() function:
  linear_least_square = function (x,y){ ### Start of function
  
    # Linear least square method:
    
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
    # kind, such as text and our result values. Use "\n"
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
  
  # Let's see what our function can do: 
  linear_least_square(data_table$time, data_table$cups)
  
  # OUTPUT:
  #  Linear least square method in R 
  #
  #  Independent variable: 	 data_table$time 
  #  Dependent   variable: 	 data_table$cups 
  #
  #  alpha 	 0.2318182 
  #  beta 	 1.008182 	 SumR2 	 9.703364
  
  # With our idealized data set:
  linear_least_square(time_passed, cups_of_tea)
  
  # Compare with the regular old lm() function:
  lm(data_table$cups~data_table$time)

  

  ###############################################
  # Linear regression via gradient descent in R #
  ###############################################
  
  x=data_table$time
  y=data_table$cups
  
  
  linear_gradient_descent = function (x,y){ 
    # squared error cost function
    cost <- function(X, y, theta) {
      sum( (X %*% theta - y)^2 ) / (2*length(y))
    }
    
    # learning rate and iteration limit
    alpha <- .01
    num_iters <- 10000
    
    # keep history
    cost_history <- double(num_iters)
    theta_history <- list(num_iters)
    
    # initialize coefficients
    theta <- matrix(c(0,0), nrow=2) 
    
    # add a column of 1's for the intercept coefficient
    X <- cbind(1, matrix(x))
    
    # gradient descent
    for (i in 1:num_iters) {
      error <- (X %*% theta - y)
      delta <- t(X) %*% error / length(y)
      theta <- theta - alpha * delta
      cost_history[i] <- cost(X, y, theta)
      theta_history[[i]] <- theta
    }
    
    # plot data and converging fit
    plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
    for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
      abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
    }
    abline(coef=theta, col='blue')
    
    cost_history1 = cost_history[1:20]
    
    plot(cost_history1, type='l', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')
    
    # Console Output:
    cat("Linear gradient descent method in R","\n","\n",
        "Independent variable:", "\t", deparse(substitute(x)),"\n", 
        "Dependent   variable:", "\t", deparse(substitute(y)),"\n","\n",
        "alpha","\t",theta[1],"\n",
        "beta","\t",theta[2]) 
    
  } #### End of function
  
  linear_gradient_descent(x = data_table$time,y = data_table$cups)

    
    
    
