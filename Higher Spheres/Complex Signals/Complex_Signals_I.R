###############################################
#      Higher Spheres: Complex Signals I:     #
#   Introducing Complex Numbers, the Fourier  #
# Series / Transformation and its Application #
#          in Information Technology          #
#                     by                      #
#           Steffen Schwerdtfeger             #
#                  08.2025                    #
#            Stat-o-Sphere at JNOS            #
###############################################

# Corresponding Tutorial and more Educational Resources incl. Code can be found here:
# https://journal.medicine.berlinexchange.de/statosphere 
# https://journal.medicine.berlinexchange.de/user/steffen-schwerdtfeger-2 

# Github page with all Stat-o-Sphere Scripts:
# https://github.com/StSchwerdtfeger 

###### Necessary packages: 
# UNCOMMENT next line, to install all necessary packages:
#install.packages("pracma","ggplot2","magick","oro.dicom","imager","rgl","plotly")
library("pracma")    # for angle(), optional... Arg() or atan2() works as well...
library("ggplot2")   # entailed in tidyverse
library("magick")    # creating .gif
library("oro.dicom") # loading DICOM files
library("imager")    # for load.image() - JPG images NEEDS DX11 (WIN) or Xquartz (Mac)!!
library("rgl")       # for persp3d(), interactive 3D plot of surfaces
library("plotly")    # for plot_ly() 3D plot of lines

# USE A R PROJECT WITH THIS SCRIPT AND PLACE THE DICOM AND JPG FILE THAT COMES WITH
# THIS TUTORIAL IN YOUR WORKING DIRECTORY FOLDER, THEN THE SCRIPT SHOULD BE
# FULLY EXECUTABLE WITHOUT ANY FURTHER TWEAKING OF THE SCRIPT!


#################################
# 1 Introducing Complex Numbers #
#################################
#####################################################################
# 1.1 History of Complex Numbers Concerning Solutions for Quadratic #
#     and Cubic Equations with the Root of Negative Values          #
#####################################################################

# The square root of -1 is not defined, but can be calculated via
# complex numbers, in fact i = sqrt(-1) and i^2 = -1
sqrt(-1)
# [1] NaN # The square root of -1 is an issue of definition, since nothing squared equals -1 

1*1
# [1] 1
(-1)*(-1)
# [1] 1

# This problem occurs when trying to solve some quadratic equations.
# Let us first look at an example that causes no problem, i.e., does
# not entail a negative square root:
# Example = 2*x^2 + 4 * x + 2 

# Set up quadratic equation of the form a*x^2 + b*x + c
quad_equ = function(x,a,b,c){
  fx = a*x^2 + b * x + c
  return(fx)
} # End of quad_equ

# Set up sequence of values (you may need to adjust for different quad. equations):
x = seq(-6,4,by=.01)
# Example = 2*x^2 + 4 * x + 2 
quad_equ(x=x,a=2,b=4,c=2)
# Plot example quad equation:
plot(x,quad_equ(x=x,a=2,b=4,c=2), type = "l", ylim = c(-10,40))
abline(v=0)
abline(h=0)

# Function applying the quadratic formula:
quad_form = function(a,b,c){
  x1 = (-b + sqrt(b^2-4*a*c)) / (2*a)
  x2 = (-b - sqrt(b^2-4*a*c)) / (2*a)
  return(c(x1,x2))
} # End of quad_form

quad_form(2,4,2)
# [1] -1 -1 # both results are the same, so in this case there is only one point  
            # crossing or better: touching the x-axis at y = 0. 

# Set up sequence of values (you may need to adjust for different quad. equations):
x = seq(-6,4,by=.01)
# Example = -2*x^2 - 4 * x - 4 
quad_equ(x = x,a = -2,b = -4,c = -2)
# Plot example quad equation - we can see that the function does not cross the 
# x-axis, so there should be no result using our quadratic formula:
plot(x = x, y = quad_equ(x = x,a = -2,b = -4,c = -4), type = "l", ylim = c(-40,10))
abline(v = 0)
abline(h = 0)

# In this case we get a NaN as result:
quad_form(-2,-4,-4)
# > quad_form(-2,-4,-4)
# [1] NaN NaN
# Warnmeldungen:
# 1: In sqrt(b^2 - 4 * a * c) : NaNs wurden erzeugt
# 2: In sqrt(b^2 - 4 * a * c) : NaNs wurden erzeugt

x1 = (-4 + sqrt((-4)^2-4*(-2)*(-4))) / (2*-(2))
# The part within the square root results in a negative number:
(-4)^2-4*(-2)*(-4)
# [1] -16

# Same in x2
x2 = (-(-4) - sqrt((-4)^2-4*(-2)*(-4))) / (2*(-2))
(-4)^2-4*(-2)*(-4)
# [1] -16

# In the case of cubic equations, we can see that they always cross the x-axis
# at some point, so we would not expect the square root of a negative as an 
# indicator for a lack of crossing points with the x-axis. However, it can be 
# shown that certain techniques lead to cases where the square root of negatives
# cancel each other out but are present.

# Cubic equation function("a" can't be zero, since then it would
# not be a cubic equation anymore!):
cubic_equ = function(x,a,b,c,d){
  fx = a * x^3 + b * x^2 + c * x + d
  return(fx)
} # End of quad_equ

#### Plot of 2*x^3+0*x^2-5*x+10
x = seq(-3,3,.1)
plot(x = x, y = cubic_equ(x,2,0,-5,10), type = "l")
abline(h = 0, v = 0)

#### Finding the roots of a cubic equation via linear algebra by
#### finding the eigenvalues of a companion matrix:

# Note that the variable "a" can't be 0, since it would 
# not be a cubic equation anymore!
cubic_roots_la = function(a,b,c,d){
  # Create a so-called companion matrix:
  comp_mat = matrix(c(0,0,(-d/a),
                      1,0,(-c/a),
                      0,1,(-b/a)), ncol = 3, byrow = TRUE)
                    # Finding the eigenvalues of the above comp_mat is equivalent 
                    # to finding the roots of a polynomial by using the QR method vi eigen()
                    roots = eigen(comp_mat)$values
                    return(roots)
} # End of cubic_formula
 
# Test function:
cubic_roots_la(2, 0, -5, 10)
# [1] -2.187603+0.00000i  1.093801+1.04365i  1.093801-1.04365i
# The first result has 0i and is therefore the only real number solution!


###################################################################################################
# 1.2 Relating Complex Numbers to Rotation of Vectors or to Rotating the Coordinate System Itself #
###################################################################################################

# Complex numbers is a combination of real numbers (1,1.777,-4, 1/2)
# Create a complex number using complex()
i = complex(real = 0, imaginary = 1)
# [1] 0+1i

# With the above 0+1i we can create any complex number (alternative use parameters of complex()):
2+2*i
# [1] 2+2i

grid2Dcomp = function() {
  # Let us first plot an empty plot, where x and y
  # range from -5 to 5. For this we will adjust the plot a little:
  
  # These vectors represent a list of tick mark values, see below
  posX = c(-4:4) # “:” states from x to y, abbreviates process
  posY = c(-4:4)
  
  # Plot empty plot (asp = 1 for steady ratio)
  plot(NA, xlim=c(-3,3), ylim=c(-3,3), xlab="x-axis = a = Real Number Plane", ylab="y-axis = bi = Imaginary Number Plane", asp = 1, 
       axes = TRUE, las = 1) # axes removes pre-set axes, las = 1 for horizontal tick marks
  abline(h = 0,lty = 1) # adds horizontal line at ylim = 0 
  abline(v = 0,lty = 1) # adds vertical line at xlim = 0
  abline(h = posY, lty=3) # adds grid both h and v
  abline(v = posX, lty=3)
  
} # End of function

# Create fresh grid
grid2Dcomp()

# Complex numbers behave a lot like vectors (at least when it comes to
# addition of vectors), but the second axes is not part of the "real" number plane: 
# 2 + 1i
arrows(x0=0,y0=0,x1=Re(2+0*i), y = Im(2+0*i), col = "darkviolet")
text(x = Re(2+0i),y = Im(2+0i)-.5, "2+0i ~ (2|0)")
# 0 + 2i
arrows(x0=0,y0=0,x1=Re(0+2*i), y = Im(0+2*i), col = "darkviolet")
text(x = Re(0+2i),y = Im(0+2i)+.2, "0+2i ~ (0|2)")

2 + 0i + 0 + 2i 
#[1] 2+2i
arrows(x0=0,y0=0,x1=Re(2+2*i), y = Im(2+2*i), col = "darkviolet")
text(x = Re(2+2i)+.2,y = Im(2+2i)+.3, paste("2+0i + 0+2i","\n","= 2+2i", "~ (2|2)"))


###############################################################################
# 1.3 The Relation Between Trigonometry, Euler's Identity and Complex Numbers #
###############################################################################
#########################################################################
# 1.3.1 Complex Numbers and 90° Rotations — First Hints of Trigonometry #
#########################################################################

# Create fresh grid
grid2Dcomp()

# Watch what happens when repeatedly multiplying by i, starting with 1:
# It basically tilts by 90° every time we multiply the previous result with i
# (again, starting with 1*i):
1i
1*i # also possible after i was defined as an object, as done further above
# [1] 0+1i # == i
arrows(x0=0,y0=0,x1=Re(0+1*i), y = Im(0+1*i), col = "darkviolet")
text(x=.6,y=1,"1*i = 0+1i")

1i*i
1*i*i
# [1] -1+0i
arrows(x0=0,y0=0,x1=Re(-1+0*i), y = Im(-1+0*i), col = "darkviolet")
text(x=-1.5,y=.25,"1*i*i = -1+0i")

-1i
-1*i
# [1] 0-1i
arrows(x0=0,y0=0,x1=Re(0-1*i), y = Im(0-1*i), col = "darkviolet")
text(x=.75,y=-1,"-1*i = 0-1i")

-1i*i
-1*i*i
# [1] 1+0i
arrows(x0=0,y0=0,x1=Re(1+0*i), y = Im(1+0*i), col = "darkviolet")
text(x=1.5,y=.25,"-1*i*i = 1+0i")


##### The polar coordinates of a complex number can be obtained via P(Mod(2+2i)|Arg(2+2i)):
# Set fresh grid:
grid2Dcomp()
# Draw 2 + 2i:
arrows(x0=0,y0=0,x1=Re(2+2*i), y = Im(2+2*i))
# Draw angle using exp(angle*i): 
angle = seq(0,Arg(2+2*i),by = .001)
lines(exp(angle*i), col = "darkviolet") # used e^Arg*i to create angle circle part in the plot below!
text(x = 2,y = .5, paste("Arg =", round(Arg(2+2*i),3), "rad", "\n", "= ", (Arg(2+2*i)*180/pi),"°"), col = "darkviolet")
# Draw line for Modulus:
segments(x0=0,y0=0, x1=Im(2+2i),y=Re(2+2i), col = "darkgreen")
text(x =.75,y = 1.5, paste("Mod = ", round(Mod(2+1i),3)), col = "darkgreen")
# Label z = c = 2 + 2i:
text(x = Re(2+2i)+.2, y = Im(2+2i)+.2, "z = 2+2i")

#  Note that 45° or 0.785... rad from Arg(2+2*i) is equivalent to 1/4*pi:
Arg(2+2*i) == (1/4) * pi # see further below for details on Arg() function...
# [1] TRUE

#####################################################################
# 1.3.2 The Nature of Euler’s Number — Growth, Trigonometry and the #
#       Taylor/Maclaurin Series of Euler’s Number                   #
#####################################################################

# e upt to 20 decimal spaces:
sprintf("%.20f",exp(1))
# [1] "2.71828182845904509080"

##### Special characteristics of Euler's number e = 2.718281...
seq = seq(-4,4, by = .01)
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
# point with a slope = e^x. In case you wonder
# the linear function x does not need to be defined in curve().
curve(exp(1)*x,-3,3, add = TRUE)

# Integral from -Inf to x = 1
expo_fun = function(x) {exp(x)}
integrate(expo_fun, lower = -Inf, upper = 1)
# 2.718282 with absolute error < 0.00015
# = e again.

# The relation between e^{x*Arg} sin and cos in the real plain, 
# regular trigonometry. We will soon see a similar formula just including
# the imaginary number i, which is then referred to as Euler's formula,
# connecting trigonometry and complex numbers:
exp(2*Arg(exp(2))) == cos(Arg(exp(2)))+sin(Arg(exp(2)))

# Example for factorials
3*2*1 == factorial(3)
# [1] TRUE

# Euler's number via 1+sum(1/n!) for n -> inf. Below we will
# go up to n_max = 100 
fact_max = c(1:100)
# Initialize result object:
euler = 1 # it's 1, since it starts with 1/1!, 
          # which equals 1 and then 1/2! ... + 1/n!
for(index in 1:100){
  euler = euler + (1/factorial(fact_max[index]))
} # End for i
euler
# [1] 2.718282

# Check for equivalence to exp(1), rounded to 5 decimal spaces:
round(euler, 5) == round(exp(1), 5)
# [1] TRUE

##### Function that calculates e^x via the formula
# e^x = 1+x + x^2/2! ... x^n/n! - Taylor/Maclaurin Series
# turning our function e^x into a series of polynomials:
ex_tayl = function(x,length_series){
  # Recursive addition of x^i/i!:
  for(i in 1:length_series){
    if(i == 1){
      base = 1+x
    } # End if i == 1
    else if(i > 1){
      base = base + (x^i/factorial(i)) 
    } # End else  
  } # End for i
  return(base)
} # End of function

# Showing that both results in 2.71828... (rounded to 5 decimal spaces):
round(ex_tayl(1,1000),5) == round(exp(1),5) && round(ex_tayl(1,1000),5) == round(euler,5)
# [1] TRUE

# Function that plots the Taylor Polynomial Series e^x via the formula
# e^x = 1+x + x^2/2! ... x^n/n! up to a certain number n. It works
# as the function before, just that x is not a single value but a series
# of values created via seq() that can be used to plot the respective 
# polynomial functions up to n:
ex_tayl_plot = function(length_series){
  # Recursive addition of x^i/i!:
  x = seq(from = -10,to = 10,by = .1)
  for(i in 1:length_series){
    if(i == 1){
      base = 1+x
    } # End if i == 1
    else if(i > 1){
      base = base + (x^i/factorial(i)) 
    } # End else  
  } # End for i
  
  # Plot resulting polynomial up to defined length of the series:
  plot(x,base, type = "l", col = "green", ylim = c(-50,50), ylab = "y", main = paste("Taylor Series for n =",length_series))
  lines(x = x, y = exp(x)) # add e^x
  abline(h=0,v=0)          # add axes
  
  return(base)
} # End of ex_tayl_plot

# Plot for the case of n = 2, n = 3, n = 5, n = 100:
par(mfrow = c(2,2))
ex_tayl_plot(2);ex_tayl_plot(3);ex_tayl_plot(10);ex_tayl_plot(100)
par(mfrow = c(1,1))

# Approximating cos() via Taylor/Maclaurin Series:
cos_tayl = function(length_series){ # where n and the factorial is even
  # Create a sequence of even numbers up to length_series, no 
  # matter if the input length_series is odd or even.
  if(length_series == 1){
    n = c(0,length_series+1)
  } # End if length series == 1
  else if((length_series) %% 2 == 0){ 
    n = c(0,seq(2, length_series, by = 2))
  } # if length_series is a even number
  else if((length_series) %% 2 != 0) {
    n = c(0,seq(2, (length_series-1), by = 2))
  } # End else if length_series is odd
  
  # Actual Taylor/Maclaurin Series for cos(x):
  # Recursive addition of x^i/i!:
  x = seq(from = -10,to = 10,by = .1)
  for(i in 1:length(n)){ # loops over the length of n, not length_series as before!
    if(i == 1){
      base = 1
    } # End if i == 1
    else if((i %% 2) == 0 && i != 1){ # If division by two gives remainder of 0 a number is even!
      base = base - (x^n[i]/factorial(n[i])) 
    } # End else if i even
    else if((i %% 2) != 0){ # If division by two gives remainder != 0 a number is odd!
      base = base + (x^n[i]/factorial(n[i])) # same as before just + instead of -
    } # End else if i odd
  } # End for i
  
  # Plot resulting polynomial up to defined length of the series:
  plot(x,base, type = "l", col = "green", ylim = c(-5,5), ylab = "y", main = paste("Taylor Series for n =",n[length(n)]))
  lines(x = x, y = cos(x)) # add cos(x)
  abline(h=0,v=0)          # add axes
  return(base)
} # End of cos_tayl

# Plot results
par(mfrow = c(2,2))
cos_tayl(1);cos_tayl(4);cos_tayl(10);cos_tayl(20)
par(mfrow = c(1,1))


# Approximating sin() via Taylor/Maclaurin Series:
sin_tayl = function(length_series){ # where n and the factorial is even
  # Create a sequence of even numbers up to length_series, no 
  # matter if the input length_series is odd or even.
  if(length_series == 1 || length_series == 2){
    n = c(1,3)
  } # End if length series == 0 or == 2
  else if((length_series) %% 2 == 0 && length_series > 2){ 
    n = c(seq(1, length_series+1, by = 2))
  } # if length_series is a even number
  else if((length_series) %% 2 != 0) {
    n = c(seq(1, (length_series), by = 2))
  } # End else if length_series is odd
  
  # Actual Taylor/Maclaurin Series for sin(x):
  # Recursive addition of x^i/i!:
  x = seq(from = -10,to = 10,by = .1)
  for(i in 1:length(n)){ # loops over the length of n, not length_series as before!
    if(i == 1){
      base = x
    } # End if i == 1
    else if((i %% 2) == 0 && i != 1){ # If division by two gives remainder of 0 a number is even!
      base = base - (x^n[i]/factorial(n[i])) 
    } # End else if i even
    else if((i %% 2) != 0){ # If division by two gives remainder != 0 a number is odd!
      base = base + (x^n[i]/factorial(n[i])) # same as before just + instead of -
    } # End else if i odd
  } # End for i
  
  # Plot resulting polynomial up to defined length of the series:
  plot(x,base, type = "l", col = "green", ylim = c(-5,5), ylab = "y", main = paste("Taylor Series for n =",n[length(n)]))
  lines(x = x, y = sin(x)) # add sin(x)
  abline(h=0,v=0)          # add axes
  return(base)
} # End of sin_tayl

# Plot results
par(mfrow = c(2,2))
sin_tayl(1);sin_tayl(5);sin_tayl(11);sin_tayl(21)
par(mfrow = c(1,1))


#######################################################################
# 1.3.3 Euler’s Identity and Euler’s Formula — Trigonometry Involving #
# Complex Numbers and Euler’s number.                                 #
#######################################################################

##### Euler's identity:
# We have to use Re(), since b entails is a tiny remnant number,
# probably added somewhere in "background code", which we could
# also round though using the round() function:
exp(i*pi)
# -1+1.224606e-16i # bi ~ 0i 
# Alternative via extracting the real part only:
Re(exp(i*pi)) == -1
# [1] TRUE
# Version via rounding, such that 1.224606e-16i == 0*i
round(exp(i*pi)) == -1
# [1] TRUE

# Equivalence to zero:
Re(exp(i*pi)) + 1 == 0
# [1] TRUE
round(exp(i*pi)) + 1 == 0
# [1] TRUE

# Set up grid for four plots:
par(mfrow = c(2,2))

# Set time from 0 to 1 for Hz = 1 second:
steps = c(0,.25,.5,1)
for(index in 1:length(steps)){ # CAVE: DON'T CALL INDEX "i" HERE, when i = complex(imaginary = 1,real = 0) 
  time = seq(0,steps[index],by=.001)
  i = complex(imaginary = 1, real = 0)
  plot(exp(-2*pi*time*i), type = "l", 
       ylim = c(-2,2), xlim = c(-2,2), # limit is important, otherwise plot is always centered and cut.
       xlab = "Real", ylab = "Imaginary")
  # We can add an arrow and axes:
  arrows(x0 = 0, y0 = 0 ,   # always in center 
         x1 = Re(exp(-2*pi*time[length(time)]*i)), 
         y1 = Im(exp(-2*pi*time[length(time)]*i)),  # dependent on Re() and Im() of last element of time
         col = "darkviolet", lty = "dashed")  
  # Add value of e^2*pi*i*time[length(time)]
  text(x = 0, y = -1.5, paste("e^(-2)*pi*i*",time[length(time)], "Hz"))
} # End for i

# Reset grid
par(mfrow = c(1,1))


#### Relation to e^i*Arg(0+1i) or *theta (angle instead of argument):
theta = 20 # for 20°
exp(i*theta) == cos(theta)+i*sin(theta) 
# 0.4080821+0.9129453i ==  0.4080821 + i * 0.9129453


#### Showing that the Taylor and Maclaurin series of cos()+i*sin() = e^ix, 
# where x = seq(from = -10,to = 10,by = .1); result is rounded to 5 decimal spaces:
round(cos_tayl(100) + complex(real=0,imaginary=1)*sin_tayl(100),5) == round(exp(complex(real=0,imaginary=1)*seq(from = -10,to = 10,by = .1)),5)


#### Plot exp(2*pi*time*i) from different perspectives of the dimensions Re(), Im() and for each time:
# Set up sequence 0-1Hz:
time = seq(0,1, by= .001)
x = Re(exp(2*pi*time*i)) # x-axis = real dimension
y = Im(exp(2*pi*time*i)) # y-axis = imaginary / complex domain
z = time                 # temporal dimension 


### Base plot perspectives triplet with exp(2*pi*time*i), where time = 0-1Hz:
par(mfrow = c(2,2))

# exp(2*pi*time*i)
plot(x,y, xlab = "Re(exp(2*pi*time*i))", ylab = "Im(exp(2*pi*time*i))",type="l")

# Sine(2*pi*time*i)
plot(z,y, xlab = paste("time = 0-1Hz","\n", "i*sine(2*pi*t)"), ylab = "Im(exp(2*pi*time*i))",type="l")

# Cosine(2*pi*time*i)
plot(x,z, xlab = paste("Re(exp(2*pi*time*i))","\n", "cosine(2*pi*t)"), ylab = "time = 0-1Hz", type="l")


#### Base plot perspectives triplet with exp(2*pi*time*i), where time = 0-1Hz,
#### Including a sequence of 0-.25 Hz on the circle:
par(mfrow = c(2,2))

# Plot exp(2*pi*time*i)
plot(x,y, xlab = "Re(exp(2*pi*time*i))", ylab = "Im(exp(2*pi*time*i))",type="l")

# Draw exp(2*pi*.25*i)
arrows(x0=0,y0=0,x1=Re(exp(2*pi*.25*i)), y = Im(exp(2*pi*.25*i)))

# Draw angle using exp(2*pi*.25*i): 
angle = seq(0,Arg(exp(2*pi*.25*i)),by = .001) # sequence for angle circle and marking of the actual circle
lines(.5*exp(angle*i), col = "violet") # angle circle
lines(1*exp(angle*i), col = "violet")  # matching circle and sine/cosine wave for Hz = .25
segments(x0=0,y0=0,x1=1,y1=0)          # draw line from Re() = 0 to Re() = 1 and 0 on th Im() plane

# Note the equality of a quarter circle rotation to 1*pi:
Arg(exp(2*pi*.25*i))*2 == pi

# Add text for Arg and Angle in Degrees:
text(x = .6,y = .6, paste("Arg = ", round(Arg(exp(2*pi*.25*i)),3),"rad", "\n", "=", 
                          (Arg(exp(2*pi*.25*i))*180/pi),"°"), col = "violet", cex=1)
text(x = .2,y = .2, "Arg" , col = "violet", cex=1)

# Draw Modulus of exp(2*pi*.25*i) and add text:
segments(x0=0,y0=0, x1=Re(exp(2*pi*.25*i)),y=Im(exp(2*pi*.25*i)), col = "darkgreen")
text(x =-.4,y = .5, paste("Mod = ", round(Mod(exp(2*pi*.25*i)),3)), col = "darkgreen")

# Label z = c = exp(2*pi*.25*i):
text(x = Re(exp(2*pi*.25*i)), y = Im(exp(2*pi*.25*i)), "z = exp(2*pi*.25*i)")

# Plot corresponding sine wave:
plot(z,y, xlab = paste("time = 0-1Hz","\n","i*sine(2*pi*time)"), ylab = "Im(exp(2*pi*time*i))", type = "l")

# Add line from 0-.25 Hz on the sine wave, corresponding to Arg = 90°;
# Setup new shortened sequence:
time2 = seq(0,.25, by= .001)
x2 = time2
y2 = Im(exp(2*pi*time2*i))
lines(x2,y2,col = "violet") # use lines to add to add line from 0-.25Hz to existing plot

# Use rev(range(z)) to reverse y-axis; in this case necessary for the cosine wave plot
# below exp(2*pi*time*i):
plot(x,z, xlab = paste("Re(exp(2*pi*time*i)","\n", "cosine(2*pi*time)"), ylab = "time = 0-1Hz",type = "l", ylim = rev(range(z)))
# Setup sequence for cosine wave 0-.25Hz:
x3 = Re(exp(2*pi*time2*i)) 
y3 = time2
lines(x3,y3,col="violet") # add line to plot...

# The above essentially shows that:
exp(2*pi*i*.25) == cos(2*pi*.25)+i*sin(2*pi*.25) 

# Rest grid to 1x1:
par(mfrow = c(1,1))


#############################################################################
# 1.4 The Real Part of a Parabolas Function in the Complex Plane (Optional) #
#############################################################################

# Plot parabola and its complex roots (will show a mirrored parabola!) by
# mapping its complex roots onto the y-axis:
x = seq(-10,10,by = .1) # possible x-values 
y = x^2+1 # our regular parabola function 
c = (x*complex(real = 0, imaginary = 1))^2+1

# Plot with colour blind friendly color palette:
palette.colors(palette = "Okabe-Ito")
plot(x,y, ylim = c(-20,20),xlim = c(-15,15), type = "l", col = "#0072B2")
abline(h=0,v=0) # add x and y axis

# Add complex root of x^2+1 to plot
lines(x,Re(c), col = "#F0E442")

# Add legend:
legend("bottomright", col = c("#0072B2","#F0E442"),
       lty =1,
       legend = c("f(x)","complex root"), cex = .5)

# Parabola x^2 + constant:
# Using regular quad_form() function gives NaN for x^2+1.
# Let us see how it evolves:
quad_form(a = 1,b = 0,c = -2)
# [1] 1.414214 -1.414214
quad_form(a = 1,b = 0,c = -1)
# [1] 1 -1
quad_form(a = 1,b = 0,c = 0)
# [1] 0 0
quad_form(a = 1,b = 0,c = +1)
# [1] NaN NaN

# Adjust quad_form function for applying the quadratic formula to complex numbers:
quad_form_adj = function(a,b,c){
  out1=b^2-4*a*c
  out2=b^2-4*a*c
  i = complex(real=0,imaginary=1)
  x1 = (-b + i*sqrt(abs(out1))) / (2*a)
  x2 = (-b - i*sqrt(abs(out2))) / (2*a)
  return(c(x1,x2))
} # End of quad_form

# Test adjusted function with x^2+1
quad_form_adj(a = 1,b = 0,c = 1)
# [1] 0+1i 0-1i

# We can now add the points (x=1|y=0) and (x=-1|y=0) to the previous plot:
points(x = Im(quad_form_adj(a = 1,b = 0,c = 1)),y = Re(quad_form_adj(a = 1,b = 0,c = 1)), pch=16, col = "darkblue")

# Used for 3d scatterplot:
library(plotly)

# Plot regular parabola x^2 + 1:
x = seq(-10, 10, length.out = 100)
y = x^2 + 1 # our regular parabola function x^2 + 1
c = x*0 # Whole sequence is 0, since a regular parabola only rests on the real number plane.

# Plot complex root of the parabola x^2 + 1:
x = seq(-10, 10, length.out = 100)
xc = x*0
yc = Re((x*complex(real = 0, imaginary = 1))^2+1) # this time yc == 0
cc = x

# Create 3D plot UNCOMMENT TO RUN!!!!!!!:
#plot = plot_ly(x = ~x, y = ~y, z = ~c, type = 'scatter3d', mode = 'lines')
#plot %>% add_trace(x = ~xc, y = ~yc, z = ~cc, type = 'scatter3d', mode = 'lines')%>%layout(scene = list(camera = list(eye = list(x = 1, y = 2, z = -3))))

# Plot real part of 4D parabola (x, y, Re(), Im() = 4D):
#install.packages("rgl")
library(rgl) # for persp3d()

# Looking at the real part of the parabola in the complex plane:
x = seq(-20, 20, length.out = 200) # possible x-values
y = seq(-20, 20, length.out = 200) # possible y-values
z = outer(x, y, function(x, y) (Re((x+y*complex(real=0,imaginary =1))^2 + 1)))

# Use persp3d from rgl
persp3d(x, y, z,
        col = "lightblue", alpha = 0.8,
        xlab = "x", ylab = "y", zlab = "Re(z)")

# Looking at the imaginary part of the parabola in the complex plane:
#z = outer(x, y, function(x, y) (Im((x+y*complex(real=0,imaginary =1))^2 + 1)))

# Use persp3d from rgl
#persp3d(x, y, z,
#        col = "lightblue", alpha = 0.8,
#        xlab = "x", ylab = "y", zlab = "Im(z)")



############################
# 2 Fourier Transformation #
############################
#########################################################################################
# 2.1 Fourier Series of a Signal — Approximation of Functions via Summing up Sine Waves #
#########################################################################################

# Plot a Square wave:
time = seq(0,4*pi, by = .001)
# Sign of a sine wave:
square = sign(sin(time)) # time*frequency
plot(time,square, type = "l")
abline(h=0) # add x-axis at y = 0

# Since we are about to calculate the integral of functions, we need
# a square wave function in order to use the integrate function, since it
# does not work using sign(cos(time)) within integrate():
square_wave = function(time){
  sign(cos(time))
} # End of function

# Function to evaluate Fourier series of a function:
fourier_series = function(n, time_max, fun){
  #### Create respective sequence for x:
  time = seq(-time_max,time_max,by =.001)

  #### Coefficients:
  # Amplitude a for 0:
  a_0 = (1/2)*(1/time_max)*integrate(fun, lower = -time_max, upper = time_max)$value
  
  # Amplitude a (cosine part) for n:
  a_n_fun = function(n){
    intermediate_fun_a = function(x){fun(x)*cos((n*pi*x)/time_max)}
    1/time_max*integrate(intermediate_fun_a, lower = -time_max, upper = time_max)$value
  } # End of function
  
  # Amplitude b (sine part) for n:
  b_n_fun = function(n){
    intermediate_fun_b = function(x){fun(x)*sin((n*pi*x)/time_max)}
    1/time_max*integrate(intermediate_fun_b, lower = -time_max, upper = time_max)$value 
  } # End of function
  
  #### Actual Fourier Series calculated with coefficients:
  series = a_0
  for(n_index in 1:n){
    series  = series + a_n_fun(n_index) * cos((n_index*pi*time) / time_max) + b_n_fun(n_index) * sin((n_index*pi*time) / time_max)
  } # End for index
  return(series)
} #  End of function

### Test function with square_wave() and plot results:
plot(seq(-pi,pi,by =.001),square_wave(seq(-pi,pi,by =.001)), type = "l", col = "blue", ylim = c(-1.3,1.3),ylab ="Amplitude", main = "n = 20", xlab = "periods in pi")
abline(h=0)
lines(seq(-pi,pi,by =.001), fourier_series(n = 20, time_max = pi, fun = square_wave), type = "l", col = "deeppink")

### Test for different sizes of n -- n= c(1,10,20,80)
# Set 2*2 grid:
par(mfrow = c(2,2))
# n = 1
plot(seq(-pi,pi,by =.001),square_wave(seq(-pi,pi,by =.001)), type = "l", col = "blue", ylim = c(-1.3,1.3), ylab ="Amplitude", main = "n = 1", xlab = "-pi to pi")
abline(h=0)
lines(seq(-pi,pi,by =.001), fourier_series(n = 1, time_max = pi, fun = square_wave), type = "l", col = "deeppink")
# n = 10
plot(seq(-pi,pi,by =.001),square_wave(seq(-pi,pi,by =.001)), type = "l", col = "blue", ylim = c(-1.3,1.3), ylab ="", main = "n = 10", xlab = "-pi to pi")
abline(h=0)
lines(seq(-pi,pi,by =.001), fourier_series(n = 10, time_max = pi, fun = square_wave), type = "l", col = "deeppink")
# n = 20
plot(seq(-pi,pi,by =.001),square_wave(seq(-pi,pi,by =.001)), type = "l", col = "blue", ylim = c(-1.3,1.3), ylab ="Amplitude", main = "n = 20", xlab = "-pi to pi")
abline(h=0)
lines(seq(-pi,pi,by =.001), fourier_series(n = 20, time_max = pi, fun = square_wave), type = "l", col = "deeppink")
# n = 80
plot(seq(-pi,pi,by =.001),square_wave(seq(-pi,pi,by =.001)), type = "l", col = "blue", ylim = c(-1.3,1.3), ylab ="", main = "n = 80", xlab = "-pi to pi")
abline(h=0)
lines(seq(-pi,pi,by =.001), fourier_series(n = 80, time_max = pi, fun = square_wave), type = "l", col = "deeppink")
# Reset plot 
par(mfrow = c(1,1))


### Test for orthogonality of cosine and sine:
x = seq(-2*pi, 2*pi, by = .0001)
g = function(x){cos(x)*sin(x)}

# Integrate g (it will also work given m or n, except they are equal):
integrate(g,lower = -2*pi, upper = 2*pi)
# 0 with absolute error < 4.5e-14

# For the discrete set it will not be zero but a very small value:
sum(g(x))
# [1] -1.037524e-05
# The above value gets smaller approaching 0 the finer our 
# seq of x will be, above by = .0001

# Plot of cosine and sine and sum/integral of 
# cosine*sine - the dot product of sine and cosine: 
plot(x,cos(x), type = "l", col = "blue", ylab = "") # cos
lines(x,sin(x), type = "l", col = "green")          # sine
lines(x, g(x), type = "l", col = "deeppink")        # sin*cos
abline(h=0,v=0)
# The integral of g(x) = integral of cos*sin will be zero, since the 
# positive and negative areas under the curve cancel each other out.

### Test for orthogonality of cosine and cosine where m == n (here both 1):
x = seq(-2*pi, 2*pi, by = .0001)
g_mn = function(x){cos(x)*cos(x)} # with m and n being both 1

# The integral ob g_mn equals the absolute value of L = time_max of the interval,
# i.e., in this case 2*pi
integrate(g_mn,lower = -2*pi, upper = 2*pi)$value == 2*pi
# [1] TRUE

# For the discrete set it will not be zero but a very small value:
sum(g_mn(x))
# [1] 62832.15
# Summing for the discrete case does not work in this case, however, 
# the value divided by 20k somehow roughly results in pi...

# Plot of cosine and sine and sum/integral of 
# cosine*sine - the dot product of sine and cosine: 
plot(x,cos(x), type = "l", col = "blue", ylab = "") # cos
lines(x, g_mn(x), type = "l", col = "deeppink")     # sin*cos
abline(h=0,v=0)
# The integral of g(x) = integral of cos*cos will be clearly above 0 in this case!


#### Uncovering the meaning of the dot product and orthogonality, 
# starting with linear algebra:
grid2Dcomp()

# Add two vectors that are in fact orthogonal (90° angle):
arrows(x0=0,y0=0,x1=2, y = 0, col = "darkviolet")
arrows(x0=0,y0=0,x1=0, y = 2, col = "darkviolet")
arrows(x0=0,y0=0,x1=-2, y = 0, col = "darkviolet")
text(x = 1, y = 2, "v1 = c(0,2)")
text(x = 2, y = -.5, "v2 = c(2,0)")
text(x = -2, y = -.5, "v3 = c(-2,0)")

# Two different ways to calculate the inner product in linear algebra:
# Via multiplying an then summing vectors:
sum(c(0,2)*c(2,0)) == 0 
# [1] TRUE # as we can see, they are orthogonal

# What R does is essentially this: multiplying vector 
# elements with the same index and summing the results:
0*2 + 2*0 == 0
# [1] TRUE

# Second method via angle/radians, here 90°, i.e. orthogonality:
Mod(c(0,2))*Mod(c(2,0))*cos(90*pi/180) == c(0,0)
# [1] TRUE TRUE


### Dot product for equivalent vectors:
sum(c(0,1)*c(0,1)) == 1
# [1] TRUE
sum(c(0,2)*c(0,2)) == 4
# [1] TRUE

### Orthogonal vectors:
sum(c(1,0)*c(0,1)) == 0 && sum(c(2,0)*c(0,2)) == 0  
# [1] TRUE

### Opposite vectors:
sum(c(0,1)*c(0,-1)) == -1
# [1] TRUE
sum(c(0,2)*c(0,-2)) == -4
# [1] TRUE

#### For pi/2, where a cosine wave and the square wave overlap:
square_wave(pi/2)*cos((1*pi*(pi/2))/pi)
# [1] 6.123032e-17 # roughly zero
all.equal(0, square_wave(pi/2)*cos((1*pi*(pi/2))/pi))
# [1] TRUE


###############################################################################
# 2.2.x Sampling a Frequency as Winding up a Signal onto to the Complex Plane #
###############################################################################


# The following is based on the 3blue1brown tutorial on Fourier Transformation.
# https://www.youtube.com/watch?v=spUNpyF58BY&t=70s  
# There is also a python script that produces mostly equivalent results, however the
# python code is not well documented and therefore hard to read for beginners and 
# some adjustments to how the output looks was done (and in general solved in a 
# very different way).
# You can find the python version corresponding the 3blue1brown video here: 
# https://github.com/thatSaneKid/fourier/blob/master/Fourier%20Transform%20-%20A%20Visual%20Introduction.ipynb


# Define the time as a frequency with a certain precision of points (by=):
time = seq(0,1,by=.0001) # This sets Hz as unit of frequency

# Winding / sampling frequency
samp_f = seq(0,10, by = .1) # from zero to 10 in steps of .1
length(samp_f) # length 101, so we will only plot up to 9.9 Hz winding freq (same in the python script)

# Set complex number "i":
i = complex(real = 0, imaginary = 1) # beware to NOT USE "i" as index variable in a for loop from now on!!

# Cos wave definition. cos() for all time multiplied by frequency of the cos() wave and 2*pi 
# for radiant period, i.e. normalized time in unit of Hz. 
# This cosine wave is used in the python script creates a circle when samp_f == freq:
# Define wave frequency 
freq = 3 # == 3 Hz
g_t = cos(2*pi*freq*time) 

# This cosine wave is used in the 3blue1brown video, shifted upwards, creates dented circle at samp_f == freq
#g_t = cos(2*pi*freq*time) + 1 

# More than one frequency
#freq1 = 3; freq2 = 6
#amp1 = 3 ; amp2 = 2
#g_t = amp1*cos(2*pi*freq1*time) + amp1*cos(2*pi*freq2*time)

# Square wave
#g_t = sin(2*pi*freq*time) + 0*sin(2*2*pi*freq*time) + (1/3)*sin(3*2*pi*freq*time)

# Sawtooth:
#g_t = sin(2*pi*freq*time)-(1/2)*sin(2*2*pi*freq*time)+(1/3)*sin(3*2*pi*freq*time)

# Plot cosine wave:
plot(x = time, y = g_t, type = "l", xlab = paste("Time in Seconds", "\n",freq,"periods in 1 Second ==",freq,"Hz"), 
     ylab = "g_t = cos(2*pi*freq*time)", 
     col = "deepskyblue3") 

# Comparing our cosine wave with 0Hz and 2.9Hz sampling frequency:
par(mfrow = c(1,2))
# Plotting output for each winding frequency is easy with basic plot().
# Let us have a look at samp_f[1] at first:
plot(g_t*exp(-2*pi*i*samp_f[1]*time), type = "l", col = "darkviolet", 
     # Title entails information on winding/sample freq and original cosine freq
     main = paste("Winding Frequency of", samp_f[30],"Hz", "\n", paste("Cosine Frequency of", freq,"Hz")), 
     xlab = "Real Numer",
     ylab = "Imaginary Number") 

# Add center of mass point, which is the average / mean of g_hat = g_t*exp(-2*pi*i*samp_f[30]*time)
# We have to extract the real and imaginary number in order to get the points coordinates for the polar 
# plot using Re() and Im():
points(x = Re(mean(g_t*exp(-2*pi*i*samp_f[1]*time))), Im(mean(g_t*exp(-2*pi*i*samp_f[1]*time))),
       col = "darkblue", pch = 19)

# Let us have a look at samp_f[30] at first:
plot(g_t*exp(-2*pi*i*samp_f[30]*time), type = "l", col = "darkviolet", 
     # Title entails information on winding/sample freq and original cosine freq
     main = paste("Winding Frequency of", samp_f[30],"Hz", "\n", paste("Cosine Frequency of", freq,"Hz")), 
     xlab = "Real Numer",
     ylab = "Imaginary Number") 

# Add center of mass point, which is the average / mean of g_hat = g_t*exp(-2*pi*i*samp_f[30]*time)
# We have to extract the real and imaginary number in order to get the points coordinates for the polar 
# plot using Re() and Im():
points(x = Re(mean(g_t*exp(-2*pi*i*samp_f[30]*time))), Im(mean(g_t*exp(-2*pi*i*samp_f[30]*time))),
       col = "darkblue", pch = 19)
# Reset grid:
par(mfrow = c(1,1))

# g_t and the code below for our wound up cosine wave regarding one winding/sampling 
# frequency produce very lengthy vectors. In the below case it's complex numbers, i.e. 
# coordinates of our polar grid for our wound up cosine wave. Plotting multiple 
# plots (101) with 10001 paired Re and Im values is a lot. 
length(g_t*exp(-2*pi*i*samp_f[1]*time))
# [1] 10001 !!!

##### Now we look at a sampling frequency of 0, .25, 1 and 1.5 Hz:
# Set plot grid and sampling frequencies:
par(mfrow=c(2,2))
samp_freq = c(0,.25,1,1.75)
# Let us have a look at samp_f = 0 at first. NOTE THAT WE ADDED A CONSTANT OF 1, so that 
# our wound up cosine wave gets shifted to the right (as in the 3blue1brown videp):
constant = 0 # Set to one to see the changes
for(index in 1:length(samp_freq)){
  plot(g_t*exp(-2*pi*i*samp_freq[index]*time)+constant, type = "l", col = "darkviolet", 
       # Title entails information on winding/sample freq and original cosine freq
       main = paste("Winding Frequency of", samp_freq[index],"Hz", "\n", paste("Cosine Frequency of", freq,"Hz")), 
       xlab = "Real Numer",
       ylab = "Imaginary Number",
       ylim = c(-2,2),
       xlim = c(-2,2)) 
} #End for index

# Reset plot grid
par(mfrow = c(1,1))

# Writing a function to do the above for all samp_f:
fourier_trans = function(g_t,samp_f,time){
  
  ### Plot all 101 plots via basic plot(). Use the arrows to mimic a gif of the transition, using different sampling 
  ### frequencies. Using for loop for the plotting:
  
  # Set complex number "i":
  i = complex(real = 0, imaginary = 1) # beware to NOT USE "i" as index variable in a for loop from now on!!
  
  # Min max of each Re and Im for flexible plotting routines (limiting the y- and x-axis of a plot):
  min_max = matrix(0, ncol = 4, nrow = length(samp_f))
  for(index in 1:length(samp_f)){
    min_max[index,1] = min(Re(g_t*exp(-2*pi*i*samp_f[index]*time)))
    min_max[index,2] = min(Im(g_t*exp(-2*pi*i*samp_f[index]*time)))
    min_max[index,3] = max(Re(g_t*exp(-2*pi*i*samp_f[index]*time)))
    min_max[index,4] = max(Im(g_t*exp(-2*pi*i*samp_f[index]*time)))
  } # End for index
  
  
  # Plotting: 
  for(index in 1:length(samp_f)){ # don't call it i when i was defined above as a constant variable!!!
    
    plot(g_t*exp(-2*pi*i*samp_f[index]*time), type = "l", col = "darkorchid4", 
         # Title entails information on winding/sample freq and original cosine freq:
         main = paste("Winding Frequency of", samp_f[index],"Hz"), 
         xlab = "Real Numer",
         ylab = "Imaginary Number",
         # The python script doesn't do this, always centers the the plot
         xlim = c(min(min_max[,1]),max(min_max[,3])), # making sure that plot is not out of bounds
         ylim = c(min(min_max[,2]),max(min_max[,4]))) 
    
    # Add center mass point == integral of g_t*exp(-2*pi*i*samp_f[index]*time)
    points(x = Re(mean(g_t*exp(-2*pi*i*samp_f[index]*time))), Im(mean(g_t*exp(-2*pi*i*samp_f[index]*time))),
           col = "darkblue", pch = 19)
    
  } # End for index
  
  # Matrix for all the means of g_hat for Re and Im:
  g_hat_mean = matrix(0, ncol = 2, nrow = length(samp_f))
  
  # Mean of Re and Im for plot of the center of mass further below
  for(index in 1:length(samp_f)){
    g_hat_mean[index,1] = Re(mean(g_t*exp(-2*pi*i*samp_f[index]*time)))
    g_hat_mean[index,2] = Im(mean(g_t*exp(-2*pi*i*samp_f[index]*time)))
  } # End for index
  
  # Function of the center of mass of the wound up graph, peaking at the 
  # frequency or frequencies of the initial cosine wave!
  plot(x = samp_f, y = g_hat_mean[,1], type ="l",   # plot of mean(Re)
       xlab = "Winding Frequency",  # rename labels
       ylab = "Mean of g_hat(f)",
       main = "Function of the Center of Mass for each Winding Frequency",
       col = "deepskyblue3", ylim = c(min(min(g_hat_mean[,1]),min(g_hat_mean[,2])), 
                                      max(max(g_hat_mean[,1]),max(g_hat_mean[,2]))))
  lines(x = samp_f, y = g_hat_mean[,2], type = "l", col = "hotpink2") # Add mean(Im) to plot
  axis(1, at = seq(min(samp_f),max(samp_f), by = 1)) # Adjust x-axis tick mark (every integer step)
  legend("bottomright",legend = c("Re","Im"),               # add legend
         col = c("deepskyblue3","hotpink2"),  # legend line color
         lty = 1,   # line type of legend (corresponding to plot)
         cex = .75) # size of legend in ratio to standard size
  
  # Smoothed out version (similar to python version):
  g_hat_smooth = g_hat_mean
  for(index in 1:length(g_hat_smooth[,1])){
    if(g_hat_smooth[index,1]<=max(g_hat_smooth[,1])/2){ # Filter for values below Nyquist frequency
      g_hat_smooth[index,1] = 0
    } # End if 
  } # End for i
  
  # Change to data.frame for ggplot and rename columns:
  g_hat_smooth = as.data.frame(g_hat_smooth)
  colnames(g_hat_smooth) = c("Re","Im")
  
  # Bar plot via ggplot:
  bar = ggplot(g_hat_smooth)+ 
    geom_bar(aes(x=samp_f, y=Re), 
             stat="identity", # frequencies on y-axis
             fill="black",  # color bars
             alpha=0.7)+ # opacity
    theme_minimal()+ # white instead of grey background
    scale_x_continuous(breaks = seq(min(samp_f),max(samp_f),by=.5)) # adjust breaks x-axis tick marks
  # Print ggplot
  print(bar)
  
  # Plot original cosine wave:
  plot(x = time, y = g_t, type = "l", xlab = paste("Time in Seconds", "\n",
                                                   freq,"periods in 1 Second ==",freq,"Hz"), 
       ylab = "g_t = cos(2*pi*freq*time)", 
       col = "deepskyblue3") 
  
} # End of function fourier_trans

# Test the function. 
# UNCOMMENT TO TEST FUNCTION!! 
# UNCOMMENT TO TEST FUNCTION!! 
# UNCOMMENT TO TEST FUNCTION!!
# UNCOMMENT TO TEST FUNCTION!! 
# UNCOMMENT TO TEST FUNCTION!! 
# UNCOMMENT TO TEST FUNCTION!!

#fourier_trans(g_t,samp_f,time)


##########################################################################
# 2.2.3 Creating a .gif Animation of Successively Winding Up Cos(2*pi*t) #
##########################################################################

##### Alternative plotting of wound up cosine wave via ggplot:
# Matrix for all the means of g_hat for Re and Im:
g_hat_mean = matrix(0, ncol = 2, nrow = length(samp_f))

# Mean of Re and Im for plot of the center of mass further below
for(index in 1:length(samp_f)){
  g_hat_mean[index,1] = Re(mean(g_t*exp(-2*pi*i*samp_f[index]*time)))
  g_hat_mean[index,2] = Im(mean(g_t*exp(-2*pi*i*samp_f[index]*time)))
} # End for index

# Turn into data.frame:
g_hat_mean_df = as.data.frame(g_hat_mean)
colnames(g_hat_mean_df) = c("Re","Im")

# Min max of each Re and Im for flexible plotting routines (limiting the y- and x-axis of a plot):
min_max = matrix(0, ncol = 4, nrow = length(samp_f))
for(index in 1:length(samp_f)){
  min_max[index,1] = min(Re(g_t*exp(-2*pi*i*samp_f[index]*time)))
  min_max[index,2] = min(Im(g_t*exp(-2*pi*i*samp_f[index]*time)))
  min_max[index,3] = max(Re(g_t*exp(-2*pi*i*samp_f[index]*time)))
  min_max[index,4] = max(Im(g_t*exp(-2*pi*i*samp_f[index]*time)))
} # End for index

# Set up list, otherwise plotting is not possible using ggplot via a loop:
plots = list()

# Alternative plot of wound up cosine waves via ggplot():
for(index in 1:length(samp_f)){ # don't call it "i" when "i" was defined above as a constant variable!!!
  # Create data frame with coordinates of wound up function for respective samp_f[index]:
  data = as.data.frame(cbind(Re(g_t*exp(-2*pi*i*samp_f[index]*time)), Im(g_t*exp(-2*pi*i*samp_f[index]*time))))
  
  # Adjust colnames:
  colnames(data) = c("Re","Im")
  
  # Extract coordinates for point of central mass:
  point = g_hat_mean_df[index,]
  
  # Plot via ggplot:
  plots[[index]] =  ggplot(data, aes(x = Re, y = Im)) +
    geom_path(color = "darkorchid4") +  # Color for line of the plot
    geom_point(data = point, aes(x = Re, y = Im), # add point of central mass
               color = "darkblue", size = 3) +  # color and size of the point
    labs(title = paste("Sample Frequency", samp_f[index], "Hz"), 
         x = "Real Number", y = "Imaginary Number") +
    xlim(c(min(min_max[,1]),max(min_max[,3]))) +  # making sure that plot is not out of bounds
    ylim(c(min(min_max[,2]),max(min_max[,4]))) +  # and not just centralized
    theme_minimal() # white instead of grey background
  
} # End for index

# Look at single plot:
plots[[30]]

# Plot all plots 
#print(plots)

# Create gif all the plots above:
#install.packages("magick")
library("magick")

# UNCOMMENT TO EXECUTE BELOW!

# Convert plots to image. This works a little weird: First you
# create img_plot and execute an image_graph object.
#img_plot = image_graph(width=318,height=362, res = 96)
# Then you just print all the plots onto it, so to speak:
#print(plots)
# Then you create an animation object:
#anime = image_animate(image_join(img_plot), fps = 5)

# Export .gif image
#image_write(anime, "fft_animation.gif")


######################################################
# 2.3 Fast Fourier Transformation (Cooley-Tukey FFT) #
######################################################

# Comparing N^2 and N*log_2 of N - below an example for 64 data points:
n = 64 
n^2
# [1] 4096
n*log2(n)
# [384]

# Presented as plot of functions:
n = c(1:100)
nsquare = n^2
nlog2n = n*log2(n)
plot(n, nsquare, type = "l", col = "blue", ylab = "Number of Calculations")
lines(n, nlog2n, type = "l", col = "deeppink")
  
#### Overlapping of cosine waves, as in the Veritasium video:
# time seq from 0 to 1Hz 
time = seq(0,1,by = .0001)

wave1hz = cos(2*pi*time*1)
wave2hz = cos(2*pi*time*2)
wave4hz = cos(2*pi*time*4)
wave8hz = cos(2*pi*time*8)
wave16hz = cos(2*pi*time*16)
plot(wave1hz, type = "l", col = "blue")
lines(wave2hz, type = "l", col = "green") 
lines(wave4hz, type = "l", col = "lightblue")
lines(wave8hz, type = "l", col = "darkviolet")
lines(wave16hz, type = "l", col = "orange")

#### Arithmetics with vectors of unequal length, where v1 is not a multiple of v2:
c(1,1) + c(1,2,3)
# [1] 2 3 4
# Warning:
# In c(1, 1) + c(1, 2, 3) : longer object length is not a multiple of shorter object length
c(1,1) * c(1,2,3)
# [1] 1 2 3
# Warning:
# In c(1, 1) * c(1, 2, 3) : longer object length is not a multiple of shorter object length

#### Arithmetics with vectors of unequal length, where v2 has twice the length of v1:
c(1,1) + c(1,2,3,4)
# [1] 2 3 4 5
c(1,1) * c(1,2,3,4)
# [1] 1 2 3 4

#### Example Cooley Tukey FFT function, based on Wikipedia pseudo code:
ct_fft = function(signal) {
  # Base case
  if (length(signal) == 1){ 
    return(signal)  # trivial DFT base case with length(signal) == 1
  } # End if length(signal) == 1

  else{
    # Recursive FFT on the even and odd index parts of the input:
    even_val = ct_fft(signal[seq(1,length(signal),by = 2)])  # DFT of even indices
    odd_val  = ct_fft(signal[seq(2,length(signal),by = 2)])  # DFT of odd indices
    
    # Twiddle factor = exp((-2*i * pi * k)/ length(signal)) - below 2i instead of 2*i!
    k = c(0:((length(signal)/2)-1)) # integer from 0 to (N/2)-1  
    twiddle_factor = exp((-2i*pi*k)/length(signal))
    
    # Combining the DFT of the two halves:
    even_plus = even_val + twiddle_factor*odd_val
    even_minus = even_val - twiddle_factor*odd_val
  } # End else
  return(c(even_plus, even_minus)) # vector with results
} # End of function ct_fft()

# Sine wave with freq of 2 as test signal with length 8^2:
x = seq(0,2*pi, length.out = 64) # make sure N == power of 2, here 8^2!
signal = sin(x*2) # our example signal

# Check for equality:
all.equal(ct_fft(signal), fft(signal)) # Some minor deviations, so == does not result in all TRUE!
# [1] TRUE

#### Discrete Fourier Transformation:
d_ft = function(signal){
  k = c(0:(length(signal)-1)) 
  i = complex(real=0,imaginary=1)
  result = 0
  # Looping over k (harmonics):
  for(index in 0:(length(signal)-1)){ # Here the index is part of the formula
    # referring to the time-step/domain
    # and has to start with zero - R vectors 
    # start with one, so it needs some work around!
    result[index+1] = sum(signal*exp((-2*pi*i*index*k)/length(signal)))
  }# End for index
  return(result)
} # End of d_ft

#### Understanding k^th harmonics of a signal:
x = seq(0,2*pi, length.out = 16) # make sure N == power of 2, here 4^2!
x2 = seq(0,2*pi, length.out = 1000) # for lines() plot of k^th freq with higher resolution
signal = sin(x*2) # our example signal
k = c(0:((length(signal)/2)-1)) # integer from 0 to (N/2)-1 

# Plot discrete data points:
plot(x, signal, ylab = "sine(x*2)", xlab = paste("Discrete Sampling Points of the Signal incl. k^th Harmonic.","\n","N_signal = 16. We see 7 sine waves, since k[1]=0."))

# In the case of a sine wave as signal, we can see that the k^th harmonic 
# with a frequency of 2 lays exactly on all the points of our initial signal sin(x*2):
for(index in 1:length(k)){
  lines(x2,sin(x2*k[index]), col = "deeppink")
} # End for index

#### Plot frequency domain of ct_fft(signal):
plot(c(1:length(ct_fft(signal)))-1,abs(ct_fft(signal)), type="l")

# Index of value with the highest magnitude. Index = Freq, since we work with 
# integer frequencies: 
which.max(abs(ct_fft(signal)))-1 # minus 1 to compensate for index starting with 1!
# [1] 2

#### Alternative formula for Fourier series of sine wave sin(-2*pi*2*x):
all.equal(Re((exp(2*pi*i*2*x)-exp(-2*pi*i*2*x))/2*i), sin(-2*pi*2*x))
# [1] TRUE
Re((exp(2*pi*i*2*x)-exp(-2*pi*i*2*x))/2*i) == sin(-2*pi*2*x)
# => Also all TRUE

#### To get rid of the symmetry we can just plot half of the frequency domain:
plot((c(1:length(ct_fft(signal)))-1)[1:length(signal)/2],abs(ct_fft(signal))[1:length(signal)/2], type="l")

#### Alternative with adjusted x-axis using 1D fftshift matlab style:
#### Function based on Matlab documentation: https://de.mathworks.com/help/matlab/ref/fftshift.html
fftshift1D = function(signal){
  x = c(1:length(signal))
  if((length(x) %% 2) == 0){
    xshift = x[c((length(x)/2+1):length(x),c(1:(length(x)/2)))]
  } # End if even
  else if((length(x) %% 2) != 0){
    xshift = x[c(ceiling(length(x)/2+1):length(x),c(1:ceiling((length(x)/2))))]
  } # End else if odd
  fftshift1D = signal[xshift] 
  return(fftshift1D)
} # End of fftshift1D

# Adjust x-axis tick-marks:
x_axis_shift_fft = function(x){
  n = length(x)
  xshift = (c((-(n/2):((n/2)-1)))*n/2/n)*2 # n/2 = Shannon-Nyquist Freq.
                  # xshift = multiply tick-marks by 2, to 
                  # compensate for index starting with 1; 
                  # unshifted x-axis -1 to compensate for index starting with 1...
} # End of x_axis_shift_fft

#### Adjusted plot for positive and negative frequencies (cat silhouette?): 
plot(x_axis_shift_fft(x), Re(fftshift1D(ct_fft(signal))), type = "l", xaxt = "n")
# Adjust tick marks to show every tick mark step; first turn of tick marks of 
# plot() via xaxt = "n":
axis(1, at= seq(min(x_axis_shift_fft(x)),max(x_axis_shift_fft(x),by=1)))

### Function for the inverse discrete Fourier transformation:
inverse_d_ft = function(ft){
  k = c(0:(length(ft)-1)) 
  i = complex(real=0,imaginary=1)
  result = 0
  # Looping over k (harmonics):
  for(index in 0:(length(ft)-1)){ # as DFT, but ft as input, 1/N as factor and +2*pi (!) etc.:
    result[index+1] = (1/length(ft))*sum(ft*exp((+2*pi*i*index*k)/length(ft)))
  }# End for index
  return(result)
} # End of inverse_d_ft

# Reconstruct original sine wave from complex numbers from DFT/FFT/FT:
xift = seq(0,2*pi, length.out = 1000) # for better resolution of IFT signal
signal = sin(xift*2) # our example signal
plot(xift, inverse_d_ft(d_ft(signal)), type = "l")
abline(h=0) # add axis at y=0

# Check for equality of the signal and the IFT of the signal:
all.equal(signal,Re(inverse_d_ft(d_ft(signal))))
# [1] TRUE


#######################################
# 2.3.1 Benchmarking FFT and DFT in R #
#######################################

#### Benchmark test ct_fft() with fft() an regular d_ft():
# New example sine wave signal with larger length:
x = seq(0,2*pi, length.out = 1024) # make sure N == power of 2
signal = sin(x*2) # our example signal

# Execute the next lines all at once!!!!!!
start_ct_fft = Sys.time()
ct_fft(signal)
end_ct_fft = Sys.time()

# Calculate time difference; note that there is a little error rate, so
# results differ when benchmarking / executing the code:
time_bench_ct_fft = end_ct_fft - start_ct_fft
# Time difference of Time difference of 0.07637405 secs 

#### Same with fft() R function:
start_fft = Sys.time()
fft(signal)
end_fft = Sys.time()
time_bench_fft = end_fft - start_fft
# Time difference of 0.02464104 secs # faster than ct_fft().

#### Benchmark regular d_ft:  
start_ft = Sys.time()
d_ft(signal)
end_ft = Sys.time()
time_bench_ft = end_ft - start_ft
# Time difference of 0.132072 secs # much slower than .07 for ct_fft() and .2 secs fft()

# Check for equality for all three methods:
all.equal(ct_fft(signal),fft(signal))
# [1] TRUE
all.equal(ct_fft(signal),d_ft(signal))
# [1] TRUE
all.equal(d_ft(signal),fft(signal))
# [1] TRUE


########################################################
# 2.4 The Relation between Rect and Sinc Functions and #
# Their Use for Low-Pass Fourier Space Filtering       #
########################################################

# Sinc function - un-normalized:
sinc_fun = function(x){
  sinc_res = sin(x)/x
  return(sinc_res)
} # End of sinc_fun

# Plot un-normalized sinc function:
x = seq(-8*pi,8*pi, length.out = 4*1024)
y = sinc_fun(x)
plot(x,y, type = "l", col ="blue", ylab = "Sine Cardinal")
abline(h=0,v=0)

# Sinc function - normalized by pi:
sinc_fun_norm = function(x){
  sinc_res = sin(pi*x)/(pi*x)
  return(sinc_res)
} # End of sinc_fun

# Add normalized sinc function to precious plot
lines(x,sinc_fun_norm(x), col = "deeppink", type = "l" )

# Plot of normalized sinc function and cos(x); ylim was adjusted:
plot(x,sinc_fun(x), type = "l", col ="blue", ylab = "Sine Cardinal", ylim = c(-1,1))
abline(h=0,v=0)
lines(x,cos(x), type="l")

# Plot of normalized sinc function and sin(x); ylim was adjusted:
plot(x,sinc_fun(x), type = "l", col ="blue", ylab = "Sine Cardinal", ylim = c(-1,1))
abline(h=0,v=0)
lines(x,sin(x), type="l")


# Rect function, i.e. single pulse square wave (non-periodic!):
rect_fun = function(x){
  rect_res = x
  for(index in 1:length(x)){
    if(abs(x[index]) > .5){
      rect_res[index] = 0
    } # End if   
    else if(round(abs(x[index]),1) <= .5){
      rect_res[index] = 1
    } # End else if
  } # End for index
  return(rect_res)
} # End of rect_fun

# Plot rect_fun with xlim from -1 to 1:
plot(x,rect_fun(x), type = "l", xlim = c(-1,1))

### Calculate quasi-CTFT - FT of rect_fun:
x = seq(-2*pi,2*pi, by = .001)
freq_range = seq(-2*pi,2*pi,by = .01)
# Initialize empty vector:
signalFT = c()
for (index in 1:length(freq_range)){
  signalFT[index] = sum(rect_fun(x)*exp(-2i*pi*freq_range[index]*x))/length(x)
} # End for index

# Plot FT of rect_fun => sinc_fun; x-axis divided by 10 for freqsteps of .01:
plot(x_axis_shift_fft(c(1:length(signalFT)))/10,signalFT, type = "l", main = "CTFT of rect(x) Function == sinc(x)", col = "deeppink")
abline(h=0,v=0)

### Calculate quasi-inverse-CTFT - IFT of FT of rect_fun:
x = seq(-2*pi,2*pi, by = .001)
freq_range = seq(-2*pi,2*pi,by = .01)
# Initialize empty vector:
IFTofSignalFT = c()
for (index in 1:length(x)){
  IFTofSignalFT[index] = sum(signalFT*exp(2i*pi*freq_range*x[index]))/length(x)
} # End for index

# Plot IFT of FT of rect_fun; x-axis divided by 1000 for time-steps of .001:
plot(x_axis_shift_fft(c(1:length(x)))/1000,IFTofSignalFT, type = "l", xlim = c(-2,2), main = "ICTFT of the CTFT of the rect(x) Function", col = "deeppink")
abline(h=0,v=0)

### Calculate CTFT of sinc(x) itself => also results in rect(x):
x = seq(-2*pi,2*pi, by = .001)
freq_range = seq(-2*pi,2*pi,by = .01)
# Initialize empty vector:
FTsinc = c()
for (index in 1:length(freq_range)){
  FTsinc[index] = sum(sinc_fun(x)*exp(-2i*pi*freq_range[index]*x))/length(x)
} # End for index

# Plot FT of sinc(x)
plot(x_axis_shift_fft(c(1:length(FTsinc))),FTsinc, type = "l",xlim = c(-50,50) ,main = "CTFT of sinc(x) Function == rect(x)", col = "deeppink")
abline(h=0,v=0)



###########################################################
# 3 2D Fourier Transformation and a FFT on a 2D JPG Image #
###########################################################





### Looking at the Frequency Space of a JPG Image:

#install.packages("imager")
library("imager") # for load.image() and grayscale()

# Load image of choice (test_img used here):
image = load.image("test_img.jpg")

# Turns RGB into gray scale:
image = grayscale(image) 

# Plot image to test if upload was correct and gray scaling was correct:
plot(image, main = "Original Image, Gray Scaled")

# Perform Fourier Transformation (via Fast Fourier Transformation == fft()):
image_fft = fft(image)

# Calculate the log scale of the magnitude of the frequency spectrum, 
# commonly used for better visualization (given wide range and difference 
# between peaks, which makes visualization harder in a linear y-axis scale;
# the below therefore compresses larger values). Additionally The "+1" avoids 
# log(0) situations and also makes no difference for small values! 
log(1)     # 0.0
log(1+.25) # 0.2231436  # remains close to .25 (little changes only) 
log(1+6)   # 1.94591
log(1+200) # 5.303305   # from 200 to around 5! This value gets highly compressed!

# Mod() can handle complex() numbers and 
# Mod(z) = sqrt(x^2+y^2) == calculates the magnitude of the frequency domain 
# (see documentation for details). 
# Finally the log scale of the magnitude:
fft_magnitude = log(1 + Mod(image_fft)) 
#fft_magnitude = Mod(image_fft) # Check result without log(1+x)

# Compare plot of original and frequency spectrum image
par(mfrow = c(2, 2))  # Set up plot grid 1 row 2 cols

# Plot Org. Image and Frequency Domain Image:
plot(image, main = "Original Image", axes = FALSE)
plot(fft_magnitude, main = "Frequency Spectrum or Domain / k-Space", axes = FALSE)

# Perform inverse Fourier Transformation to reconstruct original image from frequency domain:
plot(fft_magnitude, main = "Original Frequency Spectrum or Domain / k-Space", axes = FALSE)
image_recon = Re(fft(image_fft, inverse = TRUE)) / length(image_fft)
plot(image_recon, main = "Reconstruction of Orig. Image via Inverse FFT", axes = FALSE)

# Reset plot grid to 1x1:
par(mfrow = c(1, 1))  



#####################################################################################
# 4 Performing FFT and IFT on MRI Data and Understanding the 2D k-Space of an Image #
#####################################################################################

# Install oro.dicom to read .dcm files:
#install.packages("oro.dicom")
library("oro.dicom")

# Load example data: 
data = readDICOMFile("kspace/example_sts.dcm")
data$hdr
# Load example data from k-space explorer
# https://github.com/birogeri/kspace-explorer/blob/master/images/default.dcm
#data = readDICOMFile("kspace/default.dcm")

# Plot original DICOM image (with dark color scheme grey(0:64/64), grey.color(256)):
image(t(data$img), col = grey(0:64/64), axes = FALSE,
      main = "Original DICOM Image")

# Perform fft on data:
kspace = fft(data$img)

# k-space without fftshift2D(), again we use log magnitude and + 1
# Lowest frequency is placed in the periphery/corners, highest frequency in the middle:
# k-space images in Neuroscience are usually shown differently: 
image(log(1 + Mod(kspace)), col = grey.colors(256), axes = FALSE, main = "k-Space without shift/reshaping")


# The below function will reshape in the following way (compare
# Matlab documentation of fftshift() that inspired the below function
# https://de.mathworks.com/help/matlab/ref/fftshift.html):

#
#
#     A   B                      D   C  
#
#     C   D    transformed to:   B   A  
# 
#

#  2D fftshift() Matlab style (requires dims to be an integer when divided by 2):
fftshift2D = function(kspace) {  #
  # For better readability - could also be done via ncol() and nrow()
  rows = nrow(kspace) # Evaluate n of rows
  cols = ncol(kspace) # ... n of cols
  reshape_row = c((rows/2+1):rows, 1:(rows/2))  # rows/2+1 so it starts at first position second half
  # not last position of first half!
  reshape_col = c((cols/2+1):cols, 1:(cols/2))  # same here...
  kspace[reshape_row,reshape_col]               # reshape k-space
} # End of function fftshift2D()

# Shifted k-space image (log magnitude + 1):
kspace_shifted = fftshift2D(kspace)

# Plot shifted/reshaped image, now with low frequencies in the center, instead the
# output matrix corners:
image(log(1 + Mod(kspace_shifted)), col = grey.colors(256), axes = FALSE, 
      main = "Reshaped k-space")

# Reconstructing original image from k-space via IFT (includes normalization factor): 
IFT_image = Re(fft(kspace, inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey.colors(256), main = "Reconstructed Image via inverse FFT", axes = FALSE)

# Darker grey scale scheme:
image(t(IFT_image), col = grey(0:64/64), main = "Reconstructed Image via inverse FFT", axes = FALSE)



##############################################################################################
# 4.1 Exploring the k-Space and Plotting Gradient Plots Corresponding to Spatial Frequencies #
##############################################################################################

# Code for plotting gradients in general and explaining how the outer() function roughly works:

# Plotting a cos() wave with a vertical gradient and a horizontal
# gradient, representing both 

# Define matrix size / resolution:
size = 100  

# Create a matrix to plot a vertical grey scale gradient. Think of 
# both as a times series from 0 to 1 Hz. We need two to plot a 
# gradient, as often done in tutorials on k-spaces. We will use
# the outer() function, which need an input x and y.
x = seq(-1, 1, length.out = size) # 1 Hz 
y = seq(-1, 1, length.out = size)

# Combine as matrix:
xy = cbind(x,y)

# The resulting matrix is rather simple,
# since x == y. The image() function, using gray.colors(256) as parameter,
# turns the below into a gradient from left to right, where the darker the
# grey colors, the lower the value of x==y. 

#               x          y
#  [1,] 0.00000000 0.00000000
#  [2,] 0.01010101 0.01010101
#  [3,] 0.02020202 0.02020202
#  [4,] 0.03030303 0.03030303
#  [5,] 0.04040404 0.04040404
#  [6,] 0.05050505 0.05050505
#  [7,] 0.06060606 0.06060606
#  [8,] 0.07070707 0.07070707
#  [9,] 0.08080808 0.08080808
# [10,] 0.09090909 0.09090909
# [11,] 0.10101010 0.10101010
# ...
# ...
# ...

# Set plot grid to 2x4 for all of the below plots:
par(mfrow = c(2, 4))  


# Plot the image via image() with grey scaling. 
image(xy, col = gray.colors(256))

# Mirrored horizontally:
xy = cbind(rev(x),rev(y))
image(xy, col = gray.colors(256))


# This will flip the gradient 90° clock-wise
z = outer(x, y, function(x,y){cos(y)})
# Alternative without outer(). Creates 100 identical rows of cos(y) with length 100: 
z2 = matrix(0, nrow = length(x), ncol = length(y))
for(index in 1:length(x)){
  z2[index,] = cos(y)
} # End for i
z == z2 # All true
image(x,y,z, col = gray.colors(256))

# The outer function above creates a 100*100 matrix where the lines are all
# equivalent to cos(y). Since the function within outer only refers to y so rows are identical
# and the values in each column are too.
# For the first line:
z[1,]==cos(y)
#  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
# [21] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
# [41] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
# [61] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
# [81] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

# For all lines:
list_res = list()
for(index in 1:length(z[,1])){
  list_res[[index]] = z[index,] == cos(y)
} # End for i

list_res # ... all TRUE; it is a long list, so see for yourself

# This will flip the gradient 90° anti-clockwise:
z = outer(x, y, function(x,y){cos(rev(y))})
image(x,y,z, col = gray.colors(256))

# This will flip 90° and horizontal gradient peak is now in the middle 
# for 1 amplitude peak = 1Hz frequency
z = outer(x, y, function(x, y) cos(2*pi*y))
image(x,y,z, col = gray.colors(256))

# This will have a vertical gradient peak in the middle for 
# 1 amplitude peak = 1Hz frequency
z = outer(x, y, function(x, y) cos(2*pi*x))
image(x,y,z, col = gray.colors(256))


# This will flip 90° and horizontal gradient peak is now in the middle 
# for 1 amplitude peak = 2Hz frequency
z = outer(x, y, function(x, y) cos(2*pi*y*2))
image(x,y,z, col = gray.colors(256))

# This will have a vertical gradient peak in the middle for 
# 1 amplitude peak = 2Hz frequency
z = outer(x, y, function(x, y) cos(2*pi*x*2))
image(x,y,z, col = gray.colors(256))

#### Reset plot grid to 1x1:
par(mfrow = c(1, 1))  

# This will have a vertical gradient peak in the middle for 
# 1 amplitude peak = 2Hz frequency
z = outer(x, y, function(x, y) cos(2*pi*x*2))
image(x,y,z, col = gray.colors(256))


################ Example using one of the DICOM images:

# Chose which pixel you want to evaluate (frequency/magnitude, amplitude, phase).
# To be able to do so, we will use the un-shifted k-space matrix below:
pxx = 10
pxy = 10

# Log magnitude of k-space, otherwise frequency is to high to visualize:
# Log magnitude compression of values (result equivalent to amplitude, the 
# intensity of each frequency of a wave):
magnitude = log(1 + Mod(kspace[pxx,pxy]))
magnitude = log(1 + sqrt(Re(kspace[pxx,pxy])^2+Im(kspace[pxx,pxy])^2))
# [1] 16.21987 for pxx = 1 and pxy = 1

# The modulus / absolute value of a complex number is the real part only when 0*i.
Mod(kspace[1,1])
# [1] 11071328

# In R the function abs() can also handle complex numbers just as Mod():
Mod(kspace[50,50]) == abs(kspace[50,50])  
# For p kspace[1,1] it is very high: 11071328, therefore we take the log()

# Calculate phase via artan2(), Arg() or angle() for complex numbers, 
# converting complex to polar coordinates. Below very similar to Matlab!
phase = atan2(Im(kspace[pxx,pxy]), Re(kspace[pxx,pxy])) # via atan2()
phase = Arg(kspace[pxx,pxy])                            # via Arg()
library("pracma") # for angle()
phase = angle(kspace[pxx,pxy])                          # via angle()

# Set imaginary number:
im = complex(imaginary = 1, real = 0)

# Reverse the above:
all.equal(kspace[pxx,pxy], (abs(kspace[pxx,pxy])*exp(im*angle(kspace[pxx,pxy]))))


##### Each k-space pixel frequency as gradient (using DICOM image examples):

kspace = kspace
x = seq(-1,1,length.out = 100)
y = seq(-1,1,length.out = 100)

#### Set plot grid to 1x2:
par(mfrow = c(1, 2))  

# Choose point within the k-space image/matrix again (here pixels / matrix position, unshifted):
pxx = nrow(kspace)/2   # for kx = 0
pxy = nrow(kspace)/2+4 # for ky = -4
pxx = nrow(kspace)/2+25
pxy = nrow(kspace)/2-25

# Plot shifted/reshaped image, now with low frequencies in the center, instead the
# output matrix corners:
image(log(1 + Mod(kspace_shifted)), col = grey(0:64/64), axes = FALSE, 
      main = "Reshaped k-space")
abline(h = 0.5, v = .5)
text(x = .65, y = .95, "ky = Im()")
text(x = .85, y = .45, "kx = Re()")

# Add respective point of the fft() / frequency domain of that value that will
# be shown as gradient
points(x = pxx/length(kspace[1,]) , y = 1-(pxy)/length(kspace[,1]), 
       col = "darkviolet", pch = 16, cex = 1.5)

# Plot frequency, phase and amplitude/magnitude as gradient.
# Extract spatial frequency from k-space matrix (encoded in the matrix index position):
fxfy = function(pxx,pxy, nrow){
  if(pxx <= nrow/2){
    fx = -((nrow/2)-pxx)
  } # end if
  else if(pxx > nrow/2){
    fx = pxx-(nrow/2)
  } # End else
  if(pxy <= nrow/2){
    fy = (nrow/2)-pxy
  } # end if
  else if(pxy > nrow/2){
    fy = -(pxy-(nrow/2))
  } # End else
  return(c(fx,fy))
} # End of function

# Use function
fxfy_res = fxfy(pxx,pxy,length(kspace[,1]))

# Calculate log magnitude and phase: 
magnitude = log(1+Mod(kspace_shifted[pxx,pxy]))
phase = Arg(kspace_shifted[pxx,pxy])

# Plot gradient of frequency of k(fx,fy):
z = outer(x, y, function(x, y) magnitude*cos((2*pi/2)*(fxfy_res[1]*x+fxfy_res[2]*y))) 
image(z, col = grey(0:64/64), axes = FALSE, main = paste("Gradient Plot of kx,ky","\n", "including Magnitude and Phase"))

# If you want, you could try to adjust the above to perform the same on the already shifted k-space

# Note that we could of course also plot the gradient as just a cosine or sine wave:
# Reset grid
par(mfrow = c(1,1))
# Plot frequency phase and amplitude as wave:
time = seq(0,1,length.out = 100)
wave = magnitude*cos(2*pi*time+phase)
#wave = magnitude*cos((2*pi/2)*(fxfy_res[1]*x+fxfy_res[2]*y))+phase
plot(wave, type = "l") # Here phase only shift from left to right, and a angle of pi of the sine wave



#################################################################
# 4.2 Successively (Re-) Constructing an Image from the k-Space #
#################################################################

#### Reset plot grid to 1x2 again:
par(mfrow = c(1, 2))  

# Reconstructing original image from k-space via IFT (includes normalization factor): 
IFT_image = Re(fft(kspace, inverse = TRUE) / length(kspace))

image(t(IFT_image), col = grey(0:64/64), main = "Reconstructed Image via inverse FFT", axes = FALSE)


# PARTIALLY constructing original image from k-space via IFT (includes normalization factor): 
IFT_image = Re(fft(kspace[(nrow(kspace)/2-100):(nrow(kspace)/2+100),(nrow(kspace)/2-100):(nrow(kspace)/2+100)], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = "Partially Reconstructed Image via inverse FFT", axes = FALSE)


#### Set plot grid to 2x2 again:
par(mfrow = c(2, 2))  

# PARTIALLY constructing original image from k-space via IFT (includes normalization factor): 
rangex = 1:10
rangey = 1:10
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT row/col = 1:10"), axes = FALSE)

rangex = 1:100
rangey = 1:100
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT row/col = 1:100"), axes = FALSE)

rangex = 1:200
rangey = 1:200
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT row/col = 1:200"), axes = FALSE)

rangex = 1:ncol(kspace)
rangey = 1:nrow(kspace)
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT row/col = 1:nrow/ncol"), axes = FALSE)

# Only one line and the other entries are set to 0:
kspace2 = kspace
kspace2[2:nrow(kspace2),2:ncol(kspace2)] = 0

IFT_image = Re(fft(kspace2[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("First line, rest = 0"), axes = FALSE)

# Only first two lines and the other entries are set to 0:
kspace2 = kspace
kspace2[3:nrow(kspace2),2:ncol(kspace2)] = 0

IFT_image = Re(fft(kspace2[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("First two lines, rest = 0"), axes = FALSE)

# Only first 10 lines and the other entries are set to 0:
kspace2 = kspace
kspace2[11:nrow(kspace2),2:ncol(kspace2)] = 0

IFT_image = Re(fft(kspace2[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("First 10 lines, rest = 0"), axes = FALSE)

# Only first 20 lines and the other entries are set to 0:
kspace2 = kspace
kspace2[21:nrow(kspace2),2:ncol(kspace2)] = 0

IFT_image = Re(fft(kspace2[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("First 20 lines, rest = 0"), axes = FALSE)


# Only first two entries and all other entries are set to 0:
kspace2 = kspace
kspace2[3:length(kspace2)] = 0 

IFT_image = Re(fft(kspace2[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("First two entries, rest = 0"), axes = FALSE)


# Only one value in the center area ([129,129]) is above 0:
kspace2 = kspace
kspace2[1:(.5*nrow(kspace2)), 1:ncol(kspace2)] = 0 
kspace2[(2+.5*nrow(kspace2)):nrow(kspace2), 1:ncol(kspace2)] = 0 
kspace2[1+.5*nrow(kspace2),.5*nrow(kspace2)] = 0
kspace2[1+.5*nrow(kspace2),1:(.5*ncol(kspace2))] = 0
kspace2[1+.5*nrow(kspace2),(2+.5*ncol(kspace2)):ncol(kspace2)] = 0

IFT_image = Re(fft(kspace2[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("Entry [129,129], rest = 0"), axes = FALSE)

# Only entry [(.75*nrow(kspace2)),128] and all other entries are set to 0:
kspace2 = kspace
kspace2[1:length(kspace2)] = 0 
kspace2[(.75*nrow(kspace2)),128] = kspace[(.75*nrow(kspace2)),128]

IFT_image = Re(fft(kspace2[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("Enrtry [(.75*nrow(kspace2)),128], rest = 0"), axes = FALSE)

# Only entry [(.5*nrow(kspace2)),2*128] and all other entries are set to 0:
kspace2 = kspace
kspace2[1:length(kspace2)] = 0 
kspace2[(.5*nrow(kspace2)),2*128] = kspace[(.75*nrow(kspace2)),128]

IFT_image = Re(fft(kspace2[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("Enrtry [(.5*nrow(kspace2)),2*128], rest = 0"), axes = FALSE)


#### Set plot grid to 1x2:
par(mfrow = c(1, 2))  

# Aliasing effect
rangex = 1:ncol(kspace)
rangey = seq(1,length(kspace[,1]), by = 2)
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT of every 2nd row only."), axes = FALSE)

rangex = seq(1,length(kspace[,1]), by = 2)
rangey = 1:nrow(kspace)
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT of every 2nd column only."), axes = FALSE)

# Reset grid
par(mfrow=c(1,1))

