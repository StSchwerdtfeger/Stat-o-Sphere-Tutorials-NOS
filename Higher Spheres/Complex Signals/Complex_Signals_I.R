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
#install.packages("pracma","ggplot2","magick","oro.dicom","imager","rgl","plotly","patchwork")
library("pracma")    # for angle(), optional... Arg() or atan2() works as well...
library("ggplot2")   # entailed in tidyverse
library("magick")    # creating .gif
library("oro.dicom") # loading DICOM files
library("imager")    # for load.image() - JPG images NEEDS DX11 (WIN) or Xquartz (Mac)!!
library("rgl")       # for persp3d(), interactive 3D plot of surfaces
library("plotly")    # for plot_ly() 3D plot of lines
library("patchwork") # for simple grid syntax for more than 1 plot in .gif (alternative to e.g. grid.extra())

# USE A R PROJECT WITH THIS SCRIPT AND PLACE THE DICOM AND JPG FILE THAT COMES WITH
# THIS TUTORIAL IN YOUR WORKING DIRECTORY FOLDER, THEN THE SCRIPT SHOULD BE
# FULLY EXECUTABLE WITHOUT ANY FURTHER TWEAKING OF THE SCRIPT!


#################################
# 1 Introducing Complex Numbers #
#################################
#####################################################################
# 1.1 History of Complex Numbers Concerning Solutions for Quadratic #
#     and Cubic Equations Entailing the Root of Negative Values     #
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
            # crossing or better: tangently touching the x-axis at y = 0. 

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
# Warning:
# 1: In sqrt(b^2 - 4 * a * c) : NaNs produced
# 2: In sqrt(b^2 - 4 * a * c) : NaNs produced

x1 = (-4 + sqrt((-4)^2-4*(-2)*(-4))) / (2*-(2))
# The part within the square root results in a negative number:
(-4)^2-4*(-2)*(-4)
# [1] -16

# Same in x2:
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
} # End of cubic_roots_la
 
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

# With the above object 0+1i we can create any complex number 
# (alternative use parameters of complex()):
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
grid2Dcomp() # no input value needed!

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


################################################################################################################################
# 1.3 The Relation Between Trigonometry, Euler's Identity / Formula (incl. Taylor/Maclaurin Series Proof) and Complex Numbers  #
#############################################################################################################################
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


#### Calculate the modulus/magnitude/absolute value and argument/phase/angle in R:
Mod(2+2i)     # modulus
abs(2+2i)     # usually for regular absolute value but also works with complex numbers
sqrt(2^2+2^2) # via formula
# The modulus / absolute value /phase of a complex number is the real part only when 0*i,
# since e.g. sqrt(2^2+0^2) = 2
Mod(2+0i)
sqrt(2^2+0^2)
# [1] 2

# Reverse the above:
im = complex(real = 0, imaginary = 1)
all.equal(2+2i, (abs(2+2i)*exp(im*Arg(2+2i))))
# [1] TRUE


# Calculate phase via artan2(), Arg() or angle() for complex numbers, 
# converting complex to polar coordinates. Below very similar to Matlab!
atan2(Im(2+2i), Re(2+2i))          # via atan2()
phase = Arg(2+2i)                  # via Arg()
library("pracma") # for angle(), Matlab style:
phase = angle(2+2i)                # via angle()


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

####### Conjugate of a complex number:
z = 2+2i
# [1] 2+2i
z_conj = Conj(z)
# [1] 2-2i

# Set fresh grid:
grid2Dcomp()

# Draw 2 + 2i and its conjugate:
arrows(x0=0,y0=0,x1=Re(2+2*i), y = Im(2+2*i))
arrows(x0=0,y0=0,x1=Re(2+2*i), y = Im(2-2*i)) # just change the sign from + to - !

# Draw angle using exp(angle*i): 
angle = seq(0,Arg(2+2*i),by = .001)
lines(exp(angle*i), col = "darkviolet") # used e^Arg*i to create angle circle part in the plot below!
lines(exp(angle*-i), col = "darkviolet") # coonjugate angle e^Arg*-i 
text(x = 2.2,y = .5, paste("Arg =", round(Arg(2+2*i),3), "rad", "\n", "= ", (Arg(2+2*i)*180/pi),"°"), col = "darkviolet")
text(x = 2.2,y = -.5, paste("Arg =", round(Arg(2-2*i),3), "rad", "\n", "= ", (Arg(2-2*i)*180/pi),"°"), col = "darkviolet") # Conjugate

# Draw line for Modulus:
# Conjugate HERE 2+2i needed for the Re() part... Better use Conj() function in R
segments(x0=0,y0=0, x1=Im(2+2i),y=Re(2+2i), col = "darkgreen")
segments(x0=0,y0=0, x1=Im(2+2i),y=Re(2-2i), col = "darkgreen") 
segments(x0=0,y0=0, x1=Re(Conj(2+2i)),y=Im(Conj(2+2i)), col = "darkgreen") #
text(x =.75,y = 1.5, paste("Mod = ", round(Mod(2+1i),3)), col = "darkgreen")
text(x =.75,y = -1.5, paste("Mod = ", round(Mod(2-1i),3)), col = "darkgreen") # Conjugate

# Label z = c = 2 + 2i:
text(x = Re(2+2i)+.7, y = Im(2+2i)+.2, "z = 2+2i")
text(x = Re(2-2i)+.7, y = Im(2-2i)+.2, expression(paste(bar(z)," = 2-2i")))


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
# go up to n_max = 100: 
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
  plot(x,base, type = "l", col = "deeppink", ylim = c(-50,50), ylab = "y", main = paste("Taylor Series for n =",length_series))
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
  plot(x,base, type = "l", col = "deeppink", ylim = c(-5,5), ylab = "y", main = paste("Taylor Series for n =",n[length(n)]))
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
  plot(x,base, type = "l", col = "deeppink", ylim = c(-5,5), ylab = "y", main = paste("Taylor Series for n =",n[length(n)]))
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
# Complex Numbers and Euler’s Number.                                 #
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

# Sign of a sine wave = Square wave abstraction / simplification of a sine wave:
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

# Periodicity and why integer frequencies are used for the FS:
x = seq(0,2*pi,by= .01)
plot(x, sin(x), type = "l", col = "blue")
lines(x,sin(2*x), col = "green")
lines(x, sin(2.7*x), col= "red")
abline(v=pi ,lty = 4)
abline(v=pi/2 ,lty = 4)
abline(v=3*pi/2 ,lty = 4)


##### Visualising orthogonality of sine and cosine in comparison 
####  to exp(2*pi*time*i); time = 0-2Hz:
par(mfrow = c(1,2))

# Set up sequence 0-1Hz
time = seq(0,2, by= .001)
x = Re(exp(2*pi*time*i)) # x-axis = real dimension
y = Im(exp(2*pi*time*i)) # y-axis = imaginary / complex domain
z = time                 # temporal dimension 

# Plot exp(2*pi*time*i)
plot(x,y, xlab = "Re(exp(2*pi*time*i))", ylab = "Im(exp(2*pi*time*i))",type="l")

# Draw exp(2*pi*.25*i)
arrows(x0=0,y0=0,x1=Re(exp(2*pi*.25*i)), y = Im(exp(2*pi*.25*i)))

# Draw angle using exp(2*pi*.25*i): 
angle = seq(0,Arg(exp(2*pi*.25*i)),by = .001) # sequence for angle circle and marking of the actual circle
lines(.5*exp(angle*i), col = "violet") # angle circle
lines(1*exp(angle*i), col = "violet")  # matching circle and sine/cosine wave for Hz = .25
segments(x0=0,y0=0,x1=1,y1=0)          # draw line from Re() = 0 to Re() = 1 and 0 on th Im() plane

# Add text for Arg and Angle in Degrees:
text(x = .6,y = .6, paste("Arg = ", round(Arg(exp(2*pi*.25*i)),3),"rad", "\n", "=", (Arg(exp(2*pi*.25*i))*180/pi),"°"), col = "violet", cex=1)
text(x = .2,y = .2, "Arg" , col = "violet", cex=1)

# Draw Modulus of exp(2*pi*.25*i) and add text:
segments(x0=0,y0=0, x1=Re(exp(2*pi*.25*i)),y=Im(exp(2*pi*.25*i)), col = "darkgreen")
text(x =-.4,y = .5, paste("Mod = ", round(Mod(exp(2*pi*.25*i)),3)), col = "darkgreen")

# Label z = c = exp(2*pi*.25*i):
text(x = Re(exp(2*pi*.25*i)), y = Im(exp(2*pi*.25*i)), "z = exp(2*pi*.25*i)")

# Plot corresponding sine wave:
plot(z,y, xlab = "time = 0-2Hz", ylab = "sin(2*pi*time) and cos(2*pi*time)", type="l",
     main = "Dot product: cos(2*pi*.25)*sin(2*pi*.25) = 0")

# Add line from 0-.25 Hz on the sine wave, corresponding to Arg = 90°;
# Setup new shortened sequence:
time2 = seq(0,.25, by= .001)
x2 = time2
y2 = Im(exp(2*pi*time2*i))
lines(x2,y2,col = "violet") # use lines to add to add line from 0-.25Hz to existing plot
# Add cosine plot:
lines(z,x, xlab = "sin(2*pi*time) and cosine(2*pi*time)")
# Setup sequence for cosine wave 0-.25Hz:
x3 = Re(exp(2*pi*time2*i)) 
y3 = time2
lines(y3,x3,col="violet") # add line to plot...
points(.25,round(cos(2*pi*.25),15))
points(.25,round(sin(2*pi*.25),15))
abline(h = 0)

# ####### OPTIONAL ADD Cosine with frequency of 2 -> also orthogonal
# x_freq_2 = Im(exp(2*2*pi*time*i)) # x-axis = real dimension
# lines(z,x_freq_2)
# x3 = Im(exp(2*2*pi*time2*i)) # for pink line
# lines(y3,x3,col="violet") # add line to plot...
# points(.25,round(sin(2*2*pi*.25),15)) # WILL ALSO BE AT ZERO!!
# # Non-integer frequency 2.568:
# x_freq_2.568 = Im(exp(2.568*2*pi*time*i)) # x-axis = real dimension
# lines(z,x_freq_2.568)
# x4 = Im(exp(2.568*2*pi*time2*i)) # for pink line
# lines(y3,x4,col="red") # add line to plot...
# #points(.25,round(sin(2.568*2*pi*.25),15)) # WILL BE SLIGHTY DRIFTED!
# abline(v = .25, lty = 4)

# Rest grid to 1x1:
par(mfrow = c(1,1))

#### The above essentially shows that they are orthogonal:
round(cos(2*pi*.25),15)*round(sin(2*pi*.25),15)
# [1] 0

#### Orthogonality also holds for unequal frequency!
round(cos(2*pi*.25),15)*round(sin(4*2*pi*.25),15)
# [1] 0

#### Orthogonality also holds for unequal frequency, here cos() times cos()!
round(cos(2*pi*.25),15)*round(cos(2*2*pi*.25),15)
# [1] 0

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
lines(x,sin(x), type = "l", col = "green")          # sine
lines(x, g_mn(x), type = "l", col = "deeppink")     # sin*cos
abline(h=0,v=0)
# The integral of g(x) = integral of cos*cos will be clearly above 0 in this case!


#### For pi/2, where a cosine wave and the square wave overlap:
square_wave(pi/2)*cos((1*pi*(pi/2))/pi)
# [1] 6.123032e-17 # roughly zero
all.equal(0, square_wave(pi/2)*cos((1*pi*(pi/2))/pi))
# [1] TRUE


###############################################################################
# 2.2.2 Sampling a Frequency as Winding up a Signal onto to the Complex Plane #
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

# More than one frequency:
#freq1 = 3; freq2 = 6
#amp1 = 3 ; amp2 = 2
#g_t = amp1*cos(2*pi*freq1*time) + amp1*cos(2*pi*freq2*time)

# Square wave:
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
     main = paste("Winding Frequency of", samp_f[1],"Hz", "\n", paste("Cosine Frequency of", freq,"Hz")), 
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


########################################################################################################################
# 2.3 The Discrete Fourier Transformation (DFT and its Inverse) and the Fast Fourier Transformation (Cooley-Tukey FFT) #
########################################################################################################################
#############################################################
# 2.3.1 The Discrete Fourier Transformation and its Inverse #
#############################################################

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
x = seq(0,2*pi, length.out = 16) # make sure N == number to the power of 2, here 2^4!
x2 = seq(0,2*pi, length.out = 1000) # for lines() plot of k^th freq with higher resolution
signal = sin(x*7) # our example signal
k = c(1:((length(signal)/2))-1) # integer from 0 to (N/2)-1 

# Plot discrete data points:
plot(x, signal, ylab = "sine(x*2)", xlab = paste("Discrete Sampling Points of the Signal Sin(x*7) incl. k^th Harmonic.","\n","N_signal = 16. We see 7 sine waves, since k[1]=0."))

# In the case of a sine wave as signal, we can see that the k^th harmonic 
# with a frequency of 7 lays exactly on all the points of our initial signal sin(x*7):
for(index in 1:length(k)){
  lines(x2,sin(x2*k[index]), col = "deeppink")
} # End for index

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


#####################################################################
# 2.3.2 The Fast Fourier Transformation (Cooley-Tuckey Radix-2-FFT) #
#####################################################################

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

# Sine wave with freq of 2 as test signal with length 2^6:
x = seq(0,2*pi, length.out = 64) # make sure N == power of 2, here 2^6!
signal = sin(x*2) # our example signal

# Check for equality with R base function fft():
all.equal(ct_fft(signal), fft(signal)) # Some minor deviations, so == does not result in all TRUE!
# [1] TRUE


######################################################################
# 2.3.3 The Symmetry of Sine or Cosine Waves in the Frequency Domain #
######################################################################

#### Plot frequency domain of ct_fft(signal):
x = seq(0,2*pi, length.out = 16) # make sure N == number to the power of 2, here 2^4!
x2 = seq(0,2*pi, length.out = 1000) # for lines() plot of k^th freq with higher resolution
signal = sin(x*2) # our example signal
k = c(1:((length(signal)/2))-1) # integer from 0 to (N/2)-1 
# Plot:
plot(c(1:length(ct_fft(signal)))-1,abs(ct_fft(signal)), type="l")

# Index of value with the highest magnitude. Index = Freq, since we work with 
# integer frequencies: 
which.max(abs(ct_fft(signal)))-1 # minus 1 to compensate for index starting with 1!
# [1] 2

#### Alternative formula for sine wave sin(-2*pi*2*x):
all.equal(Re((exp(2*pi*i*2*x)-exp(-2*pi*i*2*x))/2*i), sin(-2*pi*2*x))
# [1] TRUE
Re((exp(2*pi*i*2*x)-exp(-2*pi*i*2*x))/2*i) == sin(-2*pi*2*x)
# => Also all TRUE

#### To get rid of the symmetry we can just plot half of the frequency domain:
plot((c(1:length(ct_fft(signal)))-1)[1:length(signal)/2],abs(ct_fft(signal))[1:length(signal)/2], type="l")

#### Alternative with adjusted y-axis using 1D fftshift matlab style:
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

#### Adjusted plot for positive and negative frequencies (cat silhouette? batman?): 
plot(x_axis_shift_fft(x), Re(fftshift1D(ct_fft(signal))), type = "l", xaxt = "n")
# Adjust tick marks to show every tick mark step; first turn of tick marks of 
# plot() via xaxt = "n":
axis(1, at= seq(min(x_axis_shift_fft(x)),max(x_axis_shift_fft(x),by=1)))


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


#############################################################
# 2.4 The Relation between Rect and Sinc Functions and      #
# Their Use for Low-Pass Fourier Space Filtering (incl.     #
# Convolutions and the Riemann Sum Approximation of a CTFT) #      
#############################################################

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

# Add normalized sinc function to previous plot
lines(x,sinc_fun_norm(x), col = "deeppink", type = "l" )

# Sinc can also be written as sine(x)* 1/x, which explains why
# the oscillation is decreasing the further away x is from the center:
par(mfrow = c(1,2))
plot(x, sin(x), type = "l", col = "blue", xlim = c(-4,4), main = "sin(x)")
plot(x, 1/x, type ="l", col = "blue",xlim = c(-.5,.5), main = "1/x")
par(mfrow = c(1,1))

# Check if multiplication of sinc(x) and a asymptote holds:
round(sin(x)*(1/x),7) == round(sinc_fun(x),7)
# All true! Since asymptote(0) = Inf, whe get a NA at the beginng...
# [1]   NA TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

# Plot of un-normalized sinc function and cos(x); ylim was adjusted:
plot(x,sinc_fun(x), type = "l", col ="blue", ylab = "Sine Cardinal", ylim = c(-1,1))
abline(h=0,v=0)
# Add cosine wave:
lines(x,cos(x), type="l")

# Intersections of cosine at index*pi:
for(index in 1:7){ # 
  points(x=+index*pi,y=0) # for the positive
  points(x=-index*pi,y=0) # and negative values of x
} # End for index

# Plot of un-normalized sinc function and sin(x); ylim was adjusted:
plot(x,sinc_fun(x), type = "l", col ="blue", ylab = "Sine Cardinal", ylim = c(-1,1))
abline(h=0,v=0)
# Add sine wave:
lines(x,sin(x), type="l")

for(index in 1:7){ # 
  points(x=+index*pi,y=0) # for the positive
  points(x=-index*pi,y=0) # and negative values of x
} # End for index

# Integral of sinc(x) from -Inf to Inf  == pi!:
sinc_integ = integrate(sinc_fun, lower = 0, upper = 425*pi)$value
# [1] 1.571545
# RESULTS in an error, saying it is "probably divergent" when integrating from -Inf to Inf!
all.equal(sinc_integ,(1/2)*pi)
# [1] "Mean relative difference: 0.0004765778" # => only little difference
# Alternative via Si() sinc integral function from the pracma package:
all.equal(Si(100000*pi),.5*pi)
# [1] "Mean relative difference: 2.026428e-06"

# Interestingly, when we stretch out the function, e.g. sin(x/3)/(x/3), we also get 
# a result of pi for the whole integral:
sinc_stretch3 = function(x){
  res=sin(x/3)/(x/3) 
  res2 = res*sin(x)/x
  return(res2)
} # End of sinc_stretch3
sinc_integ_stretch2 = integrate(sinc_stretch3, lower = 0, upper = 400*pi)$value
all.equal(sinc_integ_stretch2,.5*pi)
# [1] "Mean relative difference: 1.179127e-06" => roughly .5*pi


# Rect function, i.e. single pulse square wave (non-periodic!) with 
# a width of -.5 to .5:
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

### Calculate quasi-CTFT - FT of rect_fun (using Riemann sum approx.):
x = seq(-2*pi,2*pi, by = .001)
freq_range = seq(-2*pi,2*pi,by = .01)

# Initialize empty vector:
signalFT = c()
for (index in 1:length(freq_range)){
  signalFT[index] = sum(rect_fun(x)*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index

# Plot FT of rect_fun => sinc_fun:
plot(x_axis_shift_fft(c(1:length(signalFT))),signalFT, type = "l", main = "CTFT of rect(x) Function == sinc(x)", col = "deeppink")
abline(h=0,v=0)

### Calculate inverse-CTFT - IFT of FT of rect_fun (via Riemann sum approx. using delta omega!):
x = seq(-2*pi,2*pi, by = .001)
freq_range = seq(-2*pi,2*pi,by = .01)
# Initialize empty vector:
IFTofSignalFT = c()
for (index in 1:length(x)){
  IFTofSignalFT[index] = sum(signalFT*exp(2i*pi*freq_range*x[index]))*(freq_range[2]-freq_range[1])
} # End for index

# Plot IFT of FT of rect_fun; x-axis multiplied by delta x this time for time-steps of .001:
plot(x_axis_shift_fft(c(1:length(x)))*(x[2]-x[1]),IFTofSignalFT, type = "l", xlim = c(-2,2), main = "ICTFT of the CTFT of the rect(x) Function", col = "deeppink")
abline(h=0,v=0)

### Calculate CTFT of sinc(x) itself => also results in rect(x):
x = seq(-2*pi,2*pi, by = .001)
freq_range = seq(-2*pi,2*pi,by = .01)
# Initialize empty vector:
FTsinc = c()
for (index in 1:length(freq_range)){
  FTsinc[index] = sum(sinc_fun(x)*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index

# Plot FT of sinc(x)
plot(x_axis_shift_fft(c(1:length(FTsinc))),FTsinc*(x[2]-x[1]), type = "l",xlim = c(-50,50) ,main = "CTFT of sinc(x) Function == rect(x)", col = "deeppink")
abline(h=0,v=0)

####
#### Animation of convolution of rect(x) and rect(x):
####
x = seq(-.5*pi,.5*pi, length.out = 300)
shift_x = x[seq(1,length(x), by= 5)]
f1_vec = rect_fun(x)
f2_vec = rect_fun(x)

### Fast Fourier Convolution Algorithm:
### Showing again that IFT{FFT(a)*FFT(b)} == convolution of a and b:

# Create vector with the successive convolution values x-x_tau for animation:
# Convolution CTFT Riemann sum approx. for f1 and f2:
freq_range  = x
FT_f1 = c()
for(index in 1:length(freq_range)){
  FT_f1[index] = sum(f1_vec*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index
FT_f2 = c()
for(index in 1:length(freq_range)){
  FT_f2[index] = sum(f2_vec*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index

# IFT of the product of the CTFT of a and b:
conv_f1_f2 = c()
for(index in 1:length(x)){
  conv_f1_f2[index] = sum((FT_f1*FT_f1)*exp(2i*pi*freq_range*x[index]))*(freq_range[2]-freq_range[1])
} # End for index

# Create list with convolution of successive length, i.e., first only 1 value then the first 2
# then the first 3 up to all 300:
conv_x_minus_tau = list()
for(index in 1:length(x)){ # actually x tau, but when x same length then x as object enough!
  conv_x_minus_tau[[index]] = data.frame(x=x[1:index],y=Re(conv_f1_f2[1:index]))
} # End for i

# Only every 5th list for animation:
conv_anim = list()
steps = seq(1,length(conv_x_minus_tau), by = 5)
for(index in 1:length(steps)){
  conv_anim[[index]] = conv_x_minus_tau[[steps[index]]]
} # End for index

# Create data frame of f1 and f2 and only 
f1 = data.frame(x,f1_vec)
colnames(f1) = c("x","y")
f2 = data.frame(x,f2_vec)
colnames(f2) = c("x","y")

# Create empty plot lists:
plots_shift = list() # For plot of f2 shifting over steady f1
plots_conv = list()  # For plots of respective convolution
plots_grid = list()  # For the plot grid of the two above plot lists

# For loop to create all the plots for the .gif:
for(index in 1:length(shift_x)){
  # Create objects with coordinates of the shifted function and convolution at x+index:
  data_shift = data.frame(cbind(round(x+shift_x[index],2),f2[,2])) # round x for .gif plots!
  colnames(data_shift) = c("x","y")
  
  # Plot a(tau) * b(x+index - tau): 
  plots_shift[[index]] =  ggplot(f1, aes(x = x, y = y)) +
    geom_path(color = "darkorchid4") +  # Color for line of the plot for f1
    geom_line(data = data_shift, aes(x = x, y = y), # add shifting of f2
              color = "blue") +  # color and size of the point
    labs(title = paste("Shift of f2(", data_shift[index,1], "-tau) over f1(tau)."), 
         x = "tau", y = "rect(x)") +
    xlim(c((min(f1$x)-1),(max(f1$x)+1))) +  # making sure that plot window is fixed
    ylim(c(-.1,2.1)) + 
    theme_minimal() # white instead of grey background
  
  
  # Pot of convolution following successive shift of f2
  plots_conv[[index]] = ggplot(conv_anim[[index]], aes(x=x,y=y))+
    geom_path(color = "deeppink") +  # Color for line of the plot
    xlim(c((min(f1$x)-1),(max(f1$x)+1))) +  # making sure that plot window is fixed
    ylim(c(-.1,2.1)) + 
    labs(title = paste("Convolution of f1(tau) and f2(", data_shift[index,1], "-tau)."), 
         x = "tau", y = "Convolution") +
    theme_minimal() # white instead of grey background
  
  plots_grid[[index]] = plots_shift[[index]] / plots_conv[[index]]
} # End for index

# Convert plots to image. UNCOMMENT TO RUN AND CREATE GIF!!!!!!!
#img_plot_conv = image_graph(width=400,height=400, res = 96)
# Print plots onto img_plot object
#print(plots_grid)
# Then you create an animation object:
#anime = image_animate(image_join(img_plot_conv), fps = 2)
# Export .gif image
#image_write(anime, "conv_anim.gif")


### Showing again that IFT{FFT(a)*FFT(b)} == convolution of a and b:
x = seq(-pi,pi, by = .001)
freq_range = seq(-2*pi,2*pi,by = .01)
a = rect_fun(x)
b = x^2 # parabola

# CTFT Riemann sum approx. for a and b:
FT_a = c()
for(index in 1:length(freq_range)){
  FT_a[index] = sum(a*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index
FT_b = c()
for(index in 1:length(freq_range)){
  FT_b[index] = sum(b*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index

# IFT of the product of the CTFT of a and b:
conv_a_b = c()
for(index in 1:length(x)){
  conv_a_b[index] = sum((FT_a*FT_b)*exp(2i*pi*freq_range*x[index]))*(freq_range[2]-freq_range[1])
} # End for index

# Convolution of rect(x) and x^2
plot(x,conv_a_b, type ="l", main = "Convolution of rect(x) and x^2 via Riemann Sum Approx.")

# IN CASE YOU RAN THE WHOLE SCRIPT you might need to turn off imager
# since it also has a convolve function and masks convolve from "stats":
detach("package:imager", unload = TRUE)

# Comparing above approach via using convolve() in R:
conv = convolve(a,rev(b), type = "open")*(x[2]-x[1]) # incl. delta x
x_adj = x_axis_shift_fft(c(1:length(conv)))
x_adj_scale = x_adj*(x[2]-x[1]) # multiply by delta x not delta x_adj!!
# Plot of Addition of rect(x) and x^2 
plot(x_adj_scale,conv, type = "l", main = "Convolution of rect(x) and x^2")
lines(x, conv_a_b, type="l", col = "blue")

# Compare convolution with addition:
par(mfrow=c(2,2))
# Plot of rect(x):
plot(x,a, type = "l", main  = "rect(x)", xlim = c(-4,4))
# Plot of x^2:
plot(x,b, type = "l", main  = "x^2")
# Plot of convolution of rect(x) and x^2:
conv = convolve(a,rev(b),type = "open")*(x[2]-x[1]) 
x_adj = x_axis_shift_fft(c(1:length(conv)))
x_adj_scale = x_adj*(x[2]-x[1]) # multiply by delta x not delta x_adj!!
# Plot of Addition of rect(x) and x^2 
plot(x_adj_scale,conv, type = "l", main = "Convolution of rect(x) and x^2")
plot(x, (a+b), type = "l", main = "Addition of rect(x) and x^2")
par(mfrow = c(1,1))


##### Showing how sinc and rect work as a low-pass filter:
# Set sinc and .5 sin(x)
x = seq(-4*pi,4*pi, by = .01)
a = sinc_fun(x)
b = .5*sin(x) # sine curve with y_min/max = 0 / 1

# CTFT Riemann sum approx. for a and b:
freq_range  = seq(-4*pi,4*pi, by = .01)
FT_a = c()
for(index in 1:length(freq_range)){
  FT_a[index] = sum(a*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index
FT_b = c()
for(index in 1:length(freq_range)){
  FT_b[index] = sum(b*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index

# IFT of the product of the CTFT of a and b:
conv_a_b = c()
for(index in 1:length(x)){
  conv_a_b[index] = sum((FT_a*FT_b)*exp(2i*pi*freq_range*x[index]))*(freq_range[2]-freq_range[1])
} # End for index

# Plot of sinc and sine with frequency of 1 over four periods:
par(mfrow=c(2,2))
plot(x, a, type = "l", col = "deeppink", ylim = c(-1,1.2), main = "sinc(x) and .5*sin(x); x = -4*pi to 4*pi")
lines(x, b, col = "blue")

# Set sinc and .5 sin(x*5) for comparison
x = seq(-4*pi,4*pi, by = .01)
b = .5*sin(x*5) # sine curve with y_min/max = 0 / 1

# Plot second set of functions sinc(x) and .5*sin(x*5) => now higher frequency than before!!
plot(x, a, type = "l", col = "deeppink", ylim = c(-1,1.2), main = "sinc(x) and .5*sin(x*5); x = -4*pi to 4*pi")
lines(x, b, col = "blue")

# CTFT Riemann sum approx. for a and b:
freq_range  = seq(-4*pi,4*pi, by = .01)
FT_a = c()
for(index in 1:length(freq_range)){
  FT_a[index] = sum(a*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index
FT_b = c()
for(index in 1:length(freq_range)){
  FT_b[index] = sum(b*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index

# IFT of the product of the CTFT of a and b:
conv_a_b2 = c()
for(index in 1:length(x)){
  conv_a_b2[index] = sum((FT_a*FT_b)*exp(2i*pi*freq_range*x[index]))*(freq_range[2]-freq_range[1])
} # End for index

# Add convolutions of the above:
plot(x,conv_a_b,type = "l", col ="blue", main = "Convolution of sinc(x) and .5*sin(x*1)")
plot(x,conv_a_b2,type = "l", col ="blue",main = "Convolution of sinc(x) and .5*sin(x*5)")
par(mfrow=c(1,1))


#### Same for rect function:
# Set rect and 1+.5 sin(x)
x = seq(-4*pi,4*pi, by = .01)
a = rect_fun(x/10)
b = rect_fun(x) 
for(index in 1:length(b)){
  if(b[index] >0){
    b[index] = b[index]+1
  } # End if
} # End for index

# CTFT Riemann sum approx. for a and b:
freq_range  = seq(-4*pi,4*pi, by = .01)
FT_a = c()
for(index in 1:length(freq_range)){
  FT_a[index] = sum(a*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index
FT_b = c()
for(index in 1:length(freq_range)){
  FT_b[index] = sum(b*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index

# IFT of the product of the CTFT of a and b:
conv_a_b = c()
for(index in 1:length(x)){
  conv_a_b[index] = sum((FT_a*FT_b)*exp(2i*pi*freq_range*x[index]))*(freq_range[2]-freq_range[1])
} # End for index

# Plot of rect and rect with frequency of 1 over four periods:
par(mfrow=c(2,2))
plot(x, a, type = "l", col = "deeppink", ylim = c(-.2,2.2), main = "rect(x/10) and rect(x) +1 if > 0; x = -4*pi to 4*pi")
lines(x, b, col = "blue")


#### Two rect functions of same shape:
x = seq(-4*pi,4*pi, by = .01)
a = rect_fun(x)
b = rect_fun(x) 

# Plot second set of function: 
plot(x, a, type = "l", col = "deeppink", ylim = c(-.2,2.2), main = "rect(x) and rect(x); x = -4*pi to 4*pi")
lines(x, b, col = "blue")

# CTFT Riemann sum approx. for a and b:
freq_range  = seq(-4*pi,4*pi, by = .01)
FT_a = c()
for(index in 1:length(freq_range)){
  FT_a[index] = sum(a*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index
FT_b = c()
for(index in 1:length(freq_range)){
  FT_b[index] = sum(b*exp(-2i*pi*freq_range[index]*x))*(x[2]-x[1])
} # End for index

# IFT of the product of the CTFT of a and b:
conv_a_b2 = c()
for(index in 1:length(x)){
  conv_a_b2[index] = sum((FT_a*FT_b)*exp(2i*pi*freq_range*x[index]))*(freq_range[2]-freq_range[1])
} # End for index

# Add convolutions of the above:
plot(x,conv_a_b,type = "l", col ="blue", main = paste("Convolution of rect(x/10) and", "\n", "rect(x) +1 if rect(x) > 0"), ylim = c(0,2))
plot(x,conv_a_b2,type = "l", col ="blue",main = "Convolution of rect(x) and rect(x)", ylim = c(0,2))
par(mfrow=c(1,1))


###########################################################
# 3 2D Fourier Transformation and a FFT on a 2D JPG Image #
###########################################################
#############################
# 3.1 Simple 2D FFT Example #
#############################

# Load library imager:
library("imager")
# IN CASE YOU WANT TO RUN CODE FROM THE PREVIOUS SCRIPT ON CONVOLUTIONS:
# You might need to turn off imager since it also has a convolve function 
# and masks convolve from "stats":
detach("package:imager", unload = TRUE)

#### Example signal in 2D:
signal2D = matrix(c(0,0,0,0,
                    0,1,1,0,
                    0,1,1,0,
                    0,0,0,0), ncol = 4, byrow = TRUE)

# Plot the above matrix, treating the values as grey scale values.
# This can be done via setting the parameter useRaster = TRUE!!
image(signal2D, col = grey.colors(256), axes = FALSE, useRaster = TRUE)

# First FFT on rows:
freq_dom_2D = signal2D
for(index in 1:length(signal2D[,1])){
  freq_dom_2D[index,] = fft(signal2D[index,])
} # End for inex => fft on rows

# Than fft on the cols of the previous FFT on rows!
for(index in 1:length(signal2D[1,])){
  freq_dom_2D[,index] = fft(freq_dom_2D[,index])
} # End for inex => fft on rows

# Compare FFT by row and by column with fft() R function that can handle 2D automatically
freq_dom_2D == fft(signal2D)

#### Doing the same but first columns than rows:
#### First FFT on COLUMNS THIS TIME:
freq_dom_2D_alt = signal2D
for(index in 1:length(signal2D[1,])){
  freq_dom_2D_alt[,index] = fft(signal2D[,index])
} # End for inex => fft on rows

# Than fft on the ROWS of the previous FFT on columns!
for(index in 1:length(signal2D[,1])){
  freq_dom_2D_alt[index,] = fft(freq_dom_2D_alt[index,])
} # End for inex => fft on rows

# Check for equality:
freq_dom_2D == freq_dom_2D_alt
# All TRUE!!
#      [,1] [,2] [,3] [,4]
# [1,] TRUE TRUE TRUE TRUE
# [2,] TRUE TRUE TRUE TRUE
# [3,] TRUE TRUE TRUE TRUE
# [4,] TRUE TRUE TRUE TRUE

# Here we have to use image to create a raster plot of the k-space:
# Without log magnitude:
par(mfrow=c(1,2))
image(log(1+Mod(fft(signal2D))),col = grey.colors(256), axes = FALSE, useRaster = TRUE, 
      main = "Log Magnitude of k-space")

# Re-construct our original signal via the IFFT of the frequency domain / k-space:
fft(freq_dom_2D, inverse = TRUE)
#      [,1]  [,2]  [,3] [,4]
# [1,] 0+0i  0+0i  0+0i 0+0i
# [2,] 0+0i 16+0i 16+0i 0+0i
# [3,] 0+0i 16+0i 16+0i 0+0i
# [4,] 0+0i  0+0i  0+0i 0+0i

# Rescaling values via 1/N
Re(fft(freq_dom_2D, inverse = TRUE))/length(signal2D)
#      [,1] [,2] [,3] [,4]
# [1,]    0    0    0    0
# [2,]    0    1    1    0
# [3,]    0    1    1    0
# [4,]    0    0    0    0

# Reconstructing image without dividing by length(signal2D):
image(Re(fft(freq_dom_2D, inverse = TRUE)),col = grey.colors(256), axes = FALSE, useRaster = TRUE, 
      main = "IFFT of k-space")
par(mfrow=c(1,2))

#######################################################################################################
# 3.2 FFT and IFFT on JPG/JPEG Images and Recalling the Difference Between Time and Frequency Domain #
#######################################################################################################

# Example row:
test_row = c(1,2,3,4,5)
fft_test_row = round(fft(test_row), 3) # round for less overloaded console output:
# [1] 15.0+0.000i -2.5+3.441i -2.5+0.812i -2.5-0.812i -2.5-3.441i

# Plot test row:
image(as.matrix(test_row),col = grey.colors(256), axes = FALSE, useRaster = TRUE, 
      main = "Signal = c(1,2,3,4,5)")

# Frequency domain of  test row (magnitude image): 
image(as.matrix(Mod(fft_test_row)),col = grey.colors(256), axes = FALSE, useRaster = TRUE, 
      main = "Frequency Domain of fft(test_row)")

# Shifted Frequency domain of test row (magnitude image): 
image(as.matrix(Mod(fftshift1D(fft_test_row))),col = grey.colors(256), axes = FALSE, useRaster = TRUE,
      main = "Shifted Frequency Domain of fft(test_row)")

# Regular shifted frequency domain plot of the fft(test_row).
# NOTE WE DID NOT USE x_axis_shift_shift(), since it was written for 
# signals of even length:
plot(c(-2,-1,0,1,2), as.matrix(Mod(fftshift1D(fft_test_row)))/5, type = "l", xlab = "Shifted x-axis",
     main = "Regular Plot of Shifted Frequency Domain")
abline(v =0, lty = 4)

### Looking at the Frequency Space of a JPG Image:
#install.packages("imager")
library("imager") # for load.image() and grayscale()

# Load image of choice (test_img used here):
image = load.image("test_img.jpg")

# Turns RGB into gray scale:
image = grayscale(image) 

# Plot image to test if upload was correct and gray scaling was correct:
plot(image, main = "Original Image, Grey Scaled")

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

#### Ms. Miranda test image:
image = load.image("ms_miranda_lena.jpeg") # here JPEG not JPG!

# Gray scale image - it is actually b/w but there 
# is actually some color left in the image...
image = grayscale(image) 

par(mfrow = c(2, 2))  # Set up plot grid 1 row 2 cols
# Plot Org. Image and Frequency Domain Image:
plot(image, main = "Original Image", axes = FALSE)
plot(log(1+Mod(fft(image))), main = "Frequency Spectrum or Domain / k-Space", axes = FALSE)

# Perform inverse Fourier Transformation to reconstruct original image from frequency domain:
plot(log(1+Mod(fft(image))), main = "Original Frequency Spectrum or Domain / k-Space", axes = FALSE)
image_recon = Re(fft(fft(image), inverse = TRUE)) / length(image)
plot(image_recon, main = "Reconstruction of Orig. Image via Inverse FFT", axes = FALSE)

# Reset plot grid to 1x1:
par(mfrow = c(1, 1))  


#################################################################################################
# 3.3 Performing FFT and IFT on MRI Data (DICOM Magnitude Plot Only) and and Shifting a k-Space #
#################################################################################################

# Install oro.dicom to read .dcm files:
#install.packages("oro.dicom")
library("oro.dicom")

# Load example data: 
data = readDICOMFile("example_sts.dcm")
data$hdr
# Load example data from k-space explorer
# https://github.com/birogeri/kspace-explorer/blob/master/images/default.dcm
#data = readDICOMFile("kspace/default.dcm")

# Plot original DICOM image (with dark color scheme grey(0:64/64) instead of grey.color(256)):
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
# Reshaped k-space axes:
abline(h = 0.5, v = .5)
text(x = .6, y = .95, "ky")
text(x = .85, y = .45, "kx")

# Reconstructing original image from k-space via IFT (includes normalization factor): 
IFT_image = Re(fft(kspace, inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey.colors(256), main = "Reconstructed Image via inverse FFT", axes = FALSE)

# Darker grey scale scheme:
image(t(IFT_image), col = grey(0:64/64), main = "Reconstructed Image via inverse FFT", axes = FALSE)



######################################################################################################################
# 3.4.1 Same, Same — but different: Comparing a Regular 3D Plot with a 2D Gradient Plot and Introducing the Outer()  #
######################################################################################################################

# Example 2D signal:
signal2D = matrix(c(0,0,0,0,
                    0,1,1,0,
                    0,1,1,0,
                    0,0,0,0), ncol = 4, byrow = TRUE)

# Length for N as object (for the case of equal col and row length!):
n = ncol(signal2D)

# FFT of signal:
kspace_signal2D = fft(signal2D)

# Show k-space:
image(log(1+Mod(kspace_signal2D)),col = grey.colors(256), axes = FALSE, useRaster = TRUE, 
      main = "Log Magnitude of k-space")

# Set up empty array matrix for result, as well as x and y:
z_mat = vector("list", n*n)
dim(z_mat) = c(n,n)
x =  seq(from = 0, to = n-1) # has to be 0-n-1!!
y =  seq(from = 0, to = n-1)

# Plot gradient of each spatial frequency:
par(mfrow = c(n,n))
for(i1 in 0:(n-1)){ # We have to work with 0, since it is factor below!
  for(i2 in 0:(n-1)){
    magnitude = Mod(kspace_signal2D[i2+1,i1+1]) # or amplitude
    phase    =  Arg(kspace_signal2D[i2+1,i1+1]) # or angle
    # Create values of z:              
    z_mat[[i2+1,i1+1]] = outer(x,y, 
            function(x,y){
              magnitude * cos(2*pi*(i1*x/n + i2*y/n) + phase) / (n^2) # Scaling necessary!
            } # End of function
    ) # End outer()
    
# ACTIVATE TO RUN!!! NOTE YOU NEED BIG FIGURE MARGIN!!!!!!!!!!!!!!
    
#    image(Re(z_mat[[i2+1,i1+1]]),col = grey.colors(256), axes = FALSE, main = paste("row: ", i1+1, "col: ",i2+1))
  } # End for index2
} # End for index1
par(mfrow = c(1,1))

# Sum of all gradient z matrices!
sum_z = function(z){ # Start of function
  n_z = ncol(z)
  z_res = round(Re(z[[1,1]][]-z[[1,1]][])) # z-z = all zero! initializes object for recursive addition;
  for(index in 1:n){# successively add the values of a vector;
    for(j in 1:n){
      z_res = z_res + z[[index,j]][] # recursive addition
    } # End for j
  } # End for index          
  return(z_res)             # return result in the console
} # End of function

# Plot original Signal and compare with replication 
# via summing gradient values of cosine! :O
image(signal2D, col = grey.colors(256), axes = FALSE, useRaster = TRUE, main= "Original Signal")
# Image of Reconstruction of 2D signal -- Sum via sum_z:
image(Re(sum_z(z_mat)), col = grey.colors(256), axes = FALSE, main = "Sum of Gradients")
# Alternative sum via Reduce(): 
#image(Re(Reduce("+",z_mat)), col = grey.colors(256))

# Check matrices of signal and sum of gradients for equivalence:
signal2D == round(sum_z(z_mat),3)
# ALL TRUE!!
#      [,1] [,2] [,3] [,4]
# [1,] TRUE TRUE TRUE TRUE
# [2,] TRUE TRUE TRUE TRUE
# [3,] TRUE TRUE TRUE TRUE
# [4,] TRUE TRUE TRUE TRUE



#########################################################################################
# 3.4.1 Same, Same — but different: Comparing a Regular 3D Plot with a 2D Gradient Plot #
#########################################################################################

# Create x and y values:
x = c(1:10)
y = c(1:10)

# Angle addition identity (trigonometric identity)
round(sin(x+y),12) ==  round(sin(x)*cos(y) + sin(y)*cos(x),12)

# The below results in a diagonal sine wave within a plane of x and y.
# This can be represented as 2D gradient plot, where colors or shades of grey indicate
# the values (how high or low they are), or via a 3D plot:
z = outer(x,y,FUN = function(x,y){sin(x+y)}) 

# Visualize as 3D wave using persp3d from rgl (or regular persp function):
persp3d(x, y, z,
        col = "lightblue", alpha = 0.8,
        xlab = "x", ylab = "y", zlab = "z",
        xlim = c(-5,15), ylim = c(-5,15), zlim =c(-5,5))

# Visualize via 2D gradient plot in shades of grey via the image function,
# interpreting each value of the z matrix as a grey scale value of a pixel:
image(x,y,z, col = gray.colors(256))


# SOME MORE GRADIENT PLOT EXAMPLES!
# Define matrix size / resolution:
size = 100  

# Create a matrix to plot a vertical grey scale gradient. Think of 
# both as a times series from -1 to 1. We need two to plot a 
# gradient, as often done in tutorials on k-spaces. We will use
# the outer() function, which need an input x and y.
x = seq(-1, 1, length.out = size) # -1 to 1, usually 0 to N-1 
y = seq(-1, 1, length.out = size)

# Combine as matrix:
xy = cbind(x,y)

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


############################################################
# 3.5 The Gradient Plots of the k-space of our DICOM Image #
############################################################

################ K-space and gradient plots of DICOM images:
# Again load example data: 
data = readDICOMFile("example_sts.dcm")
# DICOM k-space, shifted and unshifted:
kspace = fft(data$img)
kspace_shifted = fftshift2D(kspace)

# Choose location of complex number from SHIFTED k-space image/matrix:
kxshift = 0
kyshift = -4
#kxshift = 25
#kyshift = 25

# Note again that we will use the log magnitude of k-space data, otherwise 
# the frequencies outside of the centre (thinking shifted k-space) is too high 
# to visualize and will just mostly be all black.
# Log magnitude compression of values (result equivalent to amplitude, the 
# intensity of each frequency of a wave):
Mod(kspace[1,1])  
# For kspace[1,1] it is very high: 11570937, therefore we take the log()
# for the plot of the kspace (not for the gradient plot below though!!)
log(1 + Mod(kspace[1,1]))
log(1 + sqrt(Re(kspace[1,1])^2+Im(kspace[1,1])^2))
# [1] 16.26401 for pxx = 1 and pxy = 1

##### Each k-space pixel frequency as gradient (using DICOM image examples):
x = seq(0,(ncol(kspace)-1))
y = seq(0,(nrow(kspace)-1))

#### Set plot grid to 1x2:
par(mfrow = c(1, 2))  

# Plot shifted/reshaped image, now with low frequencies in the centre, instead the
# output matrix corners:
image(log(1 + Mod(kspace_shifted)), col = grey(0:64/64), axes = FALSE, 
      main = "Reshaped k-space (log Mag.)")
abline(h = 0.5, v = .5)
text(x = .65, y = .95, "ky")
text(x = .85, y = .45, "kx")

# Add respective point of the fft() / frequency domain of that value that will
# be shown as gradient. Since the axes scale is not following the matrix indices
# but is a range between 0 and 1 each, we have to adjust x and y below to get the
# point at the right location in the shifted k-space!
px = nrow(kspace)/2+kxshift   
py = ncol(kspace)/2+kyshift   

points(x = px/length(kspace[1,]) , y = (py)/length(kspace[,1]), 
       col = "darkviolet", pch = 16, cex = 1.5)

# Calculate log magnitude and phase 
# HERE WE CAN USE THE SHIFTED K-SPACE, since we just want the complex number
# not the correct index for the spatial frequencies!!!: 
# For the gradient plot we take the regular modulus!!
magnitude = Mod(kspace[px,py])
phase = Arg(kspace[px,py]) 

# Plot gradient of frequency of k(fx,fy):
m = nrow(kspace)
n = ncol(kspace)
# HERE the object of kxky are the spatial frequencies, already starting with 0!!
z = outer(x, y, function(x, y) magnitude*cos(2*pi*((kxshift)*x/m + (kyshift)*y/n)+phase)/(m*n)) 
image(x,y,z, col = grey(0:64/64), axes = FALSE, main = paste("Gradient Plot of kx,ky","\n", "including Magnitude and Phase"),xlab="",ylab="")

# Reset plot grid:
par(mfrow=c(1,1))

# Note that we could of course also plot the gradient of each diimension
# kx and ky separately as two 1D cosine waves:
# Set grid
par(mfrow = c(1,2))
# Plot frequency phase and amplitude as wave:
time = seq(0,1,length.out = 100)
wave_kx = magnitude*cos(2*pi*(kxshift*time)+phase)
wave_ky = magnitude*cos(2*pi*(kxshift*time)+phase) 
plot(time,wave_kx, type = "l", main = paste("Frequency of kx = ",kxshift), xlab = "0 to 1 Hz") 
plot(time,wave_ky, type = "l", main = paste("Frequency of ky = ",kyshift), xlab = "0 to 1 Hz") 
par(mfrow = c(1,1))



############################################################################
# 3.6 Field of View (FOV), Aliasing Effects, Low-Pass and Gaussian Filters #
############################################################################


# Only process the spatial frequencies, magnitude and phase of the first 10 column
# and row entries of the k-space, essentially only a part on the lower right quadrant 
# of the shifted k-space in comparison with the full k-space being processed:
par(mfrow=c(1,2))
rangex = 1:10
rangey = 1:10
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT row,col = ",max(rangex),max(rangey)), axes = FALSE)

# Add complete k-space:
IFT_image = Re(fft(kspace, inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT row,col = N_row,N_col"), axes = FALSE)
par(mfrow = c(1,1))

# Example chess board like 2D signal:
par(mfrow = c(1,2))
signal2D = matrix(c(0,1,0,1,
                    1,0,1,0,
                    0,1,0,1,
                    1,0,1,0), ncol = 4, byrow = TRUE)
# Plot image:
image(signal2D, col = grey.colors(256), axes = FALSE, useRaster = TRUE, main= "Original Signal")
image(fftshift2D(log(1+Mod(fft(signal2D)))), col = grey.colors(256), axes = FALSE, useRaster = TRUE, main= "k-space")
par(mfrow = c(1,1))

# Check for equivalence of shifted and unhifted k-space:
fftshift2D(log(1+Mod(fft(signal2D)))) == log(1+Mod(fft(signal2D)))
# ALL TRUE!!

# Obscuring effect is achieved by changing the arithmetic sign 
# from + to negative and vice versa, which can be achieved via taking the -Re(z)!
IFT_image = -Re(fft(kspace, inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("Inversion of Grey Scale Values"), axes = FALSE)

# Vertical and horizontal flip:
par(mfrow = c(1,2))
IFT_image = Re(fft(kspace, inverse = TRUE) / length(kspace))
IFT_image_vertival = IFT_image[,ncol(IFT_image):1]
image(t(IFT_image_vertival), col = grey(0:64/64), main = paste("Mirrored Vertically"), axes = FALSE)
IFT_image_horizontal = IFT_image[nrow(IFT_image):1,]
image(t(IFT_image_horizontal), col = grey(0:64/64), main = paste("Mirrored Horizontally"), axes = FALSE)
par(mfrow = c(1,1))

#### Here FOV for the symmetric case of FOVx = FOVy = FOV, 
#### meaning that the length of the width and the height are equal!
#### We will name the variables FOV_x not just FOV though, to avoid confusion:
# Compare this tutorial: https://mriquestions.com/field-of-view-fov.html 

# WORKING WITH amount of pixels as length makes it hard to understand the concept
# concerning the k-space and teh FOV of the image:
# delta_kx = 1/FOV, where k_max is the length of a row col.
# Below we refer to our DICOM image:
delta_kx = 1/(length(kspace[,1]))  
# [1] 0.003921569
FOV_kx = 1/delta_kx
# [1] 256
FOV_kx == length(kspace[,1]) # => Equivalent with range of 1D frequencies! 
# [1] TRUE

# 1/FOV_Pix, where FOV_Pix = 4 Pixels (for our chess board example): 
delta_Pix = 1/4  
# [1] 0.25
FOV_Pix = 1/delta_Pix
# [1] 4

# Code for the plot of the cosine waves:
# kx from 0,1,2 and 0,-1,-2:
x = seq(0,2*pi,by = .1)
plot(x,cos(x*0)+12, type = "l", ylim = c(-1,13), axes =  FALSE, ylab = "", xlab  = "")
lines(x,cos(x*1)+9, type = "l")
lines(x,cos(x*2)+6, type = "l")
lines(x,-cos(x*1)+3, type = "l")
lines(x,-cos(x*2), type = "l")

# Relation \Delta k and \Delta Pix.:
# 1/FOV_Pix, where FOV_Pix = 4cm (for our chess board example): 
delta_Pix = 1/4 # same for \Delta k!!!   
delta_kx = 1/4 # same for \Delta Pix!!!   
# [1] 0.25 # as part of 1 we can say this is 1 of four pixels!
#          # and we need 1 cycle per pixel, delta_Pix!!
FOV_Pix = 1/delta_Pix # Same for FOV_kx!!!
FOV_kx = 1/delta_kx # Same for FOV_Pix.!!!
# [1] 4

delta_Pix == 1/FOV_kx
# [1] TRUE
delta_kx == 1/FOV_Pix
# [1] TRUE

# Cycles per pixel:
cycle_per_pixel = 1
FOV_Pix/cycle_per_pixel # delta_Pix
# [1] 4
FOV_kx/cycle_per_pixel == 1/delta_Pix
# [1] TRUE
cycle_per_pixel/FOV_Pix == delta_kx 
# [1] TRUE

# Imagine we only take every second element / pixel of the k-space,
# see what happens to the field of view:
delta_undersamp = 2/4 # k_max will still be 2!
FOV_Pix_undersamp = 1/delta_undersamp
# [1] 2


#### Set plot grid to 1x2:
par(mfrow = c(2, 2))  

# Aliasing effect
rangex = 1:ncol(kspace)
rangey = seq(1,length(kspace[,1]), by = 2)
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT of every 2nd row only."), axes = FALSE)

rangex = seq(1,length(kspace[,1]), by = 2)
rangey = 1:nrow(kspace)
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT of every 2nd column only."), axes = FALSE)

rangex = seq(1,length(kspace[,1]), by = 2)
rangey = seq(1,length(kspace[,1]), by = 2)
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT of every 2nd row/col. only."), axes = FALSE)

# Reset grid
par(mfrow=c(1,1))


# Only sampling one quadrant of the k-space:
delta_Pix_samp = (1/(FOV_kx/2)) 
# [1] 0.5 # ACTUALLY it is 2 pixels, but since we did not introduce units
#         # it is just reflected as parts of 1
delta_Pix*2 == delta_Pix_samp 
# [1] TRUE


#### Set plot grid to 2x2 again:
par(mfrow = c(2, 2))  

# Successively reconstructing one quadrant of the original image (upper-left quadrant 
# of the k-space via IFT (includes normalization factor)): 
rangex = 1:10
rangey = 1:10
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT row,col = 10,10"), axes = FALSE)

rangex = 1:25
rangey = 1:25
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT row,col =25,25"), axes = FALSE)

rangex = 1:75
rangey = 1:75
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT row,col = 75,75"), axes = FALSE)

rangex = 1:(ncol(kspace)/2)
rangey = 1:(nrow(kspace)/2)
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT row,col = 128,128"), axes = FALSE)

# Reset plot grid:
par(mfrow = c(1,1))

# R can somehow handle non-integer indices:
rangex = 1:(ncol(kspace)/2)/2
rangey = 1:(nrow(kspace)/2)/2
IFT_image = Re(fft(kspace[rangex,rangey], inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT row,col = 128,128 divided by 2"), axes = FALSE)


# High-pass filter: every element outside the radius will have the value 1,
# every matrix element inside the circle will have the value 0. Applying
# the high-pass filter will be done via a nested for loop. The kspace
# is then simply multiplied with the high-pass matrix, meaning every value inside
# the high-pass filter radius will be set to 0 (other way around with high-pass filter).
# The high-pass/low-pass matrix can essentially be seen as a 2D hat function with
# discrete values (integers essentially).

# For the low-pass filter we will use the same code, just the other way around, 
# such that everything outside the radius is set to zero not 1...
# Recreated from this python script: https://www.physi.uni-heidelberg.de/Einrichtungen/AP/python/FFT_lena.html 
signal2D = matrix(c(0,1,0,1,
                    1,0,1,0,
                    0,1,0,1,
                    1,0,1,0), ncol = 4, byrow = TRUE)
# Calculate kspace for simple chess board signal:
kspacetest = fft(signal2D)
# kspace for DICOM image:
kspaceDICOM = fft(data$img)
# Choose which k-space you want to try out
kspace_pass = kspaceDICOM
#kspace_pass = kspacetest

# Use kspace matrix as base for the high-pass/low-pass matrix (all values
# will be changed anyways):
high_pass = Re(kspace_pass) 
# Create objects for length of cols and rows:
ncols = ncol(kspace_pass)
nrows = nrow(kspace_pass)
# Set radius (essentially integer frequency boundary for high-/low-pass filter)
radius = 20
for(indexI in 1:nrow(kspace_pass)){
  for(indexJ in 1:ncol(kspace_pass)){ 
    # The below is essentially the formula for the modulus in relation to a set radius:
    if((((indexI-1) - (nrows-1)/2)^2 + ((indexJ-1) - (ncols-1)/2)^2) > radius^2){ 
      high_pass[indexI,indexJ] = 1   # FOR LOW-PASS just set if < radius^2
    }
    else{
      high_pass[indexI,indexJ] = 0
    }
  } # End for indexJ
} # End for indexI  

high_pass 
# Result for radius 1 and chess board example (> radius^2):
#      [,1] [,2] [,3] [,4]
# [1,]    1    1    1    1
# [2,]    1    0    0    1
# [3,]    1    0    0    1
# [4,]    1    1    1    1

# For low-pass version with < radius^2 set!
#      [,1] [,2] [,3] [,4]
# [1,]    0    0    0    0
# [2,]    0    1    1    0
# [3,]    0    1    1    0
# [4,]    0    0    0    0

# Apply high-pass filter to original k-space simple multiplication with high_pass! 
# (DON'T FORGET TO SHIFT k-SPACE HERE!!)
kspace_high_pass = fftshift2D(kspace_pass)*high_pass


# Plot results:
par(mfrow = c(1,2))
IFT_image = Re(fft(kspace_pass, inverse = TRUE) / length(kspace))
graphics::image(t(IFT_image), col = grey(0:64/64), main = paste("IFT of kspace"), axes = FALSE)

IFT_image = Mod(fft(kspace_high_pass, inverse = TRUE)/ length(kspace))^0.2
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT incl. high-pass filter, radius 20"), axes = FALSE)
par(mfrow = c(1,1))


#### Gaussian filter:
x = c((-(ncols-1)/2):0,1:(ncols/2))
y = c((-(nrows-1)/2):0,1:(nrows/2))
sigma = 100 # kernel bandwidth
Gaussian = outer(x,y, function(x,y) exp(-(x^2+y^2)/(2*sigma^2)))
Gaussian = Gaussian/sum(Gaussian)

# Plot 3D Gaussian Kernel:
persp3d(x,y,Gaussian, col = "deeppink")

# Apply Gaussian Kernel filter and Plot results:
kspace_Gaussian = kspace_pass*Gaussian

par(mfrow = c(1,2))
IFT_image = Re(fft(kspace_pass, inverse = TRUE) / length(kspace))
graphics::image(t(IFT_image), col = grey(0:64/64), main = paste("IFT of kspace"), axes = FALSE)

IFT_image = Re(Mod(fft(kspace_Gaussian, inverse = TRUE)/ length(kspace)))
image(t(IFT_image), col = grey(0:64/64), main = paste("IFT incl. Gaussian filter, sigma = 100"), axes = FALSE)
par(mfrow = c(1,1))




