
#####################################################
### 9.10 EXAMPLE FUNCTION X: Fourier Transformation #
#####################################################

# install.packages(c("imager","magick", "oro.dicom"))
library("imager")       # for load.image(), grayscale() and image() => DirectX 11 (Win) / Xquartz (Mac) needed!!
library("magick")       # for creating .gif of several plots
library("oro.dicom")    # for importing DICOM files, chapter 9.10 FFT MRI example

# For plotting:
library("ggplot2") # entailed in tidyverse

# The following is based on the 3blue1brown tutorial on Fourier Transformation.
# https://www.youtube.com/watch?v=spUNpyF58BY&t=70s  
# There is also a python script that produces equivalent results, however the
# python code is not well documented and therefore hard to read for beginners.
# You can find the python version here: 
# https://github.com/thatSaneKid/fourier/blob/master/Fourier%20Transform%20-%20A%20Visual%20Introduction.ipynb

# The below code also does not use list types, such as the python version above.
# The below is a rather direct representation of the mathematical formulas.
# The python code uses a list (and in R we could do so too) in order to have
# 100 times (for 100 winding / sampling frequencies) one position for the real
# and one for the imaginary part. In R this can be extracted from the result
# of rather directly typing in the formula using Im() and Re() to extract the
# imaginary and real part for plotting. However, this is only necessary in R
# using ggplot. The basic plot() function can handle the full complex number
# by itself via automatically applying Im() and Re(), so only one input,
# g_t*exp(-2*pi*i*samp_f[i]*t), where g_t = g_t = cos(2*pi*freq*t), is needed
# for the wound up version of the cos() wave. However, it did not work to plot
# multiple plots via a for loop using plot() since it kind of messes up the plot
# options which requires to clear environment... THE REASON was simple but fatal:
# I have named the index variable in the loop "i", even though i is set below
# as a constant imaginary number... This mistakes messes up your environment!

# Define the time as a frequency with a certain precision of points (by=):
time = seq(0,1,by=.0001) # This sets Hz as unit of frequency

# Define wave frequency 
freq = 3 # == 3 Hz

# Winding / sampling frequency
samp_f = seq(0,10, by = .1) # from zero to 10 in steps of .1
length(samp_f) # length 101, so we will only plot up to 9.9 Hz winding freq (same in the python script)

# Set complex number "i":
i = complex(real = 0, imaginary = 1) # beware to NOT USE "i" as index variable in a for loop from now on!!

# Cos wave definition. cos() for all time multiplied by frequency of the cos() wave and 2*pi 
# for radians period, i.e. normalized time in unit of Hz. 
# This cosine wave is used in the python script creates a circle when samp_f == freq:
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

# g_t and the code below for our wound up cosine wave regarding one winding/sampling 
# frequency produce very lengthy vectors. In the below case it's complex numbers, i.e. 
# coordinates of our polar grid for our wound up cosine wave. Plotting multiple 
# plots (101) with 10001 paired Re and Im values is a lot. 
length(g_t*exp(-2*pi*i*samp_f[1]*time))
# [1] 10001 !!!


# Plotting output for each winding frequency is easy with basic plot().
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
         # Title entails information on winding/sample freq and original cosine freq
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
  
  # Change to data.frame for ggplot and rename columns
  g_hat_smooth = as.data.frame(g_hat_smooth)
  colnames(g_hat_smooth) = c("Re","Im")
  
  # Bar plot via ggplot
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

# Test function (UNCOMMENT TO TEST FUNCTION!! UNCOMMENT TO TEST FUNCTION!! UNCOMMENT TO TEST FUNCTION!!)
#fourier_trans(g_t,samp_f,time)



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
for(index in 1:length(samp_f)){ # don't call it i when i was defined above as a constant variable!!!
  # Create data frame with coordinates of wound up function for respective sampop_f[index]:
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


### Looking at the Frequency Space of a JPG Image:

#install.packages("imager")
library("imager") # for load.image() and grayscale()

# Load image of choice (test_img used here):
image = load.image("test_img.jpg")

# Turns RGB into gray scale:
image = grayscale(image) 

# Plot image to test if upload was correct and gray scaling was correct:
plot(image, main = "Original Image, Grayscaled")

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

### SAME USING MRI DICOM FILES:
# Install oro.dicom to read .dcm files:
#install.packages("oro.dicom")
library("oro.dicom")

# Load example data: 
data = readDICOMFile("example_sts.dcm")

# Load example data from k-space explorer
# https://github.com/birogeri/kspace-explorer/blob/master/images/default.dcm
#data = readDICOMFile("kspace/example_sts.dcm")


# Plot original DICOM image (with dark color scheme grey(0:64/64), grey.color(256)):
image(t(data$img), col = grey(0:64/64), axes = FALSE,
      main = "Original DICOM Image")

# Perform fft on data:
kspace = fft(data$img)

# k-space without fftshift(), again we use log magnitude and + 1
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

#  fftshift() Matlab style (requires dims to be integer when divided by 2):
fftshift = function(kspace) {  #
  # For better readability - could also be done via ncol() and nrow()
  rows = nrow(kspace) # Evaluate n of rows
  cols = ncol(kspace) # ... n of cols
  reshape_row = c((rows/2+1):rows, 1:(rows/2))  # rows/2+1 so it starts at first position second half
  # not last position of first half!
  reshape_col = c((cols/2+1):cols, 1:(cols/2))  # same here...
  kspace[reshape_row,reshape_col]               # reshape k-space
} # End of function fftshift()

# Shifted k-space image (log magnitude + 1):
kspace_shifted = fftshift(kspace)

# Plot shifted/reshaped image, now with low frequencies in the center, instead the
# output matrix corners:
image(log(1 + Mod(kspace_shifted)), col = grey.colors(256), axes = FALSE, 
      main = "Reshaped k-space")

# Reconstructing original image from k-space via IFT (includes normalization factor): 
IFT_image = Re(fft(kspace, inverse = TRUE) / length(kspace))
image(t(IFT_image), col = grey.colors(256), main = "Reconstructed Image via inverse FFT", axes = FALSE)

