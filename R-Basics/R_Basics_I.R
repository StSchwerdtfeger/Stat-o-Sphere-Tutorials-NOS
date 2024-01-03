##################################
##################################
#      R-Basics-I Tutorial       #
#               by               #
#     Steffen Schwerdtfeger      # 
#            12.2023             #
##################################
##################################

# IMPORTANT NOTE:
# Different to all other scripts of our tutorial series,
# this script can not be executed as a whole!! A lot of examples
# involve commen errors... 

#########################################
# 3 Open a Script and Execute Test Code #
#########################################

# Mark line and execute via ALT+ENTER or Cmnd+ENTER (Mac)
test = 2 + 5
# Console output
# [1] 7

# Only one object per line!
# The below will give you an error:
test1 = 2 test2 = 3
# Error: unexpected symbol in "test1 = 2 test2"

# We can use the delimiter ";" within R scripts though:
test1 = 2; test2 = 3
# Delimiters are a general concept of demarcation. In R it
# it demarcates lines. So using ";" is the same as starting a new line!

###################################################################
# 5 Classes of Objects: Vectors, Matrices, Arrays, Lists and More #
###################################################################

# Vectors
vec = c(1,2,3)
# [1] 1 2 3  # NOTE that an output is also a vector
#      therefore the [1] at the beginning.

vec[2]  # index 2 == second element 
# [1] 2 # second element in the vector is also 2


# Matrices
mat = as.matrix(vec)
#      [,1]
# [1,]    1
# [2,]    2
# [3,]    3

mat[2]
# or
mat[2,1]
# [1] 2


mat2 = as.matrix(t(vec))  
# t() == transpose == tilting a vector (different with matrices!!!)
#      [,1] [,2] [,3]
# [1,]    1    2    3 

mat[1,2]
# Technically a column vector with only one element, when thought of as
# an element of the matrix above:
# [1] 2

mat_bind = cbind(c(1,2,3),c(1,2,3),c(1,2,3))
#      [,1] [,2] [,3]
# [1,]    1    1    1
# [2,]    2    2    2
# [3,]    3    3    3


# Data frames
mat_bind = as.data.frame(mat_bind) 
#   V1 V2 V3  # V stands for variable
# 1  1  1  1
# 2  2  2  2
# 3  3  3  3

# Column names can be changed via:
colnames(mat_bind) = c("One", "Two", "Three") # rownames() exists too
#      One Two Three
# [1,]   1   1     1
# [2,]   2   2     2
# [3,]   3   3     3


# Exemplatory array with 
# dim = c(rows, columns, further_dimension)
array(1, dim = c(50,5,5))

# Add a 1 to every entry:
array(1, dim = c(50,5,5)) + 1


# String Example
string = c("One",2,3)
# Try to add a 1 to the second element in the string above:
string[2]+1
# Error in string[2] + 1 : non-numeric argument to binary operator

# Combination of numbers and objects:
c(object,2,3)
# Error: object 'object' not found 

c("object",2,3)
# [1] "object" "2" "3"


# Lists
test_list = list("test",2,3)
# [[1]]
# [1] "test"

# [[2]]
# [1] 2

# [[3]]
# [1] 3

test_list[[2]]
# [1] 2



# Character string with numeric symbols to numeric vector:
test = c("1","2","3")
is.character(test)
# [1] TRUE

test = as.numeric(test)
is.numeric(test)
# [1] TRUE

# Check what happens if one element is an actual character string
# not just numbers symbol treated as characters:
test = c("test","2","3")
as.numeric(test)
# [1] NA  2  3
# Warning message:
# NAs introduced by coercion 



############################################################################
# 6 Import and Export of .CSV files, Understanding Delimiters and Packages #
############################################################################

# read CSVs
read.csv("Your file path")
read.csv2("Your file path")

# Working directory:
getwd() # no input needed!!

# url() Function
# read.csv(url("http://www.website.net/data_file.csv"))

# Load Dino Data Set for this!
# I called the object "dino", such that plotting is done by:
plot(x = dino$x,y = dino$y)


# install.packages("readr")
library("readr") # package also within "tidyverse"
# write.csv()
# write.csv2()



##############################################
# 7 Exemplary Datasets (DINOSAURS and more!) #
##############################################

# Install package for further data sets:
# install.packages("dplyr") # (dplyr also used for data cleaning)
library(dplyr)            # open/activate/load package
data(starwars)            # load data set
View(starwars)            # view via RStudio viewer
?starwars                 # view documentation



# Another fun and insightful package:
# install.packages("datasauRus")
library(datasauRus)
datasaurus_dozen

# Plot using ggplot2:
#install.packages("ggplot2")
library(ggplot2)

# Plot all:
ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset))+
  geom_point()+
  theme_void()+ # deletes coordinate system
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol = 3)

# Plot only certain set:
dino = filter(datasaurus_dozen, dataset == "dino")
ggplot(dino, aes(x = x, y = y, colour = dataset))+
  geom_point()+
  theme(legend.position = "none")


###########################################
# 8 Short Introduction into Data Cleaning #
###########################################

# Example for the structure of data tables:
# Creating an examplatory data set with vectors:
patient_id = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)
fam = c("yes", "yes", "no", "no", NA, NA, "n","n","no","no","ys", "ys", NA, NA)
time = c("t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2")
measurement_sysRR = c(130,122,132,123,133,121,129,125,135,119,134,127,140,125)

# Format into a data frame:
table = as.data.frame(cbind(patient_id,time,measurement_sysRR,fam))


# install.packages("dplyr")  # install package
library(dplyr)             # load/activate package
# Filter function: filter(data_object, columnname == "entry") or != for unequal
t1 = filter(table, time == "t1")
t1 = filter(table, time != "t2") # alternative
t2 = filter(table, time == "t2")

# SLIGHTLY DIFFERENT, including NA in the measurements:
patient_id = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)
fam = c("yes", "yes", "no", "no", NA, NA, "n","n","no","no","ys", "ys", NA, NA)
time = c("t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2")
measurement_sysRRalt = c(130,122,132,NA,133,121,NA,125,135,119,134,127,140,125)

# Format into a data frame:
new_table = as.data.frame(cbind(patient_id,time,measurement_sysRRalt,fam))

# Mean sysRR at t1:
t1alt = filter(new_table, time == "t1")
# Problem with NA
mean(t1alt$measurement_sysRRalt)
# [1] NA
# Warning message:
# In mean.default(t1alt$measurement_sysRRalt) :
#  Argument ist weder numerisch noch boolesch: gebe NA zurück 

### POSSIBLE SOLUTION I to get rid of patients data with only t1 or t2. not both:
# In which lines are NAs (row / line nummer, not pat. id!)
is.na(new_table$measurement_sysRRalt)
# [1] FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
na_lines = which(is.na(new_table$measurement_sysRRalt) == TRUE)
# [1] 4 7 # Index of the lines with NA entry

# Initialize vector:
pat_id_na = c()
# Determine which patient_id is in the lines from the list na_lines: 
for(i in 1:length(na_lines)){
  pat_id_na[i] = new_table$patient_id[na_lines[i]]
} # End for i
pat_id_na
#[1] "2" "4" # Patient ids with NA in either t1 or t2

# Initilize table:
fin_table = new_table
# Delete every line with the patient_id given in the list
# pat_id_na:
for(i in 1:length(pat_id_na)){ # or length(na_lines)
  fin_table = filter(fin_table, patient_id != pat_id_na[i])
} # End for i
fin_table 

# Example for loop:
# Define a object you want to loop over:
object = c(1,1,1,1)

# Initilize a list to store the results:
list_results = c()

# For loop that adds +1 to every element of the above object:
for(i in 1:length(object)){
  list_results[i] = object[i]+1
} # End for i


#### POSSIBLE SOLUTION II:
# Filter NA in measurement_sysRRalt
new_table_alt = filter(new_table, is.na(measurement_sysRRalt) == FALSE)

# Use circumstance that patient_id occurs only 1x in such cases:
# Function unique() outputs a vector with individual entries:
unique_id = unique(new_table_alt$patient_id)
# [1] "1" "2" "3" "4" "5" "6" "7"

# Initialize vector:
num_of_id = c()
for(i in 1:length(unique(new_table_alt$patient_id))){ 
  num_of_id[i] = length(which(new_table_alt$patient_id == unique_id[i]))
} # End for i
num_of_id
# [1] 2 1 2 1 2 2 2

# Create table:
table_num_of_id = as.data.frame(cbind(unique_id,num_of_id))
colnames(table_num_of_id) = c("unique_id", "num_of_id")
table_num_of_id = as.data.frame(table_num_of_id)

# Filter all IDs that only occure once:
table_single_id = filter(table_num_of_id, num_of_id == "1")
table_single_id

fin_table_alt = new_table
# The same as before: filter single id from new_table:
for(i in 1:length(table_single_id$unique_id)){ # or length(na_lines)
  fin_table_alt = filter(fin_table_alt, patient_id != table_single_id$unique_id[i])
} # End for i
fin_table_alt 

which(new_table_alt$patient_id == unique_id[1]
# [1] 1 2

length(which(new_table_alt$patient_id == unique_id[1])
# [1] 2

# Check for equality of both methods:
fin_table$patient_id == fin_table_alt$patient_id
# [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE


#### Pipe operator %>% using tidyverse:
# install.packages("tidyverse")
library("tidyverse")

# Test 1 Filter von t2 und Zeilen mit NA löschen
test1 = new_table %>% 
  select(patient_id,time,measurement_sysRRalt) %>% # select columns like cbind()
  filter(time != "t1")  # Filter for t1
# alternative: 
# na.omit() # be careful with this function
# and no %>% at the end, otherwise "+" in console!

test1
#   patient_id time measurement_sysRRalt
# 1          1   t2                  122
# 3          3   t2                  121
# 4          4   t2                  125
# 5          5   t2                  119
# 6          6   t2                  127
# 7          7   t2                  125

### POSSIBLE SOLUTION II Simple dplyr/tidyverse alternative:
new_table_alt %>%   # new_table_alt was filtered for NAs already 
  select(patient_id,time,measurement_sysRRalt)%>%
  group_by(patient_id) %>%  # sorts individ. entries into indiv. groups 
  # when executing next line!
  filter(n()>1)     # filters if n of each grouped entry is greater 
# than 1 such that all single entries get filtered

# Example na.omit()
test_na_omit = cbind(c(1,NA,3,4), c(1,2,NA,4))
#      [,1] [,2]
# [1,]    1    1
# [2,]   NA    2
# [3,]    3   NA
# [4,]    4    4

na.omit(test_na_omit)
#      [,1] [,2]
# [1,]    1    1
# [2,]    4    4
# attr(,"na.action")
# [1] 2 3
# attr(,"class")
# [1] "omit"



###### Character string manipulation:
unique(fin_table_alt$fam)
# [1] "yes" NA    "no"  "ys" 
which(fin_table_alt$fam =="ys")
# [1] 7 8


# Correct entries e.g. via: The below is a composition of the functions
# fin_table$fam[]  and  which()  and  is.na()
fin_table$fam[which(is.na(fin_table$fam) == TRUE)] = "not specified"
fin_table$fam[which(fin_table$fam =="ys")] = "yes"
unique(fin_table$fam)
# [1] "yes"           "not specified" "no"  

# Get first name only!
name = c("Name Surname","Name Surname","Name Surname")
surname = c("","","")
names = cbind(name,surname)
#      name           surname
# [1,] "Name Surname" ""       
# [2,] "Name Surname" ""       
# [3,] "Name Surname" "" 

names = as.data.frame(names)
first_name = word(names$name,1) # the 1 stands for first word of 
# the character string; the spacing
# is targeted like delimiter so to speak 
# [1] "Name" "Name" "Name"


#######################
# 9 Writing Functions #
#######################


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

# Examplatory code for a sum function (do not name it "sum",
# since it conflicts with the integrated sum() function)
sum_alt = function(x){ # Start of function
  result = 0                 # initialize object for recursive addition;
  for(i in 1:length(x)){     # successively add the values of a vector;
    result = result + x[[i]] # recursive addition; i is element of a set
  } # End for i              # I = {1 to length(vec)}; n = length(vec)
  return(result)             # return result in the console
} # End of function

# Example:
sum_alt(c(1,2,3))

# Test for equal results:
sum(c(1,2,3)) == sum_alt(c(1,2,3))
# [1] TRUE



# Examplary linear least square function, invluding a t-test:

# Go-go-gadgeto linear_least_square!!!!
linear_least_square = function(indep,dep){ # Start of function
  
  # Evaluating coefficients for an optimal linear model
  # given a set of dependent and independent variabels:
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
  t_value_b = beta/se_slope_t 
  
  # t-value of the intercept:
  t_value_a = alpha/se_a
  
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
  Res_data = as.data.frame(cbind(Results_a,Results_b))
  rownames(Res_data) = c("Estimate","Std. Error","t value","Pr(>|t|)")
  colnames(Res_data) = c("Intercept", "Reg. coeff.")
  
  # Nice output using cat() function:  
  cat(" Linear least square method in R","\n","\n",
      "Independent variable:", "\t", deparse(substitute(indep)),"\n", 
      "Dependent   variable:", "\t", deparse(substitute(dep)),"\n","\n",
      "alpha", "\t",alpha,"\n",
      "beta","\t",beta, "\t","SumR2", "\t", SumR2, "\n","\n")
  print(Res_data)
  cat("\n","Residual Standard Error:",round(residual_standard_error),
      "on", (length(indep)-2), "degrees of freedom","\n","\n") 
  
  # Let us also plot our results:
  # We will also use deparse(substitute(x)) for the 
  # labels of our plot axes.
  plot(x=indep,y=dep, ylab = deparse(substitute(dep)),       
       xlab = deparse(substitute(indep)))
  abline(a=alpha, b=beta, col = "darkblue")
  
} # End of function


# Test:
linear_least_square(indep = c(0:10), dep=(c(0:10)*3))



# Logistic Map
logistic.map <- function(r, x, N, M){
  ## r: bifurcation parameter
  ## x: initial value
  ## N: number of iteration
  ## M: number of iteration points to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r *z[i]  * (1 - z[i])
  }
  ## Return the last M iterations 
  z[c((N-M):N)]
}

# Set scanning range for bifurcation parameter r (caluclation
# may take a while)
my.r <- seq(.9, 4, by=0.003)   #!!!! alternative start 2.5, so it may be more vivid
system.time(Orbit <- sapply(my.r, logistic.map,  x=0.1, N=1000, M=300))

Orbit <- as.vector(Orbit)
r <- sort(rep(my.r, 301))

plot(Orbit ~ r, pch=".")   ## Execute up to here to plot the logistic map


# Mandel brot set function:
mandelbrot_generator <- function(
    p = 2, 
    q = 1,
    xmin = -2.1, # minimum x value
    xmax = 0.8,  # maximum x value
    nx = 500, 
    ymin = -1.3, # minimum y value
    ymax = 1.3,  # maximum y value
    ny = 500,
    n = 100, 
    showplot = TRUE, # If TRUE then display image,
    showvals = FALSE, # Turn values off/on
    cols = colorRampPalette(c("black","cyan","cyan3","black"))(11)) 
{
  
  # variables
  x <- seq(xmin, xmax, length.out=nx)
  y <- seq(ymin, ymax, length.out=ny)
  c <- outer(x,y*1i,FUN="+")
  z <- matrix(0.0, nrow=length(x), ncol=length(y))
  k <- matrix(0.0, nrow=length(x), ncol=length(y))
  
  for (rep in 1:n) { 
    index <- which(Mod(z) < 2)
    z[index] <- z[index]^p + c[index]*q
    k[index] <- k[index] + 1
  }
  
  if (showplot==TRUE) { image(x,y,k,col=cols, xlab="Re(c)", ylab="Im(c)")}
  if (showvals==TRUE) {return(k)}
  
}

mandelbrot_generator(p=2, q=1)
