##################################
##################################
#           R-Basics I           #
#         Tutorial Script        #
#               by               #
#     Steffen Schwerdtfeger      # 
#       12.2023 - 03.2025        #
##################################
##################################

# This tutorial script is equivalent with the tutorial script for the first 
# part of the in-person peer-teaching tutorial for CIPOM/LZ@Charité:
# https://doi.org/10.56776/abbd964d.665f7de5 

# Corresponding Tutorial and more Educational Resources incl. Code can be found here:
# https://journal.medicine.berlinexchange.de/statosphere 
# https://journal.medicine.berlinexchange.de/user/steffen-schwerdtfeger-2 

# Github page with all Stat-o-Sphere Scripts:
# https://github.com/StSchwerdtfeger 

#### FOLD / UNFOLD CODE of the desired chapter (small arrow at the lower line of #'s).

# Libraries used in this tutorial script (will be loaded on spot as well for
# educational purpose; however installing the below makes sure the whole script,
# exercise and Rmarkdown example can be executed as a whole in one go):

# UNCOMMENT FOLLOWING LINE AND EXECUTE TO INSTALL ALL OF THE BELOW PACKAGES:
#install.packages(c("tidyverse","stringi","effsize","shiny","readxl","tidytuesdayR","imager","magick", "oro.dicom"))
# Tidyverse entails among others the packages stringr, dplyr, ggplot, readr
library("tidyverse")    # filter(), select(), gather(), melt() group_by() summarize() 
library("stringi")      # changing symbol patterns such as Ae to Ä
library("effsize")      # cohen.d() for the exercise
library("shiny")        # create and run shiny apps
library("readxl")       # read_excel()
library("tidytuesdayR") # resource for plenty of free example data sets
library("imager")       # for load.image(), grayscale() and image()
library("magick")       # for creating .gif of several plots
library("oro.dicom")    # for importing DICOM files, chapter 9.10 FFT MRI example

# Needed for Rmarkdown example:
#install.packages(c("gt","kableExtra", "gridExtra"))
#library("gt") # gt() nice looking tables
#install.packages("kableExtra") 
#library("kableExtra") # kbl() nice looking tables
#install.packages("gridExtra") 
#library("gridExtra") # grid.arrange() multiple plot on one page

################################################################
# 1 What is the Function of a Computer? — A Short Introduction #
# into the History of Programming Languages                    #
################################################################

# BEST START WITH CHAPTER 3 when you are completely new to programming / R, 
# the below is just some Code upfront that is shown in the introduction. 

#### Example Parabola Function (also found in chapter 9 on functions):
parabola = function(x){ # Parabola is the name of a function with 1 input parameter x
  fx = x^2              # and with that x some math is done, namely squaring x-
  return(fx)            # The result object fx is then "returned" to the console
} # End of function parabola

# Example
x = c(-4:4) # creates vector from -4 to 4, integer steps
parabola(x)
# > parabola(x)
# [1] 16  9  4  1  0  1  4  9 16

#### Random facts to get a grip on the scale of a CPU compared to its small size:

# Let us say your CPU has 9.69 billion transistors. A transistor
# is essentially a very tiny switch that can be in two states (0 or 1).
# Imagine you can walk through a life sized CPU.
# Say it takes one minute to get from one to another transistor.
9690000000/60/24/365
# [1] 18436.07 # years to reach every transistor

# One second to reach each transistor:
9690000000/60/60/24/365
# [1] 307.2679 # years to reach each transistor

# A RTX 4090 GPU has 79 Billion transistors
79000000000/60/24/365
# [1] 150304.4
79000000000/60/60/24/365
# [1] 2505.074


#### Code benchmarks are not of concern, except for ML/AI or fMRI analysis and such:
# x=c(rnorm(1000000,mean=22,sd=6))
# y=c(rnorm(1000000,mean=35,sd=4))

# linear regression with 1 Million data points:
# lm(y~x) # instant result with my computer...

#y=c(rnorm(100000000,mean=35,sd=4))
#x=c(rnorm(100000000,mean=22,sd=6))
# linear regression with 100 Million data points:
#lm(y~x) # took my computer around 10-12 sec.

# CLEAR WORKSPACE WHEN EXECUTING THE LAST FEW LINES
# otherwise it takes a while to save the work space for such large numbers...

# Print "Hello World!" and "Werde 1 Cyber!" ("Become a Cyber!" in German):
print("Hello World!")
print("Werde 1 Cyber!")



#########################################
# ------------------------------------- #
#########################################
#########################################
# 3 Open a Script and Execute Test Code #
#########################################

# Mark line and execute via ALT+ENTER or Cmd+ENTER (Mac),
# or make sure that the blinking | courser is in the line you want to execute 
# and then press ALT+ENTER or Cmd+ENTER for mac users.
test = 2 + 5

# Test Code:
test = 2 +4          # Spacing is mostly no problem...
#test zwei = 2 + 5   # ... except for object / variable names, see white X 
                     # and the left, stating Syntax error. R did not expect
                     # this in the sense that it does not allow spacing in names.
                     # remove the # in above line to see the effect.

# View output in console:
print(test)
# ...or
test
# [1] 7

# Only one object per line!
# The below line will give you an error (uncomment and execute to see effect):
# test1 = 2 test2 = 3
# Error: unexpected symbol in "test1 = 2 test2"

# We can use the delimiter ";" within R scripts though:
test1 = 2 ; test2 = 3
# Delimiters are a general concept of demarcation. In R it
# it demarcates lines. So using ";" is the same as starting a new line!

# The following overwrites the content of an object, as mentioned before:
test = 4 + 4
# [1] 8

# The following code duplicates an object, giving it a new name. The blow
# does not (!) rename an object. Everything that has been executed will stay
# in the work space; use brush tool to clear environment...
test3 = test
# > test3
# [1] 8

# The following uses == (logical operator) to check for equivalence (see Chapter 10 for more 
# information on logical operators and basic math functions). The output is a logical (see
# chapter 5 on classes to understand difference between logical, character, numeric...):
test3 == test
# [1] TRUE

# The following again overwrites test and hence test and test3 are now 
# not equivalent anymore:
test = 2 + 4
test == test3
# [1] FALSE


# # # # # # # # # # # # # # # # # # # # # 
#########################################
# ------------------------------------- #
#########################################
###################################################################
# 5 Classes of Objects: Vectors, Matrices, Arrays, Lists and More #
###################################################################

# Create a vector using c() i.e. the combine function:
vec = c(1,2,3)
vec <- c(1,2,3)  
# [1] 1 2 3  # NOTE that an output is also a vector
#              therefore the [1] at the beginning.

# Execute the following line to get "Help", i.e., the 
# documentation of how the function works:
?c()

# TYPICAL ERROR, ALWAYS CHECK CONSOLE OUTPUT FOR > ON THE END INDICATING
# THAT A PROCESS WAS COMPLETED:
# > # Typical Error:
# > vec = c(
# + 

# SINCE IT SAYS "+" in the console it means that the operation is not finished yet, since
# the second ")" is missing. In fact, when selecting the whole script, R
# will "think" that everything after "(" is part of the input in between two
# brackets. IN ANY CASE you have to type in the missing ")" and execute that line again.
# It will then show an error. You can then again execute the line you wanted to run.
# If this does not help, clean the environment and restart R!
# We also recommend using the brush tool in the environment in case you lost track
# of an error! Restart executing line by line until you find the erroneous line
# or look out for the white cross behind a red circle - errors can also of
# course be code that was just written wrong but otherwise can be executed...


# Execute line to call a specific element:
vec[2] # index 2 == second element of the vector
# [1] 2 # second element in the vector is also 2

# Change element:
vec[2] = 4
# [1] 1 4 3

# Turn vector into a matrix
mat = as.matrix(vec)
#      [,1]
# [1,]    1
# [2,]    4
# [3,]    3

# Look at the third element:
mat[3]
# or, since there is now also an index for the first and only column:
# Scheme = mat[row,column]
mat[2,]
# [1] 4

mat2 = as.matrix(t(vec))
mat2[,2]
# t() == transpose == tilting a vector (different with matrices!!!)
#      [,1] [,2] [,3]
# [1,]    1    2    3 

# Creating a matrix table via cbind() and c()
# This method can be used to create quick synthetic data sets in order 
# to test functions...
mat_bind = cbind(c(1,2,3),c(1,2,3),c(1,2,3))
#      [,1] [,2] [,3]
# [1,]    1    1    1
# [2,]    2    2    2
# [3,]    3    3    3

mat_bind2 = rbind(c(1,2,3),c(1,2,3),c(1,2,3))
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    1    2    3
# [3,]    1    2    3


# Note that you can select several rows and columns via
mat_bind[c(1,2),1] # first two rows, first column
# [1] 1 2

# Check class of our vector using the class() function:
class(vec)
is.numeric(vec)

# Transforming an atomic matrix table into a "molecular" data table.
# Here the columns can have different classes, each column for itself
# though is still atomic in its class. However, with the above method
# all columns will at first have the same class (even though it can 
# be changed, since different column classes are now possible). This
# is because the vectors were combined from an atomic perspective 
# via cbind(c()) so to speak and then the change into a molecular
# data type was performed afterwards. Therefore below all columns are 
# numeric, but would be character, if one element in the table was of type
# character. However, again, different column classes are now possible
# with the data.frame format, i.e. the table does not have to follow the
# atomic vector constrains anymore - further below we will look at an 
# alternative way to combine vectors right at the molecular level 
# (first we will look at changing column names):
mat_bind = as.data.frame(mat_bind) 
#   V1 V2 V3  # V stands for variable
# 1  1  1  1
# 2  2  2  2
# 3  3  3  3

# We can still use brackets and index to go to a specific location:
mat_bind[,2]
# Or we use the dollar sign to call/select a specific row of the table by name
# We can then also still use the bracket syntax notation:
mat_bind$V2
mat_bind$V2[2]
# you can also call a column by name within the brackets (same would be
# possible with lines when lines were given names via rownames()...):
mat_bind[1,"V2"]

# Column names can be changed via:
colnames(mat_bind) = c("One", "Two", "Three") # rownames() exists too
#      One Two Three
# [1,]   1   1     1
# [2,]   2   2     2
# [3,]   3   3     3

# Change single column name:
colnames(mat_bind)[2] = "Second_Col"
#   One Second_Col Three
# 1   1          1     1
# 2   2          2     2
# 3   3          3     3

# Note that with the above method the class of all columns will be 
# character, since combining a bunch of vectors c() is happening 
# on an atomic level and therefore a soon as one entry is character
# all entries are set to character. this can be avoided in the following way:
# Create three column vectors:
character = c("one", "two", "three")
numeric1 = c(1,2,3)
numeric2 = c(3,2,1)

# Use data.frame()
data_frame = data.frame(character,numeric1,numeric2)


# Alternative: First turn one vector into a data frame:
data_frame = as.data.frame(character) 
# And then add the others, so they are binded not at the atomic
# but molecular level, so to speak, and hence get correct class 
# assigned automatically and need not to be changed. Note that
# the below also demonstrates how to use cbind() to add a vector 
# to an existing table:
data_frame = cbind(data_frame,numeric1, numeric2)
is.character(data_frame$character)
# [1] TRUE 
is.numeric(data_frame$numeric1)
# [1] TRUE
is.numeric(data_frame$numeric2)
# [1] TRUE # Col 1 is character the others numeric: molecular data frame!

# In comparison to binding on the atomic levels via cbind() first:
data_frame_atomic = as.data.frame(cbind(character,numeric1,numeric2))
is.character(data_frame_atomic$character)
# [1] TRUE
is.character(data_frame_atomic$numeric1)
# [1] TRUE
is.character(data_frame_atomic$numeric2)
# [1] TRUE # ALL ARE STILL OF THE CLASS CHARACTER! 
           # But can be changed each now as it is
           # still also now a molecular data frame format


# String Example
string = c("One",2,3)
# Try to add a 1 to the second element in the string above:
# string[2]+1
# Error in string[2] + 1 : non-numeric argument to binary operator
class(string)
# [1] "character"

# Try to add a 1 to the second element in the string above:
# string[2]+1 # uncomment this line and execute to see effect
# Error in string[2] + 1 : non-numeric argument to binary operator

# Character string to numeric vector:
test = c("1","2","3")
is.character(test)
# [1] TRUE

# Turn into a numeric vector:
test = as.numeric(test)
is.numeric(test)
# [1] TRUE

# Check what happens if one element is an actual character string
# not just numbers treated as characters when turning that vector into class numeric:
test = c("test", "12q","2","3") # including typo situation
test = as.numeric(test) 
# [1] NA NA  2  3
# Warning message:
# NAs introduced by coercion

# The above output can be use to track entries in numeric columns with typos such as 12q,
# i.e. a letter made it into the column...


# Example array with dim = c(rows, columns, further_dimension)
# Below uses a trick: 1 or 0 at the beginning and defining dimension
# fills up the array with either 1 or 0:
array(1, dim = c(3,3,3))

# , , 1

#      [,1] [,2] [,3]
# [1,]    1    1    1
# [2,]    1    1    1
# [3,]    1    1    1

# , , 2

#      [,1] [,2] [,3]
# [1,]    1    1    1
# [2,]    1    1    1
# [3,]    1    1    1

# , , 3

#      [,1] [,2] [,3]
# [1,]    1    1    1
# [2,]    1    1    1
# [3,]    1    1    1


# Add a 1 to every entry:
array(1, dim = c(3,3,3)) + 1


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


###################################################################
# --------------------------------------------------------------- #
###################################################################
############################################################################
# 6 Import and Export of .CSV files, Understanding Delimiters and Packages #
############################################################################

# Read CSVs, uncomment to use and add your path or file name only, if file is in your 
# working directory folder (of a project).
# Formally loading a file can be done via the read.csv() or read.csv2()
# read.csv("Your file path")
# read.csv2("Your file path")

# Forgot you Working directory path? check via:
getwd() # No input needed!! Shows path assigned, e.g., for a project or standard folder

# url() Function (NO ACTUAL WEBSITE DO NOT EXECUTE!)
#  read.csv(url("http://www.website.net/data_file.csv"))

# Load Dino Data Set for this!
# I called the object "dino", such that plotting is done by:
# dino = read.csv("Your file path")
# In case you set up a project or placed your data files in the folder
# that getwd() shows as output, then you can use a shortcut using the
# file name only:
dino = read.csv("dino_csv.csv")
plot(x = dino$x, y = dino$y)

# Load Excel file:
# install.packages("readxl") # also within "tidyverse"
library("readxl")
# dino_excel = read_excel("dino_csv2.xlsx") # activate line when you have a .xlsx Version of the csv

# install.packages("readr")
library("readr") # package also within "tidyverse"
# Scheme: 
# write.csv(export_table, "File Path")
# write.csv()
# write.csv2()


# # # # # # # # # # # # # # # # # # # # # # # # 
#############################################
# ----------------------------------------- #
#############################################
#############################################
# 7 Example Data sets (DINOSAURS and more!) #
#############################################

# Install package for further data sets:
# install.packages("dplyr") # (dplyr also used for data cleaning)
data()                     # View all available data sets
library(dplyr)             # open/activate/load package
#data(starwars)            # load data set
#View(starwars)            # view via RStudio viewer
#?starwars                 # view documentation

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

# Perform a linear regression on the dinsaure data set:
dino = filter (datasaurus_dozen, dataset == "dino")  # filter dino data set
# lm() for slope and intercept  
lm(dino$y~dino$x)
# Full output iincl. t-test etc.
summary(lm(dino$y~dino$x)) # Slope is not significant...

#### Tidytuesday - weekly free data sets and challenges:
# install.packages("tidytuesdayR")
library("tidytuesdayR")

# Load example data set scooby doo:
tt_scooby = tt_load(2021,week=29)

# All Scooby Doo Episodes, whoop!
scooby = tt_scooby$scoobydoo


###########################################
# --------------------------------------- #
###########################################
###########################################
# 8 Short Introduction into Data Cleaning #
###########################################
#################################################################################
### 8.1 EXAMPLE I: Filtering Rows under the Condition of Certain Column Entries #
#################################################################################

# Example for the structure of data tables:
# Creating an example data set with vectors:
patient_id = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)
fam = c("yes", "yes", "no", "no", NA, NA, "n","n","no","no","ys", "ys", NA, NA)
time = c("t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2")
measurement_sysRR = c(130,122,132,123,133,121,129,125,135,119,134,127,140,125)

# data.frame() which concatenates vectors column-wise, as cbind():
table = data.frame(patient_id,time,measurement_sysRR,fam)

# install.packages("dplyr")  # install package
library(dplyr)             # load/activate package

# Filter function: filter(data_object, column_name == "entry") or != for unequal
t1 = filter(table, time == "t1")
t1 = filter(table, time != "t2") # alternative
t2 = filter(table, time == "t2")

# Alternative without filter function:
t1_alt = table[table$time == "t1",] # placed in row position; don't forget comma
t1 == t1_alt # All true...

# Also note that you can filter for multiple factors. The below keeps only
# those lines that are either "ys" in fam and "yes" in fam:
filter(table, fam == "ys" | fam == "yes") 

# From there we could go on an explore the data:
# Simple plotting example (note that the code is spread over two lines!!!):
plot(x = t1$patient_id,y = t1$measurement_sysRR, 
     ylim=c(100,150), 
     col = "blue", 
     ylab = "sysRR")
points(x=t2$patient_id,y=t2$measurement_sysRR, col = "darkgreen")

# ... and perform a paired/dependent t-test):
# Here using as.numeric() was necessary...
result = t.test(t2$measurement_sysRR, t1$measurement_sysRR, paired  = TRUE)

#         Paired t-test

# data:  as.numeric(t2$measurement_sysRR) and as.numeric(t1$measurement_sysRR)
# t = -6.1335, df = 6, p-value = 0.0008591
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -14.189293  -6.096421
# sample estimates:
#   mean difference 
#     -10.14286 


#######################################################
### 8.2 EXAMPLE II: Filtering NA's (Non-Trivial Case) #
#######################################################

# SLIGHTLY DIFFERENT, including NA in the measurements:
patient_id = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)
fam = c("yes", "yes", "no", "no", NA, NA, "n","n","no","no","ys", "ys", NA, NA)
time = c("t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2")
measurement_sysRRalt = c(130,122,132,NA,133,121,NA,125,135,119,134,127,140,125)

# cbind() and format into a data frame:
new_table = as.data.frame(cbind(patient_id,time,measurement_sysRRalt,fam))

# ...or, again, via the below in order to keep class integrity, since via cbind()
# columns are binded on a atomic level, such that given one character entry
# all entries will be characters. This can be avoided by turning one column
# into a data frame and then add the others via cbind().
new_table = as.data.frame(patient_id)
new_table = cbind(new_table,time,measurement_sysRRalt,fam)

# Mean sysRR at t1:
# First filter for t1:
t1alt = filter(new_table, time == "t1")
# Problem with NA
mean(t1alt$measurement_sysRRalt)
# [1] NA
# Warning message:
# In mean.default(t1alt$measurement_sysRRalt) :
#  Argument ist weder numerisch noch boolesch: gebe NA zurück 

# In general mean() also entails parameter that deletes NAs automatically.
# However, in our case the solution is a bit more complicated
mean(c(1,NA,3), na.rm = TRUE) 
# [1] 2

############################ Example for loop:
# Define a object you want to loop over:
object = c(1,1,1,1)

# Initialize a list to store the results:
list_results = c()

# For loop that adds +1 to every element of the above object:
for(i in 1:length(object)){
  list_results[i] = object[i]+1
} # End for i

# Analoge without loop (same code, just each i is written out individually line by line): 
list_results[1] = object[1]+1
list_results[2] = object[2]+1
list_results[3] = object[3]+1
list_results[4] = object[4]+1


######################## POSSIBLE SOLUTION I to get rid of patients data with only t1 or t2. not both:
# In which lines are NAs (line number, not pat. id!)?
# The below line of code is not assigned to an object name, so that the output 
# will directly be returned in the console. The output however can't be called
# via a name for another process then...
is.na(new_table$measurement_sysRRalt)
# [1] FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

# Note that in order to get the frequency of NA's you can do the following:
sum(is.na(new_table$measurement_sysRRalt)*1)
# [1] 2
summary(as.numeric(new_table$measurement_sysRRalt))
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   119.0   124.2   128.5   128.6   133.2   140.0       2 

# Using the which() function linguistically corresponds to our question:
# In which lines are NAs? Here we execute the is.na() function within which():
na_lines = which(is.na(new_table$measurement_sysRRalt) == TRUE)
# [1] 4 7 # Index of the lines with NA entry
# NOTE: Locating NAs in any context demands using the is.na() function to create
#       a logical vector, so to speak, from where we can ask in which line == TRUE...
#       In other words: NA can't be treated as character string, as for most cases using which()

# Alternative without executing a function within a function:
na = is.na(new_table$measurement_sysRRalt)
na_lines = which(na == TRUE) # which numeric index position is na?
# [1] 4 7 # Index of the lines with NA entry

# Initialize vector:
pat_id_na = c()

# Determine which patient_id is in the lines from the list na_lines: 
for(i in 1:length(na_lines)){
  pat_id_na[i] = new_table$patient_id[na_lines[i]]
} # End for i
pat_id_na
#[1] "2" "4" # Patient ids with NA in either t1 or t2

# Analoge without loop:               
pat_id_na[1] = new_table$patient_id[4]  
pat_id_na[2] = new_table$patient_id[7]  

# Alternative by using na_lines within the brackets of the col patient_id:
new_table$patient_id[4] == new_table$patient_id[na_lines[1]]
new_table$patient_id[7] == new_table$patient_id[na_lines[2]]


# Initialize table:
fin_table = new_table
# Delete every line with the patient_id given in the list
# pat_id_na:
for(i in 1:length(pat_id_na)){ # or length(na_lines)
  fin_table = filter(fin_table, patient_id != pat_id_na[i])
} # End for i
fin_table 

############################ Alternative of the above Step I and II:
# Using a nested for loop to get the patient ids with na
fin_table_alt = new_table
pat_id_na_alt = c()
# Patient IDs with NA, alternative way via one nested loop:
for(i in 1:length(new_table$measurement_sysRRalt)){
  if(is.na(new_table$measurement_sysRRalt[i])==TRUE){
    pat_id_na_alt[i] = new_table$patient_id[i]
  } # End if
} # End for i

pat_id_na_alt
# [1] NA NA NA  2 NA NA  4

# Filter NA cases of a single column, where new_table$measurement_sysRRalt[i])==FALSE and 
# no i was stored to the vector pat_id_na_alt...
pat_id_na_alt = pat_id_na_alt[!is.na(pat_id_na_alt)]
# [1] 2 4

# Now we could use the previous code for filtering the table...



################################################## POSSIBLE SOLUTION II:
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

which(new_table_alt$patient_id == unique_id[1])
# [1] 1 2

length(which(new_table_alt$patient_id == unique_id[1]))
# [1] 2

# Check for equality of both methods:
fin_table$patient_id == fin_table_alt$patient_id
# [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE


#####################################################
### 8.3 EXAMPLE III: For loops and nested for loops #
#####################################################

## Example for loop:

# Define an object you want to loop over:
object = c(1,1,1,1)

# Initialize a list to store the results:
list_results = c()
# [1] NULL

# For loop that adds +1 to every element of the above object:
for(i in 1:length(object)){ # i = 1 to n = 4; you could also start at i = 2, theoretically!
  list_results[i] = object[i]+1 # same scheme as repetitive code below, just using i for the indices now
} # End for i
list_results
# [1] 2 2 2 2

# Analoge without loop (same code, just each i is written out individually line by line)
list_results[1] = object[1]+1
list_results[2] = object[2]+1
list_results[3] = object[3]+1
list_results[4] = object[4]+1

list_results
# [1] 2 2 2 2 

# A for loop is essentially used to perform a repetitive process with shorter syntax

## Nested for loop example (loop within a loop) 
object2 = cbind(c(1,1,1),c(1,1,1))
#      [,1] [,2]
# [1,]    1    1
# [2,]    1    1
# [3,]    1    1

# Create empty matrix with zeros only:
result = matrix(0,nrow = length(object2[,1]), ncol = length(object2[1,]), byrow = TRUE)

# IT WOULD ALSO BE POSSIBLE to first loop over the cols and then the rows... 
# Here we will do it the other way around (since for this case at least it makes no difference):
for(i in 1:length(object2[,1])){    # loop over rows; the length of a col tells you number of rows
  for(j in 1:length(object2[1,])){  # loop over cols; the length of a row tells you number of cols    
    result[i,j] = object2[i,j]+1
  } # End for j
} # End for i

result
#      [,1] [,2]
# [1,]    2    2
# [2,]    2    2
# [3,]    2    2

# Note that for both cases the following is enough:
object+1
# [1] 2 2 2

object2+1
#      [,1] [,2]
# [1,]    2    2
# [2,]    2    2
# [3,]    2    2


###########################################
### 8.4 EXAMPLE IV: Apply Function Family #
###########################################

# For repetitive action you can also use function of the apply family.
# The below code performs input+1 column wise for every element (MARGIN = 1 row wise).
# For apply() functions (also mapply(), lapply() etc.) you have to either call
# or write a function that does the respective action. Here we just need
# a function that adds a 1 to the input (here x = input) of the the used apply
# function.
apply(result, MARGIN = 2, FUN = function(x){x+1})
#      [,1] [,2]
# [1,]    3    3
# [2,]    3    3
# [3,]    3    3

# Here sum the cols via calling sum() within apply()
apply(result, MARGIN = 2, FUN = sum)
#[1] 6 6
# Here sum the rows
apply(result, MARGIN = 1, FUN = sum)
# [1] 4 4 4

########################################################################
### 8.5 EXAMPLE V: Introduction into “tidyverse” and the Pipe Operator #
########################################################################

#### Pipe operator %>% using tidyverse:
# install.packages("tidyverse")
library("tidyverse")

# Pipe operator reads out "and then" (executes more code at once
# and therefore makes it more convenient, but you may want to execute
# each of your steps anyways without loop...):

# Test 1 Filter of t2 and  deleting lines with NAs 
test1 = new_table %>% 
  select(patient_id,time,measurement_sysRRalt) %>% # select columns like cbind()
  filter(time != "t1")%>%  # Filter for t1
  na.omit()
# na.omit() # be careful with this function!!!
# and no %>% at the end, otherwise "+" in console!!!!

# Note that the filter() function here does not need the input for the table,
# it is already passed on from the previous piped line... This can be 
# confusing as a beginner, when starting with piped functions and then wanting 
# to use each function for itself.

test1 # for t2 only patient_id 2 has an NA...
#   patient_id time measurement_sysRRalt
# 1          1   t2                  122
# 3          3   t2                  121
# 4          4   t2                  125
# 5          5   t2                  119
# 6          6   t2                  127
# 7          7   t2                  125

# Comparison of piped and unpiped code for selecting columns:
new_table %>%
  select(patient_id,time)

# Casual way we have gone through before via $ and cbind()
new_table2 = as.data.frame(cbind(new_table$patient_id,new_table$time))

# Scheme: select(data_table, colname1, colname2) or more columns of course...
new_table3 = select(new_table, patient_id, time)

# Check for equivalence:
new_table2 == new_table3 # all TRUE...


###################################################################################
### 8.5.1 Representation of “Solution/Algorithm II” from Example II via tidyverse #
###################################################################################


### POSSIBLE SOLUTION II Simple dplyr/tidyverse alternative:
new_table_alt %>%   # new_table_alt was filtered for NAs already!!! 
  select(patient_id,time,measurement_sysRRalt)%>%
  group_by(patient_id) %>%  # sorts individ. entries into indiv. groups 
  # when executing next line!
  filter(n()>1)     # filters if n of each grouped entry is greater 
# than 1 such that all single entries get filtered



########################################################################################################
### 8.6 EXAMPLE VI: Adjusting Character String Entries and Pattern Matching and Deleting Empty Entries #
########################################################################################################

#### Duplicate our previous table:
fin_table_alt = new_table

# Use the unique() function to show all individual entries (without duplicates so to speak)
unique(fin_table_alt$fam)
# [1] "yes" "no"  NA    "n"   "ys" 

# Locate all entries of a column matching a specific criteria, here "ys"
which(fin_table_alt$fam =="ys")
# [1] 11 12

# Correct entries e.g. via: The below is a composition of the functions
# fin_table$fam[]  and  which()  and  is.na() for renaming NAs. Not
# Loop is needed when using the which function wihtin the [] of a column...
fin_table_alt$fam[which(is.na(fin_table_alt$fam) == TRUE)] = "not specified"
fin_table_alt$fam[which(fin_table_alt$fam =="ys")] = "yes"
fin_table_alt$fam[which(fin_table_alt$fam =="n")] = "no"

# Check if it worked:
unique(fin_table_alt$fam)
# [1] "yes"           "not specified" "no"    

# Pie chart (since frequencies are not shown, they do not have to be divided by two):
pie(table(fin_table_alt$fam))

#### Get last name only, which is first position!
library("stringr") # part of tidyverse
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

# All words first letter upper case (from stringr; locale = adjust language, "en" default):
#install.packages("stringr")
library(stringr)
str_to_title(c("name name","naMe","NAME"))
# [1] "Name Name" "Name" "Name"

# Select certain parts of a string:
text = c("Become a Cyber!")
substr(text,10,15) # Element 10 to 15
# [1] "Cyber!"

# Another important function is grep() which can detect a part of a character 
# string or a pattern, so speak. It is from the base package
# grep(pattern = , object)
text = c("Bla", "Blub", "Bluna")
grep("Blu", text)
# Output is the positions, equivalent to the which() function output.
# [1] 2 3

# grepl() does pattern matching returning a logical:
grepl("Blu",text)
# [1] FALSE  TRUE  TRUE

# Replace parts of a character string (similar to example in 8.11)
gsub("Blu","Bla", text)
# [1] "Bla"   "Blab"  "Blana"

# Similar to deleting NAs (as shown in the next chapter) you can also delete
# those entries that are empty and only entail "":
Col1 = c("one","","three")
Col2 = c("one", "two", "three")
table_emp_entry = as.data.frame(cbind(Col1,Col2))
#    Col1  Col2
# 1   one   one
# 2         two
# 3 three three

# Filter for "" or nchar(): 
filter_emp1 = filter(table_emp_entry,Col1 != "") # keep those entries that are not empty ""
filter_emp2 = filter(table_emp_entry,nchar(Col1) > 0) # keep entries with number of characters greater zero
filter_emp3 = table_emp_entry[table_emp_entry$Col1 != "",] # keep those entries that are not empty ""
#    Col1  Col2
# 1   one   one
# 2 three three

# Check for equality:
filter_emp1 == filter_emp2 & filter_emp2 == filter_emp3 
#      Col1 Col2
# [1,] TRUE TRUE
# [2,] TRUE TRUE

# %in% "in"-Operator, which checks if elements of one vecotr are in another:
c(1,2,3) %in% c(1,2,3,4,5)
# [1] TRUE TRUE TRUE
sum(c(1,2,3) %in% c(1,2,3,4,5))
# [3]

# Reverse case (sum will be equivalent in both cases:
c(1,2,3,4,5) %in% c(1,2,3)
# [1]  TRUE  TRUE  TRUE FALSE FALSE
sum(c(1,2,3,4,5) %in% c(1,2,3))
# [1] 3

#  Slightly adjusted:
c(1,2,3) %in% c(1,2,3,3,5)
# [1] TRUE TRUE TRUE 
sum(c(1,2,3) %in% c(1,2,3,3,5))
# [1] 3 

# Reverse case (sum will NOT BE equivalent in both cases!!!):
c(1,2,3,3,5) %in% c(1,2,3)
# [1]  TRUE  TRUE  TRUE TRUE FALSE
sum(c(1,2,3,3,5) %in% c(1,2,3))
# [1] 4

# Relation of %in% to the function duplicated(): 
duplicated(c(1,2,3,3,5))
# [1] FALSE FALSE FALSE TRUE FALSE

# The sum of TRUE  TRUE  TRUE TRUE FALSE - the sum of  TRUE  TRUE  TRUE  
#                                                               == FALSE FALSE FALSE TRUE FALSE
# 4 - 3 = 1
sum(c(1,2,3,3,5) %in% c(1,2,3)) - sum(c(1,2,3) %in% c(1,2,3,3,5)) == sum(duplicated(c(1,2,3,3,5)))
# [1] TRUE

#############################################################################################
### 8.7 EXAMPLE VII: Deleting NA's from a Single Row and Introducing the Tibble Data Format #
#############################################################################################

library("tidyverse") # needed to create tibble data tables.

mean(c(1,NA,3), na.rm = TRUE)

# Simple tibble table:
tibble_test = as.tibble(cbind(c(1,2,3), c("a","b","c"), c("ey","bee","see")))

# Tibble also shows the dimension and the 
# classes of the columns in the console output:
# A tibble: 3 × 3
#   V1    V2    V3   
#   <chr> <chr> <chr>
# 1 1     a     ey   
# 2 2     b     bee  
# 3 3     c     see  

### Deleting NAs of a column vector, rows with NAs in a certain column and
### changing NAs into "keine Angabe" (mainly) with the data format tibble, but
### the same code also works for the data.frame format.

# Delete row with data.frame:
x = new_table

# Below is not "saved" since no object name, therefore table x keeps NAs for 
# other examples below:
x$measurement_sysRRalt[!is.na(x$measurement_sysRRalt)]
# [1] 130 122 132 133 121 125 135 119 134 127 140 125

#### SAME AND MORE BUT NOW WITH TIBBLE FORMAT:

# "new_table" as tibble
x = as.tibble(x)
class(x)
# [1] "tbl_df"     "tbl"        "data.frame"

# Change entry:
x[4,3] = 122
x$measurement_sysRRalt[4]
# [1] 122

# Check class of column:
class(x$measurement_sysRRalt)
# [1] "numeric"

# Change all entries with NA to "keine Angabe", even though different class:
x$measurement_sysRRalt[which(is.na(x$measurement_sysRRalt)==TRUE)] = "keine Angabe"

# Class of column is now changed to character:
class(x$measurement_sysRRalt)
# [1] "character"

# Delete row with NAs
# We first have to recreate original x with NAs, since we changed NA into "keine Angabe" above:
x = as.tibble(new_table)
new_x = filter(x, is.na(measurement_sysRRalt) != TRUE)
# A tibble: 12 × 4
#    patient_id time  measurement_sysRRalt fam  
#         <dbl> <chr>                <dbl> <chr>
#  1          1 t1                     130 yes  
#  2          1 t2                     122 yes  
#  3          2 t1                     132 no   
#  4          3 t1                     133 NA   
#  5          3 t2                     121 NA   
#  6          4 t2                     125 n    
#  7          5 t1                     135 no   
#  8          5 t2                     119 no   
#  9          6 t1                     134 ys   
# 10          6 t2                     127 ys   
# 11          7 t1                     140 NA   
# 12          7 t2                     125 NA

# Delete NA of a column (not the whole line, just single entries of a column vector)
# Whe first have to recreate original x with NAs (above line deleted lines with NA in below column...):
x = as.tibble(new_table)
x$measurement_sysRRalt[!is.na(x$measurement_sysRRalt)]
# [1] 130 122 132 133 121 125 135 119 134 127 140 125

# You can also use na.omit(), but you should only do so for vectors, not tables/matrices:
# The output format is nasty though; change via is.numeric():
x = as.tibble(new_table)
x2 = na.omit(x$measurement_sysRRalt)
#  [1] 130 122 132 133 121 125 135 119 134 127 140 125
# attr(,"na.action")
# [1] 4 7
# attr(,"class")
# [1] "omit"

# Again, the na.omit() is risky on tables, deletes every line no matter which column
# NAs are found, which is usually not what you want at all!!
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

# Calculate mean using as.numeric():
x3 = mean(as.numeric(x2))
# [1] 128.5833

# Another function that can be used is drop_na(df,column), where
# you just have to name the table and the column with NAs, which you 
# want to delete (the lines, so similar to filter(data, is.na(data$col)==TRUE)):
# Test data for drop_na() - function only works with data frames:
a = c(1,2,3)
b = c(1,2,NA)
c = c(1,NA,3)
data = as.data.frame(cbind(a,b,c))
data
#   a  b  c
# 1 1  1  1
# 2 2  2 NA
# 3 3 NA  3

# Drop lines given NAs in column b
drop_na(data,b)
# > drop_na(df,b)
#   a b  c
# 1 1 1  1
# 2 2 2 NA


#################################################################
### 8.8 EXAMPLE VIII: Rearranging and Deleting Columns and Rows #
#################################################################

test_re = as.data.frame(cbind(c(1,2,3),c(4,5,6),c(7,8,9)))
# > test_re
# V1 V2 V3
# 1  1  4  7
# 2  2  5  8
# 3  3  6  9

rearrange = test_re[,c(3,2,1)]  
rearrange = test_re[,c("V3","V2","V1")] # same result
# > rearrange
# V3 V2 V1
# 1  7  4  1
# 2  8  5  2
# 3  9  6  3

# You can also use cbind() and rearrange a new table calling via $ row by row.
# Create an object of each row in order to cahnge the names or use the colnames()
# function to do so. 

# Deleting cols (same works for deleting lines at the 
# line index position!)
del_col = rearrange[,-c(2)]
del_col = rearrange[,-2]

# > del_col
#   V3 V1
# 1  7  1
# 2  8  2
# 3  9  3

# Delete range
del_col = rearrange[,-2:-3]
# [1] 7 8 9 # Just a vector remains...


######################################################################################
### 8.9 EXAMPLE IX: Working with Redundant Columns (Encoding under Certain Criteria) #
######################################################################################

# Say, you want to create a column within your data set that entails, e.g., a 
# binary (yes/no, 0/1, TRUE/FALSE), given a certain entry criteria is fulfilled
# in the horizontally correspondent entry within another column. This is how:

# Creating a test data frame that entails a column with all zeros at the end
# Same works with c("")...
test = as.data.frame(cbind(c(1,2,3),c(1,"certain entry criteria",3),c(0)))

# > test
#   V1                       V2 V3
# 1  1                        1  0
# 2  2   certain entry criteria  0
# 3  3                        3  0

for(i in 1:length(test$V2)){ # cols have same length, same result with V1 or V3 
  if(test$V2[i] == "certain entry criteria"){
    test$V3[i] = "criteria fulfilled"
  } # End if 
} # End for i
test
# > test
#   V1                     V2                  V3
# 1  1                      1                   0
# 2  2 certain entry criteria criteria fulfilled
# 3  3                      3                   0

######## More than one criteria:
# Creating a test data frame that entails a column with all zeros at the end
# Same works with c("")...
test_multi = as.data.frame(cbind(c("criteria","criteria",3),c(1,"criteria",3),c(0)))

# > test_multi
#         V1       V2 V3
# 1 criteria        1  0
# 2 criteria criteria  0
# 3        3        3  0

# Here V3 now entails the entry "criteria fulfilled" only   
# when two criteria are fulfilled!! Via the operator & below.
# You could also use | which functions as "or" if that's what you need...
for(i in 1:length(test_multi$V1)){ # cols have same length, same result with V2 or V3 
  if(test_multi$V1[i] == "criteria" & test_multi$V2[i] == "criteria"){
    test_multi$V3[i] = "criteria fulfilled"
  } # End if 
} # End for i
test_multi

# > test_multi
#         V1       V2                  V3
# 1 criteria        1                   0
# 2 criteria criteria criteria fulfilled
# 3        3        3                   0



##########################################
### 8.10 EXAMPLE X: Decoding and Sorting #
##########################################

code = c("a","b","a","c","a")
count = c(20,22,19,5,44)
table = as.data.frame(cbind(code,count))

#   code count
# 1    a    20
# 2    b    22
# 3    a    19
# 4    c     5
# 5    a    44

# Sort by count, decreasing (here you have to change the classes of the column count!):
table[order(as.numeric(table$count), decreasing = TRUE),]
#   code count
# 5    a    44
# 2    b    22
# 1    a    20
# 3    a    19
# 4    c     5


# Decode via loop (overwrites column "code"):
for(i in 1:length(table$code)){
  if(table$code[i] == "a"){
    table$code[i] = "Something1"
  } # End if "a"
  else if(table$code[i] == "b"){
    table$code[i] = "Something2"
  } # End if "b"
  else if(table$code[i] == "c"){
    table$code[i] = "Something3"
  } # End if "c"
} # End for i

# Check for results
print(table)
#         code count
# 1 Something1    20
# 2 Something2    22
# 3 Something1    19
# 4 Something3     5
# 5 Something1    44

# ALTERNATIVE Decode via loop (decodes in extra/redundant column "code")
decode = c("") # empty vector
table_new = as.data.frame(cbind(code,count,decode))

#   code count decode
# 1    a    20       
# 2    b    22       
# 3    a    19       
# 4    c     5       
# 5    a    44

for(i in 1:length(table_new$code)){
  if(table_new$code[i] == "a"){
    table_new$decode[i] = "Something1"
  } # End if "a"
  else if(table_new$code[i] == "b"){
    table_new$decode[i] = "Something2"
  } # End if "b"
  else if(table_new$code[i] == "c"){
    table_new$decode[i] = "Something3"
  } # End if "c"
} # End for i

# Check for results
print(table_new)
#   code count     decode
# 1    a    20 Something1
# 2    b    22 Something2
# 3    a    19 Something1
# 4    c     5 Something3
# 5    a    44 Something1

# Shorter Version with nested for loop, but more complex: 
# (at least with a longer decoding list it would be shorter...):
decode = c("") # empty vector
table_new = as.data.frame(cbind(code,count,decode))

# The below for loop is not optimal, since it loops over each vector, where
# a table would be enough - see next example:
decode = c("Something1","Something2","Something3")
codes = c("a","b","c")
for(i in 1:length(table_new$code)){ # loop over table column
  for(j in 1:length(codes)){        # then over the possible codes
    for(k in 1:length(decode)){     # and the possible decoding ("Something")
      if(j == k & table_new$code[i] == codes[j]){ # makes sure j and k are equal/parallel for decoding!
        table_new$decode[i] = decode[k]            
      } # End if
    } # End for k
  } # End for j
} # End for i

table_new
#   code count     decode
# 1    a    20 Something1
# 2    b    22 Something2
# 3    a    19 Something1
# 4    c     5 Something3
# 5    a    44 Something1


############### This time only i and j needed, so even shorter!!!
decode = c("") # empty vector
table_new = as.data.frame(cbind(code,count,decode))

decode = c("Something1","Something2","Something3")
codes = c("a","b","c")
de_code = as.data.frame(cbind(decode,codes)) # CODE AND DECODE AS TABLE NOT SINGLE VECTORS!!!
for(i in 1:length(table_new$code)){ # loop over table column
  for(j in 1:length(de_code$codes)){        # then over the possible codes
    if(table_new$code[i] == de_code$codes[j]){ # here & argument not necessary anymore!
      table_new$decode[i] = de_code$decode[j]            
    } # End if
  } # End for j
} # End for i

table_new
#   code count     decode
# 1    a    20 Something1
# 2    b    22 Something2
# 3    a    19 Something1
# 4    c     5 Something3
# 5    a    44 Something1


##################################################################
### 8.11 EXAMPLE XI: Changing Ä/ä into Ae/ae and Jumbled Unicode #
##################################################################

# Forgot to find the source for this one:
# install.packages("stringi")
library(stringi)

# Test character string:
test_umlaut = c("ae", "oe", "ue", "Ae", "Oe", "Ue")

test_umlaut = stri_replace_all_fixed(
  test_umlaut, 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue"), # exchange...
  c("ä", "ö", "ü", "Ä", "Ö", "Ü"),       # ... these vectors for the inverse case...
  vectorize_all = FALSE)

test_umlaut
# > test_umlaut
# [1] "ä" "ö" "ü" "Ä" "Ö" "Ü"

### Jumbled German text:
jumble = c("Stra\xdfe", "B\xfccher")

for(i in 1:length(jumble)){
  jumble = stri_replace_all_fixed(
    jumble, 
    c("\xc4","\xe4","\xdc", "\xfc","\xd6","\xf6", "\xdf"), # exchange to...
    c(  "Ä",   "ä",  "Ü",    "ü",    "Ö",  "ö", "ß"),   # ... this
    vectorize_all = FALSE)
} # End for i

# 1] "Straße" "Bücher"

################################################################
### 8.12 EXAMPLE XII: Deleting Duplicates (NOS Paper Example!) #
################################################################


# NOS Paper Example (https://github.com/StSchwerdtfeger/Filtering-Duplicates):

# For new release of current preprint by Rico Schmitt:
# https://journal.medicine.berlinexchange.de/pub/nqjpou17/release/1 

# FULL R SCRIPT AND DATA AVAILABLE IN THIS PUBLICATION!!!!!

# The purpose of this function: delete all duplicate values, as timepoint 1 and t2
# cannot be connected unambivalently due to redundant encoding of id values in the 
# above publication (e.g. 1000 participants but only three digit code!!). 

# Issue:   duplicated() only solves for a logical with all duplicated values but
#          does not return a logical that can be related to the original id column,
#          since it does not entail a TRUE for the value that is duplicated itself as well.

# EXAMPLE: In c( 1, 2, 2, 3) duplicated() returns: false, false, true, false. 
#          We want to delete all 2's in that column vector! I.e., we desire an output of
#          false, true, true, false, in order to use that column vector to identify 
#          the rows in the data set, which are supposed to be deleted: 
#          any duplicate id value. 

# THE BELOW presents a possible solution via a nested for loop:

# Special filtering function for Rico Schmitt:
filtering = function(x,y){ # x = id column vector; y = full data set (i.e., given class = data frame)
  library(dplyr) # load dplyr
  inter = duplicated(x)*1  # duplicated gives a logical, multiplied by 1
  # and transforms it into a binary vector  
  
  binded = as.data.frame(cbind(inter,x))  # bind id column and binary vector (column)
  # + change class to data frame,
  # otherwise upcoming dplyr filter 
  # function wont work:   
  
  inter2 = filter(binded, inter == "1" )  # filter rows with binary 0 :=
                                          # explicit non (!) duplicate values
  # inter2 = filter(binded, inter != "0") # => optionally delivers equivalent results. 
  
  blank = matrix(0,length(x))             # matrix vector list for the nested for loop:
  
  for(i in 1:length(inter2$x)){           # for each individual [i] value of inter2 
                                          # i.e., each value with duplicate(s)
    for(j in 1:length(x)){                # + for each respective duplicate
                                          #  value in column vector with the ids,
      if(x[j] == inter2$x[i]){            # if the i_dth value is equivalent to 
                                          # the evaluated explicit duplicate id values:
        blank[j] = 1   # then / in such a case assign a 1 to the redundant binary/logical column.
      } # End if       # now any duplicate and the value that is duplicated are tagged by a logical!
    } # End for j
  } # End for i
  
  data_frame = cbind(y,blank)   # Now combine the redundant binary column 
  # with the whole data frame that carries 
  # the id column
  data_frame = as.data.frame(data_frame)       # turn it into a data frame
  filtered = filter(data_frame, blank != "1" ) # ... in order to finally filter 
                                               #  all (!) redundant id values, no 
                                               #  matter how many duplicates there are,
                                               #  in other words: if any id value had a duplicate, both the original 
                                               #  and duplicate were erased, due to the fact that column and row wise
                                               #  relation cannot be evaluated unambivalently in the given data set 
                                               #  of the above mentioned paper by Rico Schmitt. 
  return(filtered) # Return filtered returns the values of the respective object, 
  # i.e., the filtered full data frame
} # End of function



# Test data frame with duplicates in row 1 and 3 (id10):
test_dup = as.data.frame(cbind(c("id10","id12","id10","id14","id15","id16"),
                               c("random entry","random entry","random entry"), 
                               c("random entry","random entry","random entry")))
# > test_dup
#     V1           V2            V3
# 1 id10 random entry random entry
# 2 id12 random entry random entry
# 3 id10 random entry random entry
# 4 id14 random entry random entry
# 5 id15 random entry random entry
# 6 id16 random entry random entry

# id10 has a duplicate. We want to delete both, i.d., row 1 and 3:
filtering(test_dup$V1,test_dup)

# Row 1 and 3, i.e. id10 has been deleted. I was lazy
# so the redundant column "blank" remains and only entails  
# zeros, since all lines that marked duplicate and the value 
# that was duplicated with a 1 have been deleted...

#     V1           V2           V3 blank
# 1 id12 random entry random entry     0
# 2 id14 random entry random entry     0
# 3 id15 random entry random entry     0
# 4 id16 random entry random entry     0



#########################################################
### 8.13 EXAMPLE XIII: Converting Wide into Long Format #
#########################################################

# Creating an example data set with vectors:
patient_id = c(1:7)
sysRR_t1 = c(130,132,133,129,135,134,140)
sysRR_t2 = c(122,123,121,125,119,127,125)

# cbind() which concatenates vectors column-wise and format into a data frame:
wide_table = as.data.frame(cbind(patient_id,sysRR_t1,sysRR_t2))
# > wide_table
#   patient_id sysRR_t1 sysRR_t2
# 1          1      130      122
# 2          2      132      123
# 3          3      133      121
# 4          4      129      125
# 5          5      135      119
# 6          6      134      127
# 7          7      140      125

# Optionally you can turn the above table into a long format again:
library(reshape2) # within tidyverse

# Converting data frame into a long format, which some functions such may demand:
# Here it only makes sense for the columns patient_id and sysRR_t1 and sysRR_t2.
# Here the parameters variable.name and value.name create new column names for 
# the reshaped table. Below I chose time for either sysRR_t1 and sysRR_t2 as entry. 
# Value.names creates a column with the actual values, which are here the actual 
# sysRRs from t1 and t2, so this column is called sysRR. The melt() function also
# asks for a column with id's, which can also be just the row index as well, but
# here we actually have a column with ids.
long_format = melt(wide_table, id.vars = "patient_id",
                                  variable.name = "time",
                                  value.name = "sysRR")

# > long_format
#    patient_id     time sysRR
# 1           1 sysRR_t1   130
# 2           2 sysRR_t1   132
# 3           3 sysRR_t1   133
# 4           4 sysRR_t1   129
# 5           5 sysRR_t1   135
# 6           6 sysRR_t1   134
# 7           7 sysRR_t1   140
# 8           1 sysRR_t2   122
# 9           2 sysRR_t2   123
# 10          3 sysRR_t2   121
# 11          4 sysRR_t2   125
# 12          5 sysRR_t2   119
# 13          6 sysRR_t2   127
# 14          7 sysRR_t2   125
 
# Alternative via gather() which does not require an id column
library(tidyr)
# Here the parameters key and value create new column names for the reshaped table.
# Below I chose time for either sysRR_t1 and sysRR_t2 as entry. Values creates a
# column with the actual values, which are here the actual sysRRs from t1 and t2,
# so this column is calles sysRR.
data_long <- gather(wide_table, key = "time", value = "sysRR", sysRR_t1:sysRR_t2)

# > long_format
#    patient_id     time sysRR
# 1           1 sysRR_t1   130
# 2           2 sysRR_t1   132
# 3           3 sysRR_t1   133
# 4           4 sysRR_t1   129
# 5           5 sysRR_t1   135
# 6           6 sysRR_t1   134
# 7           7 sysRR_t1   140
# 8           1 sysRR_t2   122
# 9           2 sysRR_t2   123
# 10          3 sysRR_t2   121
# 11          4 sysRR_t2   125
# 12          5 sysRR_t2   119
# 13          6 sysRR_t2   127
# 14          7 sysRR_t2   125


##### Alternative without fancy functions:
# Create col with repeating patient_ids:
patient_id = c(wide_table$patient_id,wide_table$patient_id) 

# Create a col with the info sysRR_t1/sysRR_t2 and a col with the sysRRs
t1_long = cbind(c("sysRR_t1"),wide_table$sysRR_t1) # "sysRR_t1" will be entailed in the whole column this ways
t2_long = cbind(c("sysRR_t2"),wide_table$sysRR_t2)
time_sysRR = rbind(t1_long,t2_long)

# Create long format table:
long_format = as.data.frame(cbind(patient_id,time_sysRR))

# Rename columns
colnames(long_format) = c("patient_id", "time", "sysRR")

# Check:
long_format

# > long_format
#    patient_id     time sysRR
# 1           1 sysRR_t1   130
# 2           2 sysRR_t1   132
# 3           3 sysRR_t1   133
# 4           4 sysRR_t1   129
# 5           5 sysRR_t1   135
# 6           6 sysRR_t1   134
# 7           7 sysRR_t1   140
# 8           1 sysRR_t2   122
# 9           2 sysRR_t2   123
# 10          3 sysRR_t2   121
# 11          4 sysRR_t2   125
# 12          5 sysRR_t2   119
# 13          6 sysRR_t2   127
# 14          7 sysRR_t2   125


# Extreme form of long format (here done with a loop; 
# code below was transformed into a function in the respective chapter on functions below):
extra_long_format = c()
for(i in 1:length(long_format[1,])){
  extra_long_inter  = as.data.frame(cbind(colnames(long_format)[i],long_format[,i]))
  extra_long_format = rbind(extra_long_format,extra_long_inter) # as.data.frame() not necessary anymore here
} # End for i

# Rename colnames:
colnames(extra_long_format) = c("colname","value") 

##### Same with e.g. gather() without extra parameters:
data_extra_long = gather(long_format)

# Check for equivalence: 
extra_long_format==data_extra_long # => all TRUE

# Results:
extra_long_format 
#       colname    value
# 1  patient_id        1
# 2  patient_id        2
# 3  patient_id        3
# 4  patient_id        4
# 5  patient_id        5
# 6  patient_id        6
# 7  patient_id        7
# 8  patient_id        1
# 9  patient_id        2
# 10 patient_id        3
# 11 patient_id        4
# 12 patient_id        5
# 13 patient_id        6
# 14 patient_id        7
# 15       time sysRR_t1
# 16       time sysRR_t1
# 17       time sysRR_t1
# 18       time sysRR_t1
# 19       time sysRR_t1
# 20       time sysRR_t1
# ...    ...   ...   ...      
# ...    ...   ...   ...      

# EXECUTE LINES ABOVE TO VIEW FULL OUTPUT IN CONSOLE!


##################################################################################################
### 8.14 EXAMPLE XIV: Imputation - Dealing with Missing Entries in Surveys via Nested for Loops  #
##################################################################################################

# This is an example I have encountered during the in-person tutorial series 
# @CIPOM/Charité (thanks to the participant and Lernzentrum tutor 
# Yannick Jenke for this example!). It was about the outcome of several 
# SF-36/RAND-36 Surveys. The SF-36/RAND-36 is a short form survey evaluating 
# “perceived health or health-related quality of life”. 

# Imputation of missing values in a survey. Thanks to Yannick Jenke for this example!

# Example data frame
x = data.frame(cbind(c(1,NA,3),c(1,2,3), c(4,3,3)))
# > x
#   X1 X2 X3
# 1  1  1  4
# 2 NA  2  3 # Missing survey score of the participant in this line!
# 3  3  3  3


# Empty vector for results:
result = c()
# For loop calculating mean of each column and then places it in the 
# nested for loop where NAs had been. 
for(i in 1:length(x[,1])){ # loop over rows/lines
  line = x[i,]             # Extract line and call it line
  result[i] = mean(line[!is.na(line)]) # deletes NA from a row and calculates mean.
  for(j in 1:length(line)){            # (Nested for loop) over the columns (each survey question)
    if(is.na(line[j])==TRUE){          # and if an entry == NA
      x[i,j] = result[i]               # then the entry will be exchanged by the row mean
    } # End if
  } # End for j
} # End for i

# Look at result in console:
print(x)
# > x
#    X1 X2 X3
# 1 1.0  1  4
# 2 2.5  2  3
# 3 3.0  3  3

# Line 1 had an NA entry, the rest of the line was a 2 and a 3, such
# that mean(2 + 3) = 2.5. As you can see, the missing values was exchanged
# with the mean of 2.5 in line 2, column 1. 

                                                                                                                                       

############################################################################
### 8.15 EXAMPLE XV: Time Difference between Dates in Days and other Units # 
############################################################################

# Example vectors with dates in the scheme dd.mm.yyyy:
dates_1 = c("12.02.1998","12.04.2004","04.05.2014")
dates_2 = c("15.07.2022","01.11.2021","06.06.2016")

# Change into a date object (this is something that can be daunting, since I
# had the experience that one has to change an object into an date object redundantly 
# in several processes (e.g. when working with a loop)). The below essentially
# reformats the dates into YYY-MM-DD:
dates_1 = as.Date(dates_1, format = "%d.%m.%Y")
# [1] "1998-02-12" "2004-04-12" "2014-05-04"
dates_2 = as.Date(dates_2, format = "%d.%m.%Y")
# [1] "2022-07-15" "2021-11-01" "2016-06-06"

# To calculate the difference use the difftime() function,
# which has the syntax: difftime(dates_2, dates_1, units = "days"):
time_diff_days = difftime(dates_2, dates_1, units = "days")
# Time differences in days
# [1] 8919 6412  764

# Calculate the mean time difference and sd:
mean(time_diff_days)
# Time difference of 5365 days # Mean difference
sd(time_diff_days)
# [1] 4177.1 # Quite high in this case...


# You can also calculate the difference within the steps of a vector
# of dates via: difftime(dates_1[-1], dates_1[-length(dates_1)], units = "days"):
time_diff_days_vector = difftime(dates_1[-1], dates_1[-length(dates_1)], units = "days")
# Time differences in days
# [1] 2251 3674

# So from "1998-02-12" to "2004-04-12" = 2251 days and
#    from "2004-04-12" to "2014-05-04" = 3674 days


#########################################################################
### 8.16 EXAMPLE XVI: Extracting the Output from summary() into a Table #
#########################################################################

# Usually it is rather simple to extract the output of certain statistical functions
x = c(0:10)
y = c(0:10)*3
result = t.test(x,y)

# Get p-value only
result$p.value
# [1] 0.008039526

# However extracting the result from summary() turned out to be a little 
# troubling, so here is an example on how to do it. First let's create a
# table with the results of a survey with a Likert scale (let us say
# every participant chose 1 for every question):
q_one   = c(1,2,1)
q_two   = c(1,3,1)
q_three = c(2,3,1)
q_four  = c(2,2,2)
results = as.data.frame(cbind(q_one,q_two,q_three))

# When we run it through the summary function, we get this col-wise analysis
# including quantiles, min/max values, mean, median:
summary_res = summary(results)

#    q_one           q_two          q_three   
# Min.   :1.000   Min.   :1.000   Min.   :1.0  
# 1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.5  
# Median :1.000   Median :1.000   Median :2.0  
# Mean   :1.333   Mean   :1.667   Mean   :2.0  
# 3rd Qu.:1.500   3rd Qu.:2.000   3rd Qu.:2.5  
# Max.   :2.000   Max.   :3.000   Max.   :3.0

# Our goal is now to get a table where the result of each column is put in
# a table with the result of each column in one row:

# Create an empty matrix that has the col length of summary_res[,1] = 6 
# and a row for every question, i.e. the length(results[1,]) = number of questions:
results_table = matrix(0, ncol = length(summary_res[,1]), nrow = length(results[1,]))

# We now use a loop to put the summary of every column together
# We have to use the summary() function again for each column,
# since I could not find a way to reformat the above object summary_res:
for(i in 1:length(results[1,])){             # loop over columns
  results_table[i,] = summary(results[,i])   # Calculate summary of each column (only way it worked)
} # End for i                                # ... and put result in row of results_table[i,]
results_table = as.data.frame(results_table) # Turn results_table into data.frame()

# > results_table
# V1  V2 V3       V4  V5 V6
# 1  1 1.0  1 1.333333 1.5  2
# 2  1 1.0  1 1.666667 2.0  3
# 3  1 1.5  2 2.000000 2.5  3

# Change colum names:
colnames(results_table) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
# or using the names function:
colnames(results_table) = names(summary(results[,1]))

# And add row names (previous colnames of object results above!) 
rownames(results_table) = colnames(results)

# Optionally you can also rearrange columns, such that, e.g.
# median and mean are in the first two columns:
results_table = results_table[,c(3,4,1,2,5,6)]
results_table$Mean = round(results_table$Mean,2) # round mean value

# You may also want to add the SD of every question as well:
SD = c() # Empty vector for results:
for(i in 1:length(results[1,])){
  SD[i] = round(sd(results[,i]),2) # get SD and round result
} # End for i

# [1] 0.5773503 1.1547005 1.0000000

# Final table:
results_table = cbind(results_table,SD)
results_table_fin  = results_table[,c(1,2,7,3:6)]

#         Median Mean   SD Min. 1st Qu. 3rd Qu. Max.
# q_one        1 1.33 0.58    1     1.0     1.5    2
# q_two        1 1.67 1.15    1     1.0     2.0    3
# q_three      2 2.00 1.00    1     1.5     2.5    3


#################################################################################################
### 8.17 EXAMPLE XVII: Summing only Parts of a Column also via using group_by() and summarize() #
#################################################################################################

# Setting up test table:
pat_id = c(c(1,1,1),c(2,2), c(3,3,3))
stay = c(c("22.01.2024","23.02.2024","12.03.2024"),c("02.01.2024","15.04.2024"),c("01.01.2024","24.02.2024","06.03.2024"))
infect_A = c(c(1,0,1),c(1,0), c(1,1,1)) # 0 == no infection A; 1 == infection A given

# Fitting it together in a data.frame()
table_infect = as.data.frame(pat_id)
table_infect = as.data.frame(cbind(table_infect, stay,infect_A))

#   pat_id       stay infect_A
# 1      1 22.01.2024        1
# 2      1 23.02.2024        0
# 3      1 12.03.2024        1
# 4      2 02.01.2024        1
# 5      2 15.04.2024        0
# 6      3 01.01.2024        1
# 7      3 24.02.2024        1
# 8      3 06.03.2024        1

# Get infdividual patient ids:
uniq_id = unique(table_infect$pat_id)
# [1] 1 2 3

# Summing only the part of the column infect_A that related to pat_id = 1:
test_filter = filter(table_infect, pat_id == "1")
sum(test_filter$infect_A)
# [1] 2

# Using loop to do this with all patient ids:
# Empty vector for answers:
sum_infect_A = c()
for(i in 1:length(uniq_id)){
  filter_id = filter(table_infect, pat_id == as.character(uniq_id[i]))
  sum_infect_A[i] = sum(filter_id$infect_A)
} # End for i

sum_infect_A
# [1] 2 1 3

# You could now make a table
as.data.frame(cbind(uniq_id,sum_infect_A))

#   uniq_id sum_infect_A
# 1       1            2
# 2       2            1
# 3       3            3

# Alternative via tidyverse syntax and using group_by()
sum_table = table_infect %>% # call table
  group_by(pat_id) %>% # Groups in the sense of creating sub-tables in the background
  summarize(sum(infect_A)) # Summing will now be down only for each group, i.e., pat_id

sum_table
# A tibble: 3 × 2
#   pat_id `sum(infect_A)`
#    <dbl>           <dbl>
# 1      1               2
# 2      2               1
# 3      3               3

#######################################################################################
### 8.18 EXAMPLE XVIII: Joining/Merging/Fusing Two Tables via full_join() and merge() #
#######################################################################################

# Create table one (Example from 8.1):
patient_id = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)
fam = c("yes", "yes", "no", "no", NA, NA, "n","n","no","no","ys", "ys", NA, NA)
time = c("t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2")
measurement_sysRR = c(130,122,132,123,133,121,129,125,135,119,134,127,140,125)

# Create table
table = as.data.frame(patient_id)
table = cbind(table,time,measurement_sysRR,fam)

# Create second table with diagnoses related to the same patients (patient id!):
patient_id = c(1:7)
diagnosis = c("dia1","dia2","dia2","dia1","dia2","dia3","dia3")
table2 = as.data.frame(patient_id)
table2 = cbind(table2,diagnosis)

# Joined table:
join = full_join(table,table2, by = "patient_id")

#    patient_id time measurement_sysRR  fam diagnosis
# 1           1   t1               130  yes      dia1
# 2           1   t2               122  yes      dia1
# 3           2   t1               132   no      dia2
# 4           2   t2               123   no      dia2
# 5           3   t1               133 <NA>      dia2
# 6           3   t2               121 <NA>      dia2
# 7           4   t1               129    n      dia1
# 8           4   t2               125    n      dia1
# 9           5   t1               135   no      dia2
# 10          5   t2               119   no      dia2
# 11          6   t1               134   ys      dia3
# 12          6   t2               127   ys      dia3
# 13          7   t1               140 <NA>      dia3
# 14          7   t2               125 <NA>      dia3

# Doing the same using merge()
merge = merge(table,table2, by = "patient_id")

# Check for equality with join
merge == join # all true

#       patient_id time measurement_sysRR  fam diagnosis
# [1,]        TRUE TRUE              TRUE TRUE      TRUE
# [2,]        TRUE TRUE              TRUE TRUE      TRUE
# [3,]        TRUE TRUE              TRUE TRUE      TRUE
# [4,]        TRUE TRUE              TRUE TRUE      TRUE
# [5,]        TRUE TRUE              TRUE   NA      TRUE
# [6,]        TRUE TRUE              TRUE   NA      TRUE
# [7,]        TRUE TRUE              TRUE TRUE      TRUE
# [8,]        TRUE TRUE              TRUE TRUE      TRUE
# [9,]        TRUE TRUE              TRUE TRUE      TRUE
# [10,]       TRUE TRUE              TRUE TRUE      TRUE
# [11,]       TRUE TRUE              TRUE TRUE      TRUE
# [12,]       TRUE TRUE              TRUE TRUE      TRUE
# [13,]       TRUE TRUE              TRUE   NA      TRUE
# [14,]       TRUE TRUE              TRUE   NA      TRUE

# IMPORTANT!! Note that merge drops those lines with values in patient_id
# that are not present in both sets!!

# Create table one (Example from 8.1) WITH PATIENT ID 8 ADDED!
patient_id = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7.,8,8)
fam = c("yes", "yes", "no", "no", NA, NA, "n","n","no","no","ys", "ys", NA, NA,"no","no")
time = c("t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2")
measurement_sysRR = c(130,122,132,123,133,121,129,125,135,119,134,127,140,125,140,125)

# Create table NOW WITH PATIENT ID 8
table = as.data.frame(patient_id)
table = cbind(table,time,measurement_sysRR,fam)

# Create second table with diagnoses related to the same patients (patient id!).
# NOTE THAT THIS TABLE DOES NOT HAPE patient_id = 8
patient_id = c(1:7) # only up to 7, not 8!
diagnosis = c("dia1","dia2","dia2","dia1","dia2","dia3","dia3")
table2 = as.data.frame(patient_id)
table2 = cbind(table2,diagnosis)

# Merging: 
merge2 = merge(table,table2, by = "patient_id")

merge == merge2 # ALL TRUE! 
# Let us have a look at the length of unique values of patient_id:
length(unique(merge2$patient_id))
# [1] 7

# Unique values for themselves:
unique(merge2$patient_id)
# [1] 1 2 3 4 5 6 7

# The function full_joint() will not do so, but leave a NA, here in the column 
# diagnosis that could not be related to the second table only entailing 
# patient id 1 to 7:
merge3 = full_join(table,table2, by = "patient_id")
merge4 = merge(table,table2, by = "patient_id", all = TRUE)

#    patient_id time measurement_sysRR  fam diagnosis
# 1           1   t1               130  yes      dia1
# 2           1   t2               122  yes      dia1
# 3           2   t1               132   no      dia2
# 4           2   t2               123   no      dia2
# 5           3   t1               133 <NA>      dia2
# 6           3   t2               121 <NA>      dia2
# 7           4   t1               129    n      dia1
# 8           4   t2               125    n      dia1
# 9           5   t1               135   no      dia2
# 10          5   t2               119   no      dia2
# 11          6   t1               134   ys      dia3
# 12          6   t2               127   ys      dia3
# 13          7   t1               140 <NA>      dia3
# 14          7   t2               125 <NA>      dia3
# 15          8   t1               140   no      <NA>  # HERE patient_id = 8 is given
# 16          8   t2               125   no      <NA>  # .. here too...

# Check again for equality:
merge3 == merge4 # ALL TRUE!!

##############################################################################
### 8.19 EXAMPLE XIX: Deleting Spacing from Character Strings (Text Entries) #
##############################################################################

#install.packages("stringr")
library(stringr) # within tidyverse

# The below comes in handy when trying to merge two table into one, in order
# to be able to detect an entry via equivalence (==):
# Example character string
string = c("age ", " name")

# Trim/delete spacing on the right
str_trim(string, "right")
# [1] "age"   " name"

# Trim/delete spacing on the left
str_trim(string, "left")
# [1] "age "   "name"

# Remove all spacing (makes sense for the above case).
# Scheme: pattern, replace with, data:
gsub(" ", "", string) 
# [1] "age"   "name"

#######################
# ------------------- #
#######################
#######################
# 9 Writing Functions #
#######################
###############################################
### 9.1 EXAMPLE FUNCTION I: Parabola Function #
###############################################

# Writing a parabola function:
parabola = function(x){ # Parabola is the name of a function with 1 input parameter x
  fx = x^2              # and with that x some math is done, namely squaring x
  return(fx)            # The result object fx is then "returned" to the console
} # End of function parabola

# Example
x = c(-4:4) # creates vector from -4 to 4, integer steps
parabola(x)

##############################################################################
### 9.2 EXAMPLE FUNCTION II: Turning Nested For Loop Example into a Function #
##############################################################################

########## You can turn any code that you would use again just for different variables into
########## a function that does the same for any kind of input object. We can use our
########## nested for loop example to demonstrate what this means:

# Nested for loop example from above:
object2 = cbind(c(1,1,1),c(1,1,1))
#      [,1] [,2]
# [1,]    1    1
# [2,]    1    1
# [3,]    1    1

# Create empty matrix with zeros only:
result = matrix(0,nrow = length(object2[,1]), ncol = length(object2[1,]), byrow = TRUE)
for(i in 1:length(object2[,1])){    # loop over rows
  for(j in 1:length(object2[1,])){  # loop over cols
    result[i,j] = object2[i,j]+1
  } # End for i
} # End for j

result
#      [,1] [,2]
# [1,]    2    2
# [2,]    2    2
# [3,]    2    2

# ALTERANTIVE of the above written as a function for any input x
add_1 = function(x){
  result = matrix(0,nrow = length(x[,1]), ncol = length(x[1,]), byrow = TRUE)
  for(i in 1:length(x[,1])){    # loop over rows (looping over cols first also possible in this case!)
    for(j in 1:length(x[1,])){  # loop over cols
      result[i,j] = x[i,j]+1
    } # End for i
  } # End for j
  return(result)
} # End of function

add_1(object2)
#      [,1] [,2]
# [1,]    2    2
# [2,]    2    2
# [3,]    2    2

# TECHNICALLY EVEN SHORTER (ABOVE ONLY EXAMPLES) without initializing empty matrix but
# just overwriting the initial object elements:

add_1 = function(x){
  for(i in 1:length(x[,1])){    # loop over rows (looping over cols first also possible in this case!)
    for(j in 1:length(x[1,])){  # loop over cols
      x[i,j] = x[i,j]+1
    } # End for i
  } # End for j
  return(x)
} # End of function

add_1(object2)


#############################################################################################
### 9.3 EXAMPLE FUNCTION III: Replication of Basic gather() Output Without Extra Parameters #
#############################################################################################


###### Replication of basic gather() output without extra parameters as function:

long_form = function(x){
  extra_long_format = c()
  for(i in 1:length(x[1,])){
    extra_long_inter  = as.data.frame(cbind(colnames(x)[i],x[,i]))
    extra_long_format = rbind(extra_long_format,extra_long_inter) # as.data.frame() not necessary anymore here
  } # End for i
  return(extra_long_format)
} # End of function

long_format # Use object from the previous chapter on long format conversion for testing:

long_form(long_format)


########################################################
### 9.4 EXAMPLE FUNCTION IV: CP / Bayes’ Rule Function #
########################################################

############# First simple version of our Bayes_Machine() function:
Bayes_Machine = function (prior,likelihood) {
  joint = prior*likelihood
  # na.rm = TRUE in sum() deletes 0 rows if given
  modelevidence = sum(joint, na.rm = TRUE) 
  posterior = joint/modelevidence
  # Needed for console output and adds text to it
  # using a matrix, which works similar as c()
  postprint = as.data.frame(matrix(c("Posterior",posterior,
                                     "Joint probability", joint,
                                     "Model evidence", modelevidence)))
  print(postprint) # or use return
}  # end of function

# Give it a try with defined prior and likelihood:
prior = c(.5,.5)
likelihood = c(.5,.5)
Bayes_Machine(prior,likelihood)

# Try these inputs and contemplate the results:
prior = c(.1,.9)
likelihood = c(.9,.1)
Bayes_Machine(prior,likelihood)


############## Here is the extended function that can also handle 
############## the model evidence as input:

Bayes_Machine = function (prior,likelihood,modelevidence) {
  if (missing(modelevidence)){
    joint = prior*likelihood
    # na.rm = TRUE in sum() deletes 0 rows if given
    modelevidence = sum(joint, na.rm = TRUE) 
    posterior = joint/modelevidence
    # Needed for console output
    postprint = as.data.frame(matrix(c("Posterior",posterior,
                                       "Joint probability", joint,
                                       "Model evidence", modelevidence))) 
    print(postprint)
  } # End if
  else {
    joint = prior*likelihood
    posterior = joint/modelevidence  
    postprint = as.data.frame(matrix(c("Posterior",posterior,
                                       "Joint probability", joint,
                                       "Model evidence", modelevidence)))
    print(postprint) 
  } # End Else
} # End of function

Bayes_Machine(likelihood = likelihood, prior = prior)


##########################################
### 9.5 EXAMPLE FUNCTION V: Sum Function #
##########################################

############### Examplatory code for a sum function (do not name it "sum",
############### since it conflicts with the integrated sum() function)
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



############################################################################
### 9.6 EXAMPLE FUNCTION VI: Filter Function, similar to filter() in dplyr #
############################################################################

# Example for a filter function, similar to filter() in the dplyr package
# Criteria means what should be deleted, equivalent to always
# working with != in the dplyr filter function... 
filter_alt = function(data_table,criteria_col,criteria){ 
  lines_to_filter = c()                # empty vector for lines to filter
  for(i in 1:length(criteria_col)){    # loop over col vector (not the cols!)
    if(criteria_col[i] == criteria){   # if criteria input is fulfilled
      lines_to_filter[i] = i           # add index position in col vector to list
    } # End if
  } # End for i
  # Output of lines_to_filter can look like this:[1]  1 NA  3 NA  5 NA  7 NA  9 NA 11 NA 13
  # Reason: given that criteria_col[i] == criteria, lines_to_filter[i] = i, ELSE nothing happens
  #         so in such cases, no i is passed on to lines_of filter on the position for 
  #         for lines_to_filter[i] - see test code below. However, filtering the NAs is needed
  #         before deleting the respective lines - at least in this solution...
  data_table = data_table[-lines_to_filter[!is.na(lines_to_filter)],]
  return(data_table)
} # End of function

# Test filter function, using new_table from chapter 8 
# Again, criteria means what should be deleted, equivalent to always
# working with != in the dplyr filter function... 
filter_alt(new_table,new_table$time,"t1")

#     patient_id time measurement_sysRRalt  fam
#  2           1   t2                  122  yes
#  4           2   t2                 <NA>   no
#  6           3   t2                  121 <NA>
#  8           4   t2                  125    n
# 10           5   t2                  119   no
# 12           6   t2                  127   ys
# 14           7   t2                  125 <NA>

# Check for equivalence with library(dplyr) filter() function (NA values in enw_table remain NA!) 
filter_alt(new_table,new_table$time,"t1") == filter(new_table,time != "t1")

#    patient_id time measurement_sysRRalt  fam
# 2        TRUE TRUE                 TRUE TRUE
# 4        TRUE TRUE                   NA TRUE
# 6        TRUE TRUE                 TRUE   NA
# 8        TRUE TRUE                 TRUE TRUE
# 10       TRUE TRUE                 TRUE TRUE
# 12       TRUE TRUE                 TRUE TRUE
# 14       TRUE TRUE                 TRUE   NA

# Test code to understand the loop within the above function
lines_to_filtertest = c()
for(i in 1:length(new_table$time)){
  if(new_table$time[i] == "t1"){
    lines_to_filtertest[i] = i 
  } # End if
} # End for i
lines_to_filtertest
#  [1]  1 NA  3 NA  5 NA  7 NA  9 NA 11 NA 13

lines_to_filtertest2 = list()
for(i in 1:length(new_table$time)){
  if(new_table$time[i] == "t1"){
    lines_to_filtertest2 = append(lines_to_filtertest2, i) 
  } # End if
} # End for i
unlist(lines_to_filtertest2)
#  [1]  1  3  5  7  9 11 13


# Recall alternative without filter function, chapter 8.1:
t1_alt = new_table[new_table$time == "t1",] # placed in row position; don't forget comma
t1 == t1_alt # All true...

# The filter function also works for filtering NAs:
# Columns
a = c(1,2,3)
b = c(1,2,NA)
c = c(1,NA,3)

# Bind to table:
data = as.data.frame(cbind(a,b,c))
#   a  b  c
# 1 1  1  1
# 2 2  2 NA
# 3 3 NA  3

# filter NAs
filter_alt(data,is.na(data$c),TRUE)



###########################################################################
### 9.7 EXAMPLE FUNCTION VII: Replication of lm(y~x) and summary(lm(y~x)) #
###########################################################################

################ Examplary linear least square function, invluding a t-test:

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
linear_least_square(indep = c(0:10), dep=(c(0:10)*3))



########################################################################################
### 9.8 EXAMPLE FUNCTION VIII: Logistic Map and the Mandelbrot Set (Functions for Fun) #
########################################################################################

############## Logistic Map from https://magesblog.com/post/2012-03-17-logistic-map-feigenbaum-diagram/ 
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

# Set scanning range for bifurcation parameter r (calculation
# may take a while)
my.r <- seq(.9, 4, by=0.003)   #!!!! alternative start 2.5, so it may be more vivid
system.time(Orbit <- sapply(my.r, logistic.map,  x=0.1, N=1000, M=300))

Orbit <- as.vector(Orbit)
r <- sort(rep(my.r, 301))

plot(Orbit ~ r, pch=".")   ## Execute up to here to plot the logistic map


# We are following this great explanation by the youtube channel 
# Numberphile:
# https://www.youtube.com/watch?v=ETrYE4MdoLQ 

# Feigenbaumconstant = 4.669... (transc. number like pi or e...)

# Logistic map: simplified without K

# CORRESPONDING TO THE NUMBERPHILE VIDEO:
# Let's say we are talking about a function for a population, a population
# of rabbits.

# Population we start with =  x1  = .5 (as in a range of 0-1)

# xn+1        =      LAMBDA     *        xn             *       (1-xn)
# next year   =      is like          previous year,         part that
# via +1          the fertility       i.e. existing          died in the
#                                      population            prev./exist. pop.

#                                       LIFE                    DEATH

#           LAMBDA has to be between 0 and 4 (reasons are 
#           eventually complicated says the tutorial... so no expl. here
#           but we will see intuitive reason soon!)

# So let us say LAMBDA is 2.3, x = .5, and the death rate is therefore
# 1-.5...

# YEAR 2, as x1 = year 1
xnPLUS1 = 2.3*.5*(1-.5)
# Result on the upper right as a value or for the console:
xnPLUS1
# 0.575  => population has increased!

# So what about year two?

# Year 3
xnPLUS2 = 2.3*.575*(1-.575)
xnPLUS2  
# 0.5620625 => population dropped!

xnPLUS3 = 2.3*.5620625*(1-.5620625)
xnPLUS3  
# 0.566141  => population increases again!! 

xnPLUS4 = 2.3*0.566141*(1-0.566141)
xnPLUS4  
# 0.5649383 => AWWWW!!! Slightly decreased again!

# We will go on, until something special happens:

xnPLUS5 = 2.3*xnPLUS4*(1-xnPLUS4)
xnPLUS5  
# 0.5653009

xnPLUS6 = 2.3*xnPLUS5*(1-xnPLUS5)
xnPLUS6  
# 0.5651923

xnPLUS7 = 2.3*xnPLUS6*(1-xnPLUS6)
xnPLUS7  
# 0.5652249

xnPLUS8 = 2.3*xnPLUS7*(1-xnPLUS7)
xnPLUS8  
# 0.5652151

xnPLUS9 = 2.3*xnPLUS8*(1-xnPLUS8)
xnPLUS9  
# 0.5652181

xnPLUS10 = 2.3*xnPLUS9*(1-xnPLUS9)
xnPLUS10  
# 0.5652172

# DID YOU SEE IT?
# It starts stagnating!!! :O So the population starts to egalize
# which is called a fixed point of iteration.

# NOW lets pick a LAMBDA of .65 this time! Before 2.3 ...

xnPLUS1 = .65*.5*(1-.5)
xnPLUS1
# Result is .1625, so the population heavily decreased!!!!

xnPLUS2 = .65*xnPLUS1*(1-xnPLUS1)
xnPLUS2
# .08846094 decreasing more and more!

xnPLUS3 = .65*xnPLUS2*(1-xnPLUS2)
xnPLUS3
# .05241314 => further decrease!

# If I'd go on with this a couple of years, around 15,
# the population will die. 

# So eventually, if one plays around with LAMBDA (remember between 0 and 1)
# at some values of LAMBDA funky things will happen!

xnPLUS1 = 3.2*.5*(1-.5)
xnPLUS1 
# .8, so increase

xnPLUS2 = 3.2*xnPLUS1*(1-xnPLUS1)
xnPLUS2 
# .512, so "competition kicks in", as they put it in the tutorial 


xnPLUS3 = 3.2*xnPLUS2*(1-xnPLUS2)
xnPLUS3 
# 0.7995392
xnPLUS4 = 3.2*xnPLUS3*(1-xnPLUS3)
xnPLUS4 
# 0.5128841
xnPLUS5 = 3.2*xnPLUS4*(1-xnPLUS4)
xnPLUS5 
# 0.7994688
xnPLUS6 = 3.2*xnPLUS5*(1-xnPLUS5)
xnPLUS6 
# 0.513019
xnPLUS7 = 3.2*xnPLUS6*(1-xnPLUS6)
xnPLUS7 
# 0.7994576

# AS YOU CAN SEE IT STARTS TO BOUNCE and balance between two values :O
# so we get a MULTIPLE FIXED POINT!!!

# If you do the same thing with LAMBDA being 3.5, then you will get
# cycle of 4!! What happens when you go roughly above 3.59 for LAMBDA is,
# that the behavior gets chaotic!! The further you get to 3.59 the faster 
# and the more cycles one gets, which are actually doubling!
# So Feigenbaum got involved in the 70s and wanted to know how
# far LAMDA has to change for that to happen.
# When he was looking at the ratio of these changes in X, i.e.
# values of LAMBDA, he figured out a constant, which is now
# called, the Feigenbaum constant ..... 4.669...
# SO WHAT THE NUMBER SAYS is that the length of X/LAMBDA is getting
# 4.669 times smaller than the previous, i.e. with the previous value
# of LAMBDA, in case of a doubling, as the length represents a
# stabilisation of the cycle within that length!!


############### Mandel brot set function:
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


#################################################
### 9.9 EXAMPLE FUNCTION IX: Fibonacci Sequence #
#################################################

# A simple function calculating the Fibonacci series up to n:
fibonacci1 = function(n){
  fn = c()
  for(i in 1:n){
    if(i == 1){
      fn[i] = 0
    } # End if
    else if(i == 2){
      fn[i] = 1
    } # End else of
    else{
      fn[i] = fn[i-1] + fn[i-2] 
    } # End else
  } # End for i
  return(fn)
} # End of function

fibonacci1(20)
# [1]    0    1    1    2    3    5    8   13   21   34   55   89  144  233  377  610  987 1597
# [19] 2584 4181

# A little shorter version:
fibonacci2 = function(n){
  fn = c()
  for(i in 1:n){
    if(i <= 2){
      fn[i] = i-1
    } # End if
    else{
      fn[i] = fn[i-1] + fn[i-2] 
    } # End else
  } # End for i
  return(fn)
} # End of function

fibonacci2(20)
# [1]    0    1    1    2    3    5    8   13   21   34   55   89  144  233  377  610  987 1597
# [19] 2584 4181

# Golden ratio can be obtained, when values are high enough via f(x_i)-f(x_i-1): G.R. == around 1.618
4181/2584
# [1] 1.618034
2584/1597
# [1] 1.618034

#####################################################
### 9.10 EXAMPLE FUNCTION X: Fourier Transformation #
#####################################################

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
data = readDICOMFile("kspace/example_sts.dcm")

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



###################################################
# ----------------------------------------------- #
###################################################
###################################################
# 10 Logical Operators and Mathematical Functions #
###################################################

##### LOGICAL OPERATORS

#  ==       equivalent
1 == 1 # [1] TRUE

#  !=       not equivalent
1 != 2 # [1] TRUE

# >=       greater-equal (e.g. x is greater than y)
2 >= 1 # [1] TRUE

# <=       smaller-equal (e.g. x is smaller than y)
2 <= 3 # [1] TRUE

# >        more/higher than
2 < 3 # [1] TRUE

# <        less/lower than
2 < 3 # [1] TRUE

# !=       NOT
1 != 2 # [1] TRUE

# &        AND
TRUE & TRUE  # [1] TRUE
TRUE & FALSE # [1] FALSE

# Either some & operatiorn AND (&&) another & operation
x = 4
x > 1 & is.numeric(x) && x > 5 & is.numeric(x) # [1] FALSE

x = 6
x > 1 & is.numeric(x) && x > 5 & is.numeric(x) # [1] TRUE

# |        # OR
x = 7
x > 5 | x < 10 # [1] TRUE

x = -5
x > 5 | x > 22 # [1] FALSE

# double | for "some operation on one side" OR "some operation on the other side"  
x = 5
x > 1 & is.numeric(x) || x > 5 & is.numeric(x) # [1] TRUE

x = -5
x > 1 & is.numeric(x) || x > 5 & is.numeric(x) # [1] FALSE
x > 0 & is.numeric(x) || abs(x) <= 4 & is.numeric(x) # [1] FALSE

# Any function any()
any(c(1,2,3) > 4) # [1] FALSE

# Pipe operator
pipe_object = as.data.frame(c(1,2,3)) # only works with data frames
# [1] 1 2 3
pipe_result = pipe_object %>% # or |> ; this line duplicates object given new name
  sum() # no input neaded, since object from line before is parsed into it directly
# [1] 6  

# %in% "in"-Operator, which checks if elements of one vector are in another:
c(1,2,3) %in% c(1,2,3,4,5)
# [1] TRUE TRUE TRUE
sum(c(1,2,3) %in% c(1,2,3,4,5))
# [3]

# Reverse case (sum will NOT BE equivalent in both cases!!!):
c(1,2,3,3,5) %in% c(1,2,3)
# [1]  TRUE  TRUE  TRUE TRUE FALSE
sum(c(1,2,3,3,5) %in% c(1,2,3))
# [1] 4

# Relation of %in% to the function duplicated(): 
duplicated(c(1,2,3,3,5))
# [1] FALSE FALSE FALSE TRUE FALSE

# The sum of those of TRUE  TRUE  TRUE TRUE FALSE - the sum of  TRUE  TRUE  TRUE  
#                                                   == FALSE FALSE FALSE TRUE FALSE
# 4 - 3 = 1
sum(c(1,2,3,3,5) %in% c(1,2,3)) - sum(c(1,2,3) %in% c(1,2,3,3,5)) == sum(duplicated(c(1,2,3,3,5)))
# [1] TRUE

# Theoretical alternative to unique function via filtering duplicates:
test = c(1,2,2,3, "test", "test")
test[duplicated(test)== FALSE]
# [1] "1"    "2"    "3"    "test"

##### MATHEMATICAL OPERATORS:

# Pi
pi # [1] 3.141593

# Euler's number
exp(1) # [1] 2.718282

# Addition
2 + 5 # [1] 7

# Subtraction
2 - 5 # [1] 3

# Multiplication
2 * 5 # [1] 15

# Division
2 / 2 # [1] 1

# Exponents
2^2 # [1] 4

# Dot product:
c(2,4,6)%*%cbind(c(1,1,1),c(2,2,2))
#      [,1] [,2]
# [1,]   12    24

# MAX/MIN
max(c(1,2,3)) # [1] 3
min(c(1,2,3)) # [1] 1

# Square root
sqrt(9) # [1] 3

# Mean
mean(c(2,3)) # [1] 2.5

# Median
median(c(1,40,55)) # [1] 40

# Mode (one way to do it...)
which.max(table(c(20,55, 55, 67))) 
# 55  # Mode value
# 2   # Frequency

# Sample SD
sd(c(0:10)) # [1] 3.316625 
sd(5.63) # [1] NA == 0 for sample size 1 

# Absolute value
abs(c(-1)) # [1] 1

# Round
round(2.9) # [1] 3
round(2.4) # [1] 2

# Truncate
trunc(1.8) # [1] 1

# Round upwards
ceiling(2.1) # [1] 3

# Round downwards
floor(2.9) # [1] 2

# Sum
sum(c(1,2,3)) # [1] 6

# Rowsum and more
test = cbind(c(1,2,3), c(1,2,3))
#      [,1] [,2]
# [1,]    1    1
# [2,]    2    2
# [3,]    3    3
rowSums(test)  # [1] 2 4 6
colSums(test)  # [1] 6 6
rowMeans(test) # [1] 1 2 3
colMeans(test) # [1] 2 2

# cos(), sin(), tan() of an angle as input
cos(45)
sin(45)
tan(15)

# Logarithm base 2 = log2(), natural = log(), dec. log. = log10()
log2(.5)  # [1] -1
log(.5)   # [1] -0.6931472
log10(.5) # [1] -0.30103

# Exponential function e to the power of x as input
exp(1) # [1] 2.718282 # Euler's number

# Random functions (normal, unif, binom, pois)
rnorm(n = 10, mean = 5, sd = 1)
runif(n = 10, min = 1, max = 10)
rbinom(n = 10, size = 2, prob = .1)
rpois(n = 10, lambda = 2)

# PDF Normal Distr.
dnorm(seq(-4,4,by=.1))
plot(x = seq(-4,4,by=.1), dnorm(seq(-4,4,by=.1)))

# CDF of PDF of Normal Distr. (for two tail)
pnorm(-1.96, lower.tail = TRUE) # [1] 0.0249979

# Inverese CDF
qnorm(.5)   # [1] 0
qnorm(.025) # [1] -1.959964
 
# Uniform PDF
dunif(seq(0,7,by=.1),min=1, max=6)
plot(seq(0,7,by=.1),dunif(seq(0,7,by=.1),min=1, max=6), type = "l", ylim = c(0,.5))

# CDF of Unif. 
punif(3, min = 1, max = 6) # [1] 0.4

# PDF Poisson
dpois(3, lambda = 3) # [1] 0.2240418

# CDF Poisson
ppois(q = 0.2240418, lambda = 3) # [1] 0.04978707


# and many more...


###################################################
# ----------------------------------------------- #
###################################################
###############################################
# 11 Creating Apps with the Shiny App Package #
###############################################
###########################################################################################
### 11.1 EXAMPLE APP I: Plotting Standardized Difference in Means for a One-Sample Z-Test #
###########################################################################################

### IMPORTANT!!!!!!!!!!! CLOSE APP AFTER USAGE, OTHERWISE OTHER CODE WONT BE PROCESSED!!!!!!
### ALSO: YOU MIGHT NEED TO UPDATE R, this was written in R 4.4.1. Same goes for the
###       Rmarkdown example, downloadable from the R Basics article https://doi.org/10.56776/abbd964d.665f7de5 


# Install and load shiny package
# install.packages("shiny")
library(shiny)

# Writing code for a Shiny app entails three components:
# The UI, the Server executing the functions etc., and then you have to run 
# both via the shinyApp(ui,server) function:


####### Code for the User Interface (UI):
ui = fluidPage(
  
  # App title banner: 
  titlePanel("Standardized Difference in Means (One-Sample)"), # End titelPanel()
  # Sidebar layout function, we choose sidebar for the input fields via:
  sidebarLayout(  
    sidebarPanel( 
      # Numeric input for samp/pop. mean and their respective sd!
      # numericInput("object input name for server function", "Label Name", standard value)
      numericInput("sampmean", "Sample Mean:", 
                   value = 120), 
      numericInput("sampsd", "Sample SD:", 
                   value = 5),
      numericInput("popmean", "Population Mean:", 
                   value = 130),
      numericInput("popsd", "Population SD:", 
                   value = 5)
    ), # End sidebarPanel()
    
    mainPanel( # Main space where the plot will be.
      # Output Object name for server will be "curves"
      plotOutput("curves")
    ) # End mainPanel()
    
  ) # End sidebarLayout()
) # End fluidPage()


############ Server entails all the functions and code that is executed in the background
server = function(input, output) {
  
  # Define how the input is processed and what is plotted: 
  output$curves = renderPlot({ # THIS IS WHERE WE NEED the output name..
    
    # Prob. dens. function (could also use dnorm()):
    prob_dens = function(x,mean,sd){
      fx = (1/sqrt(2*pi*(sd^2)))*exp((-((x-mean)^2))/(2*(sd^2)))
      return(fx)
    } # End of Function
    
    # Effect size diff_mean/po_sd 
    # RECALL: INPUT NAMES WERE SET VIA numericInput() in code for UI!
    effectsize = (input$sampmean-input$popmean)/input$popsd
    
    # Create sequence of numbers of x (sd's of the x-axis) which then run through
    # the prob_dens function above:
    seq_samp = seq((-abs(effectsize)-3),(abs(effectsize)+3),by =.01) 
    seq_pop  = seq(-5,5,by =.01)
    
    # Check which has the maximum value of prob_dens, to adjust the y-axis limit of the plot:
    yuplim_samp = max(prob_dens(seq_samp,effectsize,(input$sampsd)/input$popsd))
    yuplim_pop = max(prob_dens(seq_pop,0,1))
    yuplim = max(c(yuplim_samp,yuplim_pop))
    
    # Plot of standardized sample distribution:
    plot(x = seq_samp, y = prob_dens(seq_samp,effectsize,(input$sampsd)/input$popsd), 
         type = "l",  # lines instead of points
         lty = 2,     # dotted line
         ylab = "Density",
         xlab = "Difference in Means in Units of SD of the Population",
         ylim = c(0,yuplim)) # Uplim adjusted, so it is not cu off for some cases
    
    # Adds standard normal distribution to plot:
    lines(x = seq_pop,y = prob_dens(seq_pop,0,1), type = "l") # full line, not dotted
    
    # mean 0 for standard normal distribution (representing population):
    abline(v=0,col="lightblue") 
    
    # Effect size diff_mean/sd_pop:
    abline(v=effectsize,col="orange")
    
    # line for x-axis at y = 0:
    abline(h=0) 
    
    # Add Information on effect size to plot:
    # Formula (ifelse() function below places effect size output and 
    # formula on the left or right side of the plot depending ES > or < 0):
    text(grconvertX(ifelse(effectsize>=.1,0.25,0.75), from = "npc", to = "user"), # Makes sure text will always be static, either left or right!
         grconvertY(0.60, from = "npc", to = "user"),
         expression(frac(bar(x)-mu,sigma) == ""))
    # Result value:
    effectsize = signif(effectsize, digits = 3) # Round to 2 decimal places
    text(grconvertX(ifelse(effectsize>=.1,0.32,0.82), from = "npc", to = "user"), # Makes sure text will always be static, either left or right!
         grconvertY(0.60, from = "npc", to = "user"),                             # if effect size >= than formula on the left side...
         effectsize) # Adds value next to the above formula
      
  }) # End renderPlot()
} # End server

##### RUN THE APP (NOTE: A # WAS SET, since otherwise the RStudio RUN button disappears
##### and it then says "Run App" (which just executes the whole script). 
##### Some of you might want to use it, so I set the below as comment...


#### IMPORTANT!!!!!!!!!!! CLOSE APP AFTER USAGE, OTHERWISE OTHER CODE WONT BE PROCESSED!!!!!!
# shinyApp(ui = ui, server = server)


# Consider the following cases:

# Pop_mean = 130 		  Sample_mean = 120 	 Pop and Samp_SD = 5
# Pop_mean = 130 		  Sample_mean = 120	   Pop and Samp_SD = 20
# Pop_mean = 129.8  	Sample_mean = 130	   Pop and Samp_SD = 0.1 
# Pop_mean = 130 		  Sample_mean = 120	   Pop_SD = 5	 	          Samp_SD = 20 (makes clear why CI of effect size is important)
# Pop_mean = 130 		  Sample_mean = 20	   Pop_SD and Samp_SD = 20


###########################################################################################
# --------------------------------------------------------------------------------------- #
###########################################################################################
