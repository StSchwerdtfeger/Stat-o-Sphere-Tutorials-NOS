##################################
##################################
#      R-Basics-I Tutorial       #
#               by               #
#     Steffen Schwerdtfeger      # 
#            12.2023             #
##################################
##################################

# UNFOLD CODE of the desired chapter (small arrow at the lower line of #'s).

################################################################
# 1 What is the Function of a Computer? — A Short Introduction #
# into the History of Programming Languages                    #
################################################################

# Example Parabola Function:
parabola = function(x){
  fx = x^2
  return(fx)
} # End of function parabola

# Example
x = c(-4:4) # creates vector from -4 to 4, integer steps
parabola(x)


# Let us say your CPU has 9.69 billion transistors. A transistor
# is essentially a very tiny switch that can be in two states (0 or 1).
# Imagine you can walk through a life sized CPU.
# Say it takes one minute to get from one to another transistor.
9690000000/60/24/365
# [1] 18436.07 # years to reach every transistor

# One second to reach each transistor:
9690000000/60/60/24/365
# [1] 307.2679 # years to reach each transistor

# A RTX 4090 GPU has 79 Billion 
79000000000/60/24/365
# [1] 150304.4
79000000000/60/60/24/365
# [1] 2505.074


# Code benchmarks are not of concern, except for ML/AI or fMRI analysis and such:
# y=c(rnorm(1000000,mean=35,sd=4))
# x=c(rnorm(1000000,mean=22,sd=6))

# linear regression with 1 Million data points:
# lm(y~x) # instant result with my computer...

#y=c(rnorm(100000000,mean=35,sd=4))
#x=c(rnorm(100000000,mean=22,sd=6))
# linear regression with 100 Million data points:
#lm(y~x) # took my computer around 10-12 sec.
# CLEAR WORKSPACE WHEN EXECUTING THE LAST FEW LINES
# otherwise it takes a while to save the work space for such large numbers...


# ------------------------------------- #
#########################################
# 3 Open a Script and Execute Test Code #
#########################################

# Mark line and execute via ALT+ENTER or Cmnd+ENTER (Mac)
test = 2 + 5
# Console output
# [1] 7

# Only one object per line!
# The below will give you an error:
# test1 = 2 test2 = 3
# Error: unexpected symbol in "test1 = 2 test2"

# We can use the delimiter ";" within R scripts though:
test1 = 2; test2 = 3
# Delimiters are a general concept of demarcation. In R it
# it demarcates lines. So using ";" is the same as starting a new line!

# The following code duplicates an object, giving it a new name. The blow
# does not (!) rename an object. Everything that has been executed will stay
# in the work space; use brush tool to clear environment...

# # # # # # # # # # # # # # # # # # # # # 
# ------------------------------------- #
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

# Execute line to call a specific element:
vec[2] # index 2 == second element of the vector
# [1] 2 # second element in the vector is also 2

# Change element:
vec[2] = 4
# [1] 1 4 3

# Check which class our vector is using the class() function:
class(vec)
is.numeric(vec)

# Turn vector into a matrix
mat = as.matrix(vec)
#      [,1]
# [1,]    1
# [2,]    4
# [3,]    3

# Look at the third element:
mat[3]
# or, since there is now also an index for the first and only column:
mat[2,1]
# [1] 4

mat2 = as.matrix(t(vec))  
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

# Transforming an atomic matrix table into a "molecular" data table.
# Here the columns can have different classes, each column for itself
# though is still atomic in its class!
mat_bind = as.data.frame(mat_bind) 
#   V1 V2 V3  # V stands for variable
# 1  1  1  1
# 2  2  2  2
# 3  3  3  3

# We can still use brackets and index to go to a specific location:
mat_bind[,2]
# Or we use the dollar sign to call/select a specific row of the table by name
# We can then also still use the bracket syntax notation:
mat_bind$V2[2]

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

# Exemplatory array with 
# dim = c(rows, columns, further_dimension)
array(1, dim = c(50,5,5))

# Add a 1 to every entry:
array(1, dim = c(50,5,5)) + 1


# String Example
string = c("One",2,3)
# Try to add a 1 to the second element in the string above:
# string[2]+1
# Error in string[2] + 1 : non-numeric argument to binary operator

# Character strings have to have ""
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

# Character strings:
string = c("One",2,3) 
# Even though there are numbers involved, they will
# be converted into symbols.
# [1] "One" "2"   "3" 

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
test = c("test","2","3")
test = as.numeric(test) 
# [1] NA  2  3
# Warning message:
# NAs introduced by coercion



# --------------------------------------------------------------- #
############################################################################
# 6 Import and Export of .CSV files, Understanding Delimiters and Packages #
############################################################################

# read CSVs, uncomment to use and add your path or file name if file is in your 
# working directory folder (of a project):
# read.csv("Your file path")
# read.csv2("Your file path")

# Working directory:
getwd() # no input needed!!

# url() Function
# read.csv(url("http://www.website.net/data_file.csv"))

# Load Dino Data Set for this!
# I called the object "dino", such that plotting is done by:
# plot(x = dino$x,y = dino$y)


# install.packages("readr")
library("readr") # package also within "tidyverse"
# write.csv()
# write.csv2()



# # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------------------------- #
#############################################
# 7 Example Data sets (DINOSAURS and more!) #
#############################################

# Install package for further data sets:
# install.packages("dplyr") # (dplyr also used for data cleaning)
data()                     # View all available data sets
library(dplyr)             # open/activate/load package
#data(starwars)            # load data set
#View(starwars)            # view via RStudio viewer
?starwars                  # view documentation


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


# --------------------------------------- #
###########################################
# 8 Short Introduction into Data Cleaning #
###########################################
###############################################################################
# 8.1 EXAMPLE I: Filtering Rows under the Condition of Certain Column Entries #
###############################################################################

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


####################################################
# 8.2 EXAMPLE II: Filtering NAs (Non-Trivial Case) #
####################################################

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
# In which lines are NAs (line numbver, not pat. id!)?
# The below line of code is not assigned to an object name, so that the output 
# will directly be returned in the console. The output however can't be called
# via a name for another process then...
is.na(new_table$measurement_sysRRalt)
# [1] FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

# Using the which() function linguistically corresponds to our question:
# In which lines are NAs? Here we execute the is.na() function within which():
na_lines = which(is.na(new_table$measurement_sysRRalt) == TRUE)
# [1] 4 7 # Index of the lines with NA entry
# NOTE: Locating NAs in any context demands using the is.na() function to create
#       a logical vector, so to speak. From where we can ask in which line == TRUE...
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

# Analoge without loop (same code, just each i is written out individually line by line): 
list_results[1] = object[1]+1
list_results[2] = object[2]+1
list_results[3] = object[3]+1
list_results[4] = object[4]+1


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

which(new_table_alt$patient_id == unique_id[1])
# [1] 1 2

length(which(new_table_alt$patient_id == unique_id[1]))
# [1] 2

# Check for equality of both methods:
fin_table$patient_id == fin_table_alt$patient_id
# [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE


###################################################
# 8.3 EXAMPLE III: For loops and nested for loops #
###################################################


## Example for loop:

# Define an object you want to loop over:
object = c(1,1,1,1)

# Initilize a list to store the results:
list_results = c()

# For loop that adds +1 to every element of the above object:
for(i in 1:length(object)){ # i = 1 to n = 4; you could also start at i = 2, theoretically!
  list_results[i] = object[i]+1 # same scheme as repetitive code below, just using i for the indices now
} # End for i
list_results

# Analoge without loop (same code, just each i is written out individually line by line)
list_results[1] = object[1]+1
list_results[2] = object[2]+1
list_results[3] = object[3]+1
list_results[4] = object[4]+1

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
  for(j in 1:length(object2[1,])){  # loop over cols; the length of a row tells you number of cols    result[i,j] = object2[i,j]+1
  } # End for i
} # End for j

result
#      [,1] [,2]
# [1,]    2    2
# [2,]    2    2
# [3,]    2    2


#########################################
# 8.4 EXAMPLE IV: Apply Function Family #
#########################################

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
# Here sum the rows
apply(result, MARGIN = 1, FUN = sum)


################################################
# 8.5 EXAMPLE V: Introduction into “tidyverse” #
################################################


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

test1 # for t1 only patient_id 2 has an NA...
#   patient_id time measurement_sysRRalt
# 1          1   t2                  122
# 3          3   t2                  121
# 4          4   t2                  125
# 5          5   t2                  119
# 6          6   t2                  127
# 7          7   t2                  125


#######################################################################
# 8.5.1 Representation of “Solution/Algorithm II” from Example II via # 
# tidyverse and the Dangers of the na.omit() Function                 #
#######################################################################


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



######################################################
# 8.6 EXAMPLE VI: Adjusting Character String Entries #
######################################################

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



##########################################################################################
# 8.7 EXAMPLE VII: Deleting NAs from a Single Row and Introducing the Tibble Data Format #
##########################################################################################

library("tidyverse") # needed to create tibble data tables.

# Simple tibble table:
tibble_test = as.tibble(cbind(c(1,2,3), c("a","b","c"), c("ey","bee","see")))

# Tibble also shows the dimension and the classes of the columns in the console output:

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
x$measurement_sysRRalt[!is.na(x$measurement_sysRRalt)]

#### SAME AND MORE BUT NOW WITH TIBBLE FORMAT:

# "new_table" as tibble
x = as.tibble(new_table)
class(x)
# [1] "tbl_df"     "tbl"        "data.frame"

# Change entry, even though it is a different class (changes column to character):
x[4,3] = "keine Angabe"
x$measurement_sysRRalt[4]
# [1] "keine Angabe"

class(x$measurement_sysRRalt)
# [1] "character"

# Change all entries with NA to "keine Angabe":
x$measurement_sysRRalt[which(is.na(x$measurement_sysRRalt)==TRUE)] = "keine Angabe"

# Delete row with na
# Whe first have to recreate original x with NAs, since we changed NA into "keine Angabe" above:
x = as.tibble(new_table)
new_x = filter(x, is.na(measurement_sysRRalt) != TRUE)

# Delete NA of a column (not the whole line, just single entries of a column vector)
# Whe first have to recreate original x with NAs (above line deleted lines with NA in below column...):
x = as.tibble(new_table)
x$measurement_sysRRalt[!is.na(x$measurement_sysRRalt)]

# You can also use na.omit(), but you should only do so for vectors, not tables/matrices:
# The output format is nasty though; change via is.numeric():
x = as.tibble(new_table)
x2 = na.omit(x$measurement_sysRRalt)
# Calculate mean:
x3 = mean(as.numeric(x2))

######################################################
# 8.8 EXAMPLE VIII: Rearranging and Deleting Columns #
######################################################

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

# Deleting rows
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


##################################################
# 8.9 EXAMPLE IX: Working with Redundant Columns #
##################################################

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
# when two criterias are fulfilled!! Via the operator & below.
# You could also use | which functions as "or" if thats what you need...
for(i in 1:length(test$V2)){ # cols have same length, same result with V1 or V3 
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



########################################
# 8.10 EXAMPLE X: Decoding and Sorting #
########################################

code = c("a","b","a","c","a")
count = c(20,22,19,5,44)
table = as.data.frame(cbind(code,count))

#   code count
# 1    a    20
# 2    b    22
# 3    a    19
# 4    c     5
# 5    a    44

# Sort by count, decreasing (here you have to change the classe of the column count!):
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


############### This time only i and j!!!
decode = c("") # empty vector
table_new = as.data.frame(cbind(code,count,decode))

decode = c("Something1","Something2","Something3")
codes = c("a","b","c")
de_code = as.data.frame(cbind(decode,codes)) 
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


############################################
# 8.11 EXAMPLE XI: Changing Ä/ä into Ae/ae #
############################################

# Forgot to find the source for this one:
# install.package("stringi")
library(stringi)

# Test character string:
test_umlaut =c("ae", "oe", "ue", "Ae", "Oe", "Ue")

test_umlaut = stri_replace_all_fixed(
  test_umlaut, 
  c("ae", "oe", "ue", "Ae", "Oe", "Ue"), # exchange...
  c("ä", "ö", "ü", "Ä", "Ö", "Ü"),       # ... these vectors for the inverse case...
  vectorize_all = FALSE)

test_umlaut
# > test_umlaut
# [1] "ä" "ö" "ü" "Ä" "Ö" "Ü"


##############################################################
# 8.12 EXAMPLE XII: Deleting Duplicates (NOS Paper Example!) #
##############################################################


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
    for(j in 1:length(x)){               # + for each respective duplicate
      #  value in column vector with the ids,
      if(x[j] == inter2$x[i]){          # if the i_dth value is equivalent to 
        # the evaluated explicit duplicate id values:
        blank[j] = 1  # then / in such a case assing a 1 to the redundant binary/logical column.
      } # End if       # now any duplicate and the value that is duplicated are tagged by a logical!
    } # End for j
  } # End for i
  
  data_frame = cbind(y,blank)   # Now combinde the redundant binary column 
  # with the whole data frame that carries 
  # the id column
  data_frame = as.data.frame(data_frame)       # turn it into a data frame
  filtered = filter(data_frame, blank != "1" ) # ... in order to finally filter 
  # all (!) redundant id values, no 
  # matter how many duplicates there are,
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

# Row 1 and 3, i.e. id10 has been deleted. I was was lazy
# so the redundant column "blank" remains and only entails  
# zeros, since all lines that marked duplicate and the value 
# that was duplicated with a 1 have been deleted...

# V1               V2           V3 blank
# 1 id12 random entry random entry     0
# 2 id14 random entry random entry     0
# 3 id15 random entry random entry     0
# 4 id16 random entry random entry     0



#######################################################
# 8.13 EXAMPLE XIII: Converting Wide into Long Format #
#######################################################

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
# Here the paramters key and value create new column names for the reshaped table.
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



# # # # # # # # # # # # 
# ------------------- #
#######################
# 9 Writing Functions #
#######################
#############################################
# 9.1 EXAMPLE FUNCTION I: Parabola Function #
#############################################

# Writing a parabola function:
parabola = function(x){
  fx = x^2
  return(fx)
} # End of function parabola

# Example
x = c(-4:4) # creates vector from -4 to 4, integer steps
parabola(x)

############################################################################
# 9.2 EXAMPLE FUNCTION II: Turning Nested For Loop Example into a Function #
############################################################################

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
  for(i in 1:length(x[,1])){    # loop over rows
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

###########################################################################################
# 9.3 EXAMPLE FUNCTION III: Replication of basic gather() Output Without Extra Parameters #
###########################################################################################


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


######################################################
# 9.4 EXAMPLE FUNCTION IV: CP / Bayes’ Rule Function #
######################################################

############# First simple version of our Bayes_Machine() function:
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


############## Here is the extended function that can also handle 
############## the model evidence as input:

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


########################################
# 9.5 EXAMPLE FUNCTION V: Sum Function #
########################################

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



########################################################################
# 9.6 EXAMPLE FUNCTION VI: Replication of lm(y~x) and summary(lm(y~x)) #
########################################################################

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



#####################################################################################
# 9.7 EXAMPLE FUNCTION VII: Logistic Map and the Mandelbrot Set (Functions for Fun) #
#####################################################################################


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

# Set scanning range for bifurcation parameter r (caluclation
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
# .05241314 => further decreas!

# If I'd go on with this a couple of years, around 15,
# the population will die. 

# So eventually, if one plays around with LAMBDA (remeber between 0 and 1)
# at some values of LAMBDA funky things will happen!

xnPLUS1 = 3.2*.5*(1-.5)
xnPLUS1 
# .8, so increase

xnPLUS2 = 3.2*xnPLUS1*(1-xnPLUS1)
xnPLUS2 
# .512, so "competintion kicks in", as they put it in the tutorial 


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
# that the behaviour gets chaotic!! The further you get to 3.59 the faster 
# and the more cycles one gets, which are actually doubling!
# So Feigenbaum got involved in the 70s and wanted to know how
# far LAMDA has to change for that to happen.
# When he was looking at the ratio of these changes in X, i.e.
# values of LAMBDA, he figured out a constant, which is now
# called, the Feigenbaum constant ..... 4.669...
# SO WHAT THE NUMBER SAYS is that the length of X/LAMBDA is getting
# 4.669 times smaller than the previous, i.e. with the previous value
# of LAMBDA, in case of a doubling, as the length represents a
# stabilasation of the cylce within that length!!


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





