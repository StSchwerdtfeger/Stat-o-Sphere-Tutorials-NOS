##################################
##################################
#       R-Basic Tutorial I       #
#     CIPom @ Charité Edition    #
#               by               #
#     Steffen Schwerdtfeger      # 
#            10.2023             #
##################################
##################################

############################
# 3 New Script and Project #
############################

# Mark line and execute via ALT+ENTER or Cmnd+ENTER (Mac)
test = 2 + 5 
test = 2 + 3



###################################################################
# 4 Classes of Objects: Vectors, Matrices, Arrays, Lists and More #
###################################################################

vec = c(1,2,3)
# [1] 1 2 3  # NOTE that an output is also a vector
#      therefore the [1] at the beginning.

vec[3]  # index 2 == second element 
# [1] 2 # second element in the vector is also 2


# Typical error:
# > # Typical Error:
# > vec = c(
# + 


# SINCE IT SAYS "+" it means that the operation is not finished yet, since
# the second ")" is missing. In fact, when selecting the whole script, R
# will "think" that everything after "(" is part of the input inbetween two
# brackets. IN ANY CASE you have type in the missing ")" and execute that line again.
# It will then show an error. We also recommend using the brushtool in the environment!

class(vec)
mat = as.matrix(vec)
#      [,1]
# [1,]    1
# [2,]    2
# [3,]    3

mat[2]
# or
mat[2,1]
# [1] 2

# Exemplatory array with 
# dim = c(rows, columns, further_dimension)
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


mat_bind = cbind(c(1,2,3),c(1,2,3),c(1,2,3))
#      [,1] [,2] [,3]
# [1,]    1    1    1
# [2,]    2    2    2
# [3,]    3    3    3

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

# Columns of that data frame can also be called via $:
mat_bind$One

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

# Lists:
test_list = list("test",2,3)
# [[1]]
# [1] "test"

# [[2]]
# [1] 2

# [[3]]
# [1] 3

test_list[[2]]
# [1] 2


# Character string to numeric vector:
test = c("1","2","3")
is.character(test)
# [1] TRUE

test = as.numeric(test)
is.numeric(test)
# [1] TRUE

# Check what happens if one element is an actual character string
# not just numbers treated as characters:
test = c("test","2","3")
as.numeric(test)
# [1] NA  2  3
# Warning message:
# NAs introduced by coercion


########################
# 5 Data Cleaning in R #
########################

# Example for the structure of data tables:
# Creating an examplatory data set with vectors:
patient_id = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)
fam = c("yes", "yes", "no", "no", NA, NA, "n","n","no","no","ys", "ys", NA, NA)
time = c("t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2","t1","t2")
measurement_sysRR = c(130,122,132,123,133,121,129,125,135,119,134,127,140,125)


# Format into a data frame:
table = as.data.frame(cbind(patient_id,time,measurement_sysRR,fam))

# install.packages("dplyr")  # install package
library(dplyr)               # load/activate package
# Filter function: filter(data_object, columnname == "entry") or != for unequal
t1 = filter(table, time == "t1")
t1 = filter(table, time != "t2") # alternative
t2 = filter(table, time == "t2")

# Simple example plotting:
plot(x = t1$patient_id,y = t1$measurement_sysRR, ylim=c(100,150), 
     col = "blue")
points(x=t2$patient_id,y=t2$measurement_sysRR, col = "darkgreen")

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
# In welchen Zeilen sind NA (Zeilennummer, nicht pat. id!)
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

# Analoge:
new_table$patient_id[4]
new_table$patient_id[7]


# Initilize table:
fin_table = new_table
# Delete every line with the patient_id given in the list
# pat_id_na:
for(i in 1:length(pat_id_na)){ # or length(na_lines)
  fin_table = filter(fin_table, patient_id != pat_id_na[i])
} # End for i
fin_table 

## Example For loop:

# Define an object you want to loop over:
object = c(1,1,1,1)

# Initilize a list to store the results:
list_results = c()

# For loop that adds +1 to every element of the above object:
for(i in 1:length(object)){
  list_results[i] = object[i]+1
} # End for i
list_results

# Analog: 
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

# Filter all IDs that only occur once:
table_single_id = filter(table_num_of_id, num_of_id == "1")
table_single_id

fin_table_alt = new_table
# The same as before: filter single id from new_table:
for(i in 1:length(table_single_id$unique_id)){ # or length(na_lines)
  fin_table_alt = filter(fin_table_alt, patient_id != table_single_id$unique_id[i])
} # End for i
fin_table_alt 

# Check for equality of both methods:
fin_table$patient_id == fin_table_alt$patient_id
# [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE


### POSSIBLE SOLUTION II Simple dplyr/tidyverse alternative:
new_table_alt = filter(new_table, is.na(measurement_sysRRalt) == FALSE)
new_table_alt %>%   # new_table_alt was filtered for NAs already 
  select(patient_id,time,measurement_sysRRalt)%>%
  group_by(patient_id) %>%  # sorts individ. entries into indiv. groups 
                            # when executing next line! 
  filter(n()>1)     # filters if n of each grouped entry is greater 
                    # than 1 such that all single entries get filtered


#### Pipe operator %>% using tidyverse:
# install.packages("tidyverse")
library("tidyverse")

# Piper operater reads out "and then" (executes more code at once
# and therefore also runs faster, but as mentioned, it is not really
# noticeable):

# Test 1 Filter von t2 und Zeilen mit NA löschen
test1 = new_table %>% 
  select(patient_id,time,measurement_sysRRalt) %>% # select columns like cbind()
  filter(time != "t1")%>%  # Filter for t1
  na.omit()
  # na.omit() # be careful with this function!!!
              # and no %>% at the end, otherwise "+" in console!

test1
#   patient_id time measurement_sysRRalt
# 1          1   t2                  122
# 3          3   t2                  121
# 4          4   t2                  125
# 5          5   t2                  119
# 6          6   t2                  127
# 7          7   t2                  125


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
name = c("Vorname Nachname","Vorname Nachname","Vorname Nachname")
last_name = c("","","")
names = cbind(name,last_name)
#      name               last_name
# [1,] "Vorname Nachname" ""       
# [2,] "Vorname Nachname" ""       
# [3,] "Vorname Nachname" "" 

names = as.data.frame(names)
first_name = word(names$name,1) # 1 for first word, where a space is the delimiter
# [1] "Vorname" "Vorname" "Vorname"


#### Preinstalled data sets:
data() # offers a variety of data sets

# Install package for further data sets:
# install.packages("dplyr") # install package (also used for data cleaning)
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

########################################################
# 6 Loading Excel and .CSV Files into the environment: #
########################################################

# dino = read.csv("YOU FILE PATH")

# Sets a standard path for files, a so-called working directory, which
# is set when creating a project:

# setwd("YOUR STANDARD FILE PATH") 

# Forgott your wd? Use:

# getwd() # shows path assigned, e.g., for a project or standard folder

# In case you set up a project or placed your data files in the folder
# that getwd() shows as output, then you can use a shortcut using the
# file name only:
dino = read.csv("dino_csv.csv")
dino2 = read.csv2("dino_csv2.csv")

# I called the object "dino", such that plotting is done by:
plot(x = dino$x,y = dino$y)

# Load Excel file:
# install.packages("readxl") # also within "tidyverse"
library("readxl")
dino_excel = read_excel("dino_csv2.xlsx")


library("readr") # also within "tidyverse"
# write.csv()
# write.csv2()


#############################
# 7 Understanding functions #
#############################

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








