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
test = 2 + 5  ; test2 = 2 + 3 # ";" is a so-called delimitter...


###################################################################
# 4 Classes of Objects: Vectors, Matrices, Arrays, Lists and More #
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


# Typical error:
# > # Typical Error:
# > vec = c(
# + 

# SINCE IT SAYS "+" it means that the operation is not finished yet, since
# the second ")" is missing. In fact, when selecting the whole script, R
# will "think" that everything after "(" is part of the input inbetween two
# brackets. IN ANY CASE you have type in the missing ")" and execute that line again.
# It will then show an error. You can then again execute the line you wanted to run
# We also recommend using the brush tool in the environment in case you lost track
# of an error! Restart executing line by line...

# Check which class our vector is using the class() function:
class(vec)
is.numeric(vec)

mat = as.matrix(vec)
#      [,1]
# [1,]    1
# [2,]    2
# [3,]    3

mat[3]
# or
mat[,1]
# [1] 2

# Exemplatory array with 
# dim = c(rows, columns, further_dimension)
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


mat_bind = cbind(c(1,2,3),c(1,2,3),c(1,2,3))
#      [,1] [,2] [,3]
# [1,]    1    1    1
# [2,]    2    2    2
# [3,]    3    3    3


mat_bind = as.data.frame(mat_bind) 
is.data.frame(mat_bind)
#   V1 V2 V3  # V stands for variable
# 1  1  1  1
# 2  2  2  2
# 3  3  3  3

# Use brackets and index location:
mat_bind[,2]
# Or use the dollar sign to call/select a specific row of the table by name:
mat_bind$Two

# Column names can be changed via:
colnames(mat_bind) = c("One", "Two", "Three") # rownames() exists too
#      One Two Three
# [1,]   1   1     1
# [2,]   2   2     2
# [3,]   3   3     3

# Columns of that data frame can again be called via $:
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
test = as.numeric(test) 
# [1] NA  2  3
# Warning message:
# NAs introduced by coercion


###########
#### Lists:

# Create a matrix/table:
table = matrix(1, ncol = 3, nrow = 3)

# A list can store all kinds of formats in a list [[1:n]]
# without messing up the classes or format in any way:
test_list = list("test",2,3, table)
# [[1]]
# [1] "test"

# [[2]]
# [1] 2

# [[3]]
# [1] 3

# [[4]]
#      [,1] [,2] [,3]
# [1,]    1    1    1
# [2,]    1    1    1
# [3,]    1    1    1

test_list[[2]]
# [1] 2



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

# Simple example plotting (note that the code is spread over two lines!!!):
plot(x = t1$patient_id,y = t1$measurement_sysRR, ylim=c(100,150), 
     col = "blue", ylab = "sysRR")
points(x=t2$patient_id,y=t2$measurement_sysRR, col = "darkgreen")

# Perform a paired/dependent t-test):
# Here using as.numeric() was necessary
x =t.test(as.numeric(t2$measurement_sysRR), as.numeric(t1$measurement_sysRR), paired  = TRUE)


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
?mean()
# [1] NA
# Warning message:
# In mean.default(t1alt$measurement_sysRRalt) :
#  Argument ist weder numerisch noch boolesch: gebe NA zurück

# mean() also entails parameter that deletes NAs automatically. 
mean(c(1,NA,3), na.rm = TRUE)


#################################################################################
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
new_table$patient_id[4]  # == new_table$patient_id[na_lines[1]]
new_table$patient_id[7]
new_table$patient_id[na_lines[1]]

 
# Initilize table:
fin_table = new_table
# Delete every line with the patient_id given in the list
# pat_id_na:
for(i in 1:length(pat_id_na)){ # or length(na_lines)
  fin_table = filter(fin_table, patient_id != pat_id_na[i])
} # End for i
fin_table 

t1 = filter(fin_table, time == "t1")
t2 = filter(fin_table, time == "t2")

# Again perform a dependent t-test:
t.test(as.numeric(t2$measurement_sysRRalt),as.numeric(t1$measurement_sysRRalt), paired = TRUE)

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


##########################
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


############################################################
### POSSIBLE SOLUTION II Simple dplyr/tidyverse alternative:
new_table_alt = filter(new_table, is.na(measurement_sysRRalt) == FALSE)
new_table_alt %>%   # new_table_alt was filtered for NAs already 
  select(patient_id,time,measurement_sysRRalt)%>%
  group_by(patient_id) %>%  # sorts individ. entries into indiv. groups 
                            # when executing next line! 
  filter(n()>1)     # filters if n of each grouped entry is greater 
                    # than 1 such that all single entries get filtered


#######################################
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


########################################
# DANGERS of the na.omit() FUNCTION!!!!!

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


#####################################################
# Deleting NAs in a vector of the data format tibble:

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


#####################################
###### Character string manipulation:
fin_table_alt = new_table

unique(fin_table_alt$fam)
# [1] "yes" NA    "no"  "ys" 
which(fin_table_alt$fam =="ys")
# [1] 11 12

# Correct entries e.g. via: The below is a composition of the functions
# fin_table$fam[]  and  which()  and  is.na()
fin_table_alt$fam[which(is.na(fin_table_alt$fam) == TRUE)] = "not specified"

fin_table_alt$fam[which(fin_table_alt$fam =="ys")] = "yes"
fin_table_alt$fam[which(fin_table_alt$fam =="n")] = "no"

unique(fin_table_alt$fam)
# [1] "yes"           "not specified" "no"    

# Pie chart (since frequencies are not shown, they do not have to be divided by two):
pie(table(fin_table_alt$fam))


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


##############################
####### Decoding and sorting
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

# Decode via loop (overwrites code):
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

# ALTERNATIVE Decode via loop (decodes in extra column code)
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
#

################################## 
### Change ae into ä or vice versa

# Forgott to finde the source for this one:
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


##################################################################################
#### WEITERES Beispiel für einen sogenannte "nested for loop", d.h. for loop, die
#### eine for loop enthält:

# Für die Auswertung eiens Fragebogens ist es Usus fehlende Antworten
# mit dem Mittelwert aller Antworten des Probanden zu ersetzen.  
# Antwort = irgendein ausgewählter Skalenwert für eine Frage...

# Warum macht man das? Bisher noch keine Erklärung gefunden/gesucht, aber
# sehr gutes Beispiel!!


# WICHTIG HIER: BEISPIEL FÜR TABELLE MIT ZWEI KRITERIEN GELÖST IN EINER 
# NESTED FOR LOOP (LOOP in einer LOOP): 
# => Das Beispiel ließe sich auch Schritt für Schritt lösen. Der Code wäre
#    schlicht länger. Es gib viele Wege zum gleichen Ziel. Wichtig ist,
#    dass ihr Anwendungen von Funktionen testet und euch dadurch immer 
#    im Klaren seid, was ihr tut. 

# Data frame
x = data.frame(cbind(c(1,NA,3),c(1,2,3), c(4,3,3)))
# > x
#   X1 X2 X3
# 1  1  1  4
# 2 NA  2  3
# 3  3  3  3


# Empty vector for results:
result = c()
for(i in 1:length(x[,1])){
  line = x[i,] 
  result[i] = mean(line[!is.na(line)]) # Löscht NA aus dem Vektor und errechnet mean.
  for(j in 1:length(line)){            # (Nested for loop) über die Zeileneinträge
    if(is.na(line[j])==TRUE){          # und wenn Eintrag ein NA,
      x[i,j] = result[i]               # dann wird dieser Eintrag durch den mean der Zeile ersetzt
    } # End if
  } # End for j
} # End for i

# Look at result in console:
print(x)

# In Zeile 2 war ein NA und sonst nur eine 2, sodass der Mittelwert auch 2...
# Wie man sehen kann wurde das NA mit dem entsprechendem Mittelwert der Zeile 
# ersetzt. 
# > x
#    X1 X2 X3
# 1 1.0  1  4
# 2 2.5  2  3
# 3 3.0  3  3



###############################
####### Preinstalled data sets:
data() # offers a variety of data sets

# Install package for further data sets:
# install.packages("dplyr") # install package (also used for data cleaning)
library(dplyr)            # open/activate/load package
data(starwars)            # load data set
View(starwars)            # view via RStudio viewer
?starwars                 # view documentation

# Check out the parameters of the lm() linear modelling function:
?lm()

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
lm(dino$y~dino$x)
summary(lm(dino$y~dino$x)) # Slope is not significant...


############################################################################### 
# NOS Paper Example (https://github.com/StSchwerdtfeger/Filtering-Duplicates):

# For new release of current preprint by Rico Schmitt:
# https://journal.medicine.berlinexchange.de/pub/nqjpou17/release/1 

# FULL R SCRIPT AND DATA AVAILABLE IN THIS PUBLICATION!!!!!

# The purpose of this function: delete all duplicate values, as timepoint 1 and t2
# cannot be connected unambivalently due to redundant encoding of id values in the 
# above publication (e.g. 1000 participants but only three digit code!!). 

# Issue:   duplicated() only solves for a logical with all duplicated values but
#          does not return a logical that can be related to the original id column,
#          since it does not entails a TRUE for the value that is duplicated itself as well.

# EXAMPLE: In c( 1, 2, 2, 3) duplicated() returns: false, false, true, false. 
#          We want to delete all 2's in that column vector! I.e., we desire an output of
#          false true, true, false, in order to use that column vector to identify 
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




########################################################
# 6 Loading Excel and .CSV Files into the environment: #
########################################################

# dino = read.csv("YOU FILE PATH")

# Sets a standard path for files, a so-called working directory, which
# is set when creating a project:

# setwd("YOUR STANDARD FILE PATH") 

# Forgot your wd? Use:

getwd() # shows path assigned, e.g., for a project or standard folder

# In case you set up a project or placed your data files in the folder
# that getwd() shows as output, then you can use a shortcut using the
# file name only:
dino = read.csv("dino_csv.csv")
# dino2 = read.csv2("dino_csv2.csv") # I only have the regular .csv if my project directory (working directory folder)

# I called the object "dino", such that plotting is done by:
plot(x = dino$x,y = dino$y)

# Load Excel file:
# install.packages("readxl") # also within "tidyverse"
library("readxl")
# dino_excel = read_excel("dino_csv2.xlsx") # activate line when you have a .xlsx Version of the csv


library("readr") # also within "tidyverse"
# write.csv()
# write.csv2()



#############################
# 7 Understanding functions #
#############################

# Example Parabola Function:
parabola = function(x){
  fx = x^2
  return(fx)
} # End of function parabola

# Example
x = c(-4:4) # creates vector from -4 to 4, integer steps
parabola(x)


# Examplatory code for a sum function (do not name it "sum",
# since it conflicts with the integrated sum() function)
sum_alt = function(x){ # Start of function
  result = 0                 # initialize object for recursive addition;
  for(i in 1:length(x)){     # successively add the values of a vector;
    result = result + x[[i]] # recursive addition; i is element of a set
  } # End for i              # I = {1 to length(vec)}; n = length(vec)
  return(result)             # return result in the console
} # End of function

# Starting with 0, the result of the line within the loop adds the next
# element to the previous result and overwrites the object result with the
# current result (recursive)

# Example:
sum_alt(c(1,2,3))

# Test for equal results:
sum(c(1,2,3)) == sum_alt(c(1,2,3))
# [1] TRUE


# MORE EXAMPLE FUNCTIONS CAN BE FOUND IN THE R BASIC ARTICLE!










