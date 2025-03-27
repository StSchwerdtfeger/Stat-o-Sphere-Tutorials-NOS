#######################################
#######################################
#        R-Basic Tutorial I-III       #
#  Solution for Kick-Start Exercise   #
#       CIPom @ Charité Edition       #
#                 by                  #
#       Steffen Schwerdtfeger         # 
#               2.2025                #
#######################################
#######################################

# Werde 1 Cyber!  / Become a Cyber!
print("Werde 1 Cyber!")
print("Become A Cyber!")

####### Recreation of table in R (a)):
table = as.data.frame(cbind(c(1,2,3),c("Test","Test2","Test3"),c("Ey","Bee", "See")))

# Alternative to keep class integrity for each column (another alternative: give each vector a name and
# then bind them via data.frame():
table = as.data.frame(c(1,2,3))
table = cbind(table,c("Test","Test2","Test3"),c("Ey","Bee", "See"))

# Looks nasty, since it adapted the input as col names... We could
# rename the columns, but I leave it like it is for now:
#  > table
#    c(1, 2, 3) c("Test", "Test2", "Test3") c("Ey", "Bee", "See")
#  1          1                        Test                    Ey
#  2          2                       Test2                   Bee
#  3          3                       Test3                   See

####### Change field first row, second column to Test1 (b)):
table[1,2] = "Test1"

#  > table
#    c(1, 2, 3) c("Test", "Test2", "Test3") c("Ey", "Bee", "See")
#  1          1                        Test                    Ey
#  2          2                       Test2                   Bee
#  3          3                       Test3                   See

####### Delete first column (c)):
table = table[,-c(1)]

#   c("Test", "Test2", "Test3") c("Ey", "Bee", "See")
# 1                        Test                    Ey
# 2                       Test2                   Bee
# 3                       Test3                   See


####### Decode vector (d)):
test = c("Test1", "Test2", "Test3")

# Decode via loop (overwrites column "code"):
for(i in 1:length(test)){
  if(test[i] == "Test1"){
    test[i] = "Something1"
  } # End if "a"
  else if(test[i] == "Test2"){
    test[i] = "Something2"
  } # End if "b"
  else if(test[i] == "Test3"){
    test[i] = "Something3"
  } # End if "c"
} # End for i

test
# [1] "Something1" "Something2" "Something3"

####### Decode vector (d)), alternative via targeting the above table not seperate vector:

# Decode via loop (overwrites column "code"):
for(i in 1:length(table[,1])){
  if(table[i,1] == "Test1"){
    table[i,1] = "Something1"
  } # End if "a"
  else if(table[i,1] == "Test2"){
    table[i,1] = "Something2"
  } # End if "b"
  else if(table[i,1] == "Test3"){
    table[i,1] = "Something3"
  } # End if "c"
} # End for i

table
# 
# > table
#   c("Test", "Test2", "Test3") c("Ey", "Bee", "See")
# 1                  Something1                    Ey
# 2                  Something2                   Bee
# 3                  Something3                   See



