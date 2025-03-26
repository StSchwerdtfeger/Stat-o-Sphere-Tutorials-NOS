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

# Alternative to keep class integrity for each column:
table = as.data.frame(c(1,2,3))
table = cbind(table,c("Test","Test2","Test3"),c("Ey","Bee", "See"))

#   V1    V2  V3
# 1  1  Test  Ey
# 2  2 Test2 Bee
# 3  3 Test3 See

####### Change field first row, second column to Test1 (b)):
table[1,2] = "Test1"
# or, since it is a data frame object now as well:
table$V2[1] = "Test1"

#   V1    V2  V3
# 1  1 Test1  Ey
# 2  2 Test2 Bee
# 3  3 Test3 See

####### Delete first column (c)):
table = table[,-c(1)]

#      V2  V3
# 1 Test1  Ey
# 2 Test2 Bee
# 3 Test3 See


####### Decode vector (d)):
test = c("test1", "test2", "test3")

# Decode via loop (overwrites column "code"):
for(i in 1:length(test)){
  if(test[i] == "test1"){
    test[i] = "Something1"
  } # End if "a"
  else if(test[i] == "test2"){
    test[i] = "Something2"
  } # End if "b"
  else if(test[i] == "test3"){
    test[i] = "Something3"
  } # End if "c"
} # End for i



