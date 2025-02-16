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


####### Recreation of table in R (a)):
table = as.data.frame(cbind(c(1,2,3),c("Test","Test2","Test3"),c("Ey","Bee", "See")))

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
table[,-c(1)]

#      V2  V3
# 1 Test1  Ey
# 2 Test2 Bee
# 3 Test3 See



