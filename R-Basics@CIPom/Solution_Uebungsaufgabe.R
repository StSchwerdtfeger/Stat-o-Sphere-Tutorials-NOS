#######################################
#######################################
#        R-Basic Tutorial I-III       #
#  Solution for "Übungsaufgabe.docx"  #
#       CIPom @ Charité Edition       #
#                 by                  #
#       Steffen Schwerdtfeger         # 
#              12.2024                #
#######################################
#######################################


# The script can be executed as a whole, given all packages are installed. 
# Mark all via ALT + A and then Run via ALT + ENTER.
# More details @ https://journal.medicine.berlinexchange.de/statosphere 

# INSTALL AND LOAD NECESSARY PACKEGES. However, all packages are loaded on spot 
# in this script as well for educative purposes. A lot of the packages are 
# entailed in the package "tidyverse" which is actually a package with packages
# where a package itself is a collection of functions...

# install.packages("tidyverse"):
library(tidyverse) # filter(), write.csv(); BONUS: reshape(), gather()
# install.packages("pwr")
library(pwr) # for power analysis regarding t-test
# install.packages("effsize")
library(effsize) # cohen.d() => syntax equivalent to t.test()
# install.packages("gt")
library(gt) # gt() for nice looking table, such as in scientific papers

########## Load Data (a)):
data = read.csv("Uebungsaufgabe.csv") # Automatically in format data.frame!!
data # execute line or mark name of an object to show content in console,
      # or, recall, use print(data) to "print" the content of 
      # an object in the console


########## Add missing measurement value (b)):
data$measurement_sysRR[9] = 122


# Load dplyr to use its filter() function (another function called filter() is masked):
# install.packages(dplyr):
library(dplyr)

########## Filter the table to obtain two tables given either t1 or t2 in the column "time"
# (from 14x4 to 7x4x2, scheme row x col x number of tables). (for a), also preparation for c)):
t1 = filter(data, time == "t1")
t1
t2 = filter(data, time == "t2")
t2

# Check for normal distribution: 
plot(density(t1$measurement_sysRR))
plot(density(t2$measurement_sysRR))
# I find it always hard to exactly tell visually in this case....

# Check with shapiro.test():
shapiro.test(t1$measurement_sysRR)
shapiro.test(t2$measurement_sysRR)
# => both have a p-value > .05 
# => if the values were non-normal distr., then a non-parametric Wilcoxon signed 
#    rank test would be the choice in this case... 

######## Perform paired/dependent t-test: Note that input is t.test(t2,t1,paired=TRUE) (c)):
result_t_test = t.test(t2$measurement_sysRR, t1$measurement_sysRR, paired = TRUE)
result_t_test

# > result_t_test 
#
#           Paired t-test
#
#   data:  t2$measurement_sysRR and t1$measurement_sysRR
#   t = -8.283, df = 11, p-value = 4.686e-06
#   alternative hypothesis: true mean difference is not equal to 0
#   95 percent confidence interval:
#     -11.602460  -6.730874
#   sample estimates:
#   mean difference 
#     -9.166667 

# GOT DIFFERENT RESULTS?? Check if you have had an older t1/t2 from another 
# example in the work space and forgot to run the new filter lines for t1 and t2.


# Check if result is significant and relevant by also using the effect size package. 
# Recall CI are CI of (standardized) diff. in mean! If 0 is entailed in the span,
# the effect size is either at +/-1.96, which means that no matter the sample size
# the result will be both significant and relevant, or if it entails zero in  
# that span, it means it may not be significant, depending on the sample size
# in t/z-value = (sample_mean-pop_mean)/sd_pop/sqrt(sample_size_n), but it may be
# relevant, depending on the actual non-standardized difference in means and the
# respective context. HOWEVER, the t.test() function only delivers the CI of the
# the t-value, which, yes, is also a standardized difference, but entails sqrt(n)
# and does not represent the CI of the mere effect, without considering the sample size

# install.packages("effsize")
library(effsize)
effsize = cohen.d(t2$measurement_sysRR, t1$measurement_sysRR, paired = TRUE) 
effsize

# Cohen's d

# d estimate: -1.533941 (large)
# 95 percent confidence interval:
#      lower      upper 
# -2.1005458 -0.9673354 

# In order to reflect the result, we also have to consider the standard deviation!
effsize$sd
# [1] 5.975894

# You could also recreate it via (no matter how the sd was calculated for different
# t-test forms, e.g., if sd_pooled was used...): 
result_t_test$estimate/effsize$estimate


# Power Analysis (d)):
# install.packages("pwr")
library(pwr)
pwr.t.test(d = .5, sig.level = .05, power = .8, type = "paired") 

# Paired t test power calculation 

# n = 33.36713
# d = 0.5
# sig.level = 0.05
# power = 0.8
# alternative = two.sided

# NOTE: n is number of *pairs*


######### Look at the unique values (e)):
unique(data$fam)

# Change NA to "keine Angabe":
data$fam[which(is.na(data$fam) == TRUE)] = "not specified"

# Change "ys", "y" and "ja" to "yes":
data$fam[which(data$fam == "ys")] = "yes"
data$fam[which(data$fam == "y")] = "yes"
data$fam[which(data$fam == "ja")] = "yes"

# Alternative with for loop:
yes = c("ys", "y", "ja")
for(i in 1:length(yes)){
  data$fam[which(data$fam == yes[i])] = "yes"  
} # End for i

# Change "noo" and "n" to "no":
data$fam[which(data$fam == "noo")] = "no"
data$fam[which(data$fam == "n")] = "no"

# Result:
unique(data$fam)
# > unique(data$fam)
# [1] "yes"          "no"           "not specified"


######### Plot pie table of the column "fam" (f)):
pie(table(data$fam)) 

# Frequencies of each option (first filter for either t1 or t2, since entries are redundant
# in the table "data"!!!):
frequencies = filter(data, time == "t1")
table(frequencies$fam)
#     no not specified           yes 
#      4             2             6

# Alternative way:
table(data$fam)/2
# SAME RESULT just somehow sorte differently
#     no not specified           yes 
#      4             2             6

# NBefore exporting the table, check in console if your table is fully cleaned...
data 

# > data 
# patient_id time measurement_sysRR           fam
# 1           1   t1               130           yes
# 2           1   t2               122           yes
# 3           2   t1               132            no
# 4           2   t2               123            no
# 5           3   t1               133 not specified
# 6           3   t2               121 not specified
# 7           4   t1               129            no
# 8           4   t2               125            no
# 9           5   t1               122            no
# 10          5   t2               119            no
# 11          6   t1               134           yes
# 12          6   t2               127           yes
# 13          7   t1               140 not specified
# 14          7   t2               125 not specified
# 15          8   t1               135           yes
# 16          8   t2               128           yes
# 17          9   t1               129           yes
# 18          9   t2               122           yes
# 19         10   t1               140            no
# 20         10   t2               128            no
# 21         11   t1               134           yes
# 22         11   t2               122           yes
# 23         12   t1               144           yes
# 24         12   t2               130           yes



######### ... if so, then export cleaned data set (g)):
# install.packages("readr")
library("readr") # also within "tidyverse"

# Save/Export .CSV
write.csv(data, "export_data.csv")



######################################
######################################
########### BONUS EXERCISE! ##########
######################################
######################################


######## Synthetic data set to test na.omit() (a))
test = cbind(c(1,NA,3),c(1,4,NA))
#      [,1] [,2]
# [1,]    1    1
# [2,]   NA    4
# [3,]    3   NA

na.omit(test)
#      [,1] [,2]
# [1,]    1    1
# attr(,"na.action")
# [1] 2 3
# attr(,"class")
# [1] "omit"


######## Reformat the table such that the sysRR for t1 and t2 has an extra column (b)):
######## Filter now cleaned data set: 
t1 = filter(data, time == "t1")
t2 = filter(data, time == "t2")


######## One way to do so would be cbind() each of the columns of t1 and only sysRR from t2
######## and add an empty row called RRdiff (c)):
RRdiff = c(0)
sysRR_t1 = t1$measurement_sysRR
sysRR_t2 = t2$measurement_sysRR
fam = t1$fam
patient_id = t1$patient_id
reformat = as.data.frame(cbind(patient_id, sysRR_t1, sysRR_t2, fam, RRdiff))

#    patient_id sysRR_t1 sysRR_t2            fam RRdiff
# 1           1      130      122            yes      0
# 2           2      132      123             no      0
# 3           3      133      121  not specified      0
# 4           4      129      125             no      0
# 5           5      122      119             no      0
# 6           6      134      127            yes      0
# 7           7      140      125  not specified      0
# 8           8      135      128            yes      0
# 9           9      129      122            yes      0
# 10         10      140      128             no      0
# 11         11      134      122            yes      0
# 12         12      144      130            yes      0

# Add sysRR difference in new column:
for(i in 1:length(reformat[,1])){
  reformat$RRdiff[i] = as.numeric(reformat$sysRR_t2[i]) - as.numeric(reformat$sysRR_t1[i])
} # End for i

# View results: 
reformat
# > reformat
#    patient_id sysRR_t1 sysRR_t2           fam RRdiff
# 1           1      130      122           yes     -8
# 2           2      132      123            no     -9
# 3           3      133      121 not specified    -12
# 4           4      129      125            no     -4
# 5           5      122      119            no     -3
# 6           6      134      127           yes     -7
# 7           7      140      125 not specified    -15
# 8           8      135      128           yes     -7
# 9           9      129      122           yes     -7
# 10         10      140      128            no    -12
# 11         11      134      122           yes    -12
# 12         12      144      130           yes    -14

# I'd say reformatting in the above way is rather unusual, since conversion is mostly 
# from wide to long format. However, this exercise size shows you how to reverse
# the actions of tha long format function. 



######## Alternative easier way of adding a column with diff values (d)):
RRdiff_alt = as.numeric(reformat$sysRR_t2)-as.numeric(reformat$sysRR_t1)
cbind(reformat,RRdiff_alt) 

#    patient_id sysRR_t1 sysRR_t2           fam RRdiff RRdiff_alt
# 1           1      130      122           yes     -8         -8
# 2           2      132      123            no     -9         -9
# 3           3      133      121 not specified    -12        -12
# 4           4      129      125            no     -4         -4
# 5           5      122      119            no     -3         -3
# 6           6      134      127           yes     -7         -7
# 7           7      140      125 not specified    -15        -15 
# 8           8      135      128           yes     -7         -7
# 9           9      129      122           yes     -7         -7
# 10         10      140      128            no    -12        -12
# 11         11      134      122           yes    -12        -12
# 12         12      144      130           yes    -14        -14

# You want to know how many people have a RR diff over or equal to 10.
# You can work with the abs() function for the absolute value:
diff_10plus = reformat[abs(as.numeric(reformat$RRdiff)) >= 10,]
#    patient_id sysRR_t1 sysRR_t2           fam RRdiff
# 3           3      133      121 not specified    -12
# 7           7      140      125 not specified    -15
# 10         10      140      128            no    -12
# 11         11      134      122           yes    -12
# 12         12      144      130           yes    -14

length(diff_10plus[,1])
# [1] 5
# => five patient_ids with RR difference >= 10



####### Create a nice table for export (e)):
# install.packages("gt")
# library(gt)
gt(reformat)


# Optionally you can turn the above table into a long format again:
# install.packages("reshape2") # or load tidyverse
library(reshape2) # within tidyverse

# Converting data frame into a long format, which some functions such may demand:
# Here it only makes sense for the columns patient_id and sysRR_t1 and sysRR_t2.
# Here the parameters variable.name and value.name create new column names for 
# the reshaped table. Below I chose time for either sysRR_t1 and sysRR_t2 as entry. 
# Value.names creates a column with the actual values, which are here the actual 
# sysRRs from t1 and t2, so this column is called sysRR. The melt() function also
# asks for a column with id's, which can also be just the row index as well, but
# here we actually have a column with ids.
long_reformat = reformat[, -c(4,5)]
long_format = melt(long_reformat, id.vars = "patient_id",
                                  variable.name = "time",
                                  value.name = "sysRR")

#    patient_id sysRR_t1_t2 sysRR
# 1           1    sysRR_t1   130
# 2           2    sysRR_t1   132
# 3           3    sysRR_t1   133
# 4           4    sysRR_t1   129
# 5           5    sysRR_t1   122
# 6           6    sysRR_t1   134
# 7           7    sysRR_t1   140
# 8           8    sysRR_t1   135
# 9           9    sysRR_t1   129
# 10         10    sysRR_t1   140
# 11         11    sysRR_t1   134
# 12         12    sysRR_t1   144
# 13          1    sysRR_t2   122
# 14          2    sysRR_t2   123
# 15          3    sysRR_t2   121
# 16          4    sysRR_t2   125
# 17          5    sysRR_t2   119
# 18          6    sysRR_t2   127
# 19          7    sysRR_t2   125
# 20          8    sysRR_t2   128
# 21          9    sysRR_t2   122
# 22         10    sysRR_t2   128
# 23         11    sysRR_t2   122
# 24         12    sysRR_t2   130


# Alternative via gather() which does not require an id column
# install.packages("tidyr")  # or load tidyverse
library(tidyr) # within tidyverse
# Here the paramters key and value create new column names for the reshaped table.
# Below I chose time for either sysRR_t1 and sysRR_t2 as entry. Values creates a
# column with the actual values, which are here the actual sysRRs from t1 and t2,
# so this column is called sysRR.
data_long = gather(long_reformat, key = "time", value = "sysRR", sysRR_t1:sysRR_t2)

#    patient_id        time sysRR
# 1           1    sysRR_t1   130
# 2           2    sysRR_t1   132
# 3           3    sysRR_t1   133
# 4           4    sysRR_t1   129
# 5           5    sysRR_t1   122
# 6           6    sysRR_t1   134
# 7           7    sysRR_t1   140
# 8           8    sysRR_t1   135
# 9           9    sysRR_t1   129
# 10         10    sysRR_t1   140
# 11         11    sysRR_t1   134
# 12         12    sysRR_t1   144
# 13          1    sysRR_t2   122
# 14          2    sysRR_t2   123
# 15          3    sysRR_t2   121
# 16          4    sysRR_t2   125
# 17          5    sysRR_t2   119
# 18          6    sysRR_t2   127
# 19          7    sysRR_t2   125
# 20          8    sysRR_t2   128
# 21          9    sysRR_t2   122
# 22         10    sysRR_t2   128
# 23         11    sysRR_t2   122
# 24         12    sysRR_t2   130









