#######################################
#        R-Basic Tutorial I-III       #
#   Solution for "Übungsaufgabe.docx" #
#       CIPom @ Charité Edition       #
#                 by                  #
#       Steffen Schwerdtfeger         # 
#              12.2024                #
#######################################

# DAS SCRIPT KANN HIER VON OBEN NACH UNTEN IN EINEM ZUG KOMPLETT AUSGEFÜHRT WERDEN
# d.h. alles markieren (ALT+SHIFT oder CMD+SHIFT), dann ALT/CMD+ENTER...
# Selbiges gilt nicht für die Tut-Scipte (zumindest nicht getestet). Es gilt aber
# auch für alle Fließtextartikel auf https://journal.medicine.berlinexchange.de/statosphere 


### SCRIPT ENTHÄLT EINE WEITERE KURZE ÜBUNGSAUFGABE MIT LÖSUNG!!! 
  

# Load Data:
daten = read.csv("Uebungsaufgabe.csv") # Automatically in format data.frame!!
daten # execute line or mark name of an object to show content in console,
      # or, recall, use print(daten) to "print" the content of 
      # an object in the console


# Add missing measurement value:
daten$measurement_sysRR[9] = 122


# Load dplyr to use its filter() function (another function called filter() is masked):
# install.packages(dplyr)
library(dplyr)

# Filter the table to obtain two tables given either t1 or t2 in the column "time"
# (from 14x4 to 7x4x2, scheme row x col x number of tables):
t1 = filter(daten, time == "t1")
t1
t2 = filter(daten, time == "t2")
t2

# Perform paired/dependent t-test: Note that input is t.test(t2,t1,paired=TRUE):
result_t_test = t.test(t2$measurement_sysRR, t1$measurement_sysRR, paired = TRUE)
result_t_test

# Check if result is significant and relevant - 
# recall CI are CI of (standardized) diff. in mean! if 0 is entailed in the span,
# the effect size is either at +/-1.96, which means that no matter the sample size
# the result will be both significant and relevant, or if it entails zero in  
# that span, it means it may not be significant, depending on the sample size
# in t/z-value = (sample_mean-pop_mean)/sd_pop/sqrt(sample_size_n), but it may be
# relevant, depending on the actual non-standardized difference in means and the
# respective context. 

# Power Analysis:
# install.packages("pwr")
library(pwr)
pwr.t.test(d = .5, sig.level = .05, power = .8, type = "paired") 


# Look at the unique values:
unique(daten$fam)

# Change NA to "keine Angabe":
daten$fam[which(is.na(daten$fam) == TRUE)] = "keine Angabe"

# Change "ys", "y" and "ja" to "yes":
daten$fam[which(daten$fam == "ys")] = "yes"
daten$fam[which(daten$fam == "y")] = "yes"
daten$fam[which(daten$fam == "ja")] = "yes"

# Alternative with for loop:
yes = c("ys", "y", "ja")
for(i in 1:length(yes)){
  daten$fam[which(daten$fam == yes[i])] = "yes"  
} # End for i

# Change "noo" and "n" to "no":
daten$fam[which(daten$fam == "noo")] = "no"
daten$fam[which(daten$fam == "n")] = "no"

# Plot pie table of the column "fam":
pie(table(daten$fam)) 

# Frequencies of each option (first filter for either t1 or t2, since entries are redundant
# in the table "daten"!!!):
frequencies = filter(daten, time == "t1")
table(frequencies$fam)

# Check in Console if table is fully cleaned...
daten 
# ... if so, then export cleaned data set:
write.csv(daten, "export_daten.csv")

# > daten 
#    patient_id time measurement_sysRR          fam
# 1           1   t1               130          yes
# 2           1   t2               122          yes
# 3           2   t1               132           no
# 4           2   t2               123           no
# 5           3   t1               133 keine Angabe
# 6           3   t2               121 keine Angabe
# 7           4   t1               129           no
# 8           4   t2               125           no
# 9           5   t1               122           no
# 10          5   t2               119           no
# 11          6   t1               134          yes
# 12          6   t2               127          yes
# 13          7   t1               140 keine Angabe
# 14          7   t2               125 keine Angabe
# 15          8   t1               135          yes
# 16          8   t2               128          yes
# 17          9   t1               129          yes
# 18          9   t2               122          yes
# 19         10   t1               140           no
# 20         10   t2               128           no
# 21         11   t1               134          yes
# 22         11   t2               122          yes
# 23         12   t1               144          yes
# 24         12   t2               130          yes



  
############################  
########### BONUS EXERCISE!

# Reformat the table such that the sysRR for t1 and t2 has an extra column:

# Filter now cleaned data set: 
t1 = filter(daten, time == "t1")
t2 = filter(daten, time == "t2")

# One way to do so would be cbind() each of the columns of t1 and only sysRR from t2
# and add a empty row called RRdiff:
RRdiff = c(0)
sysRR_t1 = t1$measurement_sysRR
sysRR_t2 = t2$measurement_sysRR
fam = t1$fam
patient_id = t1$patient_id
reformat = as.data.frame(cbind(patient_id, sysRR_t1, sysRR_t2, fam, RRdiff))

# Add sysRR difference in new column:
for(i in 1:length(reformat[,1])){
  reformat$RRdiff[i] = as.numeric(reformat$sysRR_t2[i]) - as.numeric(reformat$sysRR_t1[i])
} # End for i

# View results: 
reformat
# > reformat
#    patient_id sysRR_t1 sysRR_t2          fam RRdiff
# 1           1      130      122          yes     -8
# 2           2      132      123           no     -9
# 3           3      133      121 keine Angabe    -12
# 4           4      129      125           no     -4
# 5           5      122      119           no     -3
# 6           6      134      127          yes     -7
# 7           7      140      125 keine Angabe    -15
# 8           8      135      128          yes     -7
# 9           9      129      122          yes     -7
# 10         10      140      128           no    -12
# 11         11      134      122          yes    -12
# 12         12      144      130          yes    -14

# You want to know how many people have a RR diff over or equal to 10.
# You can work with the abs() function for the absolut value:
diff_10plus = reformat[abs(as.numeric(reformat$RRdiff)) >= 10,]
# patient_id sysRR_t1 sysRR_t2          fam RRdiff
# 3           3      133      121 keine Angabe    -12
# 7           7      140      125 keine Angabe    -15
# 10         10      140      128           no    -12
# 11         11      134      122          yes    -12
# 12         12      144      130          yes    -14

length(diff_10plus[,1])
# [1] 5
# => five patient_ids with RR difference >=10




#################################################################################
#### WEITERES Beispiel für einen sogenannte "nested for loop", d.h. for loop, die
#### eine for loop enthält:

# Für die Auswertung eines Fragebogens ist es Usus fehlende Antworten
# mit dem Mittelwert aller Antworten des Probanden zu ersetzen.  
# Antwort = irgendein ausgewählter Skalenwert für eine Frage...

# Warum macht man das? Bisher noch keine Erklärung gefunden/gesucht, aber
# sehr gutes Beispiel!!


# WICHTIG HIER: BEISPIEL FÜR TABELLE MIT ZWEI KRITERIEN GELÖST IN EINER 
# NESTED FOR LOOP (LOOP in einer LOOP): 


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

# In Zeile 1 war ein NA, im Rest der Zeile eine 2+3, sodass der Mittelwert 
# auch 2.5...Wie man sehen kann wurde das NA mit dem entsprechendem 
# Mittelwert der Zeile ersetzt. 

# > x
#    X1 X2 X3
# 1 1.0  1  4
# 2 2.5  2  3
# 3 3.0  3  3










