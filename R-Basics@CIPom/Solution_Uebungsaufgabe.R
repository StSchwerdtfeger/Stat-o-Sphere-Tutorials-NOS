
##################################################
### Solution for "Übungsaufgabe.docx" and the  ###
### examplatory data set "Uebungsaufgabe.csv": ###
##################################################

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




#### WEITERES Beispiel für einen sogenannte "nested for loop", d.h. for loop, die
#### eine for loop enthält:

# Für die Asuwertung eiens Fragebogens ist es Usus fehlende Antworten
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

# In Zeile 2 war ein NA und sonst nur eine 2, sodass der Mittelwert auch 2...
# Wie man sehen kann wurde das NA mit dem entsprechendem Mittelwert der Zeile 
# ersetzt. 
# > x
#    X1 X2 X3
# 1 1.0  1  4
# 2 2.5  2  3
# 3 3.0  3  3










