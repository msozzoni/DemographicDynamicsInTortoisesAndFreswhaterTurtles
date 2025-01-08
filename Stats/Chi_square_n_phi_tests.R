#Chi square and phi test
#RStudio 2022.12.0+353 
#R version 4.2.2

#############################
# Chi square and phi tests  #
#############################

setwd()

### Load Libraries ----
library(readxl)
library(psych)


### Global MIS19 ---- 
# Create a table with the needed variables (LGM - Holocene)
# Create a data frame
turtle.data <- data.frame(read_excel("data/Contingency_table.xlsx", sheet = 1))

A.mis.lig = table(turtle.data$Ne.p_lig, turtle.data$Area.p_lig)

# Perform the Chi-Square test
print(chisq.test(A.mis.lig))
abs(phi(A.lgm.holo)) 

T.lgm.holo = table(turtle.data$Ne.p_lig, turtle.data$Temp.p_lig)
print(chisq.test(T.mis.lig))

### GLobal LIG-LGM ----
# Create a table with the needed variables (LIG - LGM)
A.lig.lgm = table(turtle.data$Ne.lig_lgm, turtle.data$Area.lig_lgm)
# Perform the Chi-Square test
print(chisq.test(A.lig.lgm))
abs(phi(A.lig.lgm)) 

T.lgi.lgm = table(turtle.data$Ne.lig_lgm, turtle.data$Temp.lig_lgm)
print(chisq.test(T.lgi.lgm))

### Temperate species ----
turtle.data <- data.frame(read_excel("data/Contingency_table.xlsx", sheet = 2))
# Create a table with the needed variables (MIS-LIG)
A.mis.lig = table(turtle.data$Ne.p_lig, turtle.data$Area.p.lig)
# Perform the Chi-Square test
print(chisq.test(A.mis.lig))
abs(phi(A.mis.lig)) 

T.mis.lig = table(turtle.data$Ne.p_lig, turtle.data$Temp.p_lig)
print(chisq.test(T.mis.lig))


# Create a table with the needed variables (LIG - LGM)
A.lig.lgm = table(turtle.data$Ne.lig_lgm, turtle.data$Area.lig.lgm)
# Perform the Chi-Square test
print(chisq.test(A.lig.lgm))
abs(phi(A.lig.lgm)) 

T.lig.lgm = table(turtle.data$Ne.lig_lgm, turtle.data$Temp.lig_lgm)
print(chisq.test(T.lig.lgm))

### Tropical species ----
turtle.data <- data.frame(read_excel("data/Contingency_table.xlsx", sheet = 3))
# Create a table with the needed variables (MIS-LIG)
A.mis.lig = table(turtle.data$Ne.p_lig, turtle.data$Area.p.lig)
# Perform the Chi-Square test
print(chisq.test(A.mis.lig))
abs(phi(A.mis.lig)) 
T.mis.lig = table(turtle.data$Ne.p_lig, turtle.data$Temp.p_lig)
print(chisq.test(T.mis.lig))


# Create a table with the needed variables (LIG - LGM)
A.lig.lgm = table(turtle.data$Ne.lig_lgm, turtle.data$Area.lig.lgm)
# Perform the Chi-Square test
print(chisq.test(A.lig.lgm ))
abs(phi(A.lig.lgm )) 

T.lig.lgm  = table(turtle.data$Ne.lig_lgm, turtle.data$Temp.lig_lgm)
print(chisq.test(T.lig.lgm))


### Terrestrial species ---- 
turtle.data <- data.frame(read_excel("data/Contingency_table.xlsx", sheet = 4))
# Create a table with the needed variables (MIS - LIG)
A.mis.lig = table(turtle.data$Ne.lig_lgm, turtle.data$Area.p.lig)
# Perform the Chi-Square test
print(chisq.test(A.mis.lig))
abs(phi(A.mis.lig)) 

T.mis.lig = table(turtle.data$Ne.p_lig, turtle.data$Temp.p_lig)
print(chisq.test(T.mis.lig))


# Create a table with the needed variables (LIG - LGM)
A.lig.lgm = table(turtle.data$Ne.lig_lgm, turtle.data$Area.lig.lgm)
# Perform the Chi-Square test
print(chisq.test(A.lig.lgm))
abs(phi(A.lig.lgm)) 

T.lig.lgm   = table(turtle.data$Ne.lig_lgm, turtle.data$Temp.lig_lgm)
print(chisq.test(T.lig.lgm  ))



#Aquatic species ----
turtle.data <- data.frame(read_excel("data/Contingency_table.xlsx", sheet = 5))
# Create a table with the needed variables (MIS - LIG)
A.mis.lig = table(turtle.data$Ne.p_lig, turtle.data$Area.p.lig)
# Perform the Chi-Square test
print(chisq.test(A.mis.lig))
abs(phi(A.mis.lig)) 

T.mis.lig = table(turtle.data$Ne.p_lig, turtle.data$Temp.p_lig)
print(chisq.test(T.mis.lig))


# Create a table with the needed variables (LIG - LGM)
A.lig.lgm = table(turtle.data$Ne.lig_lgm, turtle.data$Area.lig.lgm)
# Perform the Chi-Square test
print(chisq.test(A.lig.lgm))
abs(phi(A.lig.lgm)) 

T.lig.lgm = table(turtle.data$Ne.lig_lgm, turtle.data$Temp.lig_lgm)
print(chisq.test(T.lig.lgm))




