# ---- Packages ----

library(sensitivity) # Morris method
library(corrplot) # Correlation
library(caret) # Machine Learning
library(randomForest) # Random Forest
library(neuralnet) # Neural Network
library(DiceKriging) # krigeage
library(dplyr) # Data manipulation
library(ggplot2) # Nice plot
library(ggrepel) # Avoid text overlap in ggplot
library(plotly) # Interactive plot

# ################################ 
# ====  Lecture des données  =====
# ################################

## ---- JDD_1 ----

jdd_1 <- read.csv("input/jdd_1.csv", header=TRUE) # 29+2 variables
# Certaines valeurs sont notées avec des ',' que l'on remplace par des points
jdd_1 <- as.data.frame(apply(apply(jdd_1, 2, gsub, patt=",", replace="."), 2, as.numeric))
input_1 <- jdd_1[,-(30:31)]
# head(input_1)
output_1 <- jdd_1[,30:31]
# head(output_1)

# str(jdd_1)
# summary(jdd_1)

## ---- JDD_2 ----

jdd_2 <- read.csv("input/jdd_2.csv", header=TRUE) # 19+2 variables
jdd_2 <- as.data.frame(apply(apply(jdd_2, 2, gsub, patt=",", replace="."), 2, as.numeric))

input_2 <- jdd_2[,-(20:21)] 
# head(input_2)
output_2 <- jdd_2[,20:21] 
# head(output_2)

## ---- Scale des variables ----

# On va scale les variables de jdd1 et jdd2
# On veut scaler les variables communes avec le même min/max.
# On vérifie avant qu'ils sont de même grandeur.
input_names_jdd2 <- colnames(input_2)
variables_partagées <- colnames(input_1) %in% colnames(input_2)

temp_input_1 <- input_1[,input_names_jdd2]
for (i in 1:length(input_names_jdd2)) {
  print(input_names_jdd2[i])
  print("min")
  print(min(temp_input_1[,i]))
  print(min(input_2[,i]))
  print("max")
  print(max(temp_input_1[,i]))
  print(max(input_2[,i]))
}
# OK !

maxs1 <- apply(input_1, 2, max)
mins1 <- apply(input_1, 2, min)
maxs2 <- apply(input_2, 2, max)
mins2 <- apply(input_2, 2, min)

mins_global <- mins1
maxs_global <- maxs1
mins_global[input_names_jdd2] = apply(data.frame(rbind(mins1[input_names_jdd2], mins2[input_names_jdd2])), 2, min)
maxs_global[input_names_jdd2] = apply(data.frame(rbind(maxs1[input_names_jdd2], maxs2[input_names_jdd2])), 2, max)

# En fait, on se rend compte que maxs_global = maxs1, et mins_global = mins1
input_2_scaled <- as.data.frame(scale(input_2, center = mins_global[input_names_jdd2], scale = maxs_global[input_names_jdd2]-mins_global[input_names_jdd2]))
input_1_scaled <- as.data.frame(scale(input_1, center = mins_global, scale = maxs_global-mins_global))

# On vérifie que ça a marché
which(input_1_scaled < 0) # 0
which(input_1_scaled > 1) # 0
which(input_2_scaled < 0) # 0
which(input_2_scaled > 1) # 0
# OK !