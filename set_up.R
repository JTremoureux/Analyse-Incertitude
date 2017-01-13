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

input_2 <- jdd_2[,-(20:21)] # head(input_2)
output_2 <- jdd_2[,20:21] # head(output_2)