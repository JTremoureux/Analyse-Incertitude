##################################
## PROJET ANALYSE D'INCERTITUDE ##
##################################

# Packages
library(sensitivity)

# Lecture des données
jdd_1 <- read.csv("H:/GM/5GM/Analyse_incertitude/TP_final/projet_incertitude/input/jdd_1.csv", header=TRUE)
jdd_1 <- as.data.frame(apply(apply(jdd_1, 2, gsub, patt=",", replace="."), 2, as.numeric))

# Variables
taux_dissolution <- colnames(jdd_1)[startsWith(colnames(jdd_1), "K2diss")]
taux_precipitation <- colnames(jdd_1)[startsWith(colnames(jdd_1), "Kppt")]
surface_reaction <- colnames(jdd_1)[startsWith(colnames(jdd_1), "Kine")]
sorties <- colnames(jdd_1)[startsWith(colnames(jdd_1), "X.wgt")]

summary(jdd_1)

par(mfrow=c(3,4))
for(i in 1:10) {
  boxplot(jdd_1[,surface_reaction[i]])
  title(surface_reaction[i])
}

par(mfrow=c(3,4))
for(i in 1:10) {
  boxplot(jdd_1[,taux_dissolution[i]])
  title(taux_dissolution[i])
}

par(mfrow=c(3,3))
for(i in 1:9) {
  boxplot(jdd_1[,taux_precipitation[i]])
  title(taux_precipitation[i])
}

par(mfrow=c(1,2))
for(i in 1:2) {
  boxplot(jdd_1[,sorties[i]])
  title(sorties[i])
}

# Pourquoi dans la premiere sortie il y a des valeurs aberrantes ?

par(mfrow=c(3,4))
for(i in 1:10) {
  hist(jdd_1[,surface_reaction[i]])
}

par(mfrow=c(3,4))
for(i in 1:10) {
  hist(jdd_1[,taux_dissolution[i]])
}

par(mfrow=c(3,3))
for(i in 1:9) {
  hist(jdd_1[,taux_precipitation[i]])
}

par(mfrow=c(1,2))
for(i in 1:2) {
  hist(jdd_1[,sorties[i]])
}

## Modélisation par une regression linéaire

full.lm1 <- lm(X.wgt_calcite ~ . - X.wgt_chlorite, data=jdd_1)
full.lm2 <- lm(X.wgt_chlorite ~ . - X.wgt_calcite, data=jdd_1)

min.lm1 <- lm(X.wgt_calcite ~ 1 - X.wgt_chlorite, data=jdd_1)
step.lm1 <- step(min.lm1, scope = list(lower=min.lm1,upper=full.lm1), direction = "both")

## Morris ça glisse


scale.unif <- function(X) {
  binf = apply(X,1,min)
  bsup = apply(X,1,max)
  return(X*(bsup-binf) + binf)
}

jdd_1.simule <- function(X) {
  X.scaled <- data.frame(apply(X,1,scale.unif))
  return(predict(full.lm1,X.scaled))
}

mor1 <- morris(model = jdd_1.simule, factors = colnames(jdd_1)[-31:-30], r = 6, design = list(type = "oat",levels = 5,grid.jump = 1))

