##################################
## PROJET ANALYSE D'INCERTITUDE ##
##################################

# Onn cherche à caractériser le comportement global de deux sorties représentatives de l’état final
# du système (concentration en chlorite et concentration en calcite) en fonction des paramètres initiaux de 
# l’expérience simulée.

# Packages
library(sensitivity) # Morris method
library(corrplot) # Correlation
library(randomForest)

# MORRIS

# Lecture des données
jdd_1 <- read.csv("H:/GM/5GM/Analyse_incertitude/TP_final/projet_incertitude/input/jdd_1.csv", header=TRUE)
jdd_1 <- as.data.frame(apply(apply(jdd_1, 2, gsub, patt=",", replace="."), 2, as.numeric))

express_1 <- jdd_1[,-31:-30]

# Variables
taux_dissolution <- colnames(jdd_1)[startsWith(colnames(jdd_1), "K2diss")]
taux_precipitation <- colnames(jdd_1)[startsWith(colnames(jdd_1), "Kppt")]
surface_reaction <- colnames(jdd_1)[startsWith(colnames(jdd_1), "Kine")]
sorties <- colnames(jdd_1)[startsWith(colnames(jdd_1), "X.wgt")]

# Statistique descriptive

mat.cor <- cor(jdd_1)
corrplot(mat.cor, type="lower", method="color",tl.col="black",tl.srt=45)

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

full.lm1 <- lm(jdd_1$X.wgt_calcite ~ ., data=express_1)
full.lm2 <- lm(jdd_1$X.wgt_chlorite ~ ., data=express_1)

min.lm1 <- lm(jdd_1$X.wgt_calcite ~ 1, data=express_1)
step.lm1 <- step(min.lm1, scope = list(lower=min.lm1,upper=full.lm1), direction = "both")

selected.var <- all.vars(formula(step.lm1)[-2])

## Morris ça glisse

binf = apply(express_1,2,min)
bsup = apply(express_1,2,max)

scale.unif <- function(X) {
  return(X*(bsup-binf) + binf)
}

jdd_1.simule <- function(X) {
  X.scaled <- data.frame(t(apply(X,1,scale.unif)))
  return(predict(full.lm1,X.scaled))
}

mor1 <- morris(model = jdd_1.simule, factors = colnames(express_1), r = 100, design = list(type = "oat",levels = 4, grid.jump = 3))
plot(mor1)


# ANOVA

anova <- aov(jdd_1$X.wgt_calcite ~ (.)^2, data=express_1)
plot(anova)


# RANDOMFOREST

rf_fit <- randomForest(jdd_1$X.wgt_calcite ~ ., data=express_1)
varImpPlot(rf_fit)

morf <- morris(model = rf_fit, factors = colnames(express_1), r = 10, design = list(type = "oat",levels = 4, grid.jump = 3))
plot(morf)

