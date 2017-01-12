library(DiceKriging)

# Chargement des données

jdd_1 <- read.csv("jdd_1.csv", header=TRUE)
jdd_2 <- read.csv("jdd_2.csv", header=TRUE)
n <- 3000

plot(jdd_1[,1],jdd_1[,1], type = "p", col = "blue")
points(jdd_2[,1],jdd_2[,1], col = "red", cex = 0.1)

par(mfrow=c(1,1))
common.variables <- colnames(jdd_1)[which(colnames(jdd_1) %in% colnames(jdd_2) == TRUE)]
for (i in common.variables) {
  print(paste(max(as.vector(jdd_1[,i])),max(as.vector(jdd_2[,i]))))
}

# Variables explicatives
X <- data.frame(scale(jdd_2[,-21:-20]))
X.calcite <- data.frame(scale(jdd_2[,-21]))
X.chlorite <- data.frame(scale(jdd_2[,-20]))

# Réponses
Y <- data.frame(scale(jdd_2[,20:21]))
Y.calcite <- data.frame(scale(jdd_2[,20]))
Y.chlorite <- data.frame(scale(jdd_2[,21]))

# Régression linéaire
modele.lm <- lm(X.wgt_calcite ~ ., data = X.calcite)

# Krigeage
modele.km <- km(Y.calcite ~ ., design = X, response = Y.calcite)

calcite.predict.lm <- predict(modele.lm,X)
calcite.predict.km <- predict(modele.km,X,type="UK")

plot(Y.calcite[,1],Y.calcite[,1],col='black',type='l')
points(Y.calcite[,1],calcite.predict.lm,col='blue')
points(Y.calcite[,1],sort(calcite.predict.km$mean),col='red',lty=2)

# Erreur de prédiction
err.lm <- mean((Y.calcite[,1]-calcite.predict.lm)^2)/var(Y.calcite[,1])
err.km <- mean((Y.calcite[,1]-calcite.predict.km$mean)^2)/var(Y.calcite[,1])

print(err.lm) 
print(err.km)
