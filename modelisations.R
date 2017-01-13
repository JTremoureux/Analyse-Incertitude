# ##########################
# ====  MODELISATIONS  =====
# ##########################

## ---- REGRESSIONS LINEAIRES ---- 
par(mfrow=c(1,1))
full.lm1 <- lm(jdd_1$X.wgt_calcite ~ ., data=input_1_scaled)
full.lm2 <- lm(jdd_1$X.wgt_chlorite ~ ., data=input_1_scaled)

min.lm1 <- lm(jdd_1$X.wgt_calcite ~ 1, data=input_1_scaled)
min.lm2 <- lm(jdd_1$X.wgt_chlorite ~ 1, data=input_1_scaled)

step.lm1 <- step(min.lm1, scope = list(lower=min.lm1,upper=full.lm1), direction = "both")
step.lm2 <- step(min.lm2, scope = list(lower=min.lm1,upper=full.lm2), direction = "both")

selected.var1 <- all.vars(formula(step.lm1)[-2])
selected.var2 <- all.vars(formula(step.lm2)[-2])

selected.var <- unique(c(selected.var1, selected.var2))
colnames(input_1) %in% selected.var

# Les variables supprimées sont :
colnames(input_1)[!(colnames(input_1) %in% selected.var)]


## ---- RANDOMFOREST ---- 

rf_calcite <- randomForest(output_1$X.wgt_calcite ~ ., data=input_1_scaled)
imp_rf_calcite <- importance(rf_calcite)
varImpPlot(rf_calcite)

rf_chlorite <- randomForest(output_1$X.wgt_chlorite ~ ., data=input_1_scaled)
imp_rf_chlorite <- importance(rf_chlorite)
varImpPlot(rf_chlorite)

imp_rf <- data.frame(imp_rf_calcite + imp_rf_chlorite)
imp_rf <- mutate(imp_rf, var = factor(row.names(imp_rf))) %>%
  arrange(-IncNodePurity, var)

# Order levels for the geom_bar plot
imp_rf$var <- factor(imp_rf$var, levels = imp_rf$var[order(imp_rf$IncNodePurity)])

# Plot Var Importance
gg_imp_rf <- ggplot(data = imp_rf, aes(x = var, y = IncNodePurity, fill = IncNodePurity)) +
  geom_bar(stat = "identity") + ggtitle('Var Importance') +
  coord_flip()
gg_imp_rf

## ---- NEURAL NETWORK  ----

f <- as.formula(paste("output_1$X.wgt_calcite + output_1$X.wgt_chlorite ~", 
                      paste(c(taux_dissolution, taux_precipitation, surface_reaction), collapse = " + ")))

# Attention prend du temps (2min si rep=5)
nn <- neuralnet(f, data=input_1_scaled, hidden=7, err.fct="sse", rep=5)


# #################################
# ====  ALGORITHME DE MORRIS  =====
# #################################

get_morris <- function(x) {
  # Fonctions pour avoir les différentes sorties de l'algo de morris
  # .. en particulier les mu, mu* et sigma permettant de faire le plot
  # .. Inspiré du code donné dans le help de sensitivity
  mu <- apply(x$ee, 2, mean)
  mu.star <- apply(x$ee, 2, function(x) mean(abs(x)))
  sigma <- apply(x$ee, 2, sd)
  var <- x$factors
  
  data.frame(var, mu, mu.star, sigma, row.names = NULL)
}

## ---- MODEL FUNCTIONS ----

# -> On veut créer des fonctions que l'on va passer dans le paramètre 'model"
# .. lors des appels à la fonction morris

## ---- RF : Random Forest ----
mod_RF <- function(X){
  res_calcite <- predict(rf_calcite, X)
  res_chlorite <- predict(rf_chlorite, X)
  res_matrix <- cbind(res_calcite, res_chlorite)
  res_matrix
}
# r = 100, levels = 20, grid.jump = 6
morris_RF <- morris(model = mod_RF, factors = colnames(input_1), r = 100, design = list(type = "oat", levels = 12, grid.jump = 4))
plot(morris_RF)

df_RF <- get_morris(morris_RF)
gg_RF <- ggplot(df_RF, aes(x = mu.star, y = sigma, label=var)) +
  geom_point(color = 'red') +
  geom_text(aes(label = var))
ggplotly(gg_RF)

# Variables que l'on jette suivant le morris
RF_deleted <- filter(df_RF, mu.star < 0.0015, sigma < 0.0015)$var
RF_selected <- filter(df_RF, !(var %in% RF_deleted))$var

intersect(RF_selected, colnames(input_2))

# REMARQUE:
# LOOK AT : ggplotly(gg_RF)
# LOOK AT : gg_imp_rf
# -> Les 9 variables sélectionnées par morris, correspondent aux 9 variables les
# .. plus importantes, selon le graphe d'importance donné par le randomForest.
# .. C'est même les 9 seules que l'on aurait sélectionnés. Sauf peut-être la
# .. Kppt_Kaolinite. Mais c'est aussi celle qui se détache un peu des variables
# .. non-influentes selon morris_RF. COOL !

## ---- NN : Neural Network ----
mod_NN <- function(X){
  # Attention, compute de neuralnet est caché par dplyr
  neuralnet::compute(nn, X)$net.result
}

morris_NN <- morris(model = mod_NN, factors = colnames(input_1), r = 100, design = list(type = "oat", levels = 12, grid.jump = 4))
plot(morris_NN)

df_NN <- get_morris(morris_NN)
gg_NN <- ggplot(df_NN, aes(x = mu.star, y = sigma, label=var)) +
  geom_point(color = 'red') +
  geom_text(aes(label = var))
ggplotly(gg_NN)

# Variables que l'on jette suivant ce 
NN_deleted <- filter(df_NN, mu.star < 0.0015, sigma < 0.0015)$var
NN_selected <- filter(df_NN, !(var %in% NN_deleted))$var

intersect(NN_selected, colnames(input_2))

## ---- LM : LINEAR MODEL /!\ ERROR /!\---- 

mod_LM <- function(X){
  res_calcite <- predict(full.lm1, X)
  res_chlorite <- predict(full.lm2, X)
  res_matrix <- cbind(res_calcite, res_chlorite)
  data.frame(res_matrix)
}

# morris_LM <- morris(model = mod_LM, factors = colnames(input_1), r = 100, design = list(type = "oat", levels = 12, grid.jump = 4))
# # /!\ Ne marche pas, problème de format data.frame. tant pis
# plot(morris_LM)
# 
# df_LM <- get_morris(morris_LM)
# gg_LM <- ggplot(df_LM, aes(x = mu.star, y = sigma, label=var)) +
#   geom_point(color = 'red') +
#   geom_text(aes(label = var))
# ggplotly(gg_LM)
# 
# # Variables que l'on jette suivant ce 
# LM_deleted <- filter(df_LM, mu.star < 0.002, sigma < 0.002)$var
# LM_selected <- filter(df_LM, !(var %in% LM_deleted))$var
# 
# intersect(LM_selected, colnames(input_2))



# #################################
# ====  EFFETS D'INTERACTION  =====
# #################################

input_1_scalected <- input_1_scaled[,colnames(input_1_scaled) %in% RF_selected]
input_2_scalected <- input_2_scaled[,colnames(input_2_scaled) %in% RF_selected]

formula_calcite <- as.formula(paste("output_1$X.wgt_calcite ~", 
                                    paste(RF_selected, collapse = " + ")))
formula_chlorite <- as.formula(paste("output_1$X.wgt_chlorite ~", 
                                     paste(RF_selected, collapse = " + ")))
formula_2 <- as.formula(paste("output_1$X.wgt_calcite + output_1$X.wgt_chlorite ~", 
                              paste(RF_selected, collapse = " + ")))
formula_interac_calcite <- as.formula("output_1$X.wgt_calcite ~ .^2")
formula_interac_chlorite <- as.formula("output_1$X.wgt_chlorite ~ .^2")
formula_interac_2 <- as.formula("output_1$X.wgt_calcite + output_1$X.wgt_chlorite ~ .^2")

# Objectifs:
# Parmis les variables sélectionnées par morris_RF, on a peut-être identifié des
# .. effets linéaires (Kine_Illite, K2diss_ILlite?). Sinon, les autres variables
# .. sont soit des effets non-linéaires, soit des intéractions. On souhaite en 
# .. savoir plus sur ces intéractions.

# ---- ANOVA ----

anova <- aov(formula_interac_2, data=input_1_scalected)
# plot(anova)
summary_anova <- summary(anova)[[1]]
summary_anova <- summary_anova %>% mutate(var = row.names(summary_anova))

colnames(summary_anova) <- c("DF", "SumSq", "MeanSq", "FValue", "PValue", "var")
arrange(summary_anova, PValue, DF, SumSq, MeanSq, FValue, var)

# INTERPRETATION : 
# -> L'intéracton K2diss_Illite:Kine_Illite, a plus de variance expliquée que K2diss_Illite.
# -> En fait on va plutôt utiliser les indices de Sobol pour analyser les intéractions.

# Interprétation:
# -> A comparer avec gg_imp_rf, on a la même distribution.

# ---- KM METAMODELE ----

mod_KM_calcite <- km(formula = ~1, design = data.frame(input_2_scalected), response = output_2$X.wgt_calcite)
mod_KM_chlorite <- km(formula = ~1, design = data.frame(input_2_scalected), response = output_2$X.wgt_chlorite)


# ---- SOBOL ----
# Inspiré de https://hal.archives-ouvertes.fr/hal-00795229/document

# 5min : n = 3000
sobol_km_calcite <- fast99(model = kmPredictWrapper,, factors = 9,
                           n = 3000, q = "qunif", q.arg = list(min = 0, max = 1), km.object = mod_KM_calcite)
sobol_km_calcite
sobol_km_calcite2 <- sobol_km_calcite
colnames(sobol_km_calcite2$X) <- RF_selected
plot(sobol_km_calcite2)

fanova_calcite <- estimateGraph(f.mat = kmPredictWrapper, d = 9, n.tot = 300,
                                q.arg = list(min = 0, max = 1), km.object = mod_KM_calcite)

plot(fanova_calcite, plot.sobol_km_calcite=TRUE)

# 5min n = 3000
sobol_km_chlorite <- fast99(model = kmPredictWrapper,, factors = 9,
                            n = 3000, q = "qunif", q.arg = list(min = 0, max = 1), km.object = mod_KM_chlorite)
sobol_km_chlorite
sobol_km_chlorite2 <- sobol_km_chlorite
colnames(sobol_km_chlorite2$X) <- RF_selected
plot(sobol_km_chlorite2)


fanova_chlorite <- estimateGraph(f.mat = kmPredictWrapper, d = 9, n.tot = 300,
                                 q.arg = list(min = 0, max = 1), km.object = mod_KM_chlorite)

plot(fanova_chlorite, plot.sobol_km_chlorite=TRUE)
