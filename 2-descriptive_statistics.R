# ###########################
# = STATISTIQUE DESCRIPTIVE =
# ###########################

# Dans ce fichier, nous allons analyser les différentes variables présentes dans les deux jeux de données. Enfin,
# une observation des variables est faite à l'aide de boxplots ou encore d'histogrammes.

source('1-set_up.R', encoding = 'UTF-8')

## ---- Nom des Variables ----

sorties <- colnames(jdd_1)[startsWith(colnames(jdd_1), "X.wgt")] # 2 sorties

taux_dissolution <- colnames(jdd_1)[startsWith(colnames(jdd_1), "K2diss")] # 10 vars
taux_precipitation <- colnames(jdd_1)[startsWith(colnames(jdd_1), "Kppt")] # 9 vars
surface_reaction <- colnames(jdd_1)[startsWith(colnames(jdd_1), "Kine")] # 10 vars

taux_dissolution2 <- colnames(jdd_2)[startsWith(colnames(jdd_2), "K2diss")] # 6 var (-4)
taux_precipitation2 <- colnames(jdd_2)[startsWith(colnames(jdd_2), "Kppt")] # 4 vars (-5)
surface_reaction2 <- colnames(jdd_2)[startsWith(colnames(jdd_2), "Kine")] # 9 vars (-1)

# Quels sont les 4 taux de dissolutions qui "disparaissent" dans jdd_2 ?
taux_dissolution[!(taux_dissolution %in% taux_dissolution2)]
# > "K2diss_Gibbsite"  "K2diss_Kaolinite" "K2diss_Quartz"    "K2diss_Smectite"

# Quels sont les 5 taux de précipitation qui "disparaissent" dans jdd_2 ?
taux_precipitation[!(taux_precipitation %in% taux_precipitation2)]
# > "Kppt_Anhydrite" "Kppt_Chlorite"  "Kppt_Dolomite"  "Kppt_Illite"    "Kppt_Quartz"

# Quelle surface de réaction disparait dans jdd_2 ?
surface_reaction[!(surface_reaction %in% surface_reaction2)]
# > "Kine_Quartz"

## ---- Scale des variables ----

maxs <- apply(input_1, 2, max)
mins <- apply(input_1, 2, min)
input_1_scaled <- as.data.frame(scale(input_1, center = mins, scale = maxs-mins))

# #####################################
# ====  Observations des données  =====
# #####################################

## ---- BOXPLOTS -----
# -> Cela permet de voir rapidement les plages dans lesquelles vivent chaque variable,
# .. et leur répartition dans cette plage. 

# TAUX DE DISSOLUTIONS en mol.m-2.s-1
par(mfrow=c(3,4))
for(i in 1:10) {
  boxplot(jdd_1[,taux_dissolution[i]])
  title(taux_dissolution[i])
}

# TAUX DE PRECIPITATION en mol.m-2.s-1
par(mfrow=c(3,3))
for(i in 1:9) {
  boxplot(jdd_1[,taux_precipitation[i]])
  title(taux_precipitation[i])
}

# SURFACE DE REACTIONS en m2.g-1
par(mfrow=c(3,4))
for(i in 1:10) {
  boxplot(jdd_1[,surface_reaction[i]])
  title(surface_reaction[i])
}

# CONCENTRATIONS DE SORTIE
par(mfrow=c(1,2))
for(i in 1:2) {
  boxplot(jdd_1[,sorties[i]])
  title(sorties[i])
}

# INTERPRETATION
# -> Les variables d'entrées sont plutôt bien réparties. Il aurait été surprenant de
# .. voir des valeurs aberrantes.
# -> Dans la sortie calcite, on a de nombreuses variables extrèmes 'outliers' 
# .. R : length(boxplot(jdd_1[,sorties[1]])$out) # > (675/3000)!
# .. a quoi correspondent ces outliers ? Une variable d'entrée qui
# .. l'influencerais beaucoup ?

## ---- HISTOGRAMMES -----

# TAUX DE DISSOLUTIONS en mol.m-2.s-1
par(mfrow=c(3,4))
for(i in 1:10) {
  hist(jdd_1[,taux_dissolution[i]])
}

# TAUX DE PRECIPITATION en mol.m-2.s-1
par(mfrow=c(3,3))
for(i in 1:9) {
  hist(jdd_1[,taux_precipitation[i]])
}

# SURFACE DE REACTIONS en m2.g-1
par(mfrow=c(3,4))
for(i in 1:10) {
  hist(jdd_1[,surface_reaction[i]])
}

# SURFACE DE REACTIONS en m2.g-1
par(mfrow=c(1,2))
for(i in 1:2) {
  hist(jdd_1[,sorties[i]])
}

# INTERPRETATION
# -> Cela n'apporte pas grand chose de plus que les boxplots, mais confirme.
# -> On voit les distributions plutôt "uniformes" des variables d'entrées, 
# .. ce qui correspondrai à une sorte de plan d'expérience.
# -> Pour les variables de sorties, on voit que la chlorite (à gauche) a une distribution, 
# .. plutôt uniforme. Alors que la calcite, ressemble à une Gaussienne, centrée en 0.075
# .. environ. Avec des 'valeurs aberrantes' car les queues remontent. Cela correspond
# .. aux outliers de nos boxplots.