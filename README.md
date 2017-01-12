## Projet Analyse Incertitude


# Contexte industriel
Le dioxyde de carbone (CO2) issu de l'exploitation des énergies fossiles est le principal responsable du réchauffement climatique. Pour réduire ses émissions dans l'atmosphère, des solutions de stockage de CO2 sont étudiées. Le principe de stockage consiste à capter le CO2, puis de l'enfouir, via un puits, dans une formation géologique profonde du sous-sol possédant de bonnes propriétés de perméabilité.
Toutefois, l’étanchéité d’un tel réservoir au cours du temps peut être compromise par des réactions chimiques qui s'opèrent entre le CO2 et les composantes chimiques du lieu de stockage. A terme, ces réactions chimiques peuvent fragiliser les formations souterraines adjacentes au réservoir et compromettre ses propriétés de confinement.

# Cas d’étude
Dans le but de comprendre ces phénomènes, des expériences sont menées en laboratoire pour analyser l’évolution des espèces chimiques au sein du réservoir. Pour cela, une solution aqueuse synthétique, une poudre minérale et du CO2 sous forme gazeuse sont introduits dans un réacteur isotherme, et soumis à des conditions spécifiques de température et de pressions.
En revanche, les réactions de dissolution, de précipitations et les surfaces de contact entre les espèces étant largement méconnues, on cherche, avant de lancer ces expériences, à mieux connaître le comportement d’un tel système. Pour cela, on utilise un code numérique modélisant ces réactions, que l’on se propose d’étudier à l’aide des outils et méthodes acquis au cours du module.
Ce code prend comme paramètres d’entrée taux de dissolution et de précipitation en mol.m-2.s-1 et surface de réaction en m2.g-1 des espèces suivantes :
* anhydrite,
* calcite,
* chlorite,
* dolomite,
* gibbsite,
* illite,
* kaolinite,
* microline,
* quartz,
* smectite.

En particulier on cherche à caractériser le comportement global de deux sorties représentatives de l’état final du système (concentration en chlorite et concentration en calcite) en fonction des paramètres initiaux de l’expérience simulée.
Pour cela, on dispose de deux jeux de données fournies par le département métier développant ce modèle. Le premier, nommé jdd_1.csv, contient les entrées et sorties pour un plan d’expériences numériques de taille 𝑛1=3000 points destiné à la méthode de Morris. Le second, nommé jdd_2.csv, de taille 𝑛2=3000 points également, contient données issues d’un plan d’expériences LHS pour un sous-ensemble des grandeurs d’entrée du modèle et pour le même couple de sortie.
