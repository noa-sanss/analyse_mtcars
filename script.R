cars <- mtcars

#Installation des packages -----------------------------------------------------
install.packages("corrplot")
install.packages("FactoMineR")
install.packages("explor")
install.packages("factoextra")

library(corrplot)

cars_numeric <- cars[, !colnames(cars) %in% "manufacturer"]
cars_numeric <- data.frame(lapply(cars_numeric, as.numeric))

M <- cor(cars_numeric)
print(round(M, 3))

corrplot(M, method="circle")
corrplot(M, method="pie")
corrplot(M, method="color")
corrplot(M, method="number")
corrplot(M, type="upper")
corrplot(M, type="lower")

# Corrélogramme avec réarrangement de type hclust
corrplot(M, type="upper", order="hclust")

# Utilisation de différents spectres de couleurs
col<- colorRampPalette(c("red", "white", "blue"))(20)
corrplot(M, type="upper", order="hclust", col=col)

# Changer la couleur de fond en lightblue
corrplot(M, type="upper", order="hclust", col=c("black", "white"),
         bg="lightblue")


# Combiner le corrélogramme avec le test de significativité --------------------

# mat : matrice de donnée

# ... : Arguments supplémentaire à passer à la fonction cor.test

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# Matrice de p-value de la corrélation
p.mat <- cor.mtest(cars_numeric)
head(p.mat[, 1:5])
print(round(p.mat,7))

# Indication des corrélations non significatives par une croix
corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01)

# Remplacement des corrélations non significatives par du blanc
corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank")



# Analyse des Composantes Principales ------------------------------------------

library(FactoMineR)
head(cars_numeric)

eigen(M)

summary(cars_numeric)

resultat=PCA(cars_numeric[,1:11], graph=FALSE)

library(explor)

#TOUT LES RESULTATS CST EXCELLENT
resultat=PCA(cars_numeric,graph=FALSE)
explor::explor(resultat)

resultat$eig
#Les 2 premières lignes comporte 84% de toutes l'informations
#permet de comprendre l'importance relative de chaque composante principale.
#Une grande valeur propre signifie que la composante explique une portion importante de la variance des données.

plot.PCA(resultat, choix = "var")
#+ l'angle entre 2 var ets petit plus la cor est proche de 1 
#+ la flècbe est proche du cercle + c'est de meilleure qualité 
#Quelles variables sont corrélées (proches dans le cercle)

plot.PCA(resultat, choix="ind")
#Plus ils sont proches et plus leurs “profils” de performance sont vraisemblablement similaires

resultat$var$coord

resultat$var$contrib
resultat$var$cor

# Classification Ascendante Hiérarchique ---------------------------------------

#DIm 1 :cyl (13.98%), disp (13.56%), et mpg (13.14%).
#Dim 2 : qsec (21.47%), gear (21.38%), et am (18.44%).

#Dim.1 : Caractéristiques de performance moteur (plus le score est élevé, plus le véhicule est puissant et consomme beaucoup).
#Dim.2 : Caractéristiques liées à l'accélération et au type de transmission

#Les individus situés à droite sur Dim.1 (comme 24) ont des moteurs puissants, de grande cylindrée, mais consomment beaucoup (faible mpg).
#Les individus à gauche sur Dim.1 (comme 19) ont une faible puissance moteur, une petite cylindrée, et une bonne consommation de carburant.
#Les individus situés en haut sur Dim.2 (comme 30) ont une accélération rapide (qsec élevé) et des transmissions manuelles (am).
#Les individus en bas sur Dim.2 (comme 14) sont plus lents en accélération et utilisent une transmission automatique (am faible).


#Lincoln Continental
#Cadillac Fleetwood
#Chrysler Imperial
#

coord_ind <- resultat$ind$coord

# Tracer un graphique vide avec les points
plot(
  coord_ind[, 1], coord_ind[, 2],
  type = "n",  # Ne dessine pas les points
  xlab = "Dim.1", ylab = "Dim.2",  # Labels des axes
  main = "Graphique des individus (ACP)"
)

# Ajouter les points
points(coord_ind[, 1], coord_ind[, 2], pch = 16, col = "red")

# Ajouter les noms des marques
text(
  coord_ind[, 1], coord_ind[, 2],
  labels = cars$manufacturer,
  cex = 0.8,
  pos = 3,  # Position relative (au-dessus des points)
  col = "blue"
)


# Calculer la matrice de distance (exemple avec la distance euclidienne)
distance_matrix <- dist(cars_numeric)
print(round(distance_matrix))

# Appliquer la CAH (méthode de Ward)
hc <- hclust(distance_matrix, method = "ward.D2")

# Visualiser le dendrogramme
plot(hc, labels = cars$manufacturer, main = "Dendrogramme de la CAH")

# Découper le dendrogramme pour obtenir un certain nombre de clusters
clusters <- cutree(hc, k = 3)  # Ici, on choisit 3 clusters
table(clusters, cars$manufacturer)

#Hauteur des branches : L'axe vertical du dendrogramme montre la distance ou la dissimilarité entre les groupes. Une hauteur plus élevée signifie que les clusters sont plus distants ou moins similaires.
#Branches : Chaque branche représente un regroupement. Plus il y a de branches qui se rejoignent à une faible hauteur, plus les observations au sein des groupes sont similaires.
#Couper le dendrogramme : Pour déterminer le nombre de clusters, tu choisis une hauteur à partir de laquelle couper les branches. Cela te donnera des groupes qui sont plus ou moins homogènes selon la similarité des observations.


library(factoextra)
head(mtcars)
pca2 <- PCA(mtcars, ncp = 2, graph = FALSE)
hcpc <- HCPC(pca2, graph = FALSE)

library(RColorBrewer)

fviz_dend(hcpc, cex = 0.8, palette = "Set2", rect = T, rect_fill = T, show.legend.text = TRUE, labelsize = 10)
fviz_cluster(hcpc, palette = "Set2", show.clust.cent = T, main = "Factor map", labelsizNULLfviz_cluster(hcpc, palette = "Set2", show.clust.cent = T, main = "Factor map", labelsize = 10))



fviz_pca_var(resultat, col.var = "cos2",
             axes = c(1, 2),
             gradient.cols = c("#259e93", "orange", "#b80808"),
             repel = TRUE # Évite le chevauchement de texte
) 

res <- get_pca_var(resultat)
corrplot(res$contrib, is.corr=FALSE)


