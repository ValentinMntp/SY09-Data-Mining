#Exercice 1 Visualisation des données
library(MASS)
data(iris)
iris_quant <- iris[-5]
acp_iris <- princomp(iris_quant)

png(file = "img/plot_no_colors_iris.png")
plot(acp_iris$scores, main="Représentation des individus sans discrimination", pch=4, asp=1)
abline(h=0)
abline(v=0)
dev.off()

png(file = "img/plot_colors_iris.png")
plot(acp_iris$scores, main="Représentation des individus discriminés par l'espèce", pch=4, col=iris$Species, asp=1)
abline(h=0)
abline(v=0)
dev.off()

#Crabs
crabs2 <- read.csv("crabs2.csv", header=T)
acp_crabs<-princomp(crabs2[,1:4])


#plot dans le premier plan factoriel
png(file = "rapport/img/plot_no_colors_crabs.png")
plot(acp_crabs$scores, main = "Représentation des individus dans le premier plan factoriel",pch=4,asp=1)
abline(h=0)
abline(v=0)
dev.off()

#plot dans le premier plan factoriel en fonction d'espece
png(file = "img/plot_color_species_crabs.png")
plot(acp_crabs$scores,col=c("green","blue")[crabs2$sp],main = "Discrimination par espèce",pch=4,asp=1)
abline(h=0)
abline(v=0)
dev.off()

#plot dans le premier plan factoriel en fonction de sexe
png(file = "img/plot_color_sex_crabs.png")
plot(acp_crabs$scores,col=c("green","blue")[crabs2$sex],main = "Discrimination par sexe",pch=4,asp=1)
abline(h=0)
abline(v=0)
dev.off()


mut <- read.csv("mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut, diag=T, upper=T)
mut_aftd <- cmdscale(mut, k=2)
png(file = "img/plot_shepard_2.png")
s1 = Shepard(as.dist(mut), mut_aftd)
plot(s1, main = "diagramme de Shepard de Mutations (k = 2)",col=c("blue"), pch =4, asp=1)
abline(0, 1)
dev.off();

mut_aftd <- cmdscale(mut, k=3)
png(file = "img/plot_shepard_3.png")
s1 = Shepard(as.dist(mut), mut_aftd)
plot(s1, main = "diagramme de Shepard de Mutations (k = 3)",col=c("blue"), pch =4, asp=1)
abline(0, 1)
dev.off();

mut_aftd <- cmdscale(mut, k=4)
png(file = "img/plot_shepard_4.png")
s1 = Shepard(as.dist(mut), mut_aftd)
plot(s1, main = "diagramme de Shepard de Mutations (k = 4)",col=c("blue"), pch =4, asp=1)
abline(0, 1)
dev.off();

mut_aftd <- cmdscale(mut, k=5)
png(file = "img/plot_shepard_5.png")
s1 = Shepard(as.dist(mut), mut_aftd)
plot(s1, main = "diagramme de Shepard de Mutations (k = 5)",col=c("blue"), pch =4, asp=1)
abline(0, 1)
dev.off()

png(file = "img/dendrogramme_complete.png")
plot(hclust(mut, method="complete"), main="Dendrogramme de Mutations avec méthode 'complete'")
dev.off()

png(file = "img/dendrogramme_wardD2.png")
plot(hclust(mut, method="ward.D2"), main="Dendrogramme de Mutations avec méthode 'ward.D2'")
dev.off()

png(file = "img/dendrogramme_single.png")
plot(hclust(mut, method="single"), main="Dendrogramme de Mutations avec méthode 'single'")
dev.off()

png(file = "img/dendrogramme_median.png")
plot(hclust(mut, method="median"), main="Dendrogramme de Mutations avec méthode 'median'")
dev.off()

png(file = "img/dendrogramme_mutations_3.png")
plot(hclust(mut, method="ward.D2"), main="Dendrogramme de Mutations avec méthode 'ward.D2'")
rect.hclust(hclust(mut, method="ward.D2"), 3, border="green3")
dev.off()

png(file = "img/dendrogramme_ward_iris.png")
plot(hclust(dist_iris, method="ward.D2"), main="classification hiérarchique ascendante des données Iris")
rect.hclust(hclust(dist_iris, method="ward.D2"), 3, border=c("blue","red","green"))
dev.off()

png(file = "img/dendrogramme_diana_iris.png")
plot(diana(dist_iris, metric="euclidean"), main="classification hiérarchique descendante des données Iris")
rect.hclust(diana(dist_iris, metric="euclidian"), 3, border=c("blue","red","green"))
dev.off()