#library(ggplot2)
#ilbrary(car)
#library(dplyr)
library(FactoMineR)
library(factoextra)
library(cluster)
#library(simstudy)
#library(data.table)
library(tidyverse)
#library(PerformanceAnalytics)
library(corrr)

setwd("F:/OneDrive/Doctorado/Docencia/UV/Ejemplos")


data <- iris

data%>%summary


#### PCA ####

pca <- PCA(data%>%select(-Species), ncp=3, graph=FALSE )

plot(pca, choix = "ind")
plot(pca, choix = "var")
plot(pca, choix = "varcor")

barplot(pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(pca$eig))
barplot(pca$eig[,2],main="% Variance",names.arg=1:nrow(pca$eig))
barplot(pca$eig[,3],main="% Cum. Variance",names.arg=1:nrow(pca$eig))

summary(pca)

fviz_pca_var(pca, repel = TRUE)


fviz_pca_var(pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)


#### K-MEDIAS ####



 
pairs(data[,1:4], col = data$Species,lower.panel = NULL)
par(xpd = TRUE)
legend(x = 0.05, y = 0.4, cex = 2,
       legend=as.character(levels(data$Species)),
       fill = unique(data$Species))



k_medias <-kmeans(data[,1:4], 3, nstart = 10)
k_medias

k_medias$centers

k_medias$withinss

table(data$Species, k_medias$cluster)

# Hemos cogído el número óptimo de cluster?

fviz_nbclust(x = data[,1:4]%>%scale, FUNcluster = kmeans, 
             method = "wss", k.max = 15,  nstart = 50)



k_medias_2 <-kmeans(data[,1:4], 5, nstart = 10)
k_medias_2

k_medias_2$centers

k_medias_2$withinss

table(data$Species, k_medias_2$cluster)

