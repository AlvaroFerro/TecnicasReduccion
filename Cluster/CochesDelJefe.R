##############################################
############ Análisis Cluster ################
##############################################


############ Carga de librerias ##############
library(memisc)
library(haven)
library(foreign)
library(dplyr)
library(factoextra)
library(cluster)
library(factoextra)
require(clustertend)
library("NbClust")
library(FactoMineR)


ruta <- 'tterreno.sav'

coches <- read.spss(ruta, to.data.frame = T)
coches <- data.frame(coches[,-1], row.names = make.names(coches[,1], unique = T))

coches_sin_escalar = read.spss(ruta, to.data.frame = T)
coches_sin_escalar <- data.frame(coches_sin_escalar[,-1], row.names = make.names(coches_sin_escalar[,1], unique = T))


summary(coches)


View(coches)

coches[116, 11] <- mean(coches[c(119, 120, 118, 121, 117), 11])
coches[116, 11]
coches[c(75:79), 11] <- mean(coches[c(63:74), 11])
coches[19, 11] <- mean(coches[c(13:18, 19:22), 11])
coches[c(105, 106), 12] <- 144
coches[114, 12] <- 135


coches_sin_escalar[116, 11] <- mean(coches_sin_escalar[c(119, 120, 118, 121, 117), 11])
coches_sin_escalar[116, 11]
coches_sin_escalar[c(75:79), 11] <- mean(coches_sin_escalar[c(63:74), 11])
coches_sin_escalar[19, 11] <- mean(coches_sin_escalar[c(13:18, 19:22), 11])
coches_sin_escalar[c(105, 106), 12] <- 144
coches_sin_escalar[114, 12] <- 135

coches_sin_escalar[c(7, 8), 3] <- mean(coches_sin_escalar[c(6, 9:12), 3])
coches_sin_escalar[c(7, 8), 3]
coches_sin_escalar[19, 4] <- mean(coches_sin_escalar[c(13:18, 20:22), 4])

anyNA(coches_sin_escalar)

perfomScaling <-  T
if(perfomScaling){
  for(i in names(coches)){
    if(class(coches[,i ]) == 'integer' | class(coches[,i ]) == 'numeric'){
      coches[,i ] = scale(coches[,i ])
    }
  }
}

#Creamos un nuevo dataframe, con las columnas buenas.
columnasnum <- c('potencia','rpm','peso','consurb','velocida')
cnum <- c('potencia','rpm','peso','consurb','velocida', 'cons120')
cochesescalados <- subset(coches, select = columnasnum)
cochesescalados2 <- subset(coches, select = cnum)

coches_sub_sin_escalar = subset(coches_sin_escalar, select = columnasnum)
sum(is.na(coches_sub_sin_escalar))

coches_sub_sin_escalar[c(7, 8), 3] = 1850
coches_sub_sin_escalar[19, 4] <- mean(coches_sub_sin_escalar[c(13:18, 20:22), 4])
anyNA(coches_sub_sin_escalar)
#Peso
cochesescalados[c(7, 8), 3] <- mean(cochesescalados[c(6, 9:12), 3])
cochesescalados[c(7, 8), 3]
cochesescalados[19, 4] <- mean(cochesescalados[c(13:18, 20:22), 4])

anyNA(cochesescalados)


#Obtenemos las distancias del anterior DF a través de Pearson
qdist <- get_dist(cochesescalados, stand = T, method = 'pearson')
qdist.manhattan <- get_dist(cochesescalados, stand = T, method = 'manhattan')
qdist.mink <- get_dist(cochesescalados, stand = T, method = 'minkowski')
str(qdist.pearson)

dist.cor <- as.dist(1 - cochescorr)
round(as.matrix(dist.cor),  2)
dist.cor <- as.dist(1 - cochescorr)
round(as.matrix(dist.cor),  2)

#Realizamos la representación gráfica.
fviz_dist(qdist, lab_size = 5)
as.matrix(as.dist(qdist))

#Cambiamos la representación 
fviz_dist(qdist,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), lab_size = 5)

d <- dist(cochesescalados, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit)

plot(fit, cex = 0.6, hang = -1, main="Dendrograma - hclust")
rect.hclust(fit, k=5, border = 2:4)



groups <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")

#Ahora analizamos la correlación entre las variables
cochescorr <- cor(cochesescalados, method = 'pearson')
round(cochescorr,3)

#Y ahora la convertimos en la matriz de distancias
dist.cor <- as.dist(1 - cochescorr)
round(as.matrix(dist.cor),  2)

#Otra forma de obtención de las distancias
daisy(cochescorr, metric = c('manhattan'), stand = F)##Stand lo que hace es standarizar las variables.

#Agrupación
coches.eclust <- eclust(cochesescalados, FUNcluster = 'kmeans', stand = T, hc_metric = 'euclidean',  nstart = 25)

coches.eclust.j = eclust(cochesescalados[,-1], "hclust", k = 4)
fviz_cluster(coches.eclust.j)
fviz_silhouette(coches.eclust.j)


fviz_nbclust(cochesescalados, kmeans, method = "silhouette") +
  ggtitle("Número óptimo de clusters - k medias") +
  labs(x="Número k de clusters",y="Anchura del perfil promedio")

k2 <- kmeans(cochesescalados, centers = 2, nstart = 25)
k3 <- kmeans(cochesescalados, centers = 3, nstart = 25)
k4 <- kmeans(cochesescalados, centers = 4, nstart = 25)
k5 <- kmeans(cochesescalados, centers = 5, nstart = 25)
k6 <- kmeans(cochesescalados, centers = 6, nstart = 25)
k7 <- kmeans(cochesescalados, centers = 7, nstart = 25)
k8 <- kmeans(cochesescalados, centers = 8, nstart = 25)

p1 <- fviz_cluster(k2, geom = 'point', data = cochesescalados) + ggtitle('K = 2')
p2 <- fviz_cluster(k3, geom = 'point', data = cochesescalados) + ggtitle('K = 3')
p3 <- fviz_cluster(k4, geom = 'point', data = cochesescalados) + ggtitle('K = 4')
p4 <- fviz_cluster(k5, geom = 'point', data = cochesescalados) + ggtitle('K = 5')
p5 <- fviz_cluster(k6, geom = 'point', data = cochesescalados) + ggtitle('K = 6')
p6 <- fviz_cluster(k7, geom = 'point', data = cochesescalados) + ggtitle('K = 7')
p7 <- fviz_cluster(k8, geom = 'point', data = cochesescalados) + ggtitle('K = 8')


library(gridExtra)
require(ggrepel)
grid.arrange(p1, p2, p3, p4, p5, nrow = 2)


##Realizamos este plot, para observar cual es el número óptimo de clusteres
fviz_nbclust(x = cochesescalados, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(cochesescalados, method = "euclidean"), nstart = 50)

set.seed(123)

km_clusters <- kmeans(x = cochesescalados, centers = 6, nstart = 25)

fviz_cluster(object = km_clusters, data = cochesescalados, show.clust.cent = TRUE,
             ellipse.type = "convex", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")


###### Algoritmo PAM #######
pam.q = pam(coches_sub_sin_escalar, 6)

pam.q$silinfo$clus.avg.widths
pam.q$silinfo$avg.width

pam.q$id.med
pam.q$clustering

pam.q$objective
pam.q$isolation

pam.q$diss

clusplot(pam.q, main = "PAM de k = 6", color = TRUE)






fviz_dend(hclust(dist(cochesescalados)), k = 4, cex = 0.5, main = "Dendrograma")


coches.eclust = eclust(cochesescalados, FUNcluster = "pam", stand = TRUE,
                       hc_metric = "euclidean", k = 6)
cochesescalados2[c(1:3), 6] <- 12.6
cochesescalados2[19, 6] <- 11.5
coches.eclust2 = eclust(cochesescalados2, FUNcluster = "kmeans", stand = TRUE,
                       hc_metric = "euclidean", nstart = 25, k = 6)

coches.eclust$cluster == 1
coches.eclust


mean()


fviz_nbclust(cochesescalados[, -1], kmeans, method = "silhouette") +
  ggtitle("Número óptimo de clusters - k medias") +
  labs(x = "Número k de clusters",y = "Anchura del perfil promedio")


fviz_nbclust(cochesescalados, hcut, method = "silhouette", hc_method = "complete") +
  ggtitle("Número óptimo de clusters - jerárquico") +
  labs(x = "Número k de clusters", y = "Anchura del perfil promedio")



# Aplicamos el estadístico sobre los datos reales
set.seed(123)
# (prueba es el objeto que contiene los datos reales de EBITDA y Book Value)
hopkins(cochesescalados, n= nrow(cochesescalados)-1)

bondad_ac <- get_clust_tendency(cochesescalados, nrow(cochesescalados)-1)
bondad_ac$hopkins_stat
bondad_ac$plot +
  scale_fill_gradient(low = "steelblue", high = "white")


set.seed(123)
k.max = 15 # Máximo número de clusters
# lo aplicamos sobre sec.def, ya tipificado
wss= sapply(1:k.max,
            function(k){kmeans(cochesescalados[, -1], k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Número K de clusters",
     ylab="Suma total de cuadrados intra-clusters")
abline(v = 3, lty =2)
abline(v = 4, lty =3)

fviz_nbclust(cochesescalados, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  geom_vline(xintercept = 4, linetype = 3) +
  ggtitle("Número óptimo de clusters - k medias") +
  labs(x="Número k de clusters", y ="Suma total de cuadrados intra grupos")


caracteristicas <- kmeans(cochesescalados, 6)
sum(cadamarcacorrespondeauncluster$cluster == 6)
sum(cadamarcacorrespondeauncluster$cluster == 5)
sum(cadamarcacorrespondeauncluster$cluster == 4)
sum(cadamarcacorrespondeauncluster$cluster == 3)
sum(cadamarcacorrespondeauncluster$cluster == 2)
sum(cadamarcacorrespondeauncluster$cluster == 1)

caracteristicas$centers
cadamarcacorrespondeauncluster$totss
cadamarcacorrespondeauncluster$withinss
cadamarcacorrespondeauncluster$tot.withinss
cadamarcacorrespondeauncluster



set.seed(123)
clus.nb = NbClust(cochesescalados, distance = "euclidean",
                  min.nc = 6, max.nc = 10,
                  method = "complete", index ="gap")
clus.nb # resultados
clus.nb$Best.partition
clus.nb$Best.nc
clus.nb$All.index
clus.nb$All.CriticalValues



################ A traves del PAM ##################
library(cluster)
library(factoextra)
fviz_nbclust(x = cochesescalados, FUNcluster = pam, method = "wss", k.max = 15,
             diss = dist(cochesescalados, method = "manhattan"))

set.seed(123)
pam_clusters <- pam(x = cochesescalados, k = 6, metric = "manhattan")
pam_clusters$medoids

km_clusters <- kmeans(cochesescalados, 6)
km_clusters$cluster
km_clusters$centers

fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

medoids <- prcomp(cochesescalados)$x

# Se seleccionan únicamente las proyecciones de las observaciones que son medoids
medoids <- medoids[rownames(pam_clusters$medoids), c("PC1", "PC2")]
medoids <- as.data.frame(medoids)

# Se emplean los mismos nombres que en el objeto ggplot
colnames(medoids) <- c("x", "y")

# Creación del gráfico
fviz_cluster(object = pam_clusters, data = cochesescalados, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  # Se resaltan las observaciones que actúan como medoids
  geom_point(data = medoids, color = "firebrick", size = 2) +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

pam_clusters$silinfo$widths
pam_clusters$clusinfo
