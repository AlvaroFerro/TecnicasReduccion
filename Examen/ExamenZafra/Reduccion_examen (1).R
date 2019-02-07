
####### Paquetes ##########

library(dplyr)
library(dummies)
library(factoextra)
library(FactoMineR)
library(psych)
require(corrplot)
library(missMDA)
library(cluster)
library(gridExtra)
require(NbClust)
library(PerformanceAnalytics)

################################################################# TAREAS ################################################################# 

################ 1) Parseado #########################
#########################

datos <- read.csv('datos/EGT2010_2017.csv') # carga de datos

str(datos) # comprobar carga



################ 2) Tareas #########################
#########################

# seleccion variables enunciado

datos_sel <- datos[, c(81:93, 3, 4, 6, 21, 72, 112, 110, 109, 80, 151, 146)] 

# eliminar NAs conforme enunciado

datos_sel_clean <- na.omit(datos_sel) 

# vision global de los datos

summary(datos_sel_clean) 

# dividimos el dataset conforme al enunciado. df.1 obs de 2010 a 2014 y df.2 de 2015 a 2017.

df.1 <- subset(datos_sel_clean,datos_sel_clean$AÑO <= 2014)
df.2 <- subset(datos_sel_clean,datos_sel_clean$AÑO > 2014)

# asignamos semilla
set.seed(1234)

# Seleccionamos 150 observaciones de cada muestra.

df.1_sample <- df.1[sample(nrow(df.1), 150),]
df.2_sample <- df.2[sample(nrow(df.2), 150),]

# Eliminamos la variables creadas anteriormente excepto las dos ultimas muestras.

rm(datos,datos_sel, datos_sel_clean,df.1,df.2)

################################################################# EXAMEN ################################################################# 

################ 3) Tratamiento de variables #########################
#########################

muestra <- rbind(df.1_sample,df.2_sample)

str(muestra)

# Index

rownames(muestra) <- make.names(muestra$ID)
muestra <- muestra[,-14]

# colnames AÑO como YEAR

colnames(muestra)[23] <- "YEAR"

# tratamiento de variables

## Ingresos tratamiento 1. Convertimos la variable a tipo numérica.

ingresos <- dummy(muestra$INGRESOS)

i1=ingresos[,1]*((12000+24000)/2)
i2=ingresos[,2]*((24000 + 36000)/2)
i3=ingresos[,3]*((36001 +48000)/2)
i4=ingresos[,4]*((48001 + 60000)/2)
i5=ingresos[,5]*((60001+72000)/2)
i6=ingresos[,6]*((72001+84000)/2)
i7=ingresos[,7]*((84000+12000)/2)

ingresos <- (i1+i2+i3+i4+i5+i6+i7)

head(ingresos)

muestra$INGRESOS <- as.integer(ingresos)

summary(muestra$INGRESOS)

## Variables con alta correlación que pueden ocasionar problemas de multicolinealidad que ni su puta madre.

muestra <- muestra %>%
  mutate(val_alojamiento = (rowSums(.[1:3])/3)) %>%
  mutate(val_entorno = (rowSums(.[4:9])/6)) %>%
  mutate(val_restauracion = (rowSums(.[10:13])/4)) %>%
  select(c(AEROPUERTO:YEAR,val_alojamiento, val_entorno, val_restauracion)) %>% ## seleccionamos variables
  mutate(YEAR = as.factor(YEAR)) ## convertimos year a factor.


# Tipos de variables

## numéricas

var_con <- muestra %>% select_if(~!is.factor(.x)) # seleccionar si no es un puto factor.

str(var_con)

# factores

var_factor <-select_if(muestra, is.factor) # factores gordiiitsss

str(var_factor) # variables zafraroti

# escalado

# Al igual que en otros métodos estadísticos (PCA, ridge regression, lasso…), 
# la escala en la que se miden las variables y la magnitud de su varianza pueden afectar en gran medida a los resultados obtenidos por clustering.
# Si una variable tiene una escala mucho mayor que el resto, determinará en gran medida el valor de distancia/similitud
# obtenido al comparar las observaciones, dirigiendo así la agrupación final

var_con_sc <- scale(var_con,center = TRUE, scale = TRUE)


## Matriz de correlaciones


muestra_cor <- cor(var_con) # no escalado

muestra_cor_sc <- cor(var_con_sc) # escalado

### plots

Mmuestra_cor = as.matrix(muestra_cor)

Mmuestra_cor_sc = as.matrix(muestra_cor_sc)


#### Matriz de correlaciones clusterizada (2)
corrplot(Mmuestra_cor_sc, type="full", order="hclust", addrect = 2,
         tl.col="black", tl.cex=0.7, tl.srt=45) # ordenada conforme a los clusters 

#### mapa de calor
col<- colorRampPalette(c("red", "yellow", "blue"))(20)
heatmap(x = Mmuestra_cor_sc, col = col, symm = TRUE)

# variables numéricas

chart.Correlation(var_con, histogram=TRUE, pch=19)



################################################################# PCA ################################################################# 

# determinante
det(muestra_cor_sc)

# Obtenemos los resultados (resultado = 0.34). 
# El resultado es cercano a cero. Denota que  las correlaciones observadas anteriormente son significativas, 
# y que la aplicación de técnicas de reducción de la dimensión puede resultar efectiva
# para que una o más variables podrían expresarse como combinación lineal de otras.

## test de esfericidad de barlett

cortest.bartlett(muestra_cor, n = nrow(var_con)) # test de esfericidad de barlett.

# La prueba de esfericidad de Bartlett prueba la hipótesis de que la matriz de correlación es una matriz de identidad,
# lo que indicaría a su vez que las variables no están relacionadas y, por lo tanto, 
# no son adecuadas para la detección de estructuras subyacentes y la aplicación de técnicas de reducción de la dimensionalidad. 
# Los valores pequeños (habitualmente menos de 0,05) del nivel de significación (usualmente 0,95)
# indican que un análisis factorial puede ser útil con sus datos. 
# El valor-p es notablemente menor a 0,05, esto significa que podemos rechazar la hipótesis nula de que la varianza es la misma para todos los grupos 
# de tratamiento y aceptar la hipótesis alternativa de que al menos dos son diferentes. 
# Sin embargo, esta prueba requiere que la distribución normal multivariante de la población. 
# Esta asunción puede resultar arriesgada en nuestro caso. Es por ello que, a continuación,
# incluimos el test de Levene que es menos sensible a las desviaciones de la normalidad de nuestra muestra. 

## KMO

KMO(muestra_cor_sc)
## definicion de kmo, como se puede observar en el kmo todos con mayores de 0.7, por tanto 
##AnÃ¡lisis de componentes principales. Cuanto mÃ¡s cerca de 1 tenga el valor obtenido del test KMO, 
## implica que la relaciÃ³n entres las variables es alta. Si KMO â¥ 0.9


# PCA

muestra_acp = PCA(var_con, scale.unit = TRUE, ncp = ncol(muestra_cor), graph = TRUE)


# 2) autovalores

autoval<-round(muestra_acp$eig,2)

autoval

barplot(autoval[, 2], names.arg=1:nrow(autoval), 
        main = "Varianza explicada por los CCPP",
        xlab = "Componentes Principales",
        ylab = "Porcentaje explicado de la varianza",
        col ="chocolate3",
        ylim=c(0,105))
lines(x = 1:nrow(autoval), autoval[, 2], 
      type="b", pch=19, col = "black")
lines(x = 1:nrow(autoval), autoval[, 3], 
      type="o", pch=21, col = "blue", bg="steelblue")


# 3) coordenadas

coord<-round(muestra_acp$var$coord,2)

coord

# 4) comunalidades


cos2<-round(muestra_acp$var$cos2,2)

cos2

# Se utiliza como medida de la relación entre la variable y las dimensiones consideradas, en concreto, 
# la contribución de cada dimensión en la explicación de cada variable.  

# 5) Representacion variables pc en las dos dimensiones seleccionadas con graduacion de la suma de cos2 de cada variable

fviz_pca_var(muestra_acp, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()



# 6) contribuciones


contrib<-round(muestra_acp$var$contrib,2)


fviz_contrib(muestra_acp, choice = "var", axes =1,fill="chocolate3",col="steelblue") # dimension 1

fviz_contrib(muestra_acp, choice = "var", axes =2,fill="chocolate3",col="steelblue") # dimension 2



################################################################# clusters ################################################################# 

#Antes de aplicar un método de clustering a los datos es conveniente evaluar si hay indicios de que realmente existe algún tipo de agrupación en ellos.
# A este proceso se le conoce como assessing cluster tendecy y puede llevarse a cabo mediante test estadísticos (Hopkins statistic)
# o de forma visual (Visual Assessment of cluster Tendency).


# 1) Distancia
dist.euclidean <- get_dist(var_con, stand = T, method = 'euclidean')
dist.pearson <- get_dist(var_con, stand = T, method = 'pearson')
# Esta medida se ve menos afectada por outliers (es más robusta) que la distancia euclídea debido a que no eleva al cuadrado las diferencias.
dist.manhattan <- get_dist(var_con, stand = T, method = 'manhattan') 
dist.mink <- get_dist(var_con, stand = T, method = 'minkowski')

fviz_dist(dist.pearson, show_labels = FALSE) +labs(title = "Datos Turismo") + theme(legend.position = "bottom")


colores <- colorRampPalette(c("red", "white", "blue"))(256)
heatmap(x = var_con_sc, col = colores, cexRow = 0.7)


# 2) Distancia cor
# La correlación es una medida de distancia muy útil cuando la definición de similitud se hace en términos de patrón o forma
# y no de desplazamiento o magnitud. Los resultados pueden ser distintos que en las distancias anteriores.

dist.cor <- as.dist(1 - muestra_cor)
round(as.matrix(dist.cor),  2)


# 3) HOPKINS

# Los resultados muestran evidencias de que las observaciones del set de datos iris no siguen una distribución espacial uniforme,
# su estructura contiene algún tipo de agrupación. Por contra, el valor del estadístico H obtenido para el set de datos simulado es muy próximo a 0.5,
# lo que indica que los datos están uniformemente distribuidos y desaconseja la utilización de métodos de clustering. 

set.seed(1234)
# Cluster tendency
clustend <- get_clust_tendency(var_con_sc, nrow(var_con)-1)
# Hopkins statistic
clustend$hopkins_stat



# 4) OPTIMAL NUMBER OF CLUSTERS

## Elbow
## El método Elbow sigue una estrategia comúnmente empleada para encontrar el valor óptimo de un hiperparámetro. 
## La idea general es probar un rango de valores del hiperparámetro en cuestión, 
## representar gráficamente los resultados obtenidos con cada uno e identificar aquel punto de la curva a partir del cual la mejora deja de ser sustancial
# (principio de verosimilitud)

fviz_nbclust(x = var_con_sc, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(var_con_sc, method = "euclidean"), nstart = 25)


fviz_nbclust(x = var_con_sc, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(var_con_sc, method = "manhattan"), nstart = 25)

fviz_nbclust(x = var_con_sc, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(var_con_sc, method = "minkowski"), nstart = 25)


## silhouette

fviz_nbclust(x = var_con_sc, FUNcluster = kmeans, method = "silhouette", k.max = 15, 
             diss = get_dist(var_con_sc, method = "euclidean"), nstart = 50)  +
  ggtitle("Número óptimo de clusters método Silueta - k medias") +
  labs( x = "Número de clusters",y = "Variación del perfil promedio")


fviz_nbclust(x = var_con_sc, FUNcluster = kmeans, method = "silhouette", k.max = 15, 
             diss = get_dist(var_con_sc, method = "manhattan"), nstart = 50)  +
  ggtitle("Número óptimo de clusters método Silueta - k medias") +
  labs( x = "Número de clusters",y = "Variación del perfil promedio")


fviz_nbclust(x = var_con_sc, FUNcluster = kmeans, method = "silhouette", k.max = 15, 
             diss = get_dist(var_con_sc, method = "minkowski"), nstart = 50)  +
  ggtitle("Número óptimo de clusters método Silueta - k medias") +
  labs( x = "Número de clusters",y = "Variación del perfil promedio")



## gap

set.seed(1234)
gap_stat <- clusGap(var_con_sc, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

## NbClust

numero_clusters <- NbClust(data = var_con_sc, distance = "euclidean", min.nc = 2,
                           max.nc = 10, method = "kmeans", index = "alllong")


# 5) matriz de disimilaridades

daisy(muestra_cor, metric = "euclidean", stand = T)##Stand lo que hace es standarizar las variables.


# 6) clusters

#K-means clustering encuentra los K mejores clusters, entendiendo como mejor cluster aquel cuya varianza interna (intra-cluster variation) 
# sea lo más pequeña posible. Se trata por lo tanto de un problema de optimización, 
# en el que se reparten las observaciones en K clusters de forma que la suma de las varianzas internas de todos ellos sea lo menor posible. 
# Para poder solucionar este problema es necesario definir un modo de cuantificar la varianza interna.

# Partitioning Clustering: Este tipo de algoritmos requieren que el usuario especifique de antemano
# el número de clusters que se van a crear (K-means, K-medoids, CLARA).

## 2 clusters, distancia euclidea
clust.2 <- eclust(var_con, FUNcluster = 'kmeans', stand = T, hc_metric = 'euclidean', k=2,  nstart = 25)
fviz_cluster(clust.2)


## Presenta problemas de robustez frente a outliers. La única solución es excluirlos o recurrir a otros métodos de clustering más robustos
## como K-medoids (PAM). 

## PAM (medioides en lugar de centroides) sale peor.
clust.pam <- pam(var_con, stand = T, k = 2 ,metric = 'euclidean')
fviz_cluster(clust.pam)


## Comparativa K means

## Las agrupaciones resultantes pueden variar dependiendo de la asignación aleatoria inicial de los centroides. 
## Para minimizar este problema se recomienda repetir el proceso de clustering entre 25-50 veces
## y seleccionar como resultado definitivo el que tenga menor suma total de varianza interna. 
## Aun así, solo se puede garantizar la reproducibilidad de los resultados si se emplean semillas.

k1 <- kmeans(var_con_sc, centers = 2, nstart = 25)
k2 <- kmeans(var_con_sc, centers = 3, nstart = 25)
k3 <- kmeans(var_con_sc, centers = 4, nstart = 25)
k4 <- kmeans(var_con_sc, centers = 5, nstart = 25)


p1 <- fviz_cluster(k1, geom = 'point', data = var_con) + ggtitle('K = 2')
p2 <- fviz_cluster(k2, geom = 'point', data = var_con) + ggtitle('K = 3')
p3 <- fviz_cluster(k3, geom = 'point', data = var_con) + ggtitle('K = 4')
p4 <- fviz_cluster(k4, geom = 'point', data = var_con) + ggtitle('K = 5')

grid.arrange(p1,p2,p3,p4, nrow = 2)

## k-means plot (otro!)

fviz_cluster(object = k1, data = var_con_sc, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

# Una vez seleccionado el número adecuado de clusters y aplicado el algoritmo de clustering pertinente se tiene que evaluar la calidad de los de los mismos,
# de lo contrario, podrían derivarse conclusiones de agrupación que no se corresponden con la realidad. 

# Validación interna de los clusters:



fviz_silhouette(clust.2) # Cuantifica cómo de buena es la asignación que se ha hecho de una observación comparando su similitud
# con el resto de observaciones del mismo cluster frente a las de los otros clusters

# La función eclust() almacena, además de la información devuelta por la función de clustering empleada, en este caso kmeans, 
# información sobre los coeficientes silhouette individuales y por cluster, 
# el cluster al que se ha asignado cada observación y el cluster vecino más próximo (el segundo mejor candidato).


# Media silhouette por cluster
clust.2$silinfo$clus.avg.widths


# 7) Dendograma

# Hierarchical Clustering: Este tipo de algoritmos no requieren que el usuario especifique de antemano 
# el número de clusters. (agglomerative clustering, divisive clusterig).

dendograma.1 <- hclust(dist.euclidean, method="ward.D2")
plot(dendograma.1)
plot(dendograma.1, cex = 0.6, hang = -1, main="Dendrograma - hclust")


################################################################# tablas de contingencia ################################################################# 

# 1) asignamos los grupos clusterizados a los factores
Cluster <- k1$cluster
muestra_grupos <- cbind(var_factor, Cluster)


# 2) tablas de contingencia



# AEROPUERTOS

# (5-1)*(3-1) = 8 grados de libertad.
# H0: son independientes el voto y la situación. 
# Rechazo H0: Las variables no son independientes y existe un relación entre Pelicualas y grupos de edad.


aeropuerto <- ca(table(muestra_grupos$AEROPUERTO, muestra_grupos$Cluster))
table(muestra_grupos$SEXO, muestra_grupos$Cluster)
table(muestra_grupos$ESTANCIA_MAYOR_ISLA_G2, muestra_grupos$Cluster)
table(muestra_grupos$PAIS_RESID_AGRUP, muestra_grupos$Cluster)
table(muestra_grupos$YEAR, muestra_grupos$Cluster)








