library(dplyr)
require(cluster)
require(fpc)
require(factoextra)
require(dplyr)
library(dendextend)
require(ggrepel)
library(RWeka)
require(NbClust)
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
library(ggplot2)

datos <- read.csv('EGT2010_2017.csv')
str(datos)
datos_filtrados <- datos[, c(81:93, 3, 4, 6, 21, 72, 112, 110, 109, 80, 151, 146)]
which(is.na(datos_filtrados))

summary(datos_filtrados)

datos_filtrados_noNA <- na.omit(datos_filtrados)
summary(datos_filtrados_noNA)

primer_df <- datos_filtrados_noNA %>% filter(datos_filtrados_noNA$AÑO <= 2014)
segundo_df <- datos_filtrados_noNA %>% filter(datos_filtrados_noNA$AÑO > 2014)

set.seed(1234)

primer_150 <- primer_df[sample(nrow(primer_df), 150), ]
segundo_150 <- segundo_df[sample(nrow(segundo_df), 150),]

rm(datos)
rm(datos_filtrados)
rm(datos_filtrados_noNA)
rm(primer_df)
rm(segundo_df)


## Vamos a proceder a realizar la limpieza del dataframe
str(primer_150)

## Vamos a crear otro DF para realizar los clusters ##
primer_150_limpio <- primer_150[, c(-15, -16, -18, -21, -23)]
str(primer_150_limpio)

primer_150_limpio$INGRESOS = ingresos
View(primer_150_limpio)

rownames(primer_150_limpio) <- primer_150_limpio$ID
primer_150_limpio$ID <- NULL

primer_150_limpio_sc <- scale(primer_150_limpio)
primer_150_limpio_sc

Alojamiento_general = (primer_150$VALORACION_ALOJ + 
                         primer_150$VALORACION_TRATO_ALOJ + 
                         primer_150$VALORACION_GASTRONO_ALOJ) /3
Entorno_general = (primer_150$VALORACION_CLIMA +
                     primer_150$VALORACION_ZONAS_BANYO + primer_150$VALORACION_PAISAJES + 
                     primer_150$VALORACION_MEDIO_AMBIENTE + primer_150$VALORACION_TRANQUILIDAD +
                     primer_150$VALORACION_LIMPIEZA) / 6
Restaurante_general = (primer_150$VALORACION_CALIDAD_RESTAUR + 
                         primer_150$VALORACION_OFERTA_GASTR_LOC + 
                         primer_150$VALORACION_TRATO_RESTAUR +
                         primer_150$VALORACION_PRECIO_RESTAUR) / 4

str(Alojamiento_general)
View(primer_150_unido)

#Reestructuracion de ingresos
library("dummies")
ingresos = dummy(primer_150$INGRESOS)

i1=ingresos[,1]*((12000+24000)/2)
i2= ingresos[,2]*((24000 + 36000)/2)
i3=ingresos[,3]*((36001 +48000)/2)
i4=ingresos[,4]*((48001 + 60000)/2)
i5=ingresos[,5]*((60001+72000)/2)
i6=ingresos[,6]*((72001+84000)/2)
i7=ingresos[,7]*((84000+12000)/2)

ingresos=(i1+i2+i3+i4+i5+i6+i7)
head(ingresos)

primer_150_unido <- data.frame(primer_150$IMPRESION, Alojamiento_general, Restaurante_general,Entorno_general,
                                ingresos, primer_150$EDAD)

str(primer_150_unido)

library(factoextra)
library(FactoMineR)
library(dplyr)
#Analizamos las estructuras de los dos datas
str(primer_150)
str(segundo_150)


str(primer_150_unido)
#
##Por lo que podemos observar esto es un dataframe, que nos muestra los precios de los diferentes 
#bonos en funciÃ³n de su durabilidad y su tipo de interÃ©s.
## Como se puede observar al ser un dataframe de cientos de datos, 
#los cuales, nos van a interesar reducir, una tecnica... componentes principales...definicon.HabrÃ¡ por ejemplo que analizar la correlaciÃ³n pra interpretarlo demanera correcta.
#Hemos cambiado los valores faltantes por 0, ya que era necesario para realizar el ACP.
##https://www.youtube.com/watch?v=oiR3k9H-7K0

##Preprocesamiento de los datos(esto implica )

##Transformación de variables##

correlaciondata_pca <- cor(primer_150_unido)
str(data_pca)

correlacion_data_pca_2 <- cor(primer_150_limpio)

##FALTA LA CORRELACIÃON.PREGUNTAR MAÑANA A FERRO, TIENE SENTIDO realizar el analisis de componentes principales debido a que existe una correlacion entre algunas variables, las cuales depues de una reduccion de las mismas no se pierda valor explicativo.
correlaciondata_pca
#DetecciÃ³n de valores NA, de los valores perdidos.
#Esto se hace para ver cuantos valores ausentes existen, como estan todos definidos podemos continuar con el analisis.

#DiscretizaciÃ³n, se trata de pasar una variable de tipo numÃ©rica a tipo categÃ³rica

#2Normalizacion:Estandarizacion,la cual consiste escalar los datos, representarlos en la misma magnitud,para que por ejemplo una variable con varianza alta no tenga mayor influencia que el resto en el anÃ¡lisis.

scaledatos <- scale(primer_150_unido)#analisis de componentes principales necesita que estÃ© todo en una misma magnitud

correlaciondata_pca_sc <- cor(scaledatos)
pairs(data_pca)

##Esfericidad de bartlet
library(psych)
cortest.bartlett(correlaciondata_pca, n = nrow(primer_150)) ##La prueba de esfericidad de Bartlett contrasta la hipÃ³tesis nula de que la matriz de correlaciones es una matriz identidad, en cuyo caso no existirÃ�an correlaciones significativas entre las variables y el modelo factorial no serÃ�a pertinente.## Si Sig. (p-valor) < 0.05 aceptamos H0 (hipÃ³tesis nula) > se puede aplicar el anÃ¡lisis.
det(correlaciondata_pca)

##KMO es una prueba estadistica la cual es una MSA, medida de adecuacion muestral, definicion(compara los valores de las correlaciones entre las variables y sus correlaciones parciales si el indice Kmo esta proximo a 1 el ACP se puede hacer si es proximo a 0, el ACP no serÃ¡ relevante)
KMO(correlaciondata_pca)## definicion de kmo, como se puede observar en el kmo todos con mayores de 0.7, por tanto 
##AnÃ¡lisis de componentes principales. Cuanto mÃ¡s cerca de 1 tenga el valor obtenido del test KMO, implica que la relaciÃ³n entres las variables es alta. Si KMO â¥ 0.9
library(missMDA)
acp <- PCA(correlaciondata_pca, scale.unit = T) ## esta nos arroja resultados mas precisos. Nos retorna la dv estandar de cada uno de los componentes principales , los cuales coinciden con los autovalores de los componentes principales, y nos retorna el conjunto de componentes principales

##Utilizamos imputePCA, para eliminar los valores faltantes.


fviz_eig(acp, addlabels = TRUE, hjust = 0) + labs(title("Representacion"), x = "Dimensiones", y = "Porcentaje de v explicada") + theme_classic()
autoplot(acp)

##Cual es la proporcion de varianza explicada por cada de una de estos componente y despues de ello elegir cual es el que nos vamos a quedar

summary(acp) ##Mirar proportion of variance 


#Prediccion del Bono a 10 aÃ±os
modelo <- lm(IRS.10Y ~ DEPO.12M + IRS.2Y + IRS.3Y + IRS.4Y + IRS.5Y + IRS.7Y, data = databonos)
bono10 <- data.frame(test = c(databonos2$DEPO.12M, databonos2$IRS.2Y, databonos2$IRS.3Y, databonos2$IRS.4Y, databonos2$IRS.5Y, databonos2$IRS.7Y))
View(table(predict(modelo, newdata = databonos2)))





#para decidir cuantos componentes nos quedamos, un ejemplo es la regla de kaiser(definir regla)La suma debe de ser menor a 1 evidentemente.
#ahora miramos las varianzas individuales de cada uno.
#Se puede observar que en la varianza acumulada hasta la dim2, existe un 92 de varianza explicada acumulada. Podemos desprendernos de las 8 siguientes dimensiones, ya que solo perdemos el 10% de la informaciÃ³n, y nos quedamos solo con 2 variables.


desv_standar <- acp[[1]] ##El primer valor nos da las desviaciones estandar
desv_standar
##Tienen que ser cercanas a 1 pero que pasa que eso es la desviaciÃ³n tÃ�pica necesitamos la 

varianza <- desv_standar^2
varianza                  ##hay que fijarse en los que sean mayor que 1 , elijo tantos autovectores como autovalores >1 tenga la matriz de correlaciones delas variables.
## miramos la varianza acumulada, y nos quedamos con el 85%, los 2 primeros componentes por que no perdemos capacidad explicativa.

##GrÃ¡fico de sedimentacion

Sedimentacion <- princomp(databonos2, scores = TRUE, cor = TRUE)
plot(Sedimentacion, type = 'lines')

CP1 = acp[[2]][,1] ## hay que hacer todos los analisis con el los diferentes componentes. Observamos que los unicos componentes principales mayores que 1, son el 1 y el 2 , por tanto vamos a quedarnos con dichos componentes principales, los cuales, nos explican el 92% de la informacion.
CP1

CP2 = acp[[2]][,3]
CP2

Comp_prin = cbind(CP1,CP2)
Comp_prin
##El componente principal 1 le da mas importancia al deposito a 1 mes, y a los de a partir de 1 aÃ±o y el CP2 el de 1 mes lo explica peor, el que peor. Y el CP2, explica bien los demÃ¡s, desde 3 meses hasta 12 meses.


## Los componentes que explica 

##SOlo queda el varimax y ponerlo bonito.
library(gclus)
library(PCAmixdata)
factanal(databonos2, rotation = "varimax", factors = 3)

res.pcamix <- PCAmix(databonos2,rename.level=TRUE, graph=FALSE)
res.pcarot <- PCArot(res.pcamix,dim=2,graph=FALSE)

par(mfrow=c(2,2))
plot(res.pcarot,choice="ind",label=FALSE,axes=c(1,2),
     main="Observaciones antes de rotaciÃ³n")
plot(res.pcarot,choice="ind",label=FALSE,axes=c(1,2),
     main="Observaciones despuÃ©s de rotaciÃ³n")
plot(res.pcamix,choice="sqload", coloring.var=TRUE, leg=TRUE,axes=c(1,2),
     posleg="topright", main="Variables antes de rotaciÃ³n",
     xlim=c(0,1), ylim=c(0,1))
plot(res.pcarot,choice="sqload", coloring.var=TRUE, leg=TRUE,axes=c(1,2),
     posleg="topright", main="Variables despuÃ©s de rotaciÃ³n",
     xlim=c(0,1), ylim=c(0,1))
View(databonos)
biplot(databonos2, databonos2)

#Prediccion
datos_sp <- databonos[950:978, -1]
ultimas20 <- databonos[950:978, c(-1, -11)]
modelo <- lm(IRS.10Y ~ DEPO.1M + DEPO.3M + DEPO.6M + DEPO.12M + IRS.2Y + IRS.3Y + IRS.4Y + IRS.5Y + IRS.7Y, data = databonos)
prediction <- predict(modelo, ultimas20)
result10 <- datos_sp$IRS.10Y
pr <- as.data.frame(cbind(result10, prediction))
View(pr)

d = dist(primer_150_limpio_sc, method = "euclidean")
churn.hc = hclust(d, method = "ward.D2" )
plot(churn.hc, cex = 0.6, hang = -1, main="Dendrograma - hclust")
rect.hclust(churn.hc, k=3, border = 2:4)
rect.hclust(churn.hc,k=2,border=2:4)
grp = cutree(churn.hc, k = 2)
table(grp)

## AGNES
# library("cluster") # si no estuviese cargada
# Cálculo
viajeros.agnes = agnes(scaledatos, method = "ward")
# Coeficiente de aglomeración
viajeros.agnes$ac
pltree(viajeros.agnes, cex = 0.6, hang = -1, main = "Dendrograma - AGNES")
rect.hclust(viajeros.agnes, k = 2, border = 2:4)
grp.ag = cutree(as.hclust(viajeros.agnes), k = 2)
table(grp.ag)



###############################################
#Obtenemos las distancias del anterior DF a través de Pearson
qdist.pearson <- get_dist(primer_150_unido, stand = T, method = 'pearson')
qdist.manhattan <- get_dist(primer_150_limpio_sc, stand = T, method = 'manhattan')
qdist.mink <- get_dist(primer_150_limpio_sc, stand = T, method = 'minkowski')
str(qdist.pearson)

dist.cor <- as.dist(1 - correlaciondata_pca)
round(as.matrix(dist.cor),  2)

#Realizamos la representación gráfica.
fviz_dist(qdist.pearson, lab_size = 5)
as.matrix(as.dist(qdist.pearson))

#Cambiamos la representación
primer_150_sca <- scale(primer_150_unido)

str(primer_150)
str(primer_150_limpio)
str(primer_150_limpio_sc)
str(primer_150_sca)

fviz_dist(qdist,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), lab_size = 5)

d <- dist(primer_150_sca ,method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit)

plot(fit, cex = 0.6, hang = -1, main="Dendrograma - hclust")
rect.hclust(fit, k=5, border = 2:4)

daisy(correlacion_data_pca_2, metric = c('manhattan'), stand = T)##Stand lo que hace es standarizar las variables.

str(primer_150_limpio)

coches.eclust <- eclust(primer_150_limpio, FUNcluster = 'kmeans', stand = T, hc_metric = 'euclidean',  nstart = 25)


coches.eclust.j = eclust(primer_150_sca, "hclust", k = 3)
fviz_cluster(coches.eclust.j)
fviz_silhouette(coches.eclust.j)


fviz_nbclust(cochesescalados, kmeans, method = "silhouette") +
  ggtitle("Número óptimo de clusters - k medias") +
  labs(x="Número k de clusters",y="Anchura del perfil promedio")

fviz_nbclust(x = primer_150_limpio_sc, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(primer_150_sca, method = "euclidean"), nstart = 50)

set.seed(123)

km_clusters <- kmeans(x = primer_150_limpio_sc, centers = 2, nstart = 25)

fviz_cluster(object = km_clusters, data = primer_150_limpio_sc, show.clust.cent = TRUE,
             ellipse.type = "convex", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

####################### Ahora vamos a probar con el DF agrupado con 50D1 y 20D2 ############33

d <- dist(primer_150_sca ,method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit)

plot(fit, cex = 0.6, hang = -1, main="Dendrograma - hclust")
rect.hclust(fit, k=5, border = 2:4)

daisy(correlaciondata_pca, metric = c('manhattan'), stand = T)##Stand lo que hace es standarizar las variables.

str(primer_150_limpio)

coches.eclust <- eclust(primer_150_unido, FUNcluster = 'kmeans', stand = T, hc_metric = 'euclidean',  nstart = 25)

str(primer_150_unido)

coches.eclust.j = eclust(primer_150_sca, "hclust", k = 3)
fviz_cluster(coches.eclust.j)
fviz_silhouette(coches.eclust.j)


fviz_nbclust(primer_150_sca, kmeans, method = "silhouette") +
  ggtitle("Número óptimo de clusters - k medias") +
  labs(x="Número k de clusters",y="Anchura del perfil promedio")

fviz_nbclust(x = primer_150_sca, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(primer_150_sca, method = "euclidean"), nstart = 50)

set.seed(123)

km_clusters <- kmeans(x = primer_150_sca, centers = 3, nstart = 25)

fviz_cluster(object = km_clusters, data = primer_150_sca, show.clust.cent = TRUE,
             ellipse.type = "convex", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

k2 <- kmeans(primer_150_sca, centers = 2, nstart = 25)
k3 <- kmeans(primer_150_sca, centers = 3, nstart = 25)
k4 <- kmeans(primer_150_sca, centers = 4, nstart = 25)
k5 <- kmeans(primer_150_sca, centers = 5, nstart = 25)
k6 <- kmeans(primer_150_sca, centers = 6, nstart = 25)
k7 <- kmeans(primer_150_sca, centers = 7, nstart = 25)
k8 <- kmeans(primer_150_sca, centers = 8, nstart = 25)

p1 <- fviz_cluster(k2, geom = 'point', data = primer_150_sca) + ggtitle('K = 2')
p2 <- fviz_cluster(k3, geom = 'point', data = primer_150_sca) + ggtitle('K = 3')
p3 <- fviz_cluster(k4, geom = 'point', data = primer_150_sca) + ggtitle('K = 4')
p4 <- fviz_cluster(k5, geom = 'point', data = primer_150_sca) + ggtitle('K = 5')
p5 <- fviz_cluster(k6, geom = 'point', data = primer_150_sca) + ggtitle('K = 6')
p6 <- fviz_cluster(k7, geom = 'point', data = primer_150_sca) + ggtitle('K = 7')
p7 <- fviz_cluster(k8, geom = 'point', data = primer_150_sca) + ggtitle('K = 8')

primer_150_no_ID <- primer_150
primer_150_no_ID$ID <- NULL
str(primer_150_no_ID)

which(is.na(primer_150_no_ID))
str(primer_150_unido)

primer_150_limpio$INGRESOS <- primer_150_limpio$INGRESOS * 10000


library(gridExtra)
require(ggrepel)
grid.arrange(p1, p2, p3, p4, p5, nrow = 2)

k2 <- kmeans(primer_150_sca, centers = 2, nstart = 25)
k3 <- kmeans(primer_150_sca, centers = 3, nstart = 25)
k4 <- kmeans(primer_150_sca, centers = 4, nstart = 25)
k5 <- kmeans(primer_150_sca, centers = 5, nstart = 25)
k6 <- kmeans(primer_150_sca, centers = 6, nstart = 25)
k7 <- kmeans(primer_150_sca, centers = 7, nstart = 25)
k8 <- kmeans(primer_150_sca, centers = 8, nstart = 25)

p1 <- fviz_cluster(k2, geom = 'point', data = primer_150_sca, stand = T) + ggtitle('K = 2')
p2 <- fviz_cluster(k3, geom = 'point', data = primer_150_sca, stand = T) + ggtitle('K = 3')
p3 <- fviz_cluster(k4, geom = 'point', data = primer_150_sca, stand = T) + ggtitle('K = 4')
p4 <- fviz_cluster(k5, geom = 'point', data = primer_150_sca, stand = T) + ggtitle('K = 5')
p5 <- fviz_cluster(k6, geom = 'point', data = primer_150_sca, stand = T) + ggtitle('K = 6')
p6 <- fviz_cluster(k7, geom = 'point', data = primer_150_sca, stand = T) + ggtitle('K = 7')
p7 <- fviz_cluster(k8, geom = 'point', data = primer_150_sca, stand = T) + ggtitle('K = 8')

str(primer_150_limpio)
str(primer_150)

unscale(k2$centers)
k2$centers * attr(k2$centers, 'scaled:scale') + attr(k2$centers, 'scaled:center')

############## Inspeccion ##############
ggplot(as.data.frame(primer_150_limpio), aes(x=IMPRESION, y=VALORACION_ALOJ)) +
  geom_point() +
  geom_density_2d()

graf.datos <-ggplot(as.data.frame(primer_150_limpio), aes(x=VALORACION_TRATO_ALOJ, y=VALORACION_ALOJ)) +
  geom_point() +
  geom_density_2d()


# Generamos un conjunto aleatorio de datos para las dos variables
set.seed(123)
n = nrow(primer_150_limpio)
random_df = data.frame(
  x = as.integer(runif(nrow(primer_150_limpio), min(primer_150_limpio$VALORACION_ALOJ), max(primer_150_limpio$VALORACION_ALOJ))),
  y = as.integer(runif(nrow(primer_150_limpio), min(primer_150_limpio$VALORACION_TRATO_ALOJ), max(primer_150_limpio$VALORACION_TRATO_ALOJ))))
# Colocamos en objeto para representación posterior
graf.aleat=ggplot(random_df, aes(x, y)) + geom_point() + labs(x="ALOJAMIENTO",y="TRATO ALOJAMIENTO") + stat_density2d(aes(color = ..level..))
graf.aleat

grid.arrange(graf.datos, graf.aleat, nrow=1, ncol=2)

require(NbClust)
Nb.viajeros_general=NbClust(as.data.frame(primer_150_sca), distance = "euclidean", min.nc = 2,
                            max.nc = 10, method = "complete", index ="all")

require(factoextra)
fviz_nbclust(Nb.viajeros_general) + theme_minimal() +
  labs(x="Número k de clusters", y="Frecuencia")


str(primer_150_sca)
typeof(primer_150_sca)
typeof(primer_150_limpio)
typeof(primer_150_limpio_sc)
typeof(primer_150_unido)

## No podemos realizar CLARA por la cantidad de datos de los que disponemos
require(cluster)
viajeros_esp.clara=clara(primer_150_limpio, 2)
require(factoextra)
fviz_cluster(viajeros_esp.clara, stand = TRUE, geom = "point", pointsize = 1)
## No podemos realizar CLARA por la cantidad de datos de los que disponemos

### Vamos a probar con PAM que tampoco va a salir ###
pam.res <- pam(primer_150_sca, k = 3 ,metric = c('euclidean', 'manhattan'))
fviz_cluster(pam.res)
