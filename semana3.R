################################# SEMANA 3 #####################################
setwd("C:/Users/Mafer/Documents/R-curso4")

#agrupamiento jerarquico 
#clustering 
#ejemplo 1
set.seed(1234)
par(mar=c(0, 0, 0, 0))
x = rnorm(12, mean=rep(1:3, each=4), sd=0.2)
y = rnorm(12, mean=rep(c(1, 2, 1), each=4), sd=0.2)
plot(x, y, col = "blue", pch = 19, cex=2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

dataFrame = data.frame(x=x, y=y)
distxy = dist(dataFrame)
hClustering = hclust(distxy)
plot(hClustering)

#heatmap
set.seed(143)
dataMatrix = as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)

#### k-means Clustering 
#ejemplo 1
kmeansObj = kmeans(dataFrame, centers = 3)
names(kmeansObj)

kmeansObj$cluster

#ejemplo 2
par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex =3, lwd = 3)

#ejemplo 3
set.seed(1234)
dataMatrix = as.matrix(dataFrame)[sample(1:12, )]
kmeansObj2 = kmeans(dataMatrix, centers = 3)
par(mfrow = c(1,2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt  = "n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")

#Matrix Data
set.seed(12345)
par(mar=rep(0.2, 4))
dataMatrix = matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

#cluster jerarquico
par(mar = rep(0.2, 4))
heatmap(dataMatrix)

#agregando patron 
set.seed(678910)
for(i in 1:40){
  coinFlip = rbinom(1, size = 1, prob = 0.5)
  
  if(coinFlip){
    dataMatrix[i,] = dataMatrix[i,] + rep(c(0,3), each = 5)
  }
}
par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

par(mar = rep(0.2, 4))
heatmap(dataMatrix)

#patrones en filas y columnas
hh = hclust(dist(dataMatrix))
dataMatrixOrdered = dataMatrix[hh$order,]
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch=19)

#Componentes de SVD
svd1 = svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[,1], 40:1, ,xlab="Row", ylab= "First left singular vector", pch=19)
plot(svd1$v[,1], xlab= "Column", ylab = "First right singular vector", pch=19)


#Varianca explicada
par(mfrow = c(1, 2))
plot(svd1$d, xlab= "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of Varience explained", 
     pch = 19)

#Relacion entre componentes principales
svd1 = svd(scale(dataMatrixOrdered))
pca1 = prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch=19, xlab= "Principal Component 1", 
     ylab="Right Singular Vector 1")
abline(c(0,1))

#Componentes de SVD
constantMatrix = dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,]=rep(c(0,1), each=5)}
svd1 = svd(constantMatrix)
par(mfrow=c(1, 3))
image(t(constantMatrix)[, nrow(constantMatrix):1])
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch=19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of Varience explained",
     pch = 19)

#Missing values
dataMatrix2 = dataMatrixOrdered
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] = NA
svd1 = svd(scale(dataMatrix2)) #no funciona por los NAS

library(impute)
dataMatrix2 = dataMatrixOrdered
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] = NA
dataMatrix2 = impute.knn(dataMatrix2)$data
svd1 = svd(scale(dataMatrixOrdered))
svd2 = svd(scale(dataMatrix2))
par(mfrow = c(1, 2))
plot(svd1$v[,1], pch =19)
plot(svd2$v[,1], pch =19)

#ejemplo final
load("data/face.rda")
image(t(faceData)[,nrow(faceData):1])



