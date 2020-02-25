library(factoextra)
library(cluster)

iris2 <- iris
iris2$Species <-NULL
(kmeans.result <-kmeans(iris2, 3))

# sum of square : jumlah dari jarak kuadrat dari data ke centroid 
# maka semakin kecil semakin bagus

table(iris$Species, kmeans.result$cluster)
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Sepal.Length","Sepal.Width")], col = 1:3,pch = 8, cex=2)

#------------ k-medoids---------------
library(fpc)
# pamk : clusternya ditentuin yg terbaik
pamk.result <-pamk(iris2)
pamk.result$nc
table(pamk.result$pamobject$clustering, iris$Species)

layout(matrix(c(1,2),1,2)) # 2 graphs per page
plot(pamk.result$pamobject)
layout(matrix(1)) # change back to one graph per page

library(factoextra)
# pam : nentuin sendiri clusternya
pam.result <-pam(iris2, 3)
table(pam.result$clustering, iris$Species)

layout(matrix(c(1,2),1,2)) # 2 graphs per page
plot(pam.result)
layout(matrix(1)) # change back to one graph per page

# yang overlap di paling bawah menandakan ada anggota cluster 2 yg harusnya cluster 1
# standar average silhouette = 0.52 nah hasil ini kan 0.69 makanya udah lumayan bagus


# hirarki clustering
idx <-sample(1:dim(iris)[1], 40)
irisSample <-iris[idx,]
irisSample$Species <-NULL

# hirarki, average linkage
hc <-hclust(dist(irisSample), method="ave")

plot(hc, hang = -1, labels=iris$Species[idx])

