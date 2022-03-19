#library
library(kohonen)
library(ggplot2)
library(factoextra)

#data awal
data <- read.csv("C://Users//ikhsan//OneDrive//Documents//Sisdas//R_scripts_functions//all_data.csv")

#data yang digunakan
som_data <- data[3:7]
summary(som_data)

#dimensi data training
scale(som_data)
dim(som_data)

#cluster data
fviz_nbclust(som_data,kmeans,method="silhouette")

#algoritma SOM 
set.seed(10000)
som_grid <- somgrid(xdim=5, ydim=5, topo="hexagonal")
som_grid
som.som_data <- som(scale(som_data), grid=som_grid, alpha=c(0.05, 0.01), radius=1)
som.som_data

#Learning Progres
plot(som.som_data, type = "changes")

#Hasil Mapping Cluster
plot(som.som_data, type="mapping")
som.som_data$grid$pts
som.som_data$unit.classif
plot(som.som_data)

#Hierarchical Clustering cek berapa objek
hclust(dist(som.som_data$codes[[1]]))

#hierarchical clustering 2 cluster
peta<-cutree(hclust(dist(som.som_data$codes[[1]])),2) 
peta
summary(peta)

#menampilkan plot
plot(peta) 
plot(som.som_data,type="codes",bgcol=rainbow(5)[peta])

add.cluster.boundaries(som.som_data,peta)
kelompok <- data.frame(id=data$nama_kota, 
                       cluster=peta[som.som_data$unit.classif])
View(kelompok)

#export ke csv 
write.csv(kelompok, "C://Users//ikhsan//OneDrive//Documents//Sisdas//R_scripts_functions//hasil cluster data_sisdas_uts - training.csv", row.names=FALSE)
