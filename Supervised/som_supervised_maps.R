library(kohonen)
library(ggplot2)
library(factoextra)

#data awal
data <- read.csv("C://Users//ikhsan//OneDrive//Documents//Sisdas//Projek Sisdas//all_data_setelah_cluster_unsupervised.csv")

#split data (training 80, testing 20)
set.seed(189)
index <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[index==1,]
test <- data[index==2,]

#normalisasi data
trainX <- scale(train[,-1])
testX <- scale(test[,-1],
               center = attr(trainX, "scaled:center"),
               scale = attr(trainX, "scaled:scale"))
trainY <- factor(train[,1])
Y <- factor(test[,1])
test[,1] <- 1
testXY <- list(independent = testX, dependent = test[,1])
  
#classification
set.seed(189)
map1 <- xyf(trainX, 
            classvec2classmat(factor(trainY)),
            grid=somgrid(5,5,"hexagonal"),
            rlen=200)
#Training Progress
plot(map1, type = 'changes')

#Hasil Cluster
plot(map1)

#Prediction
pred <- predict(map1, newdata = testXY)
table(Predicted = pred$predictions[[2]], Actual=Y)

Y
#persen akurasi
accuracy <- ((21+5)/(21+3+6+5))*100
accuracy
testXY
pred

View(data)
map1
pred



map2 <- xyf(trainX, 
            classvec2classmat(factor(trainY)),
            grid=somgrid(5,5,"hexagonal"),
            rlen=500)
pred2 <- predict(map2, newdata = testXY)
table(Predicted = pred2$predictions[[2]], Actual=Y)
