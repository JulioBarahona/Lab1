#Lab #2
#Julio Barahona M
#141206

#librerias
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(corrplot)
library(sapply)

#datos de kaggle
train <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Segundo Semestre/Data Science/Lab1/train.csv", header = TRUE, sep = "," , stringsAsFactors = FALSE)
train2 <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Segundo Semestre/Data Science/Lab1/train.csv", header = TRUE, sep = "," , stringsAsFactors = TRUE)
test <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Segundo Semestre/Data Science/Lab1/test.csv", header = TRUE, sep = "," , stringsAsFactors = FALSE)
price <- read.csv("C:/Users/Julio/Desktop/Estudios/UVG/2018/Segundo Semestre/Data Science/Lab1/sample_submission.csv", header = TRUE, sep = "," , stringsAsFactors = FALSE)

#se obtienen las columnas numericas
numericTrain <- train[,(unlist(lapply(train, is.numeric)))]

#se obtienen las columnas NO numericas
noNumericTrain <- train2[,!(unlist(lapply(train2, is.numeric)))]

#id no es numerico, se omite
numericTrain$Id <- NULL

#correlacion no Numerica
must_convert<-sapply(noNumericTrain,is.factor)
inter<-sapply(noNumericTrain[,must_convert],unclass)
out<-cbind(noNumericTrain[,!must_convert],inter)

#se cambian los NA por 0
out[is.na(out)] <- 0
corNoNumeric<-cor(out)
absCor <- abs(corNoNumeric)
test <- as.data.frame(absCor[,any > 0.5])


#mapa de correlacion
corrplot(corNoNumeric, method="color")

#se usaran las columans que presentaron mas correlacion
principalesNoNumerico <- out[,c("Exterior1st","Exterior2nd","Heating","HeatingQC","Foundation",
                                           "KitchenQual","ExterQual","LandSlope","LandContour","CentralAir")]
corrplot(cor(principalesNoNumerico), method="color")

#se hace el cluster con todos
irisCompleto<-iris[complete.cases(iris),]

#se hace el cluster con los principales














