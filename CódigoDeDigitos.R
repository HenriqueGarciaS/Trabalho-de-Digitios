install.packages("ggplot2")
install.packages("tidyverse")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("FNN")
install.packages("factoextra")
install.packages("animation")
install.packages("rgl")
install.packages("BBmisc")

library(ggplot2)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(e1071)
library(FNN)
library(factoextra)
library(animation)
library(rgl)
library(BBmisc)

#funções
fazKNN <- function(treino,teste,ClasseTreino,classeTeste){
  K <- c(1,3,7,9)
  ret <- NULL
  for(K in K){
    porcentagem <- 0
    Resultado <- knn(treino,teste,ClasseTreino,K)
    for(i in  1:length(Resultado))
      if(Resultado[i] == ClasseTeste[i])
        porcentagem <- porcentagem + 1
    porcentagem <- (porcentagem / length(ClasseTeste))*100
    ret <- c(ret,porcentagem)
  }
  return(ret)
}

fazSVM <- function(treino,teste,ClasseTeste){
  classificador <- svm(ident~ ., treino, 'C-classification', 'linear')
  porcentagem <- 0
  pred = predict(classificador,teste)
  for(i in  1:length(pred))
    if(pred[i] == ClasseTeste[i])
      porcentagem <- porcentagem + 1
  porcentagem <- (porcentagem / length(ClasseTeste))*100
  return (porcentagem)
}

fazrpart <- function(treino,teste,ClasseTeste){
  modelo <- rpart(formula = ident~.,treino,method = "class", control = rpart.control(minsplit = 1))
  pred <- predict(modelo,teste,type = "class")
  porcentagem <- length(which(pred == ClasseTeste))
  porcentagem <- porcentagem/length(ClasseTeste)*100
  return (porcentagem)
}

clusteriazacao <- function(data,k){
  c <- kmeans(data,k)
  plot(c)
}

#montagem do data frame
setwd("C:/Users/Usuario/Desktop/Trabalho-de-Digitios/digitos")
data_digitos <- data.frame()
lista_arquivo <- list.files()
identificador <- NULL

for(arquivo in lista_arquivo){
  identificador <- c(identificador,as.numeric(unlist(strsplit(arquivo,'_'))[1]))
  leitura <- read_lines(arquivo)
  leitura <- leitura[-(1:3)]
  leitura <- strsplit(leitura,' ')
  leitura <- unlist(leitura)
  leitura <- as.numeric(leitura)
  data_digitos <- matrix(leitura, nrow = nrow(data_digitos)+1, ncol = length(leitura))

}
data_digitos <- as.data.frame(data_digitos)
data_digitos$ident <- identificador

#separação entre teste e treino
smp_size <- floor(0.8*nrow(data_digitos))
train_ind <- sample(seq_len(nrow(data_digitos)), size = smp_size)
treino <- data_digitos[train_ind,]
teste <- data_digitos[-train_ind,]
ClasseTreino <- treino[,4097]
ClasseTeste <- teste[,4097]

#KNN
acuraciaKNN <- fazKNN(treino,teste,ClasseTreino,ClasseTeste)

#SVM
acuraciaSVM <- fazSVM(treino,teste,ClasseTeste)

#Arvore de decisão
modelo <- rpart(formula = ident~.,treino,method = "class", control = rpart.control(minsplit = 1))
pred <- predict(modelo,teste,type = "class")
acuraciaRPART <- length(which(pred == ClasseTeste))/length(ClasseTeste)*100
acurariaRPART <- fazrpart(treino,treino,ClasseTeste)

#Cluster
fviz_nbclust(data_digitos,kmeans,method = "wss")
clusteriazacao(data_digitos,N)
c <- kmeans(data_digitos,10)
kmeans.ani(data_digitos)
dadoscluster <- data_digitos 
dadoscluster$cluster <- c$cluster
plot3d(dadoscluster, col = dadoscluster$cluster, main = "k-means clusters")

#PCA
data_digitos.pca <- prcomp(data_digitos[,1:4096], center = TRUE, scale. = TRUE)
summary(data_digitos.pca)
fviz_eig(data_digitos.pca)
eig.val <- get_eigenvalue(data_digitos.pca)
data.var <- get_pca_var(data_digitos.pca)
data.ind <- get_pca_ind(data_digitos.pca)

data.linhas <- predict(data_digitos.pca,data_digitos)
data.linhas <- as.data.frame(data.linhas)

