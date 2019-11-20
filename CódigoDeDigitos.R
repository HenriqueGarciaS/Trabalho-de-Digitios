install.packages("tidyverse")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("FNN")
install.packages("factoextra")
install.packages("animation")

library(tidyverse)
library(rpart)
library(rpart.plot)
library(e1071)
library(FNN)
library(factoextra)
library(animation)

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

fazrpart <- function(treino,teste,classe){
  modelo <- rpart(formula = ident~.,treino,method = "class", control = rpart.control(minsplit = 1))
  pred <- predict(modelo,teste, type ="class")
  porcentagem <- 0
  for(i in length(classe))
    if(pred[i] == classe[i])
      porcentagem <- porcentagem +1
  rpart.plot(modelo,type = 3,tweak = 1.8, fallen.leaves = FALSE )
  return ((porcentagem/length(classe))*100)
}

clusteriazacao <- function(data,k){
  c <- kmeans(data,k)
  kmeans.ani(data,3)
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
retorno <- fazKNN(treino,teste,ClasseTreino,ClasseTeste)

#SVM
retorno1 <- fazSVM(treino,teste,ClasseTeste)

#Arvore de decisão
retorno2 <- fazrpart(treino,treino,ClasseTeste)

#Cluster
fviz_nbclust(data_digitos,kmeans,method = "wss")
N <- readline(prompt = "Numero de clusters: ")
clusteriazacao(data_digitos,N)
kmeans.ani(data_digitos)

modelo <- rpart(formula = ident~.,treino,method = "class", control = rpart.control(minsplit = 1))
pred <- predict(modelo,teste, type ="class")

