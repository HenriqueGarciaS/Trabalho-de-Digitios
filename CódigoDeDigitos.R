install.packages("tidyverse")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("FNN")
install.packages("factoextra")

library(tidyverse)
library(rpart)
library(rpart.plot)
library(e1071)
library(FNN)
library(factoextra)

#funções
fazKNN <- function(treino,teste,ClasseTreino,classeTeste){
  K <- c(1,3,7,9)
  for(K in K){
    porcentagem <- 0
    Resultado <- knn(treino,teste,ClasseTreino,K)
    for(i in  1:length(Resultado))
      if(Resultado[i] == ClasseTeste[i])
        porcentagem <- porcentagem + 1
    porcentagem <- porcentagem / length(ClasseTeste)*100
    print(porcentagem)
  }
}

fazSVM <- function(treino,teste){
  classificador <- svm(ident ~ ., treino, 'C-classification', 'linear')
  pred = predict(classificador,teste)
  pred
  
}

#montagem do data frame
setwd("C:/Users/17079294/Desktop/Trabalho-de-Digitios/digitos")
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
ClasseTreino <- treino[,4096]
ClasseTeste <- teste[,4096]

#KNN
fazKNN(treino,teste,ClasseTreino,ClasseTeste)

#SVM
fazSVM(treino,teste)
