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



