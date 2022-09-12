##### Práticas de classificação

## Árvores de classificação

# instala o pacote mclust
install.packages("mclust")

# carrega a base de dados banknote
data('banknote', package='mclust') 

# verifica o número de linhas do dataset
numLinhas = nrow(banknote)

# sorteia 150 exemplos dentre todos os exemplos do dataset
base.treino <- sample(1:numLinhas, 150, FALSE)

# exibe as 5 primeiras linhas sorteadas
head(base.treino)

# instala o pacote C50
install.packages("C50")

# referencia o pacote C50
library(C50)

# constrói o modelo usando a variável Status como variável de classe
# o argumento ~. diz que todos os atributos devem ser utilizados
modeloC50 <- C5.0(Status ~., data = banknote[base.treino,])

# plota o gráfico da árvore construída
plot(modeloC50)

modeloC50.com.regras <- C5.0(Status ~ ., data = banknote[base.treino,], rules=TRUE)
summary(modeloC50.com.regras)

pred.treino <- predict(modeloC50, newdata = banknote[base.treino,], type="class")

# cria a matriz de confusão de treino
matriz.conf.treino <- table(banknote$Status[base.treino], pred.treino, dnn=c("Classe Observada", "Classe Predita")) 
# exibe a matriz
print(matriz.conf.treino)

# calcula a acurácia
acc.treino <- (matriz.conf.treino[1,1] + matriz.conf.treino[2,2]) / (matriz.conf.treino[1,1] + matriz.conf.treino[1,2] + matriz.conf.treino[2,1] + matriz.conf.treino[2,2]) * 100 # calcula a acurácia de treino
# exibe a acurácia
print(acc.treino)

pred.teste <- predict(modeloC50, newdata = banknote[-base.treino,], type="class")

matriz.conf.teste <- table(banknote$Status[-base.treino], pred.teste, dnn=c("Classe Observada", "Classe Predita")) # cria a matriz de confusão de teste
print(matriz.conf.teste) # exibe a matriz

# calcula a acurácia
acc.teste <- (matriz.conf.teste[1,1] + matriz.conf.teste[2,2]) / (matriz.conf.teste[1,1] + matriz.conf.teste[1,2] + matriz.conf.teste[2,1] + matriz.conf.teste[2,2]) * 100
# exibe a acurácia
print(acc.teste)

install.packages("tree")
library(tree)

# constrói o modelo
modeloTree <- tree(Status ~ ., data = banknote[base.treino,], split="deviance")
# plota o modelo
plot(modeloTree) 
text(modeloTree)

# exibe a performance de classificação
summary(modeloTree)

# aplica o modelo no conjunto de teste
pred.teste <- predict(modeloTree, newdata = banknote[-base.treino,])
# guarda as classes preditas
pred.class <- colnames(pred.teste)[max.col(pred.teste, ties.method = c("random"))]
# calcula e exibe a matriz de confusão
matriz.conf.teste <- table(banknote$Status[-base.treino], pred.class, dnn = c("Classe Observada", "Classe Predita"))
print(matriz.conf.teste)

# calcula e exibe a acurácia
acc.teste <- (matriz.conf.teste[1,1] + matriz.conf.teste[2,2]) / (matriz.conf.teste[1,1] + matriz.conf.teste[1,2] + matriz.conf.teste[2,1] + matriz.conf.teste[2,2]) * 100
print(acc.teste) 

install.packages("party")
library(party)

modeloCTree <- ctree(Status ~ ., data = banknote[base.treino,]) # constrói o modelo
plot(modeloCTree)

# aplica o modelo nos dados de teste
pred.teste <- predict(modeloCTree, newdata = banknote[-base.treino,])
# calcula e exibe a matriz de confusão
matriz.conf.teste <- table(banknote$Status[-base.treino], pred.teste, dnn=c("Classe Observada", "Classe Predita"))
print(matriz.conf.teste)

# calcula e exibe a acurácia
acc.teste <- (matriz.conf.teste[1,1] + matriz.conf.teste[2,2]) / (matriz.conf.teste[1,1] + matriz.conf.teste[1,2] + matriz.conf.teste[2,1] + matriz.conf.teste[2,2]) * 100
print(acc.teste)

## KNN

install.packages("rebmix")
data("wine", package = "rebmix")

boxplot(wine)

# pegamos apenas as características
atributos <- wine[, 1:13]
# normalizamos as características usando média 0 e desvio padrão 1
atributos <- scale(atributos)

boxplot(atributos)

set.seed(2016)

numLinhas = nrow(atributos)
baseTreino <- sample(1:numLinhas, 89, replace = FALSE)

install.packages("class")
library("class")

modeloKNN <- knn(train=atributos[baseTreino,], test=atributos[-baseTreino,], cl=wine$Cultivar[baseTreino], k=3)
                    
matrizConf <- table(modeloKNN, wine$Cultivar[-baseTreino])
print(matrizConf)

accTeste <- (matrizConf[1,1] + matrizConf[2,2] + matrizConf[3,3]) / (matrizConf[1,1] + matrizConf[1,2] + matrizConf[1,3] + matrizConf[2,1] + matrizConf[2,2] + matrizConf[2,3] + matrizConf[3,1] + matrizConf[3,2] + matrizConf[3,3]) * 100
print(round(accTeste,2))

modeloKNN <- knn(train=atributos[baseTreino,], test=atributos[-baseTreino,], cl=wine$Cultivar[baseTreino], k=5)

matrizConf <- table(modeloKNN, wine$Cultivar[-baseTreino])
print(matrizConf)

accTeste <- (matrizConf[1,1] + matrizConf[2,2] + matrizConf[3,3]) / (matrizConf[1,1] + matrizConf[1,2] + matrizConf[1,3] + matrizConf[2,1] + matrizConf[2,2] + matrizConf[2,3] + matrizConf[3,1] + matrizConf[3,2] + matrizConf[3,3]) * 100
print(round(accTeste,2))

modeloKNN <- knn(train=atributos[baseTreino,], test=atributos[-baseTreino,], cl=wine$Cultivar[baseTreino], k=7)
                    
matrizConf <- table(modeloKNN, wine$Cultivar[-baseTreino])
print(matrizConf)

accTeste <- (matrizConf[1,1] + matrizConf[2,2] + matrizConf[3,3]) / (matrizConf[1,1] + matrizConf[1,2] + matrizConf[1,3] + matrizConf[2,1] + matrizConf[2,2] + matrizConf[2,3] + matrizConf[3,1] + matrizConf[3,2] + matrizConf[3,3]) * 100
print(round(accTeste,2))

## Naïve Bayes (Bayes Ingênuo)

tabela = read.table("BD_aulaclass.csv", sep = ";", dec = ",", header = TRUE)
# Outra opção para leitura: tabela <- read.csv("https://raw.githubusercontent.com/tatianaesc/introdatascience/master/BD_aulaclass.csv", sep = ";", dec = ",", header = TRUE)

summary(tabela)

x <- tabela
x$ADIMPLENTE <- NULL
y <- tabela$ADIMPLENTE

set.seed(2018)
N = nrow(x)
baseTreino <- sample(1:N, N*0.75, FALSE)

install.packages("e1071")
library("e1071")

modeloNB <- naiveBayes(y[baseTreino]~., data = x[baseTreino,])
show(modeloNB)

probsTeste <- predict(modeloNB, x[-baseTreino,], type = "raw")
head(round(probsTeste,3),4)

classesTeste <- predict(modeloNB, x[-baseTreino,], type = "class")
head(classesTeste)

matrizConf <- table(classesTeste, y[-baseTreino])
print(matrizConf)

accTeste <- (matrizConf[1,1] + matrizConf[2,2]) / (matrizConf[1,1] + matrizConf[1,2] + matrizConf[2,1] + matrizConf[2,2]) * 100
print(round(accTeste,2))

install.packages("caret")
library("caret")

particoes <- createFolds(tabela$ADIMPLENTE, k=3)

tabTeste1 <- tabela[particoes$Fold1, ]
tabTreino1 <- tabela[-particoes$Fold1, ]

tabTeste2 <- tabela[particoes$Fold2, ]
tabTreino2 <- tabela[-particoes$Fold2, ]

tabTeste3 <- tabela[particoes$Fold3, ]
tabTreino3 <- tabela[-particoes$Fold3, ]

NB1 <- naiveBayes(ADIMPLENTE~IDADE, data = tabTreino1)
NB2 <- naiveBayes(ADIMPLENTE~IDADE, data = tabTreino2)
NB3 <- naiveBayes(ADIMPLENTE~IDADE, data = tabTreino3)

PREDNB1 <- predict(NB1, newdata = tabTeste1) 
PREDNB2 <- predict(NB2, newdata = tabTeste2) 
PREDNB3 <- predict(NB3, newdata = tabTeste3)

MATCONFNB1 <- table(PREDNB1, tabTeste1$ADIMPLENTE, deparse.level = 2)
MATCONFNB2 <- table(PREDNB2, tabTeste2$ADIMPLENTE, deparse.level = 2)
MATCONFNB3 <- table(PREDNB3, tabTeste3$ADIMPLENTE, deparse.level = 2)

show(MATCONFNB1)

show(MATCONFNB2)

show(MATCONFNB3)

ACC1 <- sum(diag(MATCONFNB1))/nrow(tabTeste1)
ACC2 <- sum(diag(MATCONFNB2))/nrow(tabTeste2)
ACC3 <- sum(diag(MATCONFNB3))/nrow(tabTeste3)

ACCFINAL <- ( ACC1 + ACC2 + ACC3 ) / 3
ACCFINAL*100

NB1 <- naiveBayes(ADIMPLENTE~ ., data = tabTreino1)
NB2 <- naiveBayes(ADIMPLENTE~ ., data = tabTreino2)
NB3 <- naiveBayes(ADIMPLENTE~ ., data = tabTreino3)

## Support Vector Machine (SVM)

tabela = read.table("BD_aulaclass.csv", sep = ";", dec = ",", header = TRUE)
summary(tabela)

x <- tabela
x$ADIMPLENTE <- NULL
y <- as.factor(tabela$ADIMPLENTE)

set.seed(2018)
N = nrow(x)
baseTreino <- sample(1:N, N*0.75, FALSE)

install.packages("kernlab")
install.packages("mlbench")
library(kernlab) 
library(mlbench)

modeloSVM <- ksvm(y[baseTreino]~., data=x[baseTreino,], kernel="rbfdot")
print(modeloSVM) 

classesTeste <- predict(modeloSVM, x[-baseTreino,], type="response") 

library("caret")

resultado <- confusionMatrix(classesTeste, y[-baseTreino])
resultado$table

resultado$overall[1]

modeloSVM <- ksvm(y[baseTreino]~., data=x[baseTreino,], kernel="polydot")
print(modeloSVM) 

classesTeste <- predict(modeloSVM, x[-baseTreino,], type="response") 

resultado <- confusionMatrix(classesTeste, y[-baseTreino])
resultado$table

resultado$overall[1]
