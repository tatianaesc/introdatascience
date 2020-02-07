cat("\014")  

#install.packages("mlbench")

# 1. Preparação do problema
set.seed(7) # definição de semente

# a) Carga de pacotes
library(mlbench)
library(caret)

# b) Carga de dataset
data(BreastCancer)

# c) Divisão do dataset em conjunto de treino e teste (validação final)
# separação do dataset em 80% para treinamento (usado para treinar e testar os modelos) 
# e 20% para teste (validação final) do modelo
indiceTeste <- createDataPartition(BreastCancer$Class, p=0.80, list=FALSE)
conjTeste <- BreastCancer[-indiceTeste,] # conjunto de teste
dataset <- BreastCancer[indiceTeste,] # conjunto de treinamento


# 2. Análise exploratória de dados

# a) Estatísticas Descritivas
dim(dataset) # dimensões do dataset
head(dataset, n=10) # 10 primeiras linhas
sapply(dataset, class) # tipos dos atributos

# Remoção do atributo Id
dataset <- dataset[,-1]

# Conversão dos valores de entrada para numéricos
for(i in 1:9) {
  dataset[,i] <- as.numeric(as.character(dataset[,i]))
}

summary(dataset) # resumo estatístico

cbind(freq=table(dataset$Class), 
      percentage=prop.table(table(dataset$Class))*100) # distribuição de classes

# Sumarização das correlações entre os atributos de entrada
complete_cases <- complete.cases(dataset)
cor(dataset[complete_cases,1:9])

# b) Visualizações de Dados

# Visualizações unimodais

# Histogramas de cada atributo
par(mfrow=c(3,3))
for(i in 1:9) {
  hist(dataset[,i], main=names(dataset)[i])
}

# Gráficos de densidade de cada atributo
par(mfrow=c(3,3))
complete_cases <- complete.cases(dataset)
for(i in 1:9) {
  plot(density(dataset[complete_cases,i]), main=names(dataset)[i])
}

# Boxplots de cada atributo
par(mfrow=c(3,3))
for(i in 1:9) {
  boxplot(dataset[,i], main=names(dataset)[i])
}

# Visualizações multimodais

# Matriz scatter plot
jittered_x <- sapply(dataset[,1:9], jitter)
pairs(jittered_x, names(dataset[,1:9]), col=dataset$Class)

# Gráfico de barras de cada atributo por classe
par(mfrow=c(3,3))
for(i in 1:9) {
  barplot(table(dataset$Class,dataset[,i]), main=names(dataset)[i],
          legend.text=unique(dataset$Class))
}

# 3. Pré-processamento
# a) Limpeza de Dados
# b) Seleção de atributos
# c) Transformação de Dados

# 4. Modelagem e Inferência

# a) Escolha de procedimentos e métricas de avaliação
# validação cruzada 10-fold com 3 repetições
configTreino <- trainControl(method="repeatedcv", number=10, repeats=3) 
metrica <- "Accuracy" # definição da métrica de avaliação

# b) Criação de Algoritmos

# Regressão Logística
modelo.glm <- train(Class~., data=dataset, method="glm", metric=metrica,
                 trControl=configTreino, na.action=na.omit)
# KNN
modelo.knn <- train(Class~., data=dataset, method="knn", metric=metrica,
                 trControl=configTreino, na.action=na.omit)

# Árvore de Classificação CART
modelo.cart <- train(Class~., data=dataset, method="rpart", metric=metrica,
                  trControl=configTreino, na.action=na.omit)
# Naive Bayes
modelo.nb <- train(Class~., data=dataset, method="nb", metric=metrica, trControl=configTreino,
                na.action=na.omit)
# SVM
modelo.svm <- train(Class~., data=dataset, method="svmRadial", metric=metrica,
                 trControl=configTreino, na.action=na.omit)

# c) Comparação de Algoritmos

resultados <- resamples(list(LG=modelo.glm, KNN=modelo.knn,
                          CART=modelo.cart, NB=modelo.nb, SVM=modelo.svm))
summary(resultados) # resultados
dotplot(resultados) # resultados gráficos

# d) Criação de novos modelos aplicando a transformação Box-Cox nos dados

# LG
modelo.glm <- train(Class~., data=dataset, method="glm", metric=metrica, preProc=c("BoxCox"),
                 trControl=configTreino, na.action=na.omit)
# KNN
modelo.knn <- train(Class~., data=dataset, method="knn", metric=metrica, preProc=c("BoxCox"),
                 trControl=configTreino, na.action=na.omit)
# CART
modelo.cart <- train(Class~., data=dataset, method="rpart", metric=metrica,
                  preProc=c("BoxCox"), trControl=configTreino, na.action=na.omit)
# Naive Bayes
modelo.nb <- train(Class~., data=dataset, method="nb", metric=metrica, preProc=c("BoxCox"),
                trControl=configTreino, na.action=na.omit)
# SVM
modelo.svm <- train(Class~., data=dataset, method="svmRadial", metric=metrica,
                 preProc=c("BoxCox"), trControl=configTreino, na.action=na.omit)

# Comparação de Algoritmos
todosResultados <- resamples(list(LG=modelo.glm, KNN=modelo.knn,
                                   CART=modelo.cart, NB=modelo.nb, SVM=modelo.svm))
summary(todosResultados)
dotplot(todosResultados)

# e) Melhoria do desempenho dos modelos através de ajuste de hiperparâmetros

# Tuning do SVM com Grid Search
grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 10, by=1))
modelo.svm <- train(Class~., data=dataset, method="svmRadial", metric=metrica, tuneGrid=grid,
                 preProc=c("BoxCox"), trControl=configTreino, na.action=na.omit)
print(modelo.svm) # resultados
plot(modelo.svm) # resultados gráficos

# Tuning do KNN com Grid Search
grid <- expand.grid(.k=seq(1,20,by=1))
modelo.knn <- train(Class~., data=dataset, method="knn", metric=metrica, tuneGrid=grid,
                 preProc=c("BoxCox"), trControl=configTreino, na.action=na.omit)
print(modelo.knn) # resultados
plot(modelo.knn) # resultados gráficos

# 5. Pós-processamento

# a) Escolha e construção do modelo final com todo o conjunto de treinamento

# Transformação de dados
datasetSemMissing <- dataset[complete.cases(dataset),]
x <- datasetSemMissing[,1:9]
parametrosPreProcessamento <- preProcess(x, method=c("BoxCox"))
x <- predict(parametrosPreProcessamento, x)

# Preparação do conjunto de teste (para validação final)
conjTeste <- conjTeste[,-1] # remoção do atributo Id
conjTeste <- conjTeste[complete.cases(conjTeste),] # remoção dos valores faltantes

for(i in 1:9) { # conversão dos valores de entrada para numéricos
  conjTeste[,i] <- as.numeric(as.character(conjTeste[,i]))
}
conjTesteX <- predict(parametrosPreProcessamento, conjTeste[,1:9]) # transformação de dados

# b) Predições no conjunto de teste (validação final)
predicoes <- knn3Train(x, conjTesteX, datasetSemMissing$Class, k=9, prob=FALSE) # predições
confusionMatrix(as.factor(predicoes), conjTeste$Class) # matriz de confusão

# c) Salvamento do modelo para uso posterior

# Salvamento do modelo
modeloFinal <- modelo.knn
saveRDS(modeloFinal, "./modeloFinal.rds")

#... no futuro, poderemos carregar o modelo
modeloCarregado <- readRDS("./modeloFinal.rds")

