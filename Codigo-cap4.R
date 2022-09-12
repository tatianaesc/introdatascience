##### Pré-processamento de dados

## Importação de Dados

# Lê de um arquivo ou URL e armazena o resultado em um novo data frame dados
dados <- read.table(
  'https://raw.githubusercontent.com/tatianaesc/datascience/main/iris.csv',
  sep=',', header=T
)

# mostra o tipo do objeto
class(dados) 
# mostra a distribuição de cada variável do dataset
summary(dados) 
# mostra quantas linhas e colunas há nos dados
dim(dados) 

dados2 <- read.table(“iris.csv”, sep=',', header=T)

dados3 <- iris

## Análise Exploratória

# cria um histograma para cada atributo
par(mfrow=c(1,4))
for(i in 1:4) {
  hist(dados[,i], main=names(dados)[i])
}

# carrega o pacote 
library(lattice)
# cria um gráfico de densidade para cada atributo
par(mfrow=c(1,4))
for(i in 1:4) {
  plot(density(dados[,i]), main=names(dados)[i])
}

# cria um boxplot para cada atributo
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(dados[,i], main=names(dados)[i])
}


# instala e carrega o pacote 
install.packages("corrplot")
library(corrplot)
# calcula as correlações
correlations <- cor(dados[,1:4])
# cria o gráfico de correlação
corrplot(correlations, method="circle")


# cria um gráfico de dispersão para cada um dos pares de atributos
pairs(dados[1:4]) 

# colore o gráfico de dispersão por classes
pairs(dados[1:4], pch=21, bg=c("red", "green3", "blue")[iris$Species])

# instala e carrega o pacote 
install.packages("caret")
library(caret)
# cria um gráfico de densidade para cada atributo, separado por classe
x <- dados[,1:4]
y <- iris$Species
escalas <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=escalas)

# cria um boxplot cada atributo, separado por classe
featurePlot(x=x, y=y, plot="box")


## Preparação e Limpeza

# carrega os dados do banco de crédito alemão
dados4 <- read.table('custdata.tsv',header = T,sep = '\t')
summary(dados4)

# normaliza os dados de idade dos clientes
summary(dados4$age)
idadeMedia <- mean(dados4$age)
dados4$age.normalized <- dados4$age/idadeMedia
summary(dados4$age.normalized)
