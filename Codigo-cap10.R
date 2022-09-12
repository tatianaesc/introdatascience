# Práticas de Associação e Agrupamento

## Apriori

tabela = read.table("ExAssocAula.csv", sep = ";", dec = ",", header = T)

install.packages("arulesViz")
library("arulesViz")

tabela = tabela[, -1] # remove somente a primeira coluna
names(tabela)

tabela$TV = as.logical(tabela$TV)
tabela$Tablet = as.logical(tabela$Tablet) 
tabela$Smartphone = as.logical(tabela$Smartphone)

tabela = as(tabela, "transactions")

param = list(supp = 1/7, conf = 2/7, minlen = 2, maxlen = 3, ext = T)
ruleset = apriori(tabela, parameter = param)

summary(ruleset)

inspect(ruleset)

ruleset = sort(ruleset, by = c("confidence", "support", "coverage")) 
inspect(ruleset)

tabela = read.table("ExRealAssoc.csv", sep = ";", dec = ",", header = T)

sort(table(tabela$Segmento), decreasing = T)

ClientesCat = discretize(tabela$NK, method="interval", breaks = 3, labels = c("Baixo", "Médio", "Alto"))

# mostra a quantidade de elementos em cada categoria
table(ClientesCat)

# mostra os valores das faixas
discretize(tabela$NK, method="interval", breaks = 3, onlycuts=T) 

# Legenda: VB = Volume Baixo, VM = Volume Médio e VA = Volume Alto
CatFLO = discretize(tabela$FLO, method="interval", breaks = 3, labels = c("VB", "VM", "VA")) 
CatJY = discretize(tabela$JY, method="interval", breaks = 3, labels = c("VB", "VM", "VA")) 
CatKIV = discretize(tabela$KIV, method="interval", breaks = 3, labels = c("VB", "VM", "VA")) 
CatLEV = discretize(tabela$LEV, method="interval", breaks = 3, labels = c("VB", "VM", "VA")) 
CatLIP = discretize(tabela$LIP, method="interval", breaks = 3, labels = c("VB", "VM", "VA")) 
CatNAR = discretize(tabela$NAR, method="interval", breaks = 3, labels = c("VB", "VM", "VA")) 
CatNK = discretize(tabela$NK, method="interval", breaks = 3, labels = c("VB", "VM", "VA")) 
CatSPD = discretize(tabela$SPD, method="interval", breaks = 3, labels = c("VB", "VM", "VA")) 
CatSUM = discretize(tabela$SUM, method="interval", breaks = 3, labels = c("VB", "VM", "VA")) 
CatTRP = discretize(tabela$TRP, method="interval", breaks = 3, labels = c("VB", "VM", "VA")) 

Seg = tabela$Segmento # separa a variável Segmento
tabitens = data.frame(Seg, CatFLO, CatJY, CatKIV, CatLEV, CatLIP, CatNAR, CatNK, CatSPD, CatSUM, CatTRP)

tabitens = as(tabitens, "transactions")

param = list(supp = 0.5, conf = 0.9, minlen = 2, maxlen = 5, ext = TRUE)
ruleset = apriori(tabitens, parameter = param)
summary(ruleset)

param = list(supp = 0.9, conf = 1, minlen = 2, maxlen = 3, ext = TRUE)
ruleset = apriori(tabitens, parameter = param)
summary(ruleset)

ruleset = sort(ruleset, by = c("confidence", "support", "coverage")) 
inspect(ruleset)

inspect(head(sort(ruleset),10))

subruleset = head(sort(ruleset, by="confidence"), 10)
plot(subruleset, method="graph", measure=c("confidence"), shading="support")


## K-means

library(datasets)
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster

table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()

# Limpa a console
cat("\014")
# Limpa o global environment
rm(list = ls())
# Carrega um novo conjunto de dados
mydata = cars

mydata <- na.omit(mydata) # eliminação de missings
mydata <- scale(mydata) # padronização de variáveis

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(mydata, 5)

install.packages("fpc")
library(fpc)
plotcluster(mydata, fit$cluster)

aggregate(mydata,by=list(fit$cluster),FUN=mean)

mydata <- data.frame(mydata, fit$cluster)
mydata
