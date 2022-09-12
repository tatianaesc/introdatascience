# Práticas de Regressão

tabela <- read.table("faturamento.csv", sep = ";",
                    dec = ",", header = TRUE)

 # determina os índices com valores faltantes de faturamento
idxNA <- is.na(tabela$FAT)
# dados com valores faltantes (NA) de faturamento
tabelaNA <- tabela[idxNA, ]
# dados sem valores faltantes de faturamento
tabela <- na.omit(tabela)

install.packages("ggplot2")
library("ggplot2")

estetica = aes(x = RENDA, y = FAT)
base = ggplot(data = tabela, estetica)
base + geom_point(size = 4)

estetica = aes(x = RENDA, y = FAT)
base = ggplot(data = tabela, estetica)
base + geom_point(size = 4) + stat_smooth(method = "lm", formula = y ~ 1, lwd = 2)

estetica = aes(x = RENDA, y = FAT)
base = ggplot(data = tabela, estetica)
base + geom_point(size = 4) + stat_smooth(method = "lm", formula = y ~ x, lwd = 2)

library("caret")
idxcv = createFolds(tabela$FAT, k = 3)

tabTeste1 = tabela[idxcv$Fold1, ]
tabTreino1 = tabela[-idxcv$Fold1, ]

tabTeste2 = tabela[idxcv$Fold2, ]
tabTreino2 = tabela[-idxcv$Fold2, ]

tabTreino3 = tabela[-idxcv$Fold3, ]
tabTeste3 = tabela[idxcv$Fold3, ]

reg1 = lm(FAT ~ RENDA + POP + ESPVIDA + 
            TXALFA + NCONC + AREA + 
            PRECOM2 + ROUBOSLOJAS + 
            ROUBOSRES, data = tabTreino1)

reg2 = lm(FAT ~ RENDA + POP + ESPVIDA + 
            TXALFA + NCONC + AREA + 
            PRECOM2 + ROUBOSLOJAS + 
            ROUBOSRES, data = tabTreino2)

reg3 = lm(FAT ~ RENDA + POP + ESPVIDA + 
            TXALFA + NCONC + AREA + 
            PRECOM2 + ROUBOSLOJAS + 
            ROUBOSRES, data = tabTreino3)

summary(reg1)

summary(reg2)

summary(reg3)

predts1 = as.numeric(predict(reg1, newdata = tabTeste1))
predts2 = as.numeric(predict(reg2, newdata = tabTeste2))
predts3 = as.numeric(predict(reg3, newdata = tabTeste3))

# RMSE
RMSE1 = sqrt(mean((tabTeste1$FAT - predts1)^2))
RMSE2 = sqrt(mean((tabTeste2$FAT - predts2)^2))
RMSE3 = sqrt(mean((tabTeste3$FAT - predts3)^2))
 # RMSE médio
RMSERL = (RMSE1 + RMSE2 + RMSE3)/3

# R2
R2reg1 = cor(tabTeste1$FAT, predts1)^2
R2reg2 = cor(tabTeste2$FAT, predts2)^2
R2reg3 = cor(tabTeste3$FAT, predts3)^2
# R2 médio
R2RL = (R2reg1 + R2reg2 + R2reg3)/3

estetica = aes(y = tabTeste1$FAT, 
               x = predts1)
ggplot(tabTeste1, estetica) + 
  geom_point(size = 4) + 
  stat_smooth(method = "lm", 
              formula = y ~ x, lwd = 2)

library("party")

controle = ctree_control(mincriterion = 0.95)

AR1 = ctree(FAT ~ POP + RENDA + ESPVIDA + 
              TXALFA + NCONC + AREA + PRECOM2 +
              ROUBOSLOJAS + ROUBOSRES, 
            data = tabTreino1, controls = controle)

AR2 = ctree(FAT ~ POP + RENDA + ESPVIDA + 
              TXALFA + NCONC + AREA + PRECOM2 +
              ROUBOSLOJAS + ROUBOSRES, 
            data = tabTreino2, controls = controle)

AR3 = ctree(FAT ~ POP + RENDA + ESPVIDA + 
              TXALFA + NCONC + AREA + PRECOM2 +
              ROUBOSLOJAS + ROUBOSRES, 
            data = tabTreino3, controls = controle)

show(AR1)

plot(AR1, terminal_panel =  node_terminal)

predAR1 = as.numeric(predict(AR1, newdata = tabTeste1))
predAR2 = as.numeric(predict(AR2, newdata = tabTeste2))
predAR3 = as.numeric(predict(AR3, newdata = tabTeste3))

#  RMSE
RMSEAR1 = sqrt(mean((tabTeste1$FAT - predAR1)^2))
RMSEAR2 = sqrt(mean((tabTeste2$FAT - predAR2)^2))
RMSEAR3 = sqrt(mean((tabTeste3$FAT - predAR3)^2))
RMSEAR = (RMSEAR1 + RMSEAR2 + RMSEAR3)/3 # RMSE Médio

# R^2
R2AR1 = cor(tabTeste1$FAT, predAR1)^2
R2AR2 = cor(tabTeste2$FAT, predAR2)^2
R2AR3 = cor(tabTeste3$FAT, predAR3)^2
R2AR = (R2AR1 + R2AR2 + R2AR3)/3 # R^2 Médio

install.packages("FNN")
library("FNN")

kvalue = 33

KNN1 = knn.reg(train = tabTreino1[, 2:10],
               y = tabTreino1[, 11],
               test = tabTeste1[, 2:10],
               k = kvalue)

KNN2 = knn.reg(train = tabTreino2[, 2:10],
               y = tabTreino2[, 11],
               test = tabTeste2[, 2:10],
               k = kvalue)

KNN3 = knn.reg(train = tabTreino3[, 2:10],
               y = tabTreino3[, 11],
               test = tabTeste3[, 2:10],
               k = kvalue)

predKNN1 = KNN1$pred
predKNN2 = KNN2$pred
predKNN3 = KNN3$pred

# RMSE
RMSEKNN1 = sqrt(mean((tabTeste1$FAT - predKNN1)^2))
RMSEKNN2 = sqrt(mean((tabTeste2$FAT - predKNN2)^2))
RMSEKNN3 = sqrt(mean((tabTeste3$FAT - predKNN3)^2))
RMSEKNN = (RMSEKNN1 + RMSEKNN2 + RMSEKNN3)/3 # RMSE médio

# R^2
R2KNN1 = cor(tabTeste1$FAT, predKNN1)^2
R2KNN2 = cor(tabTeste2$FAT, predKNN2)^2
R2KNN3 = cor(tabTeste3$FAT, predKNN3)^2
R2KNN = (R2KNN1 + R2KNN2 + R2KNN3)/3 # R2 médio

tabela_resultados <- c("Modelo", "RMSE", "R2")
resultadosRL <- c("Regressão Linear", RMSERL, R2RL)
resultadosAR <- c("Árvore", RMSEAR, R2AR)
resultadosKNN <- c("KNN", RMSEKNN, R2KNN)
tabela_resultados <- rbind(tabela_resultados, resultadosRL, resultadosAR, resultadosKNN)
print(tabela_resultados)

# Criação do modelo
regfinal = lm(FAT ~ RENDA + POP + ESPVIDA + 
            TXALFA + NCONC + AREA + 
            PRECOM2 + ROUBOSLOJAS + 
            ROUBOSRES, data = tabela)

# Predição de Teste
predFATNA = predict(regfinal, newdata = tabelaNA)
predFATNA = as.numeric(predFATNA)
tabelaNA$FAT_ESTIMADO = predFATNA

# Exportação dos resultados para planilha
write.csv2(tabelaNA, "faturamentoEstimado.csv")


## Regressão Logística


library("mlbench")
data(PimaIndiansDiabetes)
tabela = as.data.frame(PimaIndiansDiabetes)

set.seed(7)
N = nrow(tabela)
sorteio <- sample(1:N, N*0.75, FALSE)
baseTreino <- tabela[sorteio, ]
baseTeste <- tabela[-sorteio, ]

# Treina o modelo
fit <- glm(diabetes~., data=baseTreino, family=binomial(link='logit'))
print(fit)

# Aplica o modelo nos dados de treino
probabilitiesTr <- predict(fit, baseTreino[,1:8], type='response') 
predictionsTr <- ifelse(probabilitiesTr > 0.5,'pos','neg')

# Aplica o modelo nos dados de teste
probabilitiesTst <- predict(fit, baseTeste[,1:8], type='response') 
predictionsTst <- ifelse(probabilitiesTst > 0.5,'pos','neg')

library("caret")

# Resultados Treino
resultadoTr <- caret::confusionMatrix(table(predictionsTr, baseTreino$diabetes))
resultadoTr$table # exibe a matriz de confusão
resultadoTr$overall[1] # exibe a acurácia

# Resultados Teste
resultadoTst <- caret::confusionMatrix(table(predictionsTst, baseTeste$diabetes))
resultadoTst$table # exibe a matriz de confusão
resultadoTst$overall[1] # exibe a acurácia

