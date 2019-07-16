##### Introdução a R

## Criando estruturas de dados dentro do R


### Concatenando dados no R - função c()

# Dados das pessoas
idade = c(24, 48, 32, 65, 38, 56, 74, 19, 29, 22)
peso = c(80, 67, 49, 55, 89, 72, 45, 88, 56, 74)
altura = c(180, 165, 162, 175, 172, 165, 168, 185, 172, 168)
sexo = c('M','F','M','F','M','F','M','F','M','F')

### Inspecionando as estruturas

idade
show(idade)
sexo[3]
sexo[3:7]
sexo[c(3,5)]

### Alterando valores das estruturas e listando os objetos no ambiente

sexo
sexo[c(3,5)] = c("F", "M")
sexo

a = 2+3+4+5+6+7
rm(a)

a = 2+3+4+5+6+7
b = a*2
resultado = a+b
rm(a, b, resultado)

ls()

### Funções simples aplicadas em um objeto

length(idade)

idade
sort(idade)
sort(idade, decreasing = T)

str(idade)
str(sexo)

### Operadores lógicos

altura>170
sexo[altura>170]
table(sexo[altura>170])

## Trabalhando com Data Frames

### Criando um Data Frame

tabela = data.frame(altura, sexo, idade, peso)
View(tabela)


### Manipulando um data frame


str(tabela)
names(tabela)
names(tabela)[1] = "Height"
names(tabela)[1]
names(tabela)[1] = "altura"
names(tabela)[1]


head(tabela, n = 5)
tail(tabela, n = 5)
tabela$sexo
head(tabela$sexo, 7) # note que não é necessário colocar n = 7 para funcionar
tabela[1, 1] # altura da primeira pessoa
tabela[2, 1] # altura da segunda pessoa
tabela[1, 2] # sexo da primeira pessoa
tabela[, 3] # dispõe as idades de todas as pessoas
tabela[2, ] # apresenta as informações da segunda pessoa
tabela[, c(1, 3)] # apresenta somente a 1a e 3a coluna
tabela[, -2] # omite o sexo das pessoas 


### Exportando/Importando um data frame


write.csv(tabela, "BaseDados.csv")
rm(tabela)
tabela = read.csv("BaseDados.csv")
View(tabela)
tabela = tabela[, -1]
head(tabela) # por default, mostra as 6 primeiras linhas

