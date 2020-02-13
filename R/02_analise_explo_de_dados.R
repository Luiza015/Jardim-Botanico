# Aula 4 (13/02) Análise de dados exploratórios

#Parte 1
data ("anscombe")
dim(anscombe)
 #dimensao dos dados, N de linhas e N de colunas
head(anscombe)
 #seis primeiras linhas dos dados
class(anscombe)
 #classe do objeto
str(anscombe)
 #estrutura do objeto

#Selecionando a colunas dos dados;fazer a média das colunas com x.
mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)

#Função apply - A mesma tarefa mas agora com apenas uma linha de comando usando a função apply.

 ##media de todos os vetores x
apply(anscombe[,1:4], 2, mean) #aplica uma funcao a todas as colunas de um objeto
 ##media de todos os vetores y
apply(anscombe[,5:8], 2, mean)

#variancia dos dados (2 significa que estão sendo calculadas as colunas)
apply(anscombe, 2, var)

 #Correlação
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

#Coeficiente de regressão
 ##primeiro criamos objetos com as regressoes dos quatro conjuntos
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)

#Criar uma lista com todos para facilitar o trabalho
mlist <- list (m1, m2, m3, m4)
mlist
#Calculando de forma menos repetitiva os coeficientes de regressão
lapply(mlist, coef)

#Os dados têm mesma média, mesma variância, mesma correlação e mesmo valores dos coeficientes (intercepto e inclinação do modelo linear). Em que os dados são diferentes?
anscombe

#Os valores são diferentes, mas quão diferentes eles são? Para entender a função par, usar ?par
par(mfrow=c(2, 2), #abre uma janela gráfica com 2 linhas e 2 colunas
    las=1, #deixa as legendas dos eixos na vertical
    bty="l") #tipo do box do grafico em L
plot(anscombe$y1 ~ anscombe$x1)
abline(mlist[[1]]) #adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])
par(mfrow=c(1, 1)) #retorna a janela grafica para o padrao de 1 linha e 1 coluna

#Parte 2

?iris

head (iris)
summary (iris)

#Quantas informações por espécies?
table (iris$Species)

#Qual a média das variáveis por espécie? Vamos usar as funções agreggate e tapply. As duas funções são semelhantes, o que muda são os argumentos e o formato de saída de cada uma delas.

 #media do comprimento de sepala por especie
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)
 #a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)
 #ainda a mesma tarefa, com a mesma função mas em uma notação diferente
aggregate(Sepal.Length ~ Species, data = iris, mean)

aggregate(Sepal.Length ~ Species, data = iris, mean)
aggregate(Sepal.Width ~ Species, data = iris, mean)
aggregate(Petal.Length ~ Species, data = iris, mean)

#Calculando desvio padrão das variáveis

tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)

#Como calular a média por espécie de todas as variáveis. Para isso, vamos usar o comando 'for' e executar todas as tarefas em um mesmo ciclo.

 #criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol = 3, nrow = 4)
medias
 #Definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]
for (i in 1:4){
  medias[i,] <- tapply(iris[,i], iris$Species, mean)
}

#Média
vars <- iris[, -5]
apply(vars,2,mean)

#Mediana
apply(vars, 2, median)

#Moda
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1]

#Medidas de dispersão
##variancia: desvio da média
apply(vars, 2, var)

#desvio padrão: raiz quadrada da variância
sd01 <- apply(vars, 2, sd)
sd01

#Não existe no R base uma função para calcular o coeficiente de variação. Isto não é um problema. Vamos formalmente criar nossa primeira função de R. Para isso, usamos a função function

cv <- function(x){
  sd(x)/mean(x)*100
}
cv
apply(vars, 2, cv)

#Qauartis ou percentis- Por padrão, a função quantile retorna o mínimo, o 25º percentil, a mediana, o 50º percentil, o 75º percentil e o máximo, também conhecidos pelo sumário de cinco números proposto por Tuckey (que também é o retorno da função summary de um vetor numérico). Os cinco números dividem os dados em quatro quantis, que, por isso são chamados de quartis. Os quartis são uma métrica bastante útil para descrever os dados pois possuem uma interpretação simples e não são afetados pela distribuição dos dados. É possível modificar os percentis desejados com o argumento probs.

# sumario de 5 numeros
apply(vars, 2, quantile)
# 5%, 50% e 95%
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))

#Intervalo- O intervalo é a diferença entre o maior e o menor valor de determinada variável.

# a funcao range nos retorna os valores minimo e maximo
apply(vars, 2, range)
# aplicando a funcao diff ao resultado do range, temos o valor desejado
# uma boa pratica é nunca sobrescrever um objeto já existente no R, por isso
# nunca nomeie um objeto com um nome já existente
my_range <- function(x){
  diff(range(x))
}
apply(vars, 2, my_range)

#Intervalo interquartil (IIQ)- O IIQ é a diferença entre o quartil superior (75%) e o quartil inferior (25%).

apply(vars, 2, IQR)

#Correlação- uma matriz de correlação é uma tabela com os valores de correlação entre cada uma das variáveis par a par. As variáveis podem ser correlacionadas positivamentes (valores positivos) ou negativamente (valores negativos)

cor(vars)

#Gráfico de barras- um gráfico de barras mostra a frequência de observações em uma dada classe.

barplot(table(iris$Species))

#Histograma- o histograma é o equivalente do gráfico de barras para variáveis contínuas. Cada barra representa um intervalo de valores. O número de intervalos pode ser especificado pelo usuário e afeta a percepção da distribuição dos dados.

par(mfrow=c(2, 2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)

par(mfrow=c(1, 1))

#Agora para o comprimento da sépala das espécies de Iris, vamos ver o efeito do número de intervalos no histograma com o argumento breaks.

par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)
par(mfrow=c(1, 1))

#Curva de densidade- mostra a probabilidade de observar determinado valor. Em comparação ao histograma, no eixo y, ao invés de termos a frequência, temos a densidade probabilística.

par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)
par(mfrow=c(1, 1))

#No R podemos ver a curva de densidade a usando a função por meio do plot da função density.

par(mfrow=c(1, 2))
#plot da curva de densidade
plot(density(iris$Sepal.Width))
#plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="blue") #note que agora estamos usando a funcao o comando add=TRUE
par(mfrow=c(1, 1))

#Box-plots são gráficos que representam o sumário de cinco números de Tuckey mostrando os quartis (25%, 50% e 75%), valores mínimos, máximo e outliers. O box-plot mostra a forma da distrubuição dos dados, a forma da distribuição e a habilidade de comparar com outras variáveis na mesma escala.

#Vamos agora fazer os box-plots das variáveis contidas no objeto iris. Vamos começar com as variáveis gerais.
par(mfrow=c(1,1))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

#Agora vamos olhar para os valores por espécie.

boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Petal.Width ~ Species, data = iris)

#Você identifica outliers no conjunto de dados? Como podemos checar os outliers? Vamos usar a própria função boxplot para identificar os outliers.

boxplot(iris$Sepal.Width)
my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers <- my_boxplot$out
#qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)
# vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

boxplot(Sepal.Width ~ Species, data = iris)
my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out
# neste caso, queremos apenas os outliers da especie setosa
# vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 &
       iris$Species == "setosa",
     c("Sepal.Width", "Species")]

