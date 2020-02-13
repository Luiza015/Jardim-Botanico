#Aula 4 - Criando e salvando gráficos no R
#Carregue o arquivo contendo os dados ambientais de cada localidade da aula de ontem e os dados de comunidade (envir e comm).

#Vamos extrair a riqueza de espécies por Site e plotar a riqueza de espécies em função do teor de argila (clay), silte (silt) e areia (sand) do solo.
comm <- read.csv("data/cestes/comm.csv")
envir <- read.csv("data/cestes/envir.csv")
comm
envir

comm.pa <- comm[, -1] > 0
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
head(rich)
summary(rich)
boxplot(rich)
boxplot(rich, las = 1)

#Criando uma nova tabela com a coluna de riqueza- vamos acrescentar a coluna de riqueza à tabela de variáveis ambientais num objeto novo que se chame localidades
localidades <- cbind(envir, rich)
head(localidades)

#Gráfico de dispersão
#criando modelos lineares
riqsilt <- lm(rich ~ Silt, data = localidades)
riqclay <- lm(rich ~ Clay, data = localidades)
riqsand <- lm(rich ~ Sand, data = localidades)

#extraindo os coeficientes do modelo
coef_s <- coef(riqsilt)
coef_c <- coef(riqclay)
coef_d <- coef(riqsand)

#definindo os limites dos eixos
limy <- c(min(localidades$rich),
          max(localidades$rich))
limx <- c(min(localidades[,c("Clay", "Sand", "Silt")]),
          max(localidades[,c("Clay", "Sand", "Silt")]))
## definindo o nome do eixo y
laby <- "Riqueza de espécies"

#Agora vamos construir o gráfico em si. Procure o significado dos parâmetros mfrow e bty na função par()
# define parametros graficos

par(mfrow = c(1, 3),
    las = 1,
    bty = "l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas
# plot da riqueza em função do teor de Silte
plot(rich ~ Silt, data = localidades,
     col = "tomato",
     ylim = limy, xlim = limx,
     ylab = laby,
     xlab = "Teor de Silte (%)")
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_s[1], b = coef_s[2],
       col = 'tomato', lwd = 2)
mtext("A", 3, adj = 0, font = 2)

## plot da riqueza em função do teor de Argila
plot(rich ~ Clay, data = localidades,
     col = "navy",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Argila (%)")
mtext("B", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_c[1],
       b = coef_c[2],
       col = 'navy',
       lwd = 2)

## plot da riqueza em função do teor de Areia
plot(rich ~ Sand, data = localidades,
     col = "dodgerblue",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Areia (%)")
mtext("C", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_d[1],
       b = coef_d[2],
       col = 'dodgerblue',
       lwd = 2)
#Exportando o gráfico com as funções png() e dev.off()
#Para exportar o gráfico vamos usar a função png(), especificando a resolução e dimensões da figura. Quando criamos gráficos com a função png() o que fazemos é:

##1- anunciar qual extensão e arquivo vamos plotar o gráfico com a função png()

##2- determinar a sequência de comandos que cria o gráfico

##3- finalizar a construção do arquivo com a função dev.off()
