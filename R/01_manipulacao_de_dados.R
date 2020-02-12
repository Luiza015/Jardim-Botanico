# Script para manipulação de dados em bases relacionais ---#
# parte do curso Projetos de análise de dados em R
# dados originais extraídos de Jeliazkov et al 2020 Sci Data
# (https://doi.org/10.1038/s41597-019-0344-7)
# primeira versão em 2020-02-12
#-----------------------------------------------------------#
library(tidyr)
?list.files
files.path <- list.files (path = "data/cestes",
                          pattern = ".csv",
                          full.names = TRUE)
files.path

#O objeto files.path é um vetor de cinco elementos (afinal são cinco arquivos) contendo o nome completo do arquivo. Vamos usar o conteúdo desse vetor na função read.csv()

comm <- read.csv(files.path [1])
comm
coord <- read.csv(files.path [2])
coord
envir <- read.csv(files.path [3])
envir
splist <- read.csv(files.path [4])
splist
traits <- read.csv(files.path [5])
traits

#Vamos aplicar as funções head(), dim() e summary() para inspecionar todos os arquivos; head (cabeçalho); dim (número de linhas e colunas); summary (média, mediana, etc).

head (comm)
dim (comm)
summary (comm)

head (coord)
dim (coord)
summary (coord)

head (envir)
dim (envir)
summary (envir)

head (splist)
dim (splist)
summary (splist)

head (traits)
dim (traits)
summary (traits)

#Temos dados de quantas espécies? Podemos simplesmente contar o número de linhas do objeto splist

nrow (splist)

#Quantas áreas amostradas? Podemos contar o número de linhas dos objetos comm ou envir

nrow (comm)
nrow (envir)

#Quantas variáveis ambientais?
  #todas as variáveis exceto a primeira coluna com o id

names(envir)[-1]

 #contando quantas variáveis

length(names(envir)[-1])

#Qual a riqueza de cada área? Primeiro, precisamos transformar a nossa matriz que possui dados de abundância em uma matriz de presença e ausência.

comm.pa <- comm[, -1] > 0
comm.pa

 #vamos nomear as linhas das planilhas com o id dos sites

row.names (comm.pa) <- envir$Sites
row.names (comm.pa)

#No R, os valores de TRUE e FALSE contam como 1 e 0. Vamos calcular a riqueza da área 1, por exemplo, somando a primeira linha do novo objeto comm.pa.

sum(comm.pa[1,])

#Como podemos fazer a soma de forma automatizada para as 97 áreas? Podemos usar a função apply. Essa função aplica uma função às linhas ou colunas de um objeto (do tipo data.frame ou matrix).

rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
rich

#aplicar nessa planilha comm.pa em todas as linhas a função soma

summary(rich)

#Sabemos agora que a riqueza média das áreas é 6.3, o valor mínimo é 1 e o máximo, 15, através do summary
 #tem alguma área com uma espécie só? qual área tem só uma espécie?

sum(rich == 1)
which(rich == 1)

#Juntando diferentes tabelas através de identificadores comuns - Vamos usar a função merge() do pacote base para adicionar a coluna de coordenadas ao objeto contendo as variáveis ambientais. Esta função irá combinar duas planilhas por meio de um identificador comum, que é a chave primária. No caso do objeto envir a chave primária é a coluna Sites que contém a numeração das localidades amostradas. Podemos chamar essa coluna usando o operador $.

envir$Sites
summary(envir$Sites)

#transformando tipos variáveis - a  coluna Sites que representa uma variável categórica com o id de cada área está sendo entendida como uma variável numérica. Vamos converter essa coluna para um fator, dessa forma irá representar melhor o significado da variável que é simplesmente o id de cada área. Para isso, usamos a função factor()

 #se checarmos a classe desse vetor, veremos que é numerica
class(envir$Sites)

 #queremos que seja uma variável categórica. Para isso, convertemos em fator
as.factor(envir$Sites)

 #se usarmos apenas as.factor, não fazemos a conversão, vamos então fazer uma atribuição
envir$Sites <- as.factor(envir$Sites)
envir$Sites

coord$Sites <- as.factor(coord$Sites)
coord$Sites

#Aplicando a função merge (junta as duas planilhas)

envir.coord <- merge(x = envir,
                     y = coord,
                     by = "Sites")
envir.coord

#Podemos checar a junção com as funções dim() e head().

dim(envir)
dim(coord)
dim(envir.coord)
head(envir.coord)

#Transformando uma mátrix espécie vs. área em uma tabela de dados - Agora, queremos transformar a nossa matriz de espécie vs. área em uma planilha que contenha cada observação em uma linha e cada variável em uma coluna. Para fazer essa transformação iremos usar a função gather() do pacote tidyr. Como temos 97 sites e 56 espécies, terminaremos com um objeto com 5432 linhas (97 x 56).

 #vetor contendo todos os Sites
Sites <- envir$Sites
Sites
length(Sites)

 #vetor número de espécies
n.sp <- nrow(splist)
n.sp

 #criando tabela com cada especie em cada area especies em linhas
comm.df <- tidyr::gather(comm[, -1])
comm.df

dim(comm.df)
head(comm.df)

#Queremos alterar o nome das colunas de comm.df. Para isso, usaremos a função colnames().

 #nomes atuais
colnames(comm.df)
 #modificando os nomes das colunas
colnames(comm.df) <-  c("TaxCode", "Abundance")
 #checando os novos nomes
colnames(comm.df)

#Queremos agora adicionar a coluna Sites ao novo objeto. Vamos usar a função rep(). Esta função cria sequências. Vamos criar uma sequência de localidades, em que cada uma das 97 localidades se repete 56 vezes.

 #primeiro criamos a sequência
seq.site <- rep(Sites, times = n.sp)
seq.site
 #checando a dimensão
length(seq.site)
 #adicionando ao objeto comm.df
comm.df$Sites <- seq.site
 #checando como ficou
head(comm.df)

#Juntando todas as variáveis à comm.df - Para terminar, vamos juntar splist, traits e envir.coord à planilha comm.df. Como vimos na aula, as relações entre duas tabelas são sempre feitas par a par. Então, vamos juntar par a par as tabelas usando a função merge().

#Comm.df e splist - usando a coluna TaxCode.

comm.sp <- merge(comm.df, splist, by = "TaxCode")
head(comm.sp)

#Comm.sp e traits - Segundo, adicionamos os dados de atributos das espécies à tabela de comunidade. Na tabela traits, a coluna que identifica as espécies é chamada Sp. Antes de fazer a junção, precisamos mudar o nome para bater com o nome da coluna em comm.sp que é TaxCode.

names(traits)
 #renomeando o primeiro elemento
colnames(traits)[1] <- "TaxCode"
colnames(traits)[1]
comm.traits <- merge(comm.sp, traits, by = "TaxCode")
head(comm.traits)

#Comm.traits e envir.coord - untamos as variáveis ambientais (que aqui já contém as coordenadas) à tabela geral da comunidade por meio da coluna Sites

comm.total <- merge(comm.traits, envir.coord, by = "Sites")
head(comm.total)

#Exportar a planilha final modificada. Para isso, usamos a função write.csv().

write.csv(x = comm.total,
         file = "data/01_data_format_combined.csv",
         row.names = FALSE)
