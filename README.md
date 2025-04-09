# Instalando e carregando o pacote MASS (se necessário)
if (!require(MASS)) install.packages("MASS")
library(MASS)
# Carregando o banco de dados birthwt
data("birthwt")
# Visualizando as primeiras linhas do banco de dados
head(birthwt)

# Como você realizaria uma amostragem aleatória simples com 20 mulheres deste banco de dados?
library(mlbench)
data(PimaIndiansDiabetes)
set.seed(123) #para reproduçao
amostra_aleatoria <- PimaIndiansDiabetes|[sample(1:nrow(PimaIndiansDiabetes), 20,)]
#visualisar amostra
amostra_aleatoria

#Para uma amostragem estratificada, a população seria divida em subgrupos,
#nesse caso, mulheres com diabetes ("pos") e sem diabetes ("neg") e depois são
#feitas amostras aleatorias dentro de cada estrato.

#Supondo que em uma amostra de 20, com 10 pos e 10 neg

set.seed(123) #para reprodutibilidade

#seprar subgrupo
com_diabetes <- subset(PimaIndianDiabetes, diabetes == "pos")
sem_diabetes <- subset(PimaIndianDiabetes, diabetes == "neg")

#amostragem dentro de cada subgrupo
amostra_com_diabates <- com_diabetes[sample(1:nrow(com_diabetes),10),]

#unir as as amostras
amostra_estratificada <- rbind(amostra_com_diabetes, amostra_sem_diabetes)

#visualizar
amostra_estratificada

# e se fosse p/fazer uma amostragem estratificada proporcional ao banco de dados?

#primeiro checar a proporcao do banco de dados

#frequencia da variavel diabtes

table(PimaIndiansDiabetes$diabetes)

#neg = 500 pos = 268 ou seja, 65,1% neg e 34,9% pos

#fazer uma amostragem estratificada proporcional com 20 pessoas

set.seed(123)

#tamhno da amostra

n_total <- 20

#calcular tamanhos proporcionais
n_pos <- round(n_total*268/(500+268)) #7
n_neg <- n_total - n_pos #13

#separar por grupo
com_diabetes <- subset(PimaIndiansDiabetes, diabetes == "pos")
sem_diabetes <- subset(PimaIndianDiabetes, diabetes == "neg")

#amostragem proporcional
am_pos <- com_diabetes[sample(1:nrow(com_diabetes), n_pos),]
am_neg <- sem_diabetes[sample(1:nrow(sem_diabetes), n_neg),]

#unir os dois
amostra_proporcional <- rbind(am_pos, am_neg)

#visualizar amostra
amostra_proporcional

# ANALISE DE VARIAVEIS QUANTITATIVAS
#verificando a variavel "glucose", exemplo
summary(PimaIndiansDiabetes$glucose)

#media
mean(PimaIndiansDiabetes$glucose)

#mediana
median(PimaIndiansDiabetes$glucose)

#moda
moda <- function(x) {
uniqx <- unique(x)
uniqx[which.max(tabulate(match(x,uniqx)))]
}
moda(PimaIndiansDiabetes$glucose)

#variancia

#desvio padrao
sd(PimaIndiansDiabetes$glucose)

#coeficiente de variaçao
#Calcular o coeficiente de variação
cv <- (desvio_padrao / media) * 100

#amplitude interquartil(IQR)
iqr <- IQR(dados)

#quartis e percentis(25,50,75 percentil)
#No R, você pode usar a função quantile() para ver os quartis e percentis com facilidade
#Ex
dados <- c(10, 12, 9, 11, 13, 15, 8)
#Ver os quartis (25%, 50%, 75%)
quantile(dados)
#se quiser ver percentis especificos
quantile(dados, probs = c(0.25, 0.5, 0.75)) 

# COMO FAZER GRAFICO
#SE quiser tratar valores 0 como NA
PimaIndiansDiabetes$glucose[PimaIndiansDiabetes$glucose == 0] <- NA

#histograma
hist(PimaIndiansDiabetes$glucose
main = "distribuiçao dos niveis de glucose"
xlab = "glicose"
col = "pink"
breaks = 20)

#boxplot
boxplot(PimaIndiansDiabetes$glucose,
main = "boxplot dos niveis de glicose",
ylab = "glicose",
col = "green"
horizontal = TRUE)

#as medidas de tendencia central e dispersao sao fundamentais p/ entender como os dados se comportam, ou seja, onde eles se concentram e o quanto eles variam. isso ajuda a entender o perfil de amostras , identificar possiveis problemas (se media e mediana sao muito diferentes pode aver outliers distorcendo os dados, bom para comparar grupos e escolher testes estatisticos apropriados.

#10. Isso é importante para que o estudo seja valido, tenha confiabilidade e que os resultados tenham aplicabilidade.
O delineamento de pesquisa define como os dados serão coletados, analisados e interpretados, precisa estar alinhado com o objetivo da pesquisa. 
É importante para que a pergunta da pesquisa seja respondida corretamente, define se é possível estabelecer relações entre causa e efeito, ajuda a evitar viés e escolher métodos de estatística adequados, facilita a reprodução do estudo por outros pesquisadores.
Métodos de amostragem
Define quem será incluído no estudo e como os participantes serão escolhidos. 
Garante representatividade da população alvo, evita viés de seleção, influencia a confiabilidade dos resultados, permite generalizar os achados com mais segurança, afeta o tamanho da amostra necessária e a potencia estatística do estudo. 
Em estudos de saúde, isso é ainda mais importante porque estamos lidando com vidas humanas e decisões clinicas, resultados errados podem levar a condutas erradas e tratamentos ineficazes. Um bom delineamento e amostragem fortalecem a credibilidade cientifica do estudo.
