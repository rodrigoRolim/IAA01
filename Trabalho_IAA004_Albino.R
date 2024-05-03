#TRABALHO DE IAA004 – Estatística Aplicada I

# ALBINO BOGUCHESKI JUNIOR

# Instalando pacotes
install.packages("tidyverse")
install.packages("ggplot2")

# Carregando os pacotes
library(ggplot2)
library(tidyverse)
library (car)
library(fdth)

# Para execução do trabalho usar a base de dados “salarios.RData”
setwd("~/Documents/personals/IAA004_EA1")
# Carregando a base de dados "salarios" em formato R
load("salarios.RData")
# visualizando algumas estatisticas e informacoes das variaveis
summary(salarios)


# 1 Gráficos e tabelas
## a) (15 pontos) Elaborar os gráficos box-plot e histograma das variáveis “age” (idade da esposa) e 
## “husage” (idade do marido) e comparar os resultados

# histogramas “age” (idade da esposa) e “husage” (idade do marido)
# Ajusta as margens da figura
par(mar = c(5, 5, 2, 2))  
histograma.esposa <- hist(
  salarios$age, xlab="Idade da Esposa", ylab = "Frequência", main = "Distribuição das idades das esposas"
)
histograma.esposa

histograma.marido <- hist(
  salarios$husage, xlab="Idade do Marido", ylab = "Frequência", main = "Distribuição das idades dos maridos"
)
histograma.marido

# comparando (média da idade marido x esposa)

media.esposa <- mean(salarios$age)
# media da idade da esposa é: 39,43

media.marido <- mean(salarios$husage)
# media da idade do marido é: 42,46 

(media.marido/media.esposa-1)*100
# considerando a média de idade, a dos maridos é 7,68% maior que a média de idade das esposas

# comparando (mediana da idade maridos x esposas)

mediana.esposa <- median(salarios$age)
mediana.esposa
# mediana da idade das esposas é: 39

mediana.marido <- median(salarios$husage)
mediana.marido
# mediana da idade dos maridos é: 41

(mediana.marido/mediana.esposa -1)*100
# considerando a mediana da idade, a dos maridos é 5,13% maior que a mediana das esposas

# calculando a moda (idade dos maridos e idade da esposas)
# Para a idade das esposas
table(salarios$age)
moda.esposa <- subset(table(salarios$age), table(salarios$age) == max(table(salarios$age)))
moda.esposa.moda <- names(moda.esposa)
moda.esposa.total <- moda.esposa[1]  
moda.esposa

# outra forma de fazer
# tab <- table(salarios$age)
# names(tab)[which.max(tab)]

#moda.marido.moda <- as.numeric(moda.marido.moda)
moda.esposa.moda <- as.numeric(moda.esposa.moda)

# Para a idade dos maridos 
table(salarios$husage)
moda.marido <- subset(table(salarios$husage), table(salarios$husage) == max(table(salarios$husage)))
moda.marido.moda <- names(moda.marido)
moda.marido.total <- moda.marido[1]    
moda.marido.moda <- as.numeric(moda.marido.moda)
moda.marido

# A moda da idade dos maridos eh de 44 anos, com 201 pessoas 
# A moda da idade das esposas eh de 37 anos, com 217 pessoas

(moda.marido.moda / moda.esposa.moda -1)*100
# Portanto, a moda da idade dos maridos eh maior que a das esposas em 18,92%


# Gerando o boxplot para a variavel "age" que eh a idade da esposa

Boxplot( ~ age, data=salarios, id=list(method="y"), ylab="Idade da esposa")

# Gerando o boxplot para a variavel "husage" que eh a # idade do marido

Boxplot( ~ husage, data=salarios, id=list(method="y"), ylab="Idade do marido")

## b) (15 pontos) Elaborar a tabela de frequencias das variáveis “age” (idade da esposa) e “husage” 
## (idade do marido) e comparar os resultados
# Calculando a distribuicao de frequencia e guardando no objeto "tabela.freq"
tabela.freq <- fdt(salarios$husearns)
print(tabela.freq)

# 2 Medidas de posição e dispersão
## a) (15 pontos) Calcular a média, mediana e moda das variáveis “age” (idade da esposa) e “husage” 
## (idade do marido) e comparar os resultados


## b) (15 pontos) Calcular a variância, desvio padrão e coeficiente de variação das variáveis “age” 
## (idade da esposa) e “husage” (idade do marido) e comparar os resultados


## 3 Testes paramétricos ou não paramétricos
## a) (40 pontos) Testar se as médias (se você escolher o teste paramétrico) ou as medianas (se você escolher 
## o teste não paramétrico) das variáveis “age” (idade da esposa) e “husage” (idade do marido) são iguais,
## construir os intervalos de confiança e comparar os resultados.
## Obs: 
## 1) Você deve fazer os testes necessários (e mostra-los no documento pdf) para saber se você 
## deve usar o unpaired test (paramétrico) ou o teste U de Mann-Whitney (não paramétrico), 
## justifique sua resposta sobre a escolha.
## 2) Lembre-se de que os intervalos de confiança já são mostrados nos resultados dos testes 
## citados no item 1 acima.


