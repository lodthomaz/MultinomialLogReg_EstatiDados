# Pacotes
library(dplyr)
library(caret)
library(nnet)

# Base de Dados: Exemplo 1
df_iris <- iris

head(iris)

# Variável Resposta
df_iris %>% 
  group_by(Species) %>% 
  count()

# Separação da Base em Treino e Teste
aux <- createDataPartition(iris$Species, p = .80, list = FALSE)

df_treino <- df_iris[aux,]
df_test <- df_iris[-aux,]


# Escolha da Categoria de Referência
df_treino$Species <- relevel(df_treino$Species, ref = "virginica")

# Treino do Modelo
modelo <- multinom(Species ~ ., data = df_treino)

# Checking the model
summary(modelo)

exp(coef(modelo))

round(fitted(modelo), 2)
