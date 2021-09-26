# Pacotes
library(dplyr)
library(caret)
library(nnet)

# Base de Dados: Exemplo 1
df_iris <- iris

# Variável Resposta
df_iris %>% 
  group_by(Species) %>% 
  count()

# Separação da Base em Treino e Teste
aux <- createDataPartition(iris$Species, p = .80, list = FALSE)

df_treino <- df_iris[aux,]
df_teste <- df_iris[-aux,]

# Escolha da Categoria de Referência
df_treino$Species <- relevel(df_treino$Species, ref = "virginica")
df_teste$Species <- relevel(df_teste$Species, ref = "virginica")

# Treino do Modelo
modelo <- multinom(Species ~ ., data = df_treino)

# Resumo do Modelo
summary(modelo)

# Valores preditos da base de treino
df_treino$fitted_values <- predict(modelo, newdata = df_treino, "class")

# Matriz de Confusão
confusionMatrix(df_treino$Species, df_treino$fitted_values)


# Valores preditos da base de teste
df_teste$fitted_values <- predict(modelo, newdata = df_teste, "class")

# Matriz de Confusão
confusionMatrix(df_teste$Species, df_teste$fitted_values)
