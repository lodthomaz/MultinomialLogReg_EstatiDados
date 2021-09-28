# Pacotes
library(dplyr)
library(caret)
library(nnet)
library(skimr)

# Dados (https://www.kaggle.com/c/titanic/data)
df_titanic <- read.csv2("Data/train.csv", sep = ',', dec = '.') %>% 
  mutate(Pclass = as.factor(Pclass),
         Survived = as.factor(Survived)) %>% 
  select(Pclass, Survived, Sex, Age, SibSp, Parch, Embarked, Fare)

# Variável Resposta
df_titanic %>% 
  group_by(Pclass) %>% 
  count()

# skim
skim(df_titanic)

# Tratamento da base de dados 
df_titanic_trat <- df_titanic %>% 
  filter(Embarked != "") %>% 
  mutate(Age = coalesce(Age, median(df_titanic$Age, na.rm = TRUE)))

# skim
skim(df_titanic_trat)

# Separação da Base em Treino e Teste
aux <- createDataPartition(df_titanic_trat$Pclass, p = .80, list = FALSE)

df_treino <- df_titanic_trat[aux,]
df_teste <- df_titanic_trat[-aux,]

# Escolha da Categoria de Referência
df_treino$Pclass <- relevel(df_treino$Pclass, ref = "1")

# Treino do Modelo
modelo <- multinom(Pclass ~ ., data = df_treino)

# Resumo do Modelo
summary(modelo)

# Exponencial dos Coeficientes do Modelo
exp(coef(modelo))

# Valores preditos da base de treino
df_treino$fitted_values <- predict(modelo, newdata = df_treino, "class")

# Matriz de Confusão
confusionMatrix(df_treino$Pclass, df_treino$fitted_values)

# Valores preditos da base de teste
df_teste$fitted_values <- predict(modelo, newdata = df_teste, "class")

# Matriz de Confusão
confusionMatrix(df_teste$Pclass, df_teste$fitted_values)

