# Funcao de Apoio ao Trabalho 01 de Aprendizado Supervisionado I. 
# Esta função escreve a formula dos modelos polinomiais. 
# Parametros:

# real_feature_names: Um vetor com os nomes dos atributos continuos que voce
#                     quer que seja elevado ao grau desejado.
#  
# categorical_feature_names: Um vetor com os nomes dos atributos categoricos
#                            que voce quer que seja adicionado a hipotese. 
#                            Eles não são elevados ao grau especificado ja que
#                            sao valores binarios (0 ou 1). Se voce quer uma
#                            hipotese que nao tenha nenhum valor categorico, mas
#                            apenas os reais, basta nao passar nenhum valor 
#                            para este parametro quando chamar a funcao.
#
#
# degree: Grau que você deseja que os atributos reais em "real_feature_names"
#         sejam elevados. Ao chamar a funcao, escreva explicitamente
#         o grau desejado. Por exemplo, para grau igual 2, escreva degree=2

# Vejam os exercícios 02 e 03 para ver o funcionamento 
# de uma funcao similar a essa.


getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(real_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", real_feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    
    if(typeof(categorical_feature_names) != "logical"){
        for(i in 1:length(categorical_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       categorical_feature_names[i], " + ",
                                       sep = "")
        } 
    }
    
    
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}

# Comandos que leem os conjuntos de treino e de validacao
train_set <- read.csv("training_set_air_quality.csv", stringsAsFactors=TRUE)
val_set <- read.csv("validation_set_air_quality.csv", stringsAsFactors=TRUE)
test_set <- read.csv("test_set_air_quality.csv", stringsAsFactors=TRUE)

# Desenvolvam o trabalho a partir daqui, apos executarem os comandos a cima

###  PARTE 1: Inspeção dos dados ###

set.seed(40)

# Bibliotecas utilizadas para visualizar a correlação entre as features
library(ggplot2)
library(reshape2)
# install.packages("reshape2")


# Retirando as colunas de índice dos datasets
train_set$No <- NULL
val_set$No <- NULL
test_set$No <- NULL

# No conjunto de dados iniciais, nós encontramos 16 colunas, incluindo o índice e a coluna
# target. Para os conjuntos de treino, validação e teste, temos, respectivamente 244.582,
# 61.147 e 76.434 amostras.
dim(train_set)
dim(val_set)
dim(test_set)

# Utilizando a função summary, percebeu-se que a coluna 'wd' de direção do vento é a única 
# explicitamente categórica ou discreta. As demais features são features são contínuas.
summary(train_set)
summary(val_set)
summary(test_set)

# Abaixo, percebe-se que para os três conjuntos de dados não temos features faltantes.
# Caso houvessem um percentual baixo de casos de dados faltantes poderíamos ou simplesmente
# excluir os exemplos com dados faltantes ou preenchê-los com a mediana para features contínuas
# ou com a label mais frequente para features categóricas.
any(is.na(train_set))
any(is.na(val_set))
any(is.na(test_set))

# Com a função merge abaixo, não encontrou-se exemplos repetidos nos 3 conjuntos de dados.
merge(train_set, val_set)
merge(train_set, test_set)
merge(val_set, test_set)


# Como a feature wd é categorica, utilizaremos one hot encoding para transforma-la
# em feature binaria para cada uma das opções de valores.

one_hot_encoding <- function(df, coluna, uniques, prefix = ""){
    
    for (value in uniques) {
        new_col <- paste(prefix, value, sep = "", collapse = NULL)
        df[[new_col]] <- as.numeric(df[[coluna]] == value)
    }
    
    df[[coluna]] <- NULL
    
    return(df)
}

# Aplicando one hot encoding para a feature de direção de vendo para os três conjuntos
# de dados. Retirou-se uma das opções, pois esta poderia ser representada no caso em que
# todas as demais labels da feature são zero.
wind_uniques <- unique(train_set[['wd']])[-1]
train_set <- one_hot_encoding(train_set, 'wd', wind_uniques)
val_set <- one_hot_encoding(val_set, 'wd', wind_uniques)
test_set <- one_hot_encoding(test_set, 'wd', wind_uniques)

### Parte 2: Normalização dos dados ###

# Removendo a coluna target temporariamente para que a coloquemos
# como ultima coluna do data frame, facilitando indexizações das colunas

train_target <- train_set$target
train_set$target <- NULL
train_set$target <- train_target

val_target <- val_set$target
val_set$target <- NULL
val_set$target <- val_target

test_target <- test_set$target
test_set$target <- NULL
test_set$target <- test_target


# Utilizaremos a normalizacao MinMax apenas nas features continuas 
# nos dados de treino, validacao e teste.
min_features <- apply(train_set[,1:14], 2, min)
max_features <- apply(train_set[,1:14], 2, max)
diff <- max_features - min_features

train_set[,1:14] <- sweep(train_set[,1:14], 2, min_features, "-")
train_set[,1:14] <- sweep(train_set[,1:14], 2, diff, "/")
summary(train_set[,1:14])

val_set[,1:14] <- sweep(val_set[,1:14], 2, min_features, "-")
val_set[,1:14] <- sweep(val_set[,1:14], 2, diff, "/")
summary(val_set[,1:14])

test_set[,1:14] <- sweep(test_set[,1:14], 2, min_features, "-")
test_set[,1:14] <- sweep(test_set[,1:14], 2, diff, "/")
summary(test_set[,1:14])

### Parte 3: Encontrando os valores de baseline ###

# Gerando o baseline utilizando a função disponibilizada getHypothesis
feature_names <- colnames(train_set)[1:ncol(train_set)-1]
feature_names

hypothesis <- getHypothesis(feature_names, degree=1)
hypothesis

# Aplicando a hipotese no nosso conjunto de dados para treino
baseline <- lm(formula=hypothesis, data=train_set)
summary(baseline)

# Predizendo os valores para os 3 conjuntos de dados
train_pred <- predict(baseline, train_set)
val_pred <- predict(baseline, val_set)
test_pred <- predict(baseline, test_set)

# Utilizaremos o erro absoluto médio para avaliação da performance dos modelos, 
MAE <- function(preds, labels){
    mae_values <- sum(abs(preds-labels))/length(preds)
    return(mae_values)
}

mae_train_baseline <- MAE(train_pred, train_set$target); mae_train_baseline
mae_val_baseline <- MAE(val_pred, val_set$target); mae_val_baseline
mae_test_baseline <- MAE(test_pred, test_set$target); mae_test_baseline

# Com isso, encontrou-se os erros absolutos médios de 370, 371 e 372 para os
# conjuntos de treino, validação e teste, respectivamente.

### Parte 4: Utilizando combinação de features ###

# Utilizando correlacao de spearman podemos visualizar quais features estão mais 
# correlacionadas como critério para combinação de features.

corr <- round(cor(train_set[,1:14]),1)
melted_corr <- melt(corr);

p <- ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value))
p <- p + geom_tile(color = "white")
p <- p + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                              midpoint = 0, limit = c(-1,1), space = "Lab", 
                              name="Pearson\nCorrelation")

p <- p + theme_minimal(); p

# Como visto na correlação acima, combinaremos as features de data (ano, mes, dia e hora)
# por terem correlações baixas entre si, combinaremos features de PM2.5, PM10, SO2 e NO2 
# pela alta correlação entre elas e combinaremos as features RAIN e WSPM pela baixa 
# correlação entre si. Não combinaremos as features de direção de vento.

# Desse modo, faremos testes das combinações 2 a 2, 3 a 3 e 4 a 4 dos conjuntos acima e
# avaliar os seus resultados

f01 <- formula(target ~ ((year + month + day + hour)^2 
                         + (PM2.5 + PM10 + SO2 + NO2)^2 
                         + (O3 + TEMP + PRES + DEWP) ^2 
                         + (RAIN + WSPM) ^ 2 
                         + (SE + SSE + SSW + NNE + SW + S + WNW 
                            + ESE + NNW + NW + W + E + ENE + N + WSW)
                         )
               )

f02 <- formula(target ~ ((year + month + day + hour)^3 
                         + (PM2.5 + PM10 + SO2 + NO2)^3 
                         + (O3 + TEMP + PRES + DEWP) ^3 
                         + (RAIN + WSPM) ^ 3 
                         + (SE + SSE + SSW + NNE + SW + S + WNW 
                            + ESE + NNW + NW + W + E + ENE + N + WSW)
                         )
               )

f03 <- formula(target ~ ((year + month + day + hour)^4 
                         + (PM2.5 + PM10 + SO2 + NO2)^4 
                         + (O3 + TEMP + PRES + DEWP) ^4 
                         + (RAIN + WSPM) ^ 4
                         + (SE + SSE + SSW + NNE + SW + S + WNW 
                            + ESE + NNW + NW + W + E + ENE + N + WSW)
                         )
               )


# Avalidando a performance de cada hipotese acima
models <- c(f01, f02, f03)
total_mae_train <- c(length(models))
total_mae_val <- c(length(models))

i <- 1
for(f in models){
    
    # Utiliza-se a hipotese i para treino do modelo
    model <- lm(formula=f, data=train_set)
    
    # Prevê-se o valor para treino e validação
    train_pred <- predict(model, train_set)
    val_pred <- predict(model, val_set)
    
    # Avalia-se o erro absoluto médio para treino e validação
    mae_train <- MAE(train_pred, train_set$target)
    total_mae_train[i] <- mae_train
    
    mae_val <- MAE(val_pred, val_set$target)
    total_mae_val[i] <- mae_val
    i <- i + 1
    
}

# Visualizando os resultados
plot(total_mae_val, xlab="Complexity", ylab="Error (MAE)", 
     ylim=c(320, 400), pch="+", col="blue",  xaxt="n")
axis(1, at=1:length(models), labels=seq(from = 1, to = 3, by = 1), las=1)
points(total_mae_train, pch="*", col="red")
points(rep(mae_val_baseline, length(total_mae_val)), pch="o", col="green")

lines(total_mae_train, col="red", lty=2)
lines(total_mae_val, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val)), col="green", lty=2)
legend(1, 400, legend=c("Train", "Validation", "Baseline"), 
       col=c("red","blue", "green"), lty=2, cex=0.8)


# Pelo gráfico, percebe-se que a combinação 4 a 4 das features foi a melhor, portanto
# avaliaremos os resultados utilizando esse modelo.

which.min(total_mae_val)

model <- lm(formula=f03, data=train_set)

train_pred <- predict(model, train_set)
val_pred <- predict(model, val_set)
test_pred <- predict(model, test_set)


mae_train_comb <- MAE(train_pred, train_set$target); mae_train_comb
mae_val_comb <- MAE(val_pred, val_set$target); mae_val_comb
mae_test_comb <- MAE(test_pred, test_set$target); mae_test_comb

# Com isso, utilizando combinação de features, encontrou-se os erros absolutos 
# médios de 327, 328 e 329 para os conjuntos de treino, validação e teste, 
# respectivamente.

### Parte 4: Utilizando regressão com polinômios ###

# Avaliaremos as performances dos modelos adicionando 
# polinômios de até grau 10
poly_mae_train <- c()
poly_mae_val <- c()


for(i in 1:10){
    
    # Gera a hipotese para o polinomio de grau i
    hypothesis <- getHypothesis(feature_names, degree= i)
    model_poly <- lm(formula=hypothesis, data=train_set)
    
    # Prevê-se o valor com os dados de treino e validação
    train_pred <- predict(model_poly, train_set)
    val_pred <- predict(model_poly, val_set)
    
    # Avalia-se o erro absoluto medio para treino e validação
    mae_train <- MAE(train_pred, train_set$target)
    poly_mae_train[i] <- mae_train
    
    mae_val <- MAE(val_pred, val_set$target)
    poly_mae_val[i] <- mae_val
}

# Visualizando os resultados
plot(poly_mae_train, xlab="Complexity", ylab="Error (MAE)", 
    pch="+", col="red",  xaxt="n", 
    ylim=c(min(c(poly_mae_train, poly_mae_val, mae_val_baseline)),
           max(c(poly_mae_train, poly_mae_val, mae_val_baseline))))
axis(1, at=1:10, labels=seq(from = 1, to = 10, by = 1), las=1)
points(poly_mae_val, pch="*", col="blue")
points(rep(mae_val_baseline, length(poly_mae_val)), pch="o", col="green")
lines(poly_mae_train, col="red", lty=2)
lines(poly_mae_val, col="blue", lty=2)
lines(rep(mae_val_baseline, length(poly_mae_val)), col="green", lty=2)
legend(1, 395, legend=c("Train", "Validation", "Baseline"), 
       col=c("red","blue", "green"), lty=2, cex=0.7)

# Pelo gráfico, percebe-se que temos regiões de underfitting para os polinômios
# de graus 1 e 2, porque, além do erro tanto do conjunto de treino e validação 
# estarem altos eles são bem próximos, logo, esses graus são ainda simples que não
# contemplam as variações que os dados apresentam. Os graus de 3 a 5 são regiões
# de pontos ótimos, dado que o erro do conjunto de validação está levemente superior
# ao erro do conjunto de treino. Além disso, o erro diminui com o aumento do grau do
# polinômio. Agora, para os graus de 6 a 10, temos regiões de overfitting, dada 
# constante redução do erro para o conjunto de treino, mas com aumento considerável
# para o conjunto de validação.

# Assim, percebe-se que o polinômio de grau 5 foi o melhor dentre as 10 opções.

# Aplicando o melhor modelo no conjunto de teste
which.min(poly_mae_val)

hypothesis <- getHypothesis(feature_names, degree= 5)
model_poly <- lm(formula=hypothesis, data=train_set)

train_pred <- predict(model_poly, train_set)
val_pred <- predict(model_poly, val_set)
test_pred <- predict(model_poly, test_set)

mae_train_poly <- MAE(train_pred, train_set$target); mae_train_poly
mae_val_poly <- MAE(val_pred, val_set$target); mae_val_poly
mae_test_poly <- MAE(test_pred, test_set$target); mae_test_poly

# Com isso, utilizando regressão polinomial, encontrou-se os erros absolutos 
# médios de 341, 343 e 344 para os conjuntos de treino, validação e teste, 
# respectivamente.

# Por fim, o melhor modelo encontrado foi o que foi utilizado combinação 4 a 4
# de um conjunto de treino, por conta do seu menor valor de erro absoluto médio
# para o conjunto de validação (328).





