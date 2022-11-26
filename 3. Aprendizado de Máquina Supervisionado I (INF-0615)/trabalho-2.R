# Trabalho 2 - Aprendizado de Máquina Supervisionado I

# Nome dos Integrantes
# Victor Teodoro Goraieb
# André de Souza Gonçalves
# Vitor Anastacio da Silva


# Comandos que leem os conjuntos de treino e de validacao
train_set <- read.csv("proteins_training_set.csv", stringsAsFactors=TRUE)
val_set <- read.csv("proteins_validation_set.csv", stringsAsFactors=TRUE)
test_set <- read.csv("proteins_test_set.csv", stringsAsFactors=TRUE)
sars_set <- read.csv("SARS_test_set.csv", stringsAsFactors=TRUE)

train_set$target <- as.factor(train_set$target)
val_set$target <- as.factor(val_set$target)
test_set$target <- as.factor(test_set$target)
sars_set$target <- as.factor(sars_set$target)

# Desenvolvam o trabalho a partir daqui, apos executarem os comandos a cima

###  PARTE 1: Inspeção dos dados ###

set.seed(40)

# Bibliotecas utilizadas para visualizar a correla??o entre as features
library(ggplot2)
library(reshape2)
library(glmnet)
library(caret)
library(pROC)

# install.packages("reshape2")

### 1. Inspecionem os dados. Quantos exemplos vocês tem? Há exemplos com features sem anotações? Como vocês
# lidariam com isso?

# 1.a) No conjunto de dados iniciais, nós encontramos 11 colunas, incluindo a coluna
# target. Para os conjuntos de treino, validação e teste, temos, respectivamente 9.204,
# 2.303 e 2.878 amostras.
dim(train_set)
dim(val_set)
dim(test_set)

# 1.b) e c) Abaixo, percebe-se que para os três conjuntos de dados não temos features faltantes.
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

### 2. Inspecionem a frequência de cada classe. A base de dados está balanceada ? Se não, como vocês lidarão com
#o desbalanceamento ?

# Sim, a base de dados para treino está balanceada, notamos uma proporção de 2.7 para 1 na classe negativa para a positiva.
# Portanto, acreditamos que o balanceamento não será um problema a priori, caso a modelagem indique que esse é um fator prejudicial
# poderíamos aplicar diversas técnicas. Entre as principais estão: alterar os pesos das classes na função de custo (dando maior peso para a classe minoritária),
# undersampling (reduzir as amostras da classe majoritária), oversampling (aumentar as amostras da classe minoritária) ou SMOTE 
# (onde geramos dados sintéticos a partir das amostras minoritárias).

table(train_set$target)
table(val_set$target)
table(test_set$target)

### 3. Apliquem alguma técnica de normalização de forma a deixa os dados mais bem preparados para o treinamento.

# Podemos aplicar aos dados a técnica da Normalizacao Z-norma. Ela essencialmente irá substrair de cada amostra a média da feature analisada e
# depois dividir pelo desvio padrão. Um importante passo é utilizar somente a média e desvio padrão das features do treinamento para a normalização.

mean_features <- apply(train_set[,1:(ncol(train_set)-1)], 2, mean)
mean_features

sd_features <- apply(train_set[,1:(ncol(train_set)-1)], 2, sd)
sd_features

train_set[,1:(ncol(train_set)-1)] <- sweep(train_set[,1:(ncol(train_set)-1)], 2, mean_features, "-")
train_set[,1:(ncol(train_set)-1)] <- sweep(train_set[,1:(ncol(train_set)-1)], 2, sd_features, "/")
summary(train_set)

val_set[,1:(ncol(val_set)-1)] <- sweep(val_set[,1:(ncol(val_set)-1)], 2, mean_features, "-")
val_set[,1:(ncol(val_set)-1)] <- sweep(val_set[,1:(ncol(val_set)-1)], 2, sd_features, "/")
summary(val_set)

test_set[,1:(ncol(test_set)-1)] <- sweep(test_set[,1:(ncol(test_set)-1)], 2, mean_features, "-")
test_set[,1:(ncol(test_set)-1)] <- sweep(test_set[,1:(ncol(test_set)-1)], 2, sd_features, "/")
summary(test_set)

sars_set[,1:(ncol(sars_set)-1)] <- sweep(sars_set[,1:(ncol(sars_set)-1)], 2, mean_features, "-")
sars_set[,1:(ncol(sars_set)-1)] <- sweep(sars_set[,1:(ncol(sars_set)-1)], 2, sd_features, "/")
summary(sars_set)

### 4. Como baseline, treinem uma regressão logística com todas as features para predizer se haverá ou não a
#produção de anticorpos. Reportem a matriz de confusão relativa, o TPR, o TNR e a acurácia balanceada nas
#bases de treinamento, validação e teste (apenas arquivo proteins_teste_set.csv ). 

# 4.a) Treinando a Regressão Logística e gerando o baseline

# Gerando nome das features para uso posterior
feature_names <- colnames(train_set)[1:ncol(train_set)-1]
feature_names

# Adicionando código para gerar hipótese (tirado do support_functions.R)
getHypothesis <- function(feature_names, degree){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}

hypothesis <- getHypothesis(feature_names, degree=1)
hypothesis

# Aplicando a hipotese no nosso conjunto de dados para treino, validacao e teste
x_train <- model.matrix(hypothesis, train_set)
x_val <- model.matrix(hypothesis, val_set)
x_test <- model.matrix(hypothesis, test_set)

y_train <- train_set$target
y_val <- val_set$target
y_test <- test_set$target

# Treinando um modelo de regressao logistica com base no treino
baseline <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE,
                maxit = 1e+05, alpha=0, lambda = 1e-2)

# Checando os parâmetros da regressao logistica
baseline$beta #thetas aprendidos
baseline$a0 # valor do theta0 (intercept)

# Gerando as predições
train_pred <- predict(baseline, newx = x_train, type="response")
val_pred <- predict(baseline, newx = x_val, type="response")
test_pred <- predict(baseline, newx = x_test, type="response")

# Criando funcao para aplicar o threshold e usando ela nas predições anteriores
apply_threshold <- function(prediction, threshold = 0.5){
    return((prediction >= threshold)*1)
}

train_pred_class <- apply_threshold(train_pred)
val_pred_class <- apply_threshold(val_pred)
test_pred_class <- apply_threshold(test_pred)

# 4.b) Gerando matriz de confusão relativa

# Instanciando uma matriz de confusão padrão para os dados

cm_train <- confusionMatrix(data = as.factor(train_pred_class), 
                      reference = as.factor(train_set$target), 
                      positive='1')

cm_val <- confusionMatrix(data = as.factor(val_pred_class), 
                            reference = as.factor(val_set$target), 
                            positive='1')

cm_test <- confusionMatrix(data = as.factor(test_pred_class), 
                            reference = as.factor(test_set$target), 
                            positive='1')

# Adicionando o código da matriz de confusão relativa (retirado do support_functions.R)

calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposi??o para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,1] = round(cm_absolute[1,1]/sum(cm_absolute[1,]), digits=2)
    cm_relative[1,2] = round(cm_absolute[1,2]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,1] = round(cm_absolute[2,1]/sum(cm_absolute[2,]), digits=2)
    cm_relative[2,2] = round(cm_absolute[2,2]/sum(cm_absolute[2,]), digits=2)
    
    return(cm_relative)  
}

cm_rel_train <- calculaMatrizConfusaoRelativa(cm_train)
cm_rel_val <- calculaMatrizConfusaoRelativa(cm_val)
cm_rel_test <- calculaMatrizConfusaoRelativa(cm_test)

# Gerando matrizes de confusão relativas
cm_rel_train
cm_rel_val
cm_rel_test

# 4.c) Gerando o TPR (True Positive Rate), TNR (True Negative Rate) e Acurácia Balanceada

names(cm_train)

# TPR (Sensitividade)
tpr_train <- cm_train$byClass['Sensitivity']
tpr_val <- cm_val$byClass['Sensitivity']
tpr_test <- cm_test$byClass['Sensitivity']

# TNR (Especificidade)
tnr_train <- cm_train$byClass['Specificity']
tnr_val <- cm_val$byClass['Specificity']
tnr_test <- cm_test$byClass['Specificity']

# Acurácia Balanceada
balanc_acc_train <- (tpr_train + tnr_train)/2
balanc_acc_val <- (tpr_val + tnr_val)/2
balanc_acc_test <- (tpr_test + tnr_test)/2

###5. Implementem soluções alternativas baseadas em regressão logística através da combinação das features ou
#modelos polinomiais para melhorar o resultado do baseline. Comparem suas soluções reportando a matriz
#de confusão relativa e a acurácia balanceada no conjunto de validação. Tomem apenas a melhor solução,
#baseada na acurácia balanceada no conjunto de validação, e reportem a matriz de confusão relativa, TPR,
#TNR e acurácia balanceada no conjunto de teste (apenas arquivo proteins_teste_set.csv ).

# Utilizando a função de loop para comparar os ganhos com cada grau polinomial (retirado do Ex03.R)

acc_train <- c()
acc_val <- c()

# Checando como os modelos com mais graus polinomiais performam:
for(i in 1:4){  
    
    print(i)
    hypothesis <- getHypothesis(feature_names, i)
    
    # Gerando os dataframes que serão usados nas predições
    
    x_train <- model.matrix(hypothesis, train_set)
    y_train <- train_set$target
    
    x_val <- model.matrix(hypothesis, val_set)
    y_val <- val_set$target
    
    # Criando modelo com base no treino
    
    model <- glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, maxit = 1e+05, 
                    alpha=0, lambda = 1e-2)
    
    # Gerando predições
    
    trainPred <- predict(model, newx = x_train, type="response")
    valPred <- predict(model, newx = x_val, type="response")
    
    # Convertendo em classe
    trainClassPred <- apply_threshold(trainPred)
    valClassPred <- apply_threshold(valPred)
    
    # Gerando Matrizes de confusão
    
    cm_train <- confusionMatrix(data = as.factor(trainClassPred), 
                          reference = as.factor(train_set$target), 
                          positive='1')
    cm_relative_train <- calculaMatrizConfusaoRelativa(cm_train)
    
    cm_val <- confusionMatrix(data = as.factor(valClassPred), 
                                reference = as.factor(val_set$target), 
                                positive='1')
    cm_relative_val <- calculaMatrizConfusaoRelativa(cm_val)
    
    acc_train[i] <- (cm_relative_train[1,1] + cm_relative_train[2,2])/2
    acc_val[i] <- (cm_relative_val[1,1] + cm_relative_val[2,2])/2
    
}

# Checando como seria o impacto de balanceamento (mudar os pesos) no baseline e outros modelos:

classes_frequency = table(train_set$target)
classes_frequency

relative_classes_frequency = classes_frequency/sum(classes_frequency)
relative_classes_frequency

w_positive = (1 - relative_classes_frequency[2])
w_negative = (1 - relative_classes_frequency[1])

# Inicializando com zeros o vetor de pesos
weights <- rep(0.0, dim(train_set)[1])

# Associando o peso dos positivos (w_positive) aos respectivos exemplos
weights[train_set$target == 1] = w_positive

# Associando o peso dos negatives (w_negative) aos respectivos exemplos
weights[train_set$target == 0] = w_negative

# Instanciando um novo baseline e gerando métricas

hypothesis <- getHypothesis(feature_names, degree=1)

x_train <- model.matrix(hypothesis, train_set)
x_val <- model.matrix(hypothesis, val_set)
x_test <- model.matrix(hypothesis, test_set)

y_train <- train_set$target
y_val <- val_set$target
y_test <- test_set$target

baseline_blc <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE,
                       maxit = 1e+05, alpha=0, lambda = 1e-2, weights = weights)

train_pred_blc <- predict(baseline_blc, newx = x_train, type="response")
val_pred_blc <- predict(baseline_blc, newx = x_val, type="response")
test_pred_blc <- predict(baseline_blc, newx = x_test, type="response")

train_pred_class_blc <- apply_threshold(train_pred_blc)
val_pred_class_blc <- apply_threshold(val_pred_blc)
test_pred_class_blc <- apply_threshold(test_pred_blc)

cm_train_blc <- confusionMatrix(data = as.factor(train_pred_class_blc), 
                            reference = as.factor(train_set$target), 
                            positive='1')

cm_val_blc <- confusionMatrix(data = as.factor(val_pred_class_blc), 
                          reference = as.factor(val_set$target), 
                          positive='1')

cm_test_blc <- confusionMatrix(data = as.factor(test_pred_class_blc), 
                           reference = as.factor(test_set$target), 
                           positive='1')

# Comparando o efeito do balanceamento no baseline

balanc_acc_train_blc <- (cm_train_blc$byClass['Sensitivity'] + cm_train_blc$byClass['Specificity'])/2
cat('Acc balanceada s/ balanceamento de pesos', round(balanc_acc_train,2))
cat('Acc balanceada c/ balanceamento de pesos', round(balanc_acc_train_blc,2));


balanc_acc_val_blc <- (cm_val_blc$byClass['Sensitivity'] + cm_val_blc$byClass['Specificity'])/2
cat('Acc balanceada s/ balanceamento de pesos', round(balanc_acc_val,2))
cat('Acc balanceada c/ balanceamento de pesos', round(balanc_acc_val_blc,2));


balanc_acc_test_blc <- (cm_test_blc$byClass['Sensitivity'] + cm_test_blc$byClass['Specificity'])/2
cat('Acc balanceada s/ balanceamento de pesos', round(balanc_acc_test,2))
cat('Acc balanceada c/ balanceamento de pesos', round(balanc_acc_test_blc,2));



# Avaliando impacto dos polinomios com balanceamento de pesos

acc_train_blc <- c()
acc_val_blc <- c()

for(i in 1:10){  
    
    print(i)
    hypothesis <- getHypothesis(feature_names, i)
    
    # Gerando os dataframes que serão usados nas predições
    
    x_train <- model.matrix(hypothesis, train_set)
    y_train <- train_set$target
    
    x_val <- model.matrix(hypothesis, val_set)
    y_val <- val_set$target
    
    # Criando modelo com base no treino
    
    model <- glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, maxit = 1e+05, 
                    alpha=0, lambda = 1e-2, weights = weights)
    
    # Gerando predições
    
    trainPred <- predict(model, newx = x_train, type="response")
    valPred <- predict(model, newx = x_val, type="response")
    
    # Convertendo em classe
    trainClassPred <- apply_threshold(trainPred)
    valClassPred <- apply_threshold(valPred)
    
    # Gerando Matrizes de confusão
    
    cm_train <- confusionMatrix(data = as.factor(trainClassPred), 
                                reference = as.factor(train_set$target), 
                                positive='1')
    cm_relative_train <- calculaMatrizConfusaoRelativa(cm_train)
    
    cm_val <- confusionMatrix(data = as.factor(valClassPred), 
                              reference = as.factor(val_set$target), 
                              positive='1')
    cm_relative_val <- calculaMatrizConfusaoRelativa(cm_val)
    
    acc_train_blc[i] <- (cm_relative_train[1,1] + cm_relative_train[2,2])/2
    acc_val_blc[i] <- (cm_relative_val[1,1] + cm_relative_val[2,2])/2
    
}

acc_train_blc
acc_val_blc

# Visualizando os resultados

plot(acc_train_blc, xlab="Complexity", ylab="Balanced Accuracy", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train_blc, acc_val_blc)),
            max(c(acc_train_blc, acc_val_blc))))
axis(1, at=1:10, labels=seq(from = 1, to = 10, by = 1), las=1)
points(acc_val_blc, pch="*", col="blue")

lines(acc_train_blc, col="red", lty=2)
lines(acc_val_blc, col="blue", lty=2)
legend(7, 0.65, legend=c("Train", "Validation"),
       col=c("red","blue"), lty=2, cex=0.7)

# Notamos que o melhor grau com balanceamento de pesos é o grau 6. Vamos agora aplicar para o
# conjunto de teste

hypothesis <- getHypothesis(feature_names, 6)

# Gerando os dataframes que serão usados nas predições

x_train <- model.matrix(hypothesis, train_set)
y_train <- train_set$target

x_val <- model.matrix(hypothesis, val_set)
y_val <- val_set$target

x_test <- model.matrix(hypothesis, test_set)
y_test <- test_set$target

x_sars <- model.matrix(hypothesis, sars_set)
y_sars <- sars_set$target

# Criando modelo com base no treino
best_model <- glmnet(x_train, y_train,  family="binomial", 
                standardize = FALSE, maxit = 1e+05, 
                alpha=0, lambda = 1e-2, weights = weights)

# Gerando métricas de teste

test_pred_bst_model_class <- apply_threshold(predict(best_model, newx = x_test, type="response"))

cm_test_bst_model <- confusionMatrix(data = as.factor(test_pred_bst_model_class), 
                               reference = as.factor(test_set$target), 
                               positive='1')

cm_rel_test_bst_model <- calculaMatrizConfusaoRelativa(cm_test_bst_model)
cm_rel_test_bst_model

TPR_test_bst_model <- cm_test_bst_model$byClass['Sensitivity']
TPR_test_bst_model
TNR_test_bst_model <- cm_test_bst_model$byClass['Specificity']
TNR_test_bst_model
acc_blc_test_bst_model <- (TPR_test_bst_model + TNR_test_bst_model) / 2
acc_blc_test_bst_model

###6. Tomem um dos modelos do item anterior e varie o fator de regularização (λ). Criem a curva viés/variância
#colocando os diferentes valores de λ no eixo das das abscissas. Identifiquem as regiões de underfitting, ponto
#ótimo e overfitting, e então tomem o modelo com o melhor fator de regularização e reporte a matriz de confusão
#relativa, o TPR, o TNR e a acurácia balanceada no conjunto de teste (apenas arquivo proteins_teste_set.csv ).
#Lembrem-se que a curva viés/variância é criada utilizando apenas os dados de treinamento e
#de validação!.

complexidade <- c()
acc_blc_lambda_val <- c()
acc_blc_lambda_train <- c()
i <- 1
# Aplicando um loop variando os lambdas para o modelo de grau 6

for(lambda in rev(c(1e-8,1e-7,1e-6,1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 10, 100, 1000, 10000, 100000))){
    
    model_lambda <- glmnet(x_train, y_train,  family="binomial", 
                           standardize = FALSE, maxit = 1e+05, 
                           alpha=0, lambda = lambda, weights = weights)
    
    train_pred_model_lambda_class <- apply_threshold(predict(model_lambda, newx = x_train, type="response"))
    val_pred_model_lambda_class <- apply_threshold(predict(model_lambda, newx = x_val, type="response"))
    
    
    cm_train_model_lambda <- confusionMatrix(data = as.factor(train_pred_model_lambda_class), 
                                           reference = as.factor(train_set$target), 
                                           positive='1')
    
    cm_val_model_lambda <- confusionMatrix(data = as.factor(val_pred_model_lambda_class), 
                                         reference = as.factor(val_set$target), 
                                         positive='1')
    
    cm_rel_train_model_lambda <- calculaMatrizConfusaoRelativa(cm_train_model_lambda)
    TPR_train_model_lambda <- cm_train_model_lambda$byClass['Sensitivity']
    TNR_train_model_lambda <- cm_train_model_lambda$byClass['Specificity']
    acc_blc_train_model_lambda <- (TPR_train_model_lambda + TNR_train_model_lambda) / 2
    
    cm_rel_val_model_lambda <- calculaMatrizConfusaoRelativa(cm_val_model_lambda)
    TPR_val_model_lambda <- cm_val_model_lambda$byClass['Sensitivity']
    TNR_val_model_lambda <- cm_val_model_lambda$byClass['Specificity']
    acc_blc_val_model_lambda <- (TPR_val_model_lambda + TNR_val_model_lambda) / 2
    
    
    acc_blc_lambda_val[i] <- acc_blc_val_model_lambda
    acc_blc_lambda_train[i] <- acc_blc_train_model_lambda
    complexidade[i]<- 1/lambda
    
    i <- i+1
}

# Visualizando os resultados

plot(acc_blc_lambda_train, xlab="Complexity", ylab="Balanced Accuracy", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_blc_lambda_train, acc_blc_lambda_val)),
            max(c(acc_blc_lambda_train, acc_blc_lambda_val))))
axis(1, at=1:length(complexidade), labels=seq(from = 1, to = length(complexidade), by = 1), las=1)
points(acc_blc_lambda_val, pch="*", col="blue")
lines(rep(balanc_acc_val_blc, length(acc_blc_lambda_val)), col="green")

lines(acc_blc_lambda_train, col="red", lty=2)
lines(acc_blc_lambda_val, col="blue", lty=2)
legend(7.5, 0.575, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

# Nota-se que o melhor modelo é o com lambda 1e-3

### 7. Após desenvolverem todos os modelos, tomem o melhor modelo de todos (melhor performance no conjunto
#de validação). Reportem a matriz de confusão relativa e acurácia balanceada nos conjuntos de teste
#proteins_teste_set.csv e SARS_test_csv. Há uma diferença significativa entre eles ? Se sim, qual
#explicação você daria para essa diferença?

x_test <- model.matrix(hypothesis, test_set)
y_test <- test_set$target

best_model_lambda <- glmnet(x_train, y_train,  family="binomial", 
                     standardize = FALSE, maxit = 1e+05, 
                     alpha=0, lambda = 1e-3, weights = weights)

# Gerando métricas de teste

test_pred_bst_model_lambda_class <- apply_threshold(predict(best_model_lambda, newx = x_test, type="response"))

cm_test_bst_model_lambda <- confusionMatrix(data = as.factor(test_pred_bst_model_lambda_class), 
                                     reference = as.factor(test_set$target), 
                                     positive='1')

cm_rel_test_bst_model <- calculaMatrizConfusaoRelativa(cm_test_bst_model_lambda)
cm_rel_test_bst_model

TPR_test_bst_model_lambda <- cm_test_bst_model_lambda$byClass['Sensitivity']
TPR_test_bst_model_lambda
TNR_test_bst_model_lambda <- cm_test_bst_model_lambda$byClass['Specificity']
TNR_test_bst_model_lambda
acc_blc_test_bst_model_lambda <- (TPR_test_bst_model_lambda + TNR_test_bst_model_lambda) / 2
acc_blc_test_bst_model_lambda

cat('Acc balanceada do baseline (c/balanc de pesos)', round(balanc_acc_test_blc,2));
cat('Acc balanceada do melhor modelo (c/balanc de pesos)', round(acc_blc_test_bst_model_lambda,2))


# Gerando métricas de SARS

sars_pred_bst_model_lambda_class <- apply_threshold(predict(best_model_lambda, newx = x_sars, type="response"))

cm_sars_bst_model_lambda <- confusionMatrix(data = as.factor(sars_pred_bst_model_lambda_class), 
                                            reference = as.factor(sars_set$target), 
                                            positive='1')

cm_rel_sars_bst_model <- calculaMatrizConfusaoRelativa(cm_sars_bst_model_lambda)
cm_rel_sars_bst_model

TPR_sars_bst_model_lambda <- cm_sars_bst_model_lambda$byClass['Sensitivity']
TPR_sars_bst_model_lambda
TNR_sars_bst_model_lambda <- cm_sars_bst_model_lambda$byClass['Specificity']
TNR_sars_bst_model_lambda
acc_blc_sars_bst_model_lambda <- (TPR_sars_bst_model_lambda + TNR_sars_bst_model_lambda) / 2
acc_blc_sars_bst_model_lambda

cat('Acc balanceada do melhor modelo (c/balanc de pesos)', round(acc_blc_sars_bst_model_lambda,2))



