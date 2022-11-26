####### C?digo de apoio ao Trabalho 03 da disciplina INF-0615 #######

# Funcao que calcula a matriz de confusao relativa para 3 classes
calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposi??o para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,] = round(cm_absolute[1,]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,] = round(cm_absolute[2,]/sum(cm_absolute[2,]), digits=2)
    cm_relative[3,] = round(cm_absolute[3,]/sum(cm_absolute[3,]), digits=2)
    
    return(cm_relative)  
}

####### ======= PREPARAÇÃO DOS DADOS ======= #######

library(ggplot2)
library(caret)
library(reshape2)
library(rpart)
library(rpart.plot)
library(randomForest)

set.seed(60) 

# Leitura da base de treinamento+validacao

train_val_set <- read.csv("train_val_set_patient_status_covid19.csv", stringsAsFactors = T)

test_set <- read.csv("test_set_patient_status_covid19.csv", stringsAsFactors = T)
temporary_test <- rbind(train_val_set[1,], test_set) 
test_set <- temporary_test[-1,] 

# Removendo os dados duplicados antes de fazer as divisões entre conjuntos
# de treino e de validação.
dim(train_val_set)
train_val_set <- unique(train_val_set)
dim(train_val_set)


# Dividindo os dados em 80% conjunto de treino e 20% de validação.
random_train_val_indexes <- sample(1:nrow(train_val_set), size=0.8*nrow(train_val_set))
train_set <- train_val_set[random_train_val_indexes, ]
val_set  <- train_val_set[-random_train_val_indexes, ] 

dim(train_set)
dim(val_set)
dim(test_set)

# Não foi encontrado exemplos de dados presentes nos dois conjuntos de dados
merge(train_set, val_set)
merge(test_set, val_set)
merge(train_set, test_set)

# Também não há dados faltantes
any(is.na(train_set))
any(is.na(val_set))
any(is.na(test_set))


# Percebe-se que as classes estão desbalanceadas. Cerca de 71% dos exemplos do
# conjunto de treino é da classe onTreatment.
round(100*table(train_set$label)/sum(table(train_set$label)),1)

# Utilizaremos a técnica de undersample para redução de exemplos da classe majoritária
majority_set <- train_set[train_set$label == "onTreatment",]
minority_set <- train_set[train_set$label != "onTreatment",] 

randomNoIdx <- sample(1:nrow(majority_set), size=1*nrow(minority_set))
subsampling_majority <- majority_set[randomNoIdx,]
und_train_set <- rbind(minority_set, subsampling_majority)

# Após a redução da classe onTreament, ficamos com: 
# dead: 10.1%
# onTreatment: 50%
# recovered: 39.9%
round(100*table(und_train_set$label)/sum(table(und_train_set$label)),1)

# Houve no fim uma redução de 42% dos dados no conjunto de treino original
1 - nrow(und_train_set)/nrow(train_set)


####### ======= GERANDO O BASELINE ======= #######


baseline_tree_model <- rpart(formula=label ~ age + sex + country + latitude + longitude + 
                       date_onset_symptoms + date_admission_hospital + date_confirmation + lives_in_Wuhan +
                       travel_history_dates + travel_history_location + chronic_disease_binary +
                       date_death_or_discharge + travel_history_binary, 
                   data=und_train_set, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                   parms= list(split="information"))


summary(baseline_tree_model)

# Anlisando a importância das features

baseline_train_pred <- predict(baseline_tree_model, und_train_set, type="class")
baseline_val_pred <- predict(baseline_tree_model, val_set, type="class")
baseline_test_pred <- predict(baseline_tree_model, test_set, type="class")


retornaMatrizConfusaoRelativa <- function(true, predicted){
    
    cm <- confusionMatrix(data = as.factor(predicted), 
                          reference = as.factor(true), 
                          positive='forgery')
    
    return(calculaMatrizConfusaoRelativa(cm))  
}

acuraciaBalanceada <- function(cm){
    return (sum(diag(cm)/length(diag(cm))))
}

geraTabelaAcuraciaBalanceada <- function (train_cm, val_cm, test_cm) {
    
    train_ac <- acuraciaBalanceada(train_cm)
    val_ac <- acuraciaBalanceada(val_cm)
    test_ac <- acuraciaBalanceada(test_cm)
    
    tabela_ac <- data.frame(conjunto = c('train', 'val', 'test'),
                            acuracia_balanceada = round(c(train_ac, val_ac, test_ac),2))
    
    return(tabela_ac)
}

baseline_train_cm <- retornaMatrizConfusaoRelativa(und_train_set$label, baseline_train_pred)
baseline_train_cm

baseline_val_cm <- retornaMatrizConfusaoRelativa(val_set$label, baseline_val_pred)
baseline_val_cm

baseline_test_cm <- retornaMatrizConfusaoRelativa(test_set$label, baseline_test_pred)
baseline_test_cm

# Tabela resumida 

tabela_ac_baseline <- geraTabelaAcuraciaBalanceada(baseline_train_cm, baseline_val_cm, baseline_test_cm)
tabela_ac_baseline


####### ======= ÁRVORE DE DECISÃO ======= #######

## Variando o tamanho das arvores geradas

max_depth <- 30
acc_per_depth <- data.frame(depth=numeric(max_depth), ac_bal_train=numeric(max_depth), ac_bal_val=numeric(max_depth))

for (maxDepth in 1:max_depth){
    tree_model <- rpart(formula=label ~ age + sex + country + latitude + longitude + 
                           date_onset_symptoms + date_admission_hospital + date_confirmation + lives_in_Wuhan +
                           travel_history_dates + travel_history_location + chronic_disease_binary +
                           date_death_or_discharge + travel_history_binary,
                       data=und_train_set, method="class",
                       control=rpart.control(minsplit=2, cp=0.0, 
                                             maxdepth=maxDepth, xval = 0),
                       parms= list(split="information"))
    
    # Avaliando no conjunto de treinamento e teste
    train_pred <- predict(tree_model, und_train_set, type="class")
    val_pred <- predict(tree_model, val_set, type="class")
    
    # Calculando as acurácias balanceadas
    train_acc_bal <- acuraciaBalanceada(retornaMatrizConfusaoRelativa(und_train_set$label, train_pred))
    vall_acc_bal <- acuraciaBalanceada(retornaMatrizConfusaoRelativa(val_set$label, val_pred))
    
    acc_per_depth[maxDepth,] = c(maxDepth, 
                               train_acc_bal, 
                               vall_acc_bal)
}

# Visualizando o gráfico de viés e variância por profundidade de árvore
acc_per_depth_melted <- melt(acc_per_depth, id="depth")  # convert to long format
ggplot(data=acc_per_depth_melted, aes(x=depth, y=value, colour=variable)) + geom_line() + geom_point() + theme_minimal()


# Selecionando a melhor profundidade árvore 

best_depth <- acc_per_depth[acc_per_depth$ac_bal_val ==  max(acc_per_depth$ac_bal_val), ]$depth
best_depth

best_tree_model <- rpart(formula=label ~ age + sex + country + latitude + longitude + 
                        date_onset_symptoms + date_admission_hospital + date_confirmation + lives_in_Wuhan +
                        travel_history_dates + travel_history_location + chronic_disease_binary +
                        date_death_or_discharge + travel_history_binary,
                    data=und_train_set, method="class",
                    control=rpart.control(minsplit=2, cp=0, 
                                          maxdepth=best_depth, xval = 0),
                    parms= list(split="information"))



# Imprimindo o melhor modelo de Árvore
prp(best_tree_model)


# Plota a árvore de decisăo usando rpart.plot
rpart.plot(best_tree_model, 
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)


# Avaliando no conjunto de treinamento, validação e teste
train_pred <- predict(best_tree_model, und_train_set, type="class")
val_pred <- predict(best_tree_model, val_set, type="class")
test_pred <- predict(best_tree_model, test_set, type="class")


tree_train_cm <- retornaMatrizConfusaoRelativa(und_train_set$label, train_pred)
tree_train_cm

tree_val_cm <- retornaMatrizConfusaoRelativa(val_set$label, val_pred)
tree_val_cm

tree_test_cm <- retornaMatrizConfusaoRelativa(test_set$label, test_pred)
tree_test_cm



# Calculando as acurácias balanceadas
tree_train_acc_bal <- acuraciaBalanceada(tree_train_cm); tree_train_acc_bal
tree_val_acc_bal <- acuraciaBalanceada(tree_val_cm); tree_val_acc_bal
tree_test_acc_bal <- acuraciaBalanceada(tree_test_cm); tree_test_acc_bal

tabela_ac_tree <- geraTabelaAcuraciaBalanceada(tree_train_cm, tree_val_cm, tree_test_cm)
tabela_ac_tree


## Feature Selection

### Verificando importancia das variáveis ###
importance_per_features <- best_tree_model$variable.importance
importance_per_features

relative_importance <- importance_per_features/sum(importance_per_features)
relative_importance


fi_df <- data.frame(feature = names(relative_importance), importance = unname(relative_importance))
fi_df$acum_importance <- cumsum(fi_df$importance)
fi_df

features_91percent <- fi_df[fi_df$acum_importance < 0.91, ]$feature
und_train_set_f91p <- und_train_set[c(features_91percent, 'label')]

f91p_tree_model <- rpart(formula=label ~ country + longitude + 
                             date_admission_hospital + lives_in_Wuhan +
                             travel_history_dates + date_death_or_discharge,
                         data=und_train_set_f91p, method="class",
                         control=rpart.control(minsplit=2, cp=0.0, 
                                               maxdepth=best_depth, xval = 0),
                         parms= list(split="information"))

# Avaliando no conjunto de validação
f91p_val_pred <- predict(f91p_tree_model, val_set, type="class")
f91p_tree_val_cm <- retornaMatrizConfusaoRelativa(val_set$label, f91p_val_pred)

f91p_tree_vall_acc_bal <- acuraciaBalanceada(f91p_tree_val_cm)
f91p_tree_vall_acc_bal

# Seleção de features por remoção de features muito correlacionadas

nums <- unlist(lapply(und_train_set, is.numeric))  
correlationMatrix <- cor(und_train_set[nums], method = 'spearman')

melted_corr <- melt(correlationMatrix); 
melted_corr$value <- round(melted_corr$value,2)

p <- ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value))
p <- p + geom_tile(color = "white")
p <- p + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                              midpoint = 0, limit = c(-1,1), space = "Lab", 
                              name="Pearson\nCorrelation")
p <- p + theme_minimal(); p


most_corr_features <- melted_corr[(abs(melted_corr$value) >= 0.8) & melted_corr$Var1 != melted_corr$Var2, ]
most_corr_features

# Treinando o modelo apenas com features não muito correlacionadas

fs_tree_model <- rpart(formula=label ~ age + sex + country + latitude + longitude + 
                                 date_onset_symptoms + date_admission_hospital + date_confirmation + lives_in_Wuhan +
                                 travel_history_location + chronic_disease_binary +
                                 travel_history_binary, 
                             data=und_train_set, method="class",
                             control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                             parms= list(split="information"))

# Avaliando o resultado no conjunto de validação

fs_val_pred <- predict(fs_tree_model, val_set, type="class")
fs_tree_val_cm <- retornaMatrizConfusaoRelativa(val_set$label, fs_val_pred)

fs_tree_vall_acc_bal <- acuraciaBalanceada(fs_tree_val_cm)
fs_tree_vall_acc_bal


# Avaliando melhor modelo (correlação) no teste

test_pred_corr <- predict(fs_tree_model, test_set, type="class")
fs_tree_test_cm <- retornaMatrizConfusaoRelativa(test_set$label, test_pred_corr)
fs_tree_test_cm

fs_tree_test_acc_bal <- acuraciaBalanceada(fs_tree_test_cm)
fs_tree_test_acc_bal

####### ======= RANDOM FOREST ======= #######

set.seed(60)
n_tree_list = c(1, 5, 10, 25, 50, 100, 250, 500, 1000)
acc_per_n_trees <- data.frame(n_tree=numeric(length(n_tree_list)), 
                            ac_bal_train=numeric(length(n_tree_list)), 
                            ac_bal_val=numeric(length(n_tree_list)))


for (i in 1:length(n_tree_list)){
    random_forest_model <- randomForest(formula=label ~ age + sex + country + latitude + longitude + 
                                date_onset_symptoms + date_admission_hospital + date_confirmation + lives_in_Wuhan +
                                travel_history_dates + travel_history_location + chronic_disease_binary +
                                date_death_or_discharge + travel_history_binary,
                            data= und_train_set, ntree=n_tree_list[i], mtry=3)
    
   
    # Avaliando no conjunto de treinamento e teste
    train_pred <- predict(random_forest_model, und_train_set, type="class")
    val_pred <- predict(random_forest_model, val_set, type="class")
    
    # Calculando as acurácias balanceadas
    train_acc_bal <- acuraciaBalanceada(retornaMatrizConfusaoRelativa(und_train_set$label, train_pred))
    vall_acc_bal <- acuraciaBalanceada(retornaMatrizConfusaoRelativa(val_set$label, val_pred))
    
    acc_per_n_trees[i,] = c(n_tree_list[i], 
                         train_acc_bal, 
                         vall_acc_bal)
}


melt_acc_per_n_trees <- melt(acc_per_n_trees, id="n_tree") 
ggplot(data=melt_acc_per_n_trees, aes(x=n_tree, y=value, colour=variable)) + geom_line() + geom_point()

# Treinando com o melhor número de árvores para predição

best_n_tree <- min(acc_per_n_trees[acc_per_n_trees$ac_bal_val ==  max(acc_per_n_trees$ac_bal_val), ]$n_tree)
best_n_tree


best_random_forest_model <- randomForest(formula=label ~ age + sex + country + latitude + longitude + 
                                        date_onset_symptoms + date_admission_hospital + date_confirmation + lives_in_Wuhan +
                                        travel_history_dates + travel_history_location + chronic_disease_binary +
                                        date_death_or_discharge + travel_history_binary,
                                    data= und_train_set, ntree=best_n_tree, mtry=3)


# Avaliando no conjunto de treinamento e teste
train_pred <- predict(best_random_forest_model, und_train_set, type="class")
val_pred <- predict(best_random_forest_model, val_set, type="class")


rf_train_cm <- retornaMatrizConfusaoRelativa(und_train_set$label, train_pred)
rf_train_cm

rf_val_cm <- retornaMatrizConfusaoRelativa(val_set$label, val_pred)
rf_val_cm

rf_test_cm <- retornaMatrizConfusaoRelativa(test_set$label, test_pred)
rf_test_cm



# Calculando as acurácias balanceadas
rf_train_acc_bal <- acuraciaBalanceada(rf_train_cm); rf_train_acc_bal
rf_val_acc_bal <- acuraciaBalanceada(rf_val_cm); rf_val_acc_bal
rf_test_acc_bal <- acuraciaBalanceada(rf_test_cm); rf_test_acc_bal

tabela_ac_rf <- geraTabelaAcuraciaBalanceada(tree_train_cm, tree_val_cm, tree_test_cm)
tabela_ac_rf





