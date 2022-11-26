########################################
# Trabalho Final        
# Nome(s): 
# Victor Teodoro Goraieb
# André de Souza Gonçalves
# Vitor Anastacio da Silva
########################################


# Importação de pacotes necessários
library(ggplot2)
library(reshape2)
library(stringr)
library(tseries)
# install.packages("reshape2")
# install.packages("tseries")

# 1. Lendo os dados a partir do csv disponibilizado
names <- c("horario", "temp", "vento","umid", "sensa")
setwd('C:/Users/vgora/Desktop/MINERAÇÃO DE DADOS COMPLEXOS/INF0612010 - Análise de Dados/Trabalho Final')
cepagri <- read.csv('cepagri.csv', sep = ';', header = FALSE, col.names = names)

# Utilizando a funcao summary, podemos ver que temos 31451 dados faltantes nas colunas
# de umidade e de sensacao termica no total de 388248 registros (8%).
summary(cepagri)
sum(is.na(cepagri$sensa))/length(cepagri$horario)

# Nota-se que o máximo da sensação térmica é 99,9, o que não faz muito sentido. Vamos substituir por NA para não afetar outras medidas:
cepagri$sensa[cepagri$sensa == 99.9 & is.na(cepagri$sensa) == FALSE] <- NA

# Também notamos que a temperatura está com tipo character, temos que converter para numeric:
cepagri$temp <- as.numeric(cepagri$temp)

# É possível checar que o horário também está como character, vamos fazer a conversão
cepagri$horario <- as.POSIXlt(cepagri$horario, format = '%d/%m/%Y-%H:%M', tz = "America/Sao_Paulo")

# Criando colunas de mes, ano e hora a partir do horario
cepagri$ano <- unclass(cepagri$horario)$year + 1900
cepagri$mes <- unclass(cepagri$horario)$mon + 1
cepagri$hora <- unclass(cepagri$horario)$hour

# Como o dado está em constante atualizacao, utilizamos dados de 2015 ate 2020
cepagri <- cepagri[cepagri$horario >= as.POSIXct("2015-01-01 00:00:00", tz="America/Sao_Paulo"), ]
cepagri <- cepagri[cepagri$horario < as.POSIXct("2021-01-01 00:00:00", tz="America/Sao_Paulo"), ]


# 1. A temperatura media ao longo dos anos veem aumentando?

# Ao fazer a contagem de dados faltantes, podemos perceber que a maioria da 
# falta de registros de temperatura ocorreram em 2017 e em 2020. 2017 possui uma alta porcentagem de nulo, mas nada comparado a 2020.
# Para 2020 vamos retirar o ano inteiro dado que quase 40% dos registros está nula, já para os outros anos, incluindo 2017 vamos 
# apenas retirar somente os valores nulos posteriormente.

vazios_ano <- tapply(cepagri$temp, cepagri$ano, is.na)
vazios_ano <- data.frame(ano = names(vazios_ano), vazio = sapply(unname(vazios_ano), mean)); vazios_ano

# O mesmo pode ser feito avaliando os meses também. Nesse caso, notamos que os meses de julho até outubro possuem uma alta porcentagem de 
# valores nulos, enquanto que os outros meses ficaram em aprox 1% - 3%.
vazios_mes <- tapply(cepagri$temp, cepagri$mes, is.na)
vazios_mes <- data.frame(ano = names(vazios_mes), mes = sapply(unname(vazios_mes), mean)); vazios_mes


# Agora retirando de fato os valores nulos e salvando em um dataframe diferente
cepagri_filtered <- cepagri[is.na(cepagri$temp)==FALSE, ]
cepagri_filtered <- cepagri_filtered[cepagri_filtered$horario < as.POSIXct("2020-01-01 00:00:00", tz="America/Sao_Paulo"), ]

# Checando os dados vemos que ainda temos a sensação térmica com alguns valores nulos, entretanto notamos que todas as outras colunas estão com valores preenchidos.
# Nesse caso, decidimos preencher com a mediana para não perder as informações das outras colunas

summary(cepagri_filtered)
cepagri_filtered$sensa[is.na(cepagri_filtered$sensa) == TRUE] <- median(cepagri_filtered$sensa[is.na(cepagri_filtered$sensa) == FALSE])
summary(cepagri_filtered)

# Calculando a temperatura média para cada mês, com arredondamento
temp_media_mes_ano <- as.data.frame(tapply(cepagri_filtered$temp, list(cepagri_filtered$mes, cepagri_filtered$ano), mean, na.rm = TRUE))

#names(temp_media_mes_ano) <- paste("ano_", names(temp_media_mes_ano), sep = "")

temp_media_mes_ano <- round(temp_media_mes_ano, 1)
temp_media_mes_ano$mes <- rownames(temp_media_mes_ano)
temp_media_mes_ano$mes <- str_pad(temp_media_mes_ano$mes, 2, pad = "0")
temp_media_mes_ano

# Salvando em um csv para o pdf
write.csv(temp_media_mes_ano, "temp_media_mes_ano.csv")


# Manipulando os dados para alimentar no ggplot2 e gerar a 
# visualização de gráfico de linha

temp_media_mes_ano_melt <- reshape2::melt(temp_media_mes_ano, id.var='mes')
names(temp_media_mes_ano_melt)[names(temp_media_mes_ano_melt) == "value"] <- "temp_media"
names(temp_media_mes_ano_melt)[names(temp_media_mes_ano_melt) == "variable"] <- "ano"
temp_media_mes_ano_melt$mes <- str_pad(temp_media_mes_ano_melt$mes, 2, pad = "0")

p <- ggplot(temp_media_mes_ano_melt, 
            aes(x = mes, y = temp_media,  group=ano)) + geom_line(aes(color = ano, linetype = ano))
p <- p + theme_minimal();
#p <- p + labs(title = "Temperatura média por mês e por ano",  x = "Mês", y = "Temperatura média"); p
p <- p + labs(x = "Mês", y = "Temperatura média"); p

# Outra análise interessante é avaliar em quantos meses cada ano teve uma
# temperatura máxima/mínima no período. Para isso vamos criar uma função customizada

rowMaxs <- function(m) {
  apply(m, 1, max)
}

rowMins <- function(m) {
  apply(m, 1, min)
}

temp_media_mes_ano$max_temp <- rowMaxs(temp_media_mes_ano[,1:5])
temp_media_mes_ano

temp_media_mes_ano$min_temp <- rowMins(temp_media_mes_ano[,1:5])
temp_media_mes_ano

tbl_mes_max <- reshape2::melt(colSums(temp_media_mes_ano == temp_media_mes_ano$max_temp)[1:5])
names(tbl_mes_max)[names(tbl_mes_max) == "value"] <- "Temperaturas máximas no ano"
names(tbl_mes_max)[names(tbl_mes_max) == "variable"] <- "Ano"
write.csv(tbl_mes_max, "tbl_mes_max.csv")


tbl_mes_min <- reshape2::melt(colSums(temp_media_mes_ano == temp_media_mes_ano$min_temp)[1:5])
names(tbl_mes_min)[names(tbl_mes_min) == "value"] <- "Temperaturas mínimas no ano"
names(tbl_mes_min)[names(tbl_mes_min) == "variable"] <- "Ano"
write.csv(tbl_mes_min, "tbl_mes_min.csv")


# 2. A sensacao terminca eh mais influencidada pela temperatura? Em em dias com mais vento e mais 
# umidade temos sensacoes termicasmenores?


# 2.1 Matriz de correlacao

corr<- round(cor(cepagri_filtered[ ,c("temp","vento","umid","sensa","ano","mes","hora")]),2)
melted_corr <- melt(corr); melted_corr

p <- ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value))
p <- p + geom_tile(color = "white")
p <- p + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                              midpoint = 0, limit = c(-1,1), space = "Lab", 
                              name="Pearson\nCorrelation")
p <- p + theme_minimal(); p


# 3. Como sao as distribuicoes de temperatura ao longo do dia em meses quentes e em meses frios

cepagri_filtered_v2 <- cepagri_filtered
cepagri_filtered_v2 <- cepagri_filtered_v2[complete.cases(cepagri_filtered_v2$temp), ]

temp_hora_mes <- cepagri_filtered[,c("mes","hora","temp")] 

meses_quentes_ou_frios <- function(mes) {
  return(ifelse(mes >=5 & mes < 9, "Inverno", "Verão"))
}

periodo_hora_dia <- function(hora) {
  
  if(hora <= 6)
    return('1. 0 até 6 horas')
  if(hora <= 12)
    return('2. 7 até 12 horas')
  if(hora <= 18)
    return('3. 13 até 18 horas')
  
  return('4. 19 até 24 horas')
}

temp_hora_mes$tipo_mes <- unlist(lapply(temp_hora_mes$mes,meses_quentes_ou_frios))
temp_hora_mes$periodo_dia <- unlist(lapply(temp_hora_mes$hora,periodo_hora_dia))

p <- ggplot(temp_hora_mes, aes(x=periodo_dia, y=temp, fill=tipo_mes))
p <- p + geom_boxplot()
p <- p + labs(x = "Periodo do dia", y = "Temperatura");
p <- p + theme_minimal()
#p <- p + scale_fill_brewer(palette="RdBu")
p <- p + scale_fill_manual(values = c("steelblue", "red"))
p


# 4. Avaliação via violino

p<- ggplot(temp_hora_mes, aes(x = periodo_dia, y = temp, group = periodo_dia)) + 
  geom_violin()
p <- p + labs(x = "Periodo do dia", y = "Temperatura")
p <- p + theme_minimal()
p

p<- ggplot(temp_hora_mes, aes(x = as.factor(mes), y = temp, group = mes)) + 
  geom_violin()
p <- p + labs(x = "Mês", y = "Temperatura")
p <- p + theme_minimal()
p

# 5. Avaliação de série temporal

cepagri_filtered_v2 <- cepagri
cepagri_filtered_v2 <- cepagri_filtered_v2[cepagri_filtered_v2$horario < as.POSIXct("2018-01-01 00:00:00", tz="America/Sao_Paulo"), ]
cepagri_filtered_v2 <- cepagri_filtered_v2[complete.cases(cepagri_filtered_v2$temp), ]
cepagri_filtered_v2$dia <- as.Date(cepagri_filtered_v2$horario)


p <- ggplot(cepagri_filtered_v2, aes(x=dia, y=temp, group = dia)) +
  geom_line(alpha=0.4) + 
  xlab("")
p <- p + labs(y = "Temperatura")
p <- p + theme_minimal()
p

temp_trend <- cepagri_filtered_v2$temp/max(cepagri_filtered_v2$temp)

# Teste de Dickey Fuller para verificação de séries estacionárias
adf.test(temp_trend)


# 4. Avaliando sensação térmica vs literatura

cepagri_test <- cepagri_filtered[complete.cases(cepagri_filtered[ , c("temp","sensa","vento")]),]
summary(cepagri_test)

cepagri_test$st_lit <- 33 + (10 * sqrt(cepagri_test$vento)+ 10.45 - cepagri_test$vento)*  ((cepagri_test$temp - 33))/22

cepagri_test$abs_error_st <- abs(cepagri_test$sensa - cepagri_test$st_lit)


avg_mes_st <- tapply(cepagri_test$abs_error_st,cepagri_test$mes, mean)

resultado <- reshape2::melt(avg_mes_st)
names(resultado) <- c("Mês", "MAE")

resultado
