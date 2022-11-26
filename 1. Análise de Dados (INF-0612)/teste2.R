########################################
# Teste 2         
# Nome(s): Victor Teodoro Goraieb, Andre de Souza Goncalves, Vitor Anastacio da Silva
########################################

## 1 - Agrupamento

groupsum <- function(df, colgroup, colsum){
  
  grouped_df <- tapply(df[[colsum]],df[[colgroup]],sum)
  
  final_df <- data.frame( c1 = names(grouped_df), c2 = grouped_df, row.names = NULL)
  
  names(final_df)<- c(colgroup,colsum)
  
  return(final_df)
}

##### Exemplos no PDF:
dia <- c(01, 01, 02, 02, 03, 03, 04, 04, 05, 05)
cidade <- c('Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas')
chuva <- c(0.15, 0.02, 0.01, 0.13, 0.12, 2.19, 1.11, 0.76, 2.98, 0.45)
chuvas <- data.frame(cidade, dia, chuva)
groupsum(chuvas, "cidade", "chuva")

## 2 - Binario para Decimal

binToDec <- function(...){
    
  accum_final <- NULL
  
  for (i in list(...)){
    
    # Criando vetor de potencias de 2 do tamanho do vetor de entrada
    twos = 2 ** (0:(length(i) - 1))
    
    # Assim fazemos a soma dos produtos elemento a elemento do vetor de potencias de 2 e o vetor de entrada invertido 
    accum_final <- c(accum_final, sum(rev(i) * twos))
    
  }
  return(accum_final)
}

##### Exemplos no PDF:
binToDec(c(1, 0))
binToDec(c(0, 0, 1), c(1, 1))
binToDec(rep(1, 3), rep(0, 2), rep(c(1,0), 2))

## 3 - Ocorrencia de Palavras

wordCount <- function(word, text) {
  
  text <- tolower(text);text
  
  #Definindo as pontuacoes do texto
  pontuacoes <- c("\\.", ",", "!","?")
  
  #Retirada das pontuacoes do texto
  for (pontuacao in pontuacoes) {
    text <- gsub(pontuacao, "", text)
  }
  
  # Quebra do texto em palavras
  palavras <- strsplit(text, split = " ")
  
  # Retornando o total de ocorrencia da palavra
  return (sum(unlist(palavras) == word))
}

##### Exemplos no PDF:
text <- "O rAto roeu a roupa do Rei de Roma! RainhA raivosa rasgou o resto."
wordCount("rato", text)
wordCount("roma", text)
text <- "A vaca malHada foi molhADA por outra VACA, MOLhada e MALhaDa."
wordCount("outra", text)
wordCount("vaca", text)
wordCount("malhada", text)
text <- "Se a liga me ligasse, eu tambem ligava a liga. Mas a liga nao me liga, eu tambem nao ligo a liga."
wordCount("liga", text)
wordCount("ligasse", text)

