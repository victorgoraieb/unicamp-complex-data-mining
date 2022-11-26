#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao                             #
#                                                                #
# Trabalho Avaliativo 2                                          #
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes do grupo:                        #
# Victor Teodoro Goraieb                                         #
# Andre de Souza Goncalves                                       #
# Vitor Anastacio da Silva                                       #
#                                                                #
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares   
#----------------------------------------------------------------#
# configure o caminho antes de executar
setwd("C:/Users/vgora/Desktop/MINERAÇÃO DE DADOS COMPLEXOS/INF0611010 - Recuperação de Informação/Trabalho 2") 
source("./ranking_metrics.R")
source("./trabalho2_base.R")


# caminho da pasta de imagens
path_plantas = './plantas'

#----------------------------------------------------------------#
# Leitura das imagens                 
#----------------------------------------------------------------#
imagens <- read_images(path_plantas)

#----------------------------------------------------------------#
# Obtem classe de cada imagem             
#----------------------------------------------------------------#
nome_classes <- get_classes(path_plantas)

#----------------------------------------------------------------#
# obtem ground_truth para cada classe 
#----------------------------------------------------------------#
ground_truth_biloba <- get_ground_truth(path = path_plantas, classes = nome_classes, classe_relevante = 'biloba')
ground_truth_europaea <- get_ground_truth(path = path_plantas, classes = nome_classes, classe_relevante = 'europaea')
ground_truth_ilex <- get_ground_truth(path = path_plantas, classes = nome_classes, classe_relevante = 'ilex')
ground_truth_monogyna <- get_ground_truth(path = path_plantas, classes = nome_classes, classe_relevante = 'monogyna')
ground_truth_regia <- get_ground_truth(path = path_plantas, classes = nome_classes, classe_relevante = 'regia')

#----------------------------------------------------------------#
# Questao 1                               
#----------------------------------------------------------------#

# obtem caracteristicas de cor  
hist_cor_desc <- function(img){
  r <- hist(img[,,1]*255, plot=FALSE, breaks=0:255)$counts
  g <- hist(img[,,2]*255, plot=FALSE, breaks=0:255)$counts
  b <- hist(img[,,3]*255, plot=FALSE, breaks=0:255)$counts
  return(c(r, g, b))
}


# obtem caracteristicas de textura   
lbp_desc <- function(img){
  img <- grayscale(img)
  r1 <- lbp(img[,,1,1],1)
  lbp_uniforme <- hist(r1$lbp.u2, plot=FALSE, breaks=59)$counts
  return(c(lbp_uniforme))
}

# obtem caracteristicas de forma 
Momentos <-function(img){
  
  centroide <- function(M) {
    c(momento(M, 1, 0) / momento(M, 0, 0),
      momento(M, 0, 1) / momento(M, 0, 0))
  }
  
  momento <- function(M, p, q, central = FALSE) {
    r <- 0
    if (central) {
      c <- centroide(M)
      x <- c[1]
      y <- c[2]
    } else {
      x <- 0
      y <- 0
    }
    for (i in 1:nrow(M))
      for (j in 1:ncol(M))
        r <- r + (i - x)^p * (j - y)^q * M[i,j]  
    return(r)
  }
  
  img <- grayscale(img)[,,1,1]
  features <-NULL
  
  for(i in 0:2){
    for(j in 0:2){
      features <- cbind(features,momento(img, i,j, central=TRUE))
    }
  }
  return(features)
}

#----------------------------------------------------------------#
# obtem caracteristicas de cor, textura e forma para todas as imagens e 
# armazena em matrizes onde uma linha representa uma imagem 
features_c <- t(sapply(imagens, hist_cor_desc))
rownames(features_c) <- names(imagens)
features_t <- t(sapply(imagens, lbp_desc))
rownames(features_t) <- names(imagens)
features_s <- t(sapply(imagens, Momentos))
rownames(features_s) <- names(imagens)

#----------------------------------------------------------------#
# Questao 2                               
#----------------------------------------------------------------#

# definindo as consultas
# obs.:  use o caminho completo para a imagem
consulta_biloba <- "./plantas/biloba_02.jpg"
consulta_europaea <- "./plantas/europaea_01.jpg"
consulta_ilex <- "./plantas/ilex_08.jpg"
consulta_monogyna <- "./plantas/monogyna_04.jpg"
consulta_regia <- "./plantas/regia_07.jpg"

# visualizando as consultas
par(mfrow = c(3,3), mar = rep(2, 4))
mostrarImagemColorida(consulta_biloba)
mostrarImagemColorida(consulta_europaea)
mostrarImagemColorida(consulta_ilex)
mostrarImagemColorida(consulta_monogyna)
mostrarImagemColorida(consulta_regia)


#-----------------------------#
# construindo rankings                          
# para cada uma das 5 consultas, construa um ranking com base na cor
ranking_c_biloba <- get_ranking_by_distance(features_c,consulta_biloba)
ranking_c_europaea <- get_ranking_by_distance(features_c,consulta_europaea)
ranking_c_ilex <- get_ranking_by_distance(features_c,consulta_ilex)
ranking_c_monogyna <- get_ranking_by_distance(features_c,consulta_monogyna)
ranking_c_regia <- get_ranking_by_distance(features_c,consulta_regia)

# para cada uma das 5 consultas, construa um ranking com base na textura
ranking_t_biloba <- get_ranking_by_distance(features_t,consulta_biloba)
ranking_t_europaea <- get_ranking_by_distance(features_t,consulta_europaea)
ranking_t_ilex <- get_ranking_by_distance(features_t,consulta_ilex)
ranking_t_monogyna <- get_ranking_by_distance(features_t,consulta_monogyna)
ranking_t_regia <- get_ranking_by_distance(features_t,consulta_regia)
  
# para cada uma das 5 consultas, construa um ranking com base na forma
ranking_s_biloba <- get_ranking_by_distance(features_s,consulta_biloba)
ranking_s_europaea <- get_ranking_by_distance(features_s,consulta_europaea)
ranking_s_ilex <- get_ranking_by_distance(features_s,consulta_ilex)
ranking_s_monogyna <- get_ranking_by_distance(features_s,consulta_monogyna)
ranking_s_regia <- get_ranking_by_distance(features_s,consulta_regia)

#-----------------------------#
# comparando  rankings                              

## utilize as funções do arquivo ranking_metrics.R para calcular 
# a precisão, revocação, taxa F1 e precisão média nos 
# top 5, 10, 15 e 20

analyse_rankings <- function(ranking, ground_truth) {
  
  #Calculo da precisao
  
  resultados <- NULL
  
  p<- NULL
  
  r<- NULL
  
  f1<- NULL
  
  ap<- NULL
  
  for (k in c(5,10,15,20)){
  
    p<-c(p,precision(ground_truth,ranking,k))
    
    r<-c(r,recall(ground_truth,ranking,k))
    
    f1<-c(f1,f1_score(ground_truth,ranking,k))
    
    ap <- c(ap,ap(ground_truth, ranking,k))
    
  }
  
  resultados <- data.frame(k = c(5,10,15,20), precision =p,recall=r,f1_score=f1,avg_precision=ap)
  
  return(resultados)
}

# analisando rankings gerados com caracteristicas de cor
analyse_rankings(ranking_c_biloba, ground_truth_biloba)
analyse_rankings(ranking_c_europaea, ground_truth_europaea)
analyse_rankings(ranking_c_ilex, ground_truth_ilex)
analyse_rankings(ranking_c_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_c_regia, ground_truth_regia)

# analisando rankings gerados com caracteristicas de textura
analyse_rankings(ranking_t_biloba, ground_truth_biloba)
analyse_rankings(ranking_t_europaea, ground_truth_europaea)
analyse_rankings(ranking_t_ilex, ground_truth_ilex)
analyse_rankings(ranking_t_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_t_regia, ground_truth_regia)

# analisando rankings gerados com caracteristicas de forma
analyse_rankings(ranking_s_biloba, ground_truth_biloba)
analyse_rankings(ranking_s_europaea, ground_truth_europaea)
analyse_rankings(ranking_s_ilex, ground_truth_ilex)
analyse_rankings(ranking_s_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_s_regia, ground_truth_regia)


#----------------------------------------------------------------#
# Questao 2 - RESPONDA:                   
# (a) Escolha uma consulta para analisar mais detalhadamente e
# responda: Para essa consulta qual descritor retornou o melhor
# ranking? Lembre-se de analisar visualmente as imagens da classe,
# contextualizando o que foi extraído em cada descritor. Também
# aponte pontos fortes e fracos dos descritores usados que podem
# justificar esse comportamento.
#                                         
# Para a planta biloba, notamos que o descritor de cor foi o que apresentou                                         
# as melhores métricas no geral. Isso talvez possa ser atribuído ao fato da biloba ter
# uma cor característica de um verde mais claro que está muito presente nas imagens da amostra.
# 

# Para avaliar especificamente cada métrica, criamos 3 variáveis, uma para cada ranking e geramos a média de cada medida.
# O descritor de cor apresentou todas as maiores medidas, seguido pelo de textura e finalmente pelo de forma.

resultados_c_biloba <- analyse_rankings(ranking_c_biloba, ground_truth_biloba)
resultados_s_biloba <- analyse_rankings(ranking_t_biloba, ground_truth_biloba)
resultados_t_biloba <- analyse_rankings(ranking_s_biloba, ground_truth_biloba)

apply(resultados_c_biloba[-1],2,mean)
apply(resultados_t_biloba[-1],2,mean)
apply(resultados_s_biloba[-1],2,mean)


# (b) Considerando as 5 consultas definidas, calcule a m?dia das precis?es m?dias em top 10. 
# Avaliando por essa medida, qual descritor obteve melhores resultados? Justifique. 
# Lembre-se que para justificar sua resposta, voc? pode complementar sua an?lise usando 
# tamb?m outras medidas de avalia??o de ranking adicionais vistas na Aula 1, caso seja pertinente
#                                         

# Avaliando a média das precisões médias para cada um dos descritores, o de cor teve uma precisao media de 1,
# métrica maior que para o de texture (0.78) e forma (0.62). Acreditamos que isso seja devido ao fato de algumas plantas
# possuirem formatos parecidos como a regia e a monogyna. Além disso, a textura das plantas ilex e monogyna aparentam ser similares.
# Assim, o descritor de cor acabou sendo o melhor para distinguir entre as espécies de planta.

resultados_c_biloba <- analyse_rankings(ranking_c_biloba, ground_truth_biloba)
resultados_c_europaea <- analyse_rankings(ranking_c_europaea, ground_truth_europaea)
resultados_c_ilex <- analyse_rankings(ranking_c_ilex, ground_truth_ilex)
resultados_c_monogyna <- analyse_rankings(ranking_c_monogyna, ground_truth_monogyna)
resultados_c_regia <- analyse_rankings(ranking_c_regia, ground_truth_regia)

resultados_t_biloba <- analyse_rankings(ranking_t_biloba, ground_truth_biloba)
resultados_t_europaea <- analyse_rankings(ranking_t_europaea, ground_truth_europaea)
resultados_t_ilex <- analyse_rankings(ranking_t_ilex, ground_truth_ilex)
resultados_t_monogyna <- analyse_rankings(ranking_t_monogyna, ground_truth_monogyna)
resultados_t_regia <- analyse_rankings(ranking_t_regia, ground_truth_regia)

resultados_s_biloba <- analyse_rankings(ranking_s_biloba, ground_truth_biloba)
resultados_s_europaea <- analyse_rankings(ranking_s_europaea, ground_truth_europaea)
resultados_s_ilex <- analyse_rankings(ranking_s_ilex, ground_truth_ilex)
resultados_s_monogyna <- analyse_rankings(ranking_s_monogyna, ground_truth_monogyna)
resultados_s_regia <- analyse_rankings(ranking_s_regia, ground_truth_regia)



map_color <- mean(resultados_c_biloba[resultados_c_biloba$k == 10,-1]$avg_precision, 
                 +                 resultados_c_europaea[resultados_c_europaea$k == 10,-1]$avg_precision,
                 +                 resultados_c_ilex[resultados_c_ilex$k == 10,-1]$avg_precision,
                 +                 resultados_c_monogyna[resultados_c_monogyna$k == 10,-1]$avg_precision,
                 +                 resultados_c_regia[resultados_c_regia$k == 10,-1]$avg_precision)


map_texture <- mean(resultados_t_biloba[resultados_t_biloba$k == 10,-1]$avg_precision, 
                 +                 resultados_t_europaea[resultados_t_europaea$k == 10,-1]$avg_precision,
                 +                 resultados_t_ilex[resultados_t_ilex$k == 10,-1]$avg_precision,
                 +                 resultados_t_monogyna[resultados_t_monogyna$k == 10,-1]$avg_precision,
                 +                 resultados_t_regia[resultados_t_regia$k == 10,-1]$avg_precision)


map_shape <- mean(resultados_s_biloba[resultados_s_biloba$k == 10,-1]$avg_precision, 
                 +                 resultados_s_europaea[resultados_s_europaea$k == 10,-1]$avg_precision,
                 +                 resultados_s_ilex[resultados_s_ilex$k == 10,-1]$avg_precision,
                 +                 resultados_s_monogyna[resultados_s_monogyna$k == 10,-1]$avg_precision,
                 +                 resultados_s_regia[resultados_s_regia$k == 10,-1]$avg_precision)

map_color;map_texture;map_shape

#                                         
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Questao 3
#----------------------------------------------------------------#
# concatenando caracteristicas                      

## obter vetores finais de caracteristicas pela concatenação de 
# cada tipo de caracteristica (cor, textura e forma):
features_concat = cbind(features_c,features_t,features_s)
  
# gerar novos rankings

ranking_concat_biloba <- get_ranking_by_distance(features_concat,consulta_biloba)
ranking_concat_europaea <- get_ranking_by_distance(features_concat,consulta_europaea)
ranking_concat_ilex <- get_ranking_by_distance(features_concat,consulta_ilex)
ranking_concat_monogyna <- get_ranking_by_distance(features_concat,consulta_monogyna)
ranking_concat_regia <- get_ranking_by_distance(features_concat,consulta_regia)


# analisando rankings gerados com caracteristicas concatenadas
analyse_rankings(ranking_concat_biloba, ground_truth_biloba)
analyse_rankings(ranking_concat_europaea, ground_truth_europaea)
analyse_rankings(ranking_concat_ilex, ground_truth_ilex)
analyse_rankings(ranking_concat_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_concat_regia, ground_truth_regia)


#----------------------------------------------------------------#
# Questao 3 - RESPONDA:  
# (a) Qual o impacto dessas alterações nas medidas de avaliação
# calculadas?
# 

# Para o caso da Biloba notamos que as métricas dos descritores combinados
# foram exatamente as mesmas do que para o descritor de forma.
# Assim, continuamos com a nossa hipotese que o descritor de cor é o melhor nessa consulta


resultados_concat_biloba <- analyse_rankings(ranking_concat_biloba, ground_truth_biloba)
resultados_concat_europaea <- analyse_rankings(ranking_concat_europaea, ground_truth_europaea)
resultados_concat_ilex <- analyse_rankings(ranking_concat_ilex, ground_truth_ilex)
resultados_concat_monogyna <- analyse_rankings(ranking_concat_monogyna, ground_truth_monogyna)
resultados_concat_regia <- analyse_rankings(ranking_concat_regia, ground_truth_regia)


apply(resultados_c_biloba[-1],2,mean)
apply(resultados_t_biloba[-1],2,mean)
apply(resultados_s_biloba[-1],2,mean)
apply(resultados_concat_biloba[-1],2,mean)


# (b) Os descritores combinados apresentaram melhores resultados?
# Justifique sua resposta.
# 
# Podemos avaliar o novo descritor através da média das precisões médias e comparar com os resultados obtidos anteriormente.
# Notamos que a media das precisoes medias para o descritor agrupado é a mesma do de forma. Assim, comparando os valores, esse descritor
# não se mostra o mais adequado para as consultas.

map_concat <- mean(resultados_concat_biloba[resultados_concat_biloba$k == 10,-1]$avg_precision, 
                  +                 resultados_concat_europaea[resultados_concat_europaea$k == 10,-1]$avg_precision,
                  +                 resultados_concat_ilex[resultados_concat_ilex$k == 10,-1]$avg_precision,
                  +                 resultados_concat_monogyna[resultados_concat_monogyna$k == 10,-1]$avg_precision,
                  +                 resultados_concat_regia[resultados_concat_regia$k == 10,-1]$avg_precision)

map_color;map_texture;map_shape;map_concat


# 
# (c) Você acredita que algum dos descritores apresentou maior
# influência na combinação? Justifique sua resposta.
# 
# Acreditamos que o descritor de forma possui uma influencia alta na combinação, dado
# que os valores das métricas de avaliação foram exatamente os mesmos desse descritor puro.
# 
#----------------------------------------------------------------#
