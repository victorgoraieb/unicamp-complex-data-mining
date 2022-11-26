#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao       
#                       
# Trabalho Avaliativo 3 
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes do grupo:  
# Victor Teodoro Goraieb                                         #
# Andre de Souza Goncalves                                       #
# Vitor Anastacio da Silva                                       #
# 
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares 
#----------------------------------------------------------------#
# configure o caminho antes de executar
setwd("C:/Users/vgora/Desktop/MINERAÇÃO DE DADOS COMPLEXOS/INF0611010 - Recuperação de Informação/Trabalho 3") 
source("./ranking_metrics.R")
source("./trabalho3_base.R")

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
# obtem características de cor, textura e forma  
# para todas as imagens e armazena em matrizes 
# onde uma linha e uma imagem 
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


# analisando rankings
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


# criando descritor concatenando 
desc_all <- cbind(features_c,features_t,features_s)

# criando rankings com descrito concatenado
ranking_all_biloba <- get_ranking_by_distance(desc_all,consulta_biloba)
ranking_all_monogyna <- get_ranking_by_distance(desc_all,consulta_monogyna)

# analisando os rankings 
resultados_all_biloba <- analyse_rankings(ranking_all_biloba, ground_truth_biloba)
resultados_all_monogyna <- analyse_rankings(ranking_all_monogyna, ground_truth_monogyna)

resultados_all_biloba;resultados_all_monogyna


#----------------------------------------------------------------#
# Questao 3 
#----------------------------------------------------------------#

# calculando as distancias, descritor:  histograma de cor 
dist_hist_biloba <- get_distance_vector(features_c, consulta_biloba) 
dist_hist_monogyna <- get_distance_vector(features_c, consulta_monogyna)

# calculando as distancias, descritor:  textura 
dist_text_biloba <- get_distance_vector(features_t, consulta_biloba) 
dist_text_monogyna <- get_distance_vector(features_t, consulta_monogyna)

# calculando as distancias, descritor:  forma
dist_forma_biloba <- get_distance_vector(features_s, consulta_biloba) 
dist_forma_monogyna <- get_distance_vector(features_s, consulta_monogyna)

# calculando e analisando  rankings combmin
r_combmin_biloba <- names(imagens)[combmin(dist_hist_biloba, dist_text_biloba, dist_forma_biloba)]
r_combmin_monogyna <- names(imagens)[combmin(dist_hist_monogyna, dist_text_monogyna, dist_forma_monogyna)]

analyse_rankings(r_combmin_biloba, ground_truth_biloba)
analyse_rankings(r_combmin_monogyna,  ground_truth_monogyna)


# calculando e analisando  rankings combmax
r_combmax_biloba <- names(imagens)[combmax(dist_hist_biloba, dist_text_biloba, dist_forma_biloba)]
r_combmax_monogyna <- names(imagens)[combmax(dist_hist_monogyna, dist_text_monogyna, dist_forma_monogyna)]

analyse_rankings(r_combmax_biloba, ground_truth_biloba)
analyse_rankings(r_combmax_monogyna,  ground_truth_monogyna)

# calculando e analisando  rankings combsum
r_combsum_biloba <- names(imagens)[combsum(dist_hist_biloba, dist_text_biloba, dist_forma_biloba)]
r_combsum_monogyna <- names(imagens)[combsum(dist_hist_monogyna, dist_text_monogyna, dist_forma_monogyna)]

analyse_rankings(r_combsum_biloba, ground_truth_biloba)
analyse_rankings(r_combsum_monogyna,  ground_truth_monogyna)

# calculando e analisando  rankings borda
r_bordacount_biloba <- names(imagens)[bordacount(dist_hist_biloba, dist_text_biloba, dist_forma_biloba)]
r_bordacount_monogyna <- names(imagens)[bordacount(dist_hist_monogyna, dist_text_monogyna, dist_forma_monogyna)]

analyse_rankings(r_bordacount_biloba, ground_truth_biloba)
analyse_rankings(r_bordacount_monogyna,  ground_truth_monogyna)

#----------------------------------------------------------------#
# Questao 3 - RESPONDA:                   
# (i) Para a consulta da Biloba o melhor descritor foi a combinação de todos os descritores e ponderando pelo combsum.
# Ele apresentou a maior precisão média, o maior recall médio, o maior f1 score médio e a segunda maior média das precisões médias.
# Especificamente para essa consulta, acreditamos que o combsum conseguiu capturar bem a questão da cor da planta, visto que no próprio trabalho anterior
# notamos que ela possui uma cor verde claro característica em todas as amostras. Assim, também notamos que a combinação de todos os descritores era muito afetada
# pelo descritor de forma, afetando negativamente a performance.
 apply(resultados_all_biloba,2,mean)
 apply(analyse_rankings(r_combmin_biloba, ground_truth_biloba),2,mean)
 apply(analyse_rankings(r_combmax_biloba, ground_truth_biloba),2,mean)
 apply(analyse_rankings(r_combsum_biloba, ground_truth_biloba),2,mean)
 apply(analyse_rankings(r_bordacount_biloba, ground_truth_biloba),2,mean)
 
# 
# (j) Para a consulta da Monogyna, identificamos que o melhor descritor é utilizando 
# a combinação dos três rankings (cor, textura e forma) com o bordercount. Nele obtivemos
# as melhores métricas (precisão, recall, f1 e média das precisões médias). Acreditamos que
# a combinação pela borda seja a melhor pois a Monogyna se trata de uma planta em que as imagens
# da amostra se mostram consideravelmente diferentes entre si. Assim, quando combinamos a posição dos rankings
# conseguimos diluir o efeito de rankings perceberem diferentes traços da amostra.
 
 cat("Map dos Descritores combinado: ", apply(resultados_all_monogyna,2,mean)[5])
 cat("Map dos Descritores CombMin: ", apply(analyse_rankings(r_combmin_monogyna, ground_truth_monogyna),2,mean)[5])
 cat("Map dos Descritores CombMax: ", apply(analyse_rankings(r_combmax_monogyna, ground_truth_monogyna),2,mean)[5])
 cat("Map dos Descritores CombSum: ", apply(analyse_rankings(r_combsum_monogyna, ground_truth_monogyna),2,mean)[5])
 cat("Map dos Descritores BordaCount: ", apply(analyse_rankings(r_bordacount_monogyna, ground_truth_monogyna),2,mean)[5])
 
# Já para a Biloba, continuamos com a nossa afirmação que o método CombSum foi o melhor nesse caso, dado que apresentou a maior média das precisões médias.
# 
 cat("Map dos Descritores combinado: ", apply(resultados_all_biloba,2,mean)[5])
 cat("Map dos Descritores CombMin: ", apply(analyse_rankings(r_combmin_biloba, ground_truth_biloba),2,mean)[5])
 cat("Map dos Descritores CombMax: ", apply(analyse_rankings(r_combmax_biloba, ground_truth_biloba),2,mean)[5])
 cat("Map dos Descritores CombSum: ", apply(analyse_rankings(r_combsum_biloba, ground_truth_biloba),2,mean)[5])
 cat("Map dos Descritores BordaCount: ", apply(analyse_rankings(r_bordacount_biloba, ground_truth_biloba),2,mean)[5])
 
#----------------------------------------------------------------#


#----------------------------------------------------------------#
# Questao 4 - RESPONDA:                   
# (i) Comparando o descritor obtido pela combinação de todos os descritores com o descritor
# utilizando os três rankings e ponderando pelo combsum detectamos que o segundo apresentou resultados
# consideravelmente melhores tanto para a Biloba como para a Monogyna.
# Acreditamos que isso se deve ao fato do descritor combinado acabar sendo afetado pelo descritor de forma,
# apresentando as mesmas métricas desse (conforme Trabalho 2). Por outro lado, o descritor que combina os rankings pelo COMBSUM,
# apresenta métricas muito melhores. Possivelmente, isso se deve ao fato desse descritor conseguir ponderar melhor todos os três rankings,
# e assim nenhum deles acaba levando o resultado para muito fora dos outros dois.
 
 apply(resultados_all_biloba,2,mean)
 apply(analyse_rankings(r_combsum_biloba, ground_truth_biloba),2,mean)
 
 apply(resultados_all_monogyna,2,mean)
 apply(analyse_rankings(r_combsum_monogyna, ground_truth_monogyna),2,mean)
 
# 
# 
# (ii) Através da consulta que fizemos na Questão 3, a Monogyna, e comparando o ranking gerado
# pelo descritor combinado e pelo descritor com bordacount obtivemos resultados muito diferentes.
# Enquanto o primeiro ranking apresentou diversas amostras de Biloba no meio, o com o bordacount não se
# confundiu em nenhum momento, apresentando apenas amostras de Monogyna.
 
 par(mfrow = c(3,3), mar = rep(2, 4))
 lapply(names(imagens[ranking_all_monogyna])[1:6], mostrarImagemColorida)
 par(mfrow = c(3,3), mar = rep(2, 4))
 lapply(names(imagens[r_bordacount_monogyna])[1:6], mostrarImagemColorida)
 
# Como dito anteriormente, o descritor combinado acaba sendo muito afetado pelo descritor de forma e é exatamente
# o que vemos aqui, onde as amostras apresentadas são relativamente parecidas em termos de forma geral (não tanto para cor e textura).
#
# (iii) Comparando todos os rankings para a Biloba, vemos que os descritores com a average precision mais alta foram de fato os com os rankings
# mais coerentes com a consulta. Entretanto para uma amostra de apenas 6 plantas, o combsum, combmax e bordacount foram igualmente corretos em seus rankings trazendo apenas Biloba.
# Então podemos concluir que sim as métricas são um bom indicativo de performance do ranking, entretanto a partir de certo limiar de imagens retornadas pode ser difícil definir qual ranking é melhor.
 
 par(mfrow = c(3,3), mar = rep(2, 4))
 lapply(names(imagens[ranking_all_biloba])[1:6], mostrarImagemColorida)
 par(mfrow = c(3,3), mar = rep(2, 4))
 lapply(names(imagens[r_bordacount_biloba])[1:6], mostrarImagemColorida)
 par(mfrow = c(3,3), mar = rep(2, 4))
 lapply(names(imagens[r_combmin_biloba])[1:6], mostrarImagemColorida)
 par(mfrow = c(3,3), mar = rep(2, 4))
 lapply(names(imagens[r_combmax_biloba])[1:6], mostrarImagemColorida)
 par(mfrow = c(3,3), mar = rep(2, 4))
 lapply(names(imagens[r_combsum_biloba])[1:6], mostrarImagemColorida)
 
 names(imagens[ranking_all_biloba])[1:6]
 names(imagens[r_bordacount_biloba])[1:6]
 names(imagens[r_combmin_biloba])[1:6]
 names(imagens[r_combmin_biloba])[1:6]
 names(imagens[r_combmax_biloba])[1:6]
 names(imagens[r_combsum_biloba])[1:6]
 
# 
# (iv) 
# Para avaliar a média das precisões médias levando em conta as 5 consultas, montamos um loop que passa por cada consulta e computa a precisão média
# para cada método
 
 par_consulta_gtruth<-NULL
 par_consulta_gtruth <- list( list(consulta_biloba, ground_truth_biloba),
                           list(consulta_europaea, ground_truth_europaea),
                           list(consulta_ilex, ground_truth_ilex),
                           list(consulta_monogyna, ground_truth_monogyna),
                           list(consulta_regia, ground_truth_regia))

 medias_c <- NULL
 medias_t <- NULL
 medias_s <- NULL
 medias_all <- NULL
 medias_bordacount <- NULL
 medias_combsum <- NULL
 medias_combmin <- NULL
 medias_combmax <- NULL
 
 for (par in par_consulta_gtruth){
   
   consulta = par[[1]]
   g_truth = par[[2]]
   
   # Gerando descritores individuais
   ranking_c <- get_ranking_by_distance(features_c,consulta)
   ranking_t <- get_ranking_by_distance(features_t,consulta)
   ranking_s <- get_ranking_by_distance(features_s,consulta)
   
   # Gerando de descritor combinado
   ranking_all <- get_ranking_by_distance(desc_all,consulta)
   
   # Gerando descritor combinado para o BordaCount
   dist_hist <- get_distance_vector(features_c, consulta) 
   dist_text <- get_distance_vector(features_t, consulta)
   dist_forma <- get_distance_vector(features_s, consulta)
   
   
   r_bordacount <- names(imagens)[bordacount(dist_hist, dist_text, dist_forma)]
   r_combsum <- names(imagens)[combsum(dist_hist, dist_text, dist_forma)]
   r_combmin <- names(imagens)[combmin(dist_hist, dist_text, dist_forma)]
   r_combmax <- names(imagens)[combmax(dist_hist, dist_text, dist_forma)]
   
   medias_c <- c(medias_c, analyse_rankings(ranking_c,g_truth)[2,5] )
   medias_t <- c(medias_t, analyse_rankings(ranking_t,g_truth)[2,5] )
   medias_s <- c(medias_s, analyse_rankings(ranking_s,g_truth)[2,5] )
   medias_all <- c(medias_all, analyse_rankings(ranking_all,g_truth)[2,5] )
   medias_bordacount <- c(medias_bordacount, analyse_rankings(r_bordacount,g_truth)[2,5] )
   medias_combsum <- c(medias_combsum, analyse_rankings(r_combsum,g_truth)[2,5] )
   medias_combmin <- c(medias_combmin, analyse_rankings(r_combmin,g_truth)[2,5] )
   medias_combmax <- c(medias_combmax, analyse_rankings(r_combmax,g_truth)[2,5] )
   
 }
 
 # Comparando a média das precisões médias para cada método identificamos que os que apresentaram as melhores métricas foram o combsum, bordacount e o de cor.
 # Esses descritores combinados são os que conseguem balancear melhor todos os descritores da imagem sem deixar que um descritor em específico tome conta do resultado, enquanto que o descritor unico de cor
 # já se mostrava satisfatorio dada que as amostras são bem diferentes nesse sentido.
 
 cat("Descritor Cor: ",mean(medias_c) )
 cat("Descritor Textura: ",mean(medias_t) )
 cat("Descritor Forma: ",mean(medias_s) )
 cat("Descritores Combinados: ",mean(medias_all) )
 cat("Descritores BordaCount: ",mean(medias_bordacount) )
 cat("Descritores CombSum: ",mean(medias_combsum) )
 cat("Descritores CombMin: ",mean(medias_combmin) )
 cat("Descritores CombMax: ",mean(medias_combmax) )
#
#----------------------------------------------------------------#