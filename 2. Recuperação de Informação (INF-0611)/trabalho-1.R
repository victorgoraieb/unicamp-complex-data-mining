######################################################################
# INF-0611 Recuperação de Informação                                 #
#                                                                    #
# Trabalho 1 - Recuperação de Texto                                  #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   Victor Teodoro Goraieb                                           #
#   Andre de Souza Gonçalves                                         #
#   Vitor Anastacio da Silva                                         #
#                                                                    #
######################################################################

######################################################################
# Configurações Preliminares                                         #
######################################################################

# Carregando as bibliotecas
library(corpus)
library(dplyr)
library(udpipe)
library(tidytext)
library(tidyverse)


# Carregando os arquivos auxiliares
source("./ranking_metrics.R", encoding = "UTF-8")
source("./trabalho1_base.R", encoding = "UTF-8")

# Configure aqui o diretório onde se encontram os arquivos do trabalho
setwd("C:/Users/vgora/Desktop/MINERAÇÃO DE DADOS COMPLEXOS/INF0611010/Trabalho 1")

######################################################################
#
# Questão 1
#
######################################################################

# Lendo os documentos (artigos da revista TIME)
# sem processamento de texto (não mude essa linha)
docs <- process_data("time.txt", "XX-Text [[:alnum:]]", "Article_0", 
                     convertcase = TRUE, remove_stopwords = FALSE)
# Visualizando os documentos (apenas para debuging)
head(docs)

# Lendo uma lista de consultas (não mude essa linha)
queries <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                        "Query_0", convertcase = TRUE, 
                        remove_stopwords = FALSE)
# Visualizando as consultas (apenas para debuging)
head(queries)
# Exemplo de acesso aos tokens de uma consulta
q1 <- queries[queries$doc_id == "Query_01",]; q1

# Lendo uma lista de vetores de ground_truth
ground_truths <- read.csv("relevance.csv", header = TRUE)

# Visualizando os ground_truths (apenas para debuging)
head(ground_truths)
# Exemplo de acesso vetor de ground_truth da consulta 1:
ground_truths[1,]
# Exemplo de impressão dos ids dos documentos relevantes da consulta 1:
# Visualizando o ranking (apenas para debuging)
names(ground_truths)[ground_truths[1,]==1]

# Computando a matriz de termo-documento
term_freq <- document_term_frequencies(docs);

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats <- as.data.frame(document_term_frequencies_statistics(term_freq, k=1.2, b=0.8));
# Visualizando as estatísticas da coleção (apenas para debuging)
head(docs_stats)

######################################################################
#
# Questão 2
#
######################################################################


# query: Elemento da lista de consultas, use a segunda coluna desse 
#        objeto para o cálculo do ranking
# ground_truth: Linha do data.frame de ground_truths referente a query
# stats: data.frame contendo as estatísticas da base
# stat_name: Nome da estatística de interesse, como ela está escrita 
#            no data.frame stats
# top: Tamanho do ranking a ser usado nos cálculos de precisão 
#      e revocação
# text: Título adicional do gráfico gerado, deve ser usado para 
#       identificar a questão e a consulta
computa_resultados <- function(query, ground_truth, stats, stat_name, 
                               top, text) {
  # Criando ranking (função do arquivo base)
  ranking <- get_ranking_by_stats(stat_name,stats,query$word)
  # Visualizando o ranking (apenas para debuging)
  head(ranking, n = 5)
  
  # Calculando a precisão
  p <- precision(ground_truth, ranking$doc_id, top);

  # Calculando a revocação
  r <- recall(ground_truth, ranking$doc_id, top)
  
  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], "\nPrecisão: ", p, 
            "\tRevocação: ", r, "\n"))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  plot_prec_e_rev(ranking$doc_id,ground_truth,top,text)
}


# Definindo a consulta 1 
consulta1 <- queries[queries$doc_id == "Query_01",]
n_consulta1 <- 1

## Exemplo de uso da função computa_resultados:
computa_resultados(consulta1, ground_truths[n_consulta1, ], 
                    docs_stats, "tf_idf", 
                    top = 15, "Consulta 1 - Tf-idf")


# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1, ground_truths[n_consulta1, ], 
                   docs_stats, "tf_idf", 
                   top = 15, "Consulta 1 - Tf-idf")

# Resultados para a consulta 1 e bm25

computa_resultados(consulta1, ground_truths[n_consulta1, ], 
                   docs_stats, "bm25", 
                   top = 15, "Consulta 1 - BM-25")

# Definindo a consulta 2 
consulta2 <- queries[queries$doc_id == "Query_034",]
n_consulta2 <- 34

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2, ground_truths[n_consulta2, ], 
                   docs_stats, "tf_idf", 
                   top = 15, "Consulta 2 - Tf-idf")

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2, ground_truths[n_consulta2, ], 
                   docs_stats, "bm25", 
                   top = 15, "Consulta 2 - BM-25")


######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################

# Inicialmente, para as consulta 1 e 2, identificamos que o BM25 apresenta um 
# recall e precisão consideravelmente maiores para um top k de 7 a 9, indicando que seja a melhor
# opcao para esse SRI. Especificamente o modelo tf-idf para a consulta 1 teve um
# recall e precisão de 0.43 para top k=7 enquanto que o BM25, 0.85 para as duas medidas para esse top k. Já para a consulta 2, 
# observamos que para top-k menores que 9, o modelo BM25 apresentou precisão e acurácia maiores que o tf-idf

# Alem disso, como forma de avaliar todas as 59 consultas disponíveis, criamos 
# a função abaixo que utiliza todas as queries e computa a média das precisões 
# médias. Com isso, conseguimos um valor de map para o modelo BM25
# de 0.66 e para o tf_idf de 0.58. Dessa forma, conseguimos concluir que
# nesse quesito o modelo BM25 é o modelo mais adequado para essa busca.


total_mean_avp_prec <- function(queries, ground_truths, doc_stats, stats_name,k=15){

  lista_avg_precision<-list(NULL)
  
  for(query in unique(queries$doc_id)){
    
    consulta <- queries[queries$doc_id == query,]
    n_consulta <- as.integer(substr(query,7,nchar(query)))
    
    ranking <- get_ranking_by_stats(stats_name,docs_stats,consulta$word)
  
    lista_avg_precision[[n_consulta]] <- list(ground_truths[n_consulta, ], ranking$doc_id)
      
  }
  
  return(map(lista_avg_precision, k))
}

map_bm25<-total_mean_avp_prec(queries, ground_truths, doc_stats,'bm25',15)
map_tf_idf<-total_mean_avp_prec(queries, ground_truths, doc_stats,'tf_idf',15)

######################################################################
#
# Questão 3
#
######################################################################
# Na função process_data está apenas a função para remoção de 
# stopwords está implementada. Sinta-se a vontade para testar 
# outras técnicas de processamento de texto vista em aula.

# Lendo os documentos (artigos da revista TIME) 
# com processamento de texto
docs_proc <- process_data("time.txt", "XX-Text [[:alnum:]]",  
                          "Article_0", convertcase = TRUE, 
                          remove_stopwords = TRUE)
# Visualizando os documentos (apenas para debuging)
head(docs_proc)


# Lendo uma lista de consultas
queries_proc <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                             "Query_0", convertcase = TRUE, 
                             remove_stopwords = TRUE)
# Visualizando as consultas (apenas para debuging)
head(queries_proc)

# Computando a matriz de termo-documento
term_freq_proc <- document_term_frequencies(docs_proc)

# Computando as estatísticas da coleção e convertendo em data.frame
docs_stats_proc <- as.data.frame(document_term_frequencies_statistics(term_freq_proc, k=1.2, b=0.8));


# Definindo a consulta 1 
consulta1_proc <- queries_proc[queries_proc$doc_id == "Query_01",]
n_consulta1_proc <- 1
# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ], 
                   docs_stats_proc, "tf_idf", 
                   top = 15, "Consulta 1 Proc- Tf-idf")

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ], 
                   docs_stats_proc, "bm25", 
                   top = 15, "Consulta 1 Proc- BM25")


# Definindo a consulta 2 
consulta2_proc <- queries_proc[queries_proc$doc_id == "Query_034",]
n_consulta2_proc <- 34

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ], 
                   docs_stats_proc, "tf_idf", 
                   top = 15, "Consulta 2 Proc- Tf-idf")

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ], 
                   docs_stats_proc, "bm25", 
                   top = 15, "Consulta 2 Proc- BM25")

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################
# Para a consulta 1 (Query 01) vimos que o tf-idf apresentou ganhos
# consideráveis para top k menores. Já o BM25 os ganhos foram quase
# imperceptíveis.
#
# Para a consuta 2 (Query 34) vimos que o tf-idf apresentou bons ganhos
# em precisão e recall para top k's maiores que 10. Já para o BM25 nessa
# mesma consulta os ganhos foram bem menos perceptíveis, conseguimos identificar
# apenas uma precisao maior para top k=3.
# 
# Aproveitando a funcao que criamos para a questao 2, conseguimos com-
# parar a media das precisoes medias para as consultas removendo as 
# stop-words. No caso vimos que tanto o modelo BM25 como o tf_idf 
# apresentaram ganhos no map, sendo que esse ganho foi de 3.4% para
# o BM25 e de 0.1% para o tf-idf.
#
# No geral acreditamos que o BM25 seja o melhor modelo para esse SRI
#

map_bm25_proc<-total_mean_avp_prec(queries_proc, ground_truths, docs_stats_proc,'bm25',15)
map_tf_idf_proc<-total_mean_avp_prec(queries_proc, ground_truths, docs_stats_proc,'tf_idf',15)

map_bm25_proc/map_bm25
map_tf_idf_proc/map_tf_idf

######################################################################
#
# Extra
#
# # Comando para salvar todos os plots gerados e que estão abertos no 
# Rstudio no momemto da execução. Esse comando pode ajudar a comparar 
# os gráfico lado a lado.
# 
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics",
                              full.names = TRUE);
plots.png.paths <- list.files(plots.dir.path, pattern=".png", 
                               full.names = TRUE)
file.copy(from=plots.png.paths, to="C:/Users/vgora/Desktop/MINERAÇÃO DE DADOS COMPLEXOS/INF0611010/Trabalho 1/resultados")
######################################################################
































