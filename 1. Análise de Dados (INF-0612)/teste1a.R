########################################
# Teste 1a - INF-0612          
# Nome(s): Victor Teodoro Goraieb, Andre de Souza Goncalves, Vitor Anastacio da Silva
########################################

## Os vetores C, L e V representam os produtos distribuidos nas cidades de Campinas, Limeira e Vinhedo, respectivamente.

C <- c("Xampu", "Sabonete", "Arroz", "Chocolate", "Leite", "Refrigerante", "Queijo", "Suco", "Vinho", "Cerveja")
L <- c("Leite", "Cerveja", "Arroz", "Chocolate")
V <- c("Sabonete", "Detergente", "Refrigerante", "Carne", "Vinho", "Chocolate", "Papel", "Leite", "Iogurte")


## Perguntas:

## Quais os produtos que sao vendidos em Campinas, mas nao sao vendidos em Limeira?

##RESPOSTA: Para saber quais produtos sao vendidos em uma cidade mas nao em outra podemos tratar cada cidade como um conjunto. 
## No caso, usando a funcao setdiff(cidade1, cidade2) teriamos exatamente esse resultado
diferenca_camp_limeira<-setdiff(C,L)

## Quais os produtos que sao vendidos em Vinhedo, mas nao sao vendidos em Campinas?
##RESPOSTA: Mesmo racional da letra a
diferenca_vinhedo_camp <- setdiff(V,C)

## Quais os produtos que sao vendidos em pelo menos uma cidade?
##RESPOSTA: Para saber todos os produtos vendidos, podemos simplismente pegar a uniao dos vetores (funcao union())
venda_cidades <- union(union(C,L),V)

## Quais os produtos que sao vendidos em todas as cidades?
##RESPOSTA: Para saber quais produtos sao comuns a todas as cidades, podemos utilizar a funcao intersect() nos tres vetores
prod_tds_cidades <- intersect(intersect(C,V),L)

## Se a filial de Limeira parar a distribuicao de produtos, a filial de Campinas 
## possui todos os itens necessarios para atender a demanda de Limeira?
## RESPOSTA: Para saber se Campinas consegue atender a demanda de Limeira podemos checar se todo o vetor Limeira está em Campinas pelo setequal
## No caso, comparando com a intersecção dos dois vetores vemos que sim, Campinas contém todos os produtos de Limeira
atende_prod <- setequal(L, intersect(C, L))
