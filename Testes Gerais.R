library(pacman)
p_load(tidyverse)


#assumindo type=undirected, depois fazer com direcionadas.
dados_got = read_csv("asoiaf-all-edges.csv")
directed = FALSE

nomes = unique(c(dados_got[["Source"]],dados_got[["Target"]]))
nrow = length(nomes)

#adicionar parametro para sort ou nao
nomes = sort(nomes)

matriz = matrix(0, nrow=nrow,ncol=nrow,dimnames = list(nomes, nomes))

#deve ter um jeito mais eficiente não é possível
## considerando que é não direcionado
for(i in dados_got$Source){
  for(j in dados_got[dados_got$Source==i,]$Target){
    w = dados_got[dados_got$Source==i & dados_got$Target==j,]$weight
    matriz[i,j] = w
    if(!directed) matriz[j,i] = w
  }
}

