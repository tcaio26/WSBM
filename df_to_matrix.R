## Função df->matriz

dados = read_csv("asoiaf-book1-edges.csv")

df_to_matriz = function(data, source=1, target=2, weight=3, directed=FALSE, sort=TRUE, def.weight=0){
  #tratamento
  df = data[c(source, target, weight)]
  names(df) = c('source','target','weight')
  #nao pode ter duplicadas
  #source e target devem ser texto, weight int/float.
  
  #função
  names = unique(c(pull(df, source),pull(df,target)))
  if(sort) names = sort(names)
  n = length(names)
  
  matriz = matrix(def.weight, nrow = n, ncol = n, dimnames = list(names, names))
  for(i in df$source){
    for(j in df[df$source==i,]$target){
      w = df[df$source==i & df$target==j,]$weight
      matriz[i,j]=w
      if(!directed) matriz[j,i]=w
    }
  }
  
  return(matriz)
}

m = df_to_matriz(dados)
view(m)
