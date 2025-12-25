#Simulando dados
set.seed(247005)
######binomial

##params
Q = 3
n = 100

max_weight = 20
probs_conexão_cluster = matrix(c(
  0.05, 0.2, 0.4,
  0.2, 0.35, 0.6,
  0.4, 0.6, 0.8
),nrow=Q) #deixando customizável mas aqui vou automar
probclusters = c(0.5,0.3,0.2) #mesma coisa

##simulação

etiquetas = sample(1:Q, n, replace = T, prob = probclusters)
conexoes = matrix(numeric(n^2),nrow=n)
for(i in 1:n){
  con = rbinom(n-i+1, max_weight, probs_conexão_cluster[etiquetas[i],etiquetas[i:n]]) + sample((-3):3,1)
  conexoes[i,i:n] = con
  if(i<n) conexoes[(i+1):n,i] = con[-1]
}

simulado_binom = list(labels = etiquetas, conections = conexoes)

#########gamma
Q = 3
n = 200
ProbMinCluster = 0.1

probs_conexão_cluster = matrix(c(
  0.2, 0.7, 0.05,
  0.2, 0.8, 0.15,
  0.05, 0.1, 0.85
),nrow=Q) #estilo terceiros, protagonistas, isolados. unilateral
probclusters = c(0.6,0.15,0.25) #mesma coisa
alphas = matrix(c(
  4, 4, 15,
  30, 2, 3,
  20, 20, 1
),nrow=Q) #idem
betas = matrix(c(
  10/4, 10/6, 10,
  15, 0.2, 0.9,
  10, 4, 0.2
),nrow=Q) #idem


etiquetas = sample(1:Q, n, replace = T, prob = probclusters)
conexoes = matrix(numeric(n^2),nrow=n)
for(i in 1:n){
  con = runif(n) < probs_conexão_cluster[etiquetas[i],etiquetas]
  conexoes[i,] = con
}
pesos = matrix(numeric(n^2),nrow=n)
for(i in 1:n){
  p = rgamma(n, shape = alphas[etiquetas[i],etiquetas], rate = betas[etiquetas[i],etiquetas])
  pesos[i,] = ifelse(conexoes[i,],p,0)
}

simulado_gamma = list(labels = etiquetas, conections = pesos)

remove(list = c("alphas", "betas", "conexoes", "pesos", "probs_conexão_cluster", "clusters_faltantes", "con",
                "etiquetas", "i", "j", "max_weight", "n", "p", "probclusters", "ProbMinCluster", "Q"))
