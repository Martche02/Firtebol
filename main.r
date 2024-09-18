library(dplyr)
library(mirt)

calcula_rankings <- function(X) {
  # Carregar os dados
  dados <- read.csv("Cleaned_Brasileirao_2023_Data.csv")
  
  
  # Restringir os dados conforme X
  dados <- dados[(1 + 10 * (X-19)):(10 * X), ]
  
  # Calcular saldos de gols
  dados <- dados %>%
    mutate(Saldo_Home = Home_Goals - Away_Goals,
           Saldo_Away = Away_Goals - Home_Goals)
  
  # Obter lista única de todos os times
  times <- unique(c(dados$Home_Team, dados$Away_Team))
  
  # Criar uma matriz de saldos de gols
  matriz_saldos <- matrix(0, nrow = length(times), ncol = length(times), dimnames = list(times, times))
  
  for (i in 1:nrow(dados)) {
    home <- dados$Home_Team[i]
    away <- dados$Away_Team[i]
    matriz_saldos[home, away] <- dados$Saldo_Home[i]
    matriz_saldos[away, home] <- dados$Saldo_Away[i]
  }
  
  # Assegurar que os saldos contra si mesmo sejam 0
  diag(matriz_saldos) <- 0
  
  # Converter a matriz em um formato adequado para a função mirt
  matriz_interacao <- t(matriz_saldos)
  
  # Aplicar o modelo Rasch usando a biblioteca mirt
  modelo_tri <- mirt(matriz_interacao, model = 1, itemtype = 'Rasch')
  
  # Extrair os scores dos times
  scores <- fscores(modelo_tri)
  
  # Atribuir os nomes dos times às pontuações
  nomes_times <- rownames(matriz_interacao)
  scores_df <- data.frame(Time = nomes_times, Score = scores[,1])
  
  # Ordenar os times com base nos scores
  rankings <- scores_df %>%
    arrange(desc(Score))
  
  # Exibir os rankings
  # print("Rankings:")
  # print(rankings)
  
  # Ordenar os times com base nos scores em ordem ascendente
  rankings_inverso <- scores_df %>%
    arrange(Score)
  
  # Exibir os rankings em ordem inversa
  print("Rankings Inverso:")
  print(rankings_inverso)
}

# Exemplo de uso da função
calcula_rankings(36)  # Substitua 5 pelo valor de X desejado

