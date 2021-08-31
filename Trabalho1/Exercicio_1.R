pacman::p_load(LexisPlotR, ggplot2, dplyr, read.dbc, tibble, purrr, stringr)

## Anos de interesse
ano_inicio <- 2000
ano_fim <- 2019
anos <- seq(ano_inicio, ano_fim)
n_anos <- length(anos)

## Datas para diagrama de Lexis
mes <- "01" # Janeiro
dia <- "01" # Primeiro
datas <- as.Date(map_chr(anos, ~paste(.x,mes,dia, sep = "-")))

idade_max <- 5

nascidos.vivos <- read.csv("Trabalho1/Nascidos_vivos.csv")[2] %>%
  unlist() %>%
  unname()

obitos <- read.csv("Trabalho1/Obitos_l5anos.csv")[-1] %>%
  rename_all(~ as.character(anos))


## Diagrama de Lexis (grid)
lexis <- lexis_grid(ano_inicio, ano_fim+1,0, 5) +
   xlab("Anos") + ylab("Idade")

## Diagrama de Lexis com dados
## Dados do SINASC
for (i in 1:n_anos) {
  lexis <- lexis +
    annotate(geom = "text", label = nascidos.vivos[i],
             # Posicionamento pouco a frente do meio do ano por conta das diagonais
             x = datas[i] + 366/2 + 15, y = 0.1,
             color = "green")
}

## Dados do SIM
end <- idade_max+1
# Idades para posicionar os labels
idades_meio <- c(0.25, seq(1, idade_max-1), idade_max-0.25)
for (j in 1:n_anos) {
  for (i in 1:end) {
    lexis <- lexis +
    annotate(geom = "text", label = obitos[i,j],
             # Ajuste diferente na primeira e na última observação por conta do posicionamento da diagonal
             x = datas[j] + 366/2 + 15 + 15*min(1,i) + 365.25*(i-1) -50*max(0,i-5), y = idades_meio[i],
             color = "red")
  }
  if (anos[j] >= (ano_fim-4)) {
    end <- end - 1
  }
}
