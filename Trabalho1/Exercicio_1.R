pacman::p_load(LexisPlotR, ggplot2, dplyr, read.dbc, tibble, purrr, stringr)

## Anos de interesse
ano_inicio <- 2000
ano_fim <- 2020
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
end <- idade_max*2
# Idades para posicionar os labels
idades_meio <- seq(0.25, 4.75, 0.5)
anos_frente <- c(0, rep(1:4, each = 2), 5)
for (j in 1:n_anos) {
  for (i in 1:end) {
    lexis <- lexis +
    annotate(geom = "text", label = obitos[i,j],
             # Ajuste diferente na primeira e na última observação por conta do posicionamento da diagonal
             x = datas[j] + 366/2 + 365.25*anos_frente[i] + 30*((i %% 2)*2 - 1), y = idades_meio[i],
             color = "red")
  }
  if (anos[j] >= (ano_fim-4)) {
    end <- end - 1
  }
}

# ggsave("Trabalho1/Assets/Lexis.png", lexis)

##### Probabilidade de sobreviver ate a idade exata 5 anos
obitos.coorte <- map_dbl(obitos[as.character(ano_inicio:(ano_fim-idade_max))], sum)
probs.coorte <- 1 - obitos.coorte/nascidos.vivos[1:(n_anos-idade_max)]
tab5a <- round(probs.coorte*100, 2)


plot_y_scale <- c(seq(min(tab5a), max(tab5a), 0.1), max(tab5a))
plot_y_scale.lab <- plot_y_scale %>%
  as.character() %>%
  map_chr(paste0, "%")

obitos_coorte <- ggplot() +
  geom_line(aes(as.integer(names(tab5a)), tab5a),
            color = "blue", lwd = 1.5) +
  scale_y_continuous(breaks = plot_y_scale,
                     labels = plot_y_scale.lab)  +
  theme_bw() +
  xlab("Anos") + ylab("Probabilidade de sobreviver à idade exata 5 anos")
ggsave("Trabalho1/Assets/Obitos5a.png", obitos_coorte)



##### Probabilidade de sobrevir ate o primeiro aniversário
# obitos[1:2,] representa os obitos de menores de 1 ano
ida_max <- 1
obitos.m1ano <- map_dbl(obitos[1:2, 1:(n_anos-ida_max)], sum)
probs.m1ano <- 1 - obitos.m1ano/nascidos.vivos[1:(n_anos-ida_max)]
tab1a <- round(probs.m1ano*100, 2)


plot_y_scale <- c(seq(min(tab1a), max(tab1a), 0.1), max(tab1a))
plot_y_scale.lab <- plot_y_scale %>%
  as.character() %>%
  map_chr(paste0, "%")

obitos_1a <- ggplot() +
  geom_line(aes(as.integer(names(tab1a)), tab1a),
            color = "blue", lwd = 1.5) +
  scale_y_continuous(breaks = plot_y_scale,
                     labels = plot_y_scale.lab)  +
  theme_bw() +
  xlab("Anos") + ylab("Probabilidade de sobreviver até primeiro aniversário")
ggsave("Trabalho1/Assets/Obitos1a.png", obitos_1a)
