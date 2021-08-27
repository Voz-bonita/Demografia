#### NOT RUN {
### Algoritmo dedicado à preparação dos dados do exercicio 1 do trabalho


ano_inicio <- 2000
ano_fim <- 2019
anos <- seq(ano_inicio, ano_fim)

mes <- "01" # Janeiro
dia <- "01" # Primeiro
datas <- as.Date(map_chr(anos, ~paste(.x,mes,dia, sep = "-")))
n_anos <- length(anos)


## Dados do SINASC
nascidos.vivos <- numeric(n_anos)

for (i in 1:n_anos) {
  ano <- as.character(anos[i])
  base_i_nome <- paste0("Trabalho1/SINASC/DNSP", ano, ".DBC")
  qnt_nacimentos <- read.dbc(base_i_nome) %>%
    nrow()
  nascidos.vivos[i] <- qnt_nacimentos
}

write.csv(nascidos.vivos, "Trabalho1/Nascidos_vivos.csv")


## Dados do SIM
qnt_anos <-5
obitos_p_ano <- data.frame(matrix(nrow = qnt_anos+1, ncol = n_anos)) %>%
  rename_all(~ as.character(anos))

gap <- 0
for (ano in anos) {
  ano <- as.character(ano)
  base <- read.dbc(paste0("Trabalho1/SIM/DOSP", ano, ".dbc"))
  for (j in seq(0, gap)) {
    past <- as.character(as.integer(ano) - j)
    n_obitos <- sum(str_detect(na.omit(base$DTNASC), paste0("[0-9][0-9][0-9][0-9]", past)))
    obitos_p_ano[(j+1), past] <- n_obitos
  }
  if (gap < 5) {
    gap <- gap + 1
  }
}

write.csv(obitos_p_ano, "Trabalho1/Obitos_l5anos.csv")
#### }