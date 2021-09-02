#### NOT RUN {
pacman::p_load(LexisPlotR, ggplot2, dplyr, read.dbc, tibble, purrr, stringr)
### Algoritmo dedicado à preparação dos dados do exercicio 1 do trabalho

f.idade <- function (base, past, idade){
  if (idade == 0) {
    base.idade <- (base$IDADE <= 400)
  } else if (idade > 0) {
    base.idade <- (base$IDADE == 400+idade)
  }
  obts <- str_detect(base$DTNASC, paste0("[0-9][0-9][0-9][0-9]", past)) & base.idade

  return(obts)
}

ano_inicio <- 2000
ano_fim <- 2019
anos <- seq(ano_inicio, ano_fim)

mes <- "01" # Janeiro
dia <- "01" # Primeiro
datas <- as.Date(map_chr(anos, ~paste(.x,mes,dia, sep = "-")))
n_anos <- length(anos)

idade_max <- 5

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
qnt_anos <- 5
obitos_p_ano <- data.frame(matrix(nrow = qnt_anos*2, ncol = n_anos)) %>%
  rename_all(~ as.character(anos))


gap <- 0
for (ano in anos) {

  base <- read.dbc(paste0("Trabalho1/SIM/DOSP", as.character(ano), ".dbc"))
  base$IDADE <- levels(base$IDADE)[as.numeric(base$IDADE)]
  base <- base %>%
    filter(IDADE <= (400 + idade_max-1)) %>%
    select(c("IDADE", "DTNASC")) %>%
    na.omit()

  for (ref in seq(0, gap)) {
    past <- as.character(ano - ref)
    idade.atu <- ano-as.integer(past)
    if (ref == 0) {
      n_obitos <- sum(f.idade(base, past, idade.atu))
      obitos_p_ano[ref+1, past] <- n_obitos
    } else if (ref == gap & ano >= 2005) {
      n_obitos <- sum(f.idade(base, past, idade.atu-1))
      obitos_p_ano[ref*2, past] <- n_obitos
    } else {
      n_obitos_aniinc <- sum(f.idade(base, past, idade.atu-1))
      n_obitos_anicmp <- sum(f.idade(base, past, idade.atu))
      obitos_p_ano[ref*2, past] <- n_obitos_aniinc
      obitos_p_ano[ref*2+1, past] <- n_obitos_anicmp
    }

  }
  if (gap < idade_max) {
    gap <- gap + 1
  }
}

write.csv(obitos_p_ano, "Trabalho1/Obitos_l5anos.csv")
#### }