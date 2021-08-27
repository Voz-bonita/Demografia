pacman::p_load(LexisPlotR, ggplot2, dplyr, read.dbc, tibble, purrr, stringr)


ano_inicio <- 2000
ano_fim <- 2019
anos <- seq(ano_inicio, ano_fim)

mes <- "01" # Janeiro
dia <- "01" # Primeiro
datas <- as.Date(map_chr(anos, ~paste(.x,mes,dia, sep = "-")))
n_anos <- length(anos)

# nascidos.vivos <- numeric(n_anos)
#
# for (i in 1:n_anos) {
#   ano <- as.character(anos[i])
#   base_i_nome <- paste0("Trabalho1/SINASC/DNSP", ano, ".DBC")
#   qnt_nacimentos <- read.dbc(base_i_nome) %>%
#     nrow()
#   nascidos.vivos[i] <- qnt_nacimentos
# }
#
# write.csv(nascidos.vivos, "Trabalho1/Nascidos_vivos.csv")

nascidos.vivos <- read.csv("Trabalho1/Nascidos_vivos.csv")[2] %>%
  unlist() %>%
  unname()

qnt_anos <-5
obitos_p_ano <- data.frame(matrix(nrow = qnt_anos+1, ncol = n_anos)) %>%
  rename_all(~ as.character(anos))

gap <- 0
for (ano in anos[1:5]) {
  ano <- as.character(ano)
  base <- read.dbc(paste0("Trabalho1/SIM/DOSP", ano, ".dbc"))
  for (j in seq(0, gap)) {
    past <- as.character(as.integer(ano) - j)
    n_obitos <- sum(str_detect(na.omit(base$DTNASC), paste0("[0-9][0-9][0-9][0-9]", past)))
    obitos_p_ano[(j+1), past] <- n_obitos


  }
  gap <- gap + 1
}

lexis <- lexis_grid(ano_inicio, ano_fim,0, 5) +
   xlab("Anos") + ylab("Idade")


for (i in 1:n_anos) {
  lexis <- lexis +
    annotate(geom = "text", label = nascidos.vivos[i],
             x = datas[i] + 366/2 + 15, y = 0.1,
             color = "green")
}
## 1 ano tem 365.25 dias
anos.5 <- floor(5*365.25)

obitos.l5 <-
base <- read.dbc("Trabalho1/SIM/DOSP2000.dbc") %>%
  mutate(DTNASCFORM = as.Date(paste(substr(as.character(DTNASC), 5, 8),
                        substr(as.character(DTNASC), 3, 4),
                        substr(as.character(DTNASC), 1, 2),
                        sep = "-")),
         DTOBTFORM = as.Date(paste(substr(as.character(DTOBITO), 5, 8),
                        substr(as.character(DTOBITO), 3, 4),
                        substr(as.character(DTOBITO), 1, 2),
                        sep = "-")))

## Idade na data do obito
id.ob <- na.omit(base$DTOBTFORM - base$DTNASCFORM)
sum(id.ob <= anos.5)

