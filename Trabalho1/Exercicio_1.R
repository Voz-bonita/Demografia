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


nascidos.vivos <- read.csv("Trabalho1/Nascidos_vivos.csv")[2] %>%
  unlist() %>%
  unname()

obitos <- read.csv("Trabalho1/Obitos_l5anos.csv")[-1] %>%
  rename_all(~ as.character(anos))


## Diagrama de Lexis (grid)
lexis <- lexis_grid(ano_inicio, ano_fim+1,0, 5) +
   xlab("Anos") + ylab("Idade")

## Diagrama de Lexis com dados
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

