pacman::p_load('ggplot2', 'purrr', 'stringr')

#### Item d) ----------------------------------
obitos_cid <- read.csv("Trabalho1/Obitos_CID10.csv", header = TRUE)
anos <- seq(2015, 2020)
Grupos_i <- seq(1, 7)
Grupos <- paste("Grupo", Grupos_i, sep = " ")
n <- length(Grupos_i)


grupo_data.i <- map(Grupos, ~filter(obitos_cid, `Causa.Base` == .x))
for (i in 1:n) {
  ggplot(data = grupo_data.i[[i]]) +
    geom_line(aes(x = Ano, y = Obitos, colour = Sexo), size = 1.5) +
    theme_bw() +
    ggsave(paste0("Trabalho1/Assets/CID10_", str_replace(Grupos[i], " ", ""), ".png"))
}


