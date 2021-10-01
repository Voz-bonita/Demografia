pacman::p_load('ggplot2', 'purrr', 'stringr', 'openxlsx', 'xtable')

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


#### Item e) ----------------------------------
tabua <- read.xlsx('Trabalho1/Tabua_de_vida.xlsx')
tabua[is.na(tabua)] <- ''
tabua_masculino <- tabua[1:(length(tabua)/2)]
tabua_feminino <- tabua[-1:-(length(tabua)/2)]


## Remove linha de comentário nos prints de xtable
options(xtable.comment = FALSE)

## Nomes especiais das colunas em modo equacao
TeX_names <- c('$x$', '$n$', '$_{n}M_x$',
               '$_nk_x$', '$_nq_x$', '$l_x$',
               '$_nd_x$', '$_nL_x$', '$T_x$', '$e_x$')
colnames(tabua_masculino) <- TeX_names
colnames(tabua_feminino) <- TeX_names

print(xtable(tabua_masculino, caption = "Tábua de vida para o sexo masculino, no estado de São Paulo."),
      sanitize.text.function=function(x){x},
      latex.environments = "center",
      caption.placement = "top",
      include.rownames=FALSE)
print(xtable(tabua_feminino, caption = "Tábua de vida para o sexo feminino, no estado de São Paulo."),
      sanitize.text.function=function(x){x},
      latex.environments = "center",
      caption.placement = "top",
      include.rownames=FALSE)