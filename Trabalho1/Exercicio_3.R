pacman::p_load('ggplot2', 'purrr', 'stringr', 'openxlsx', 'xtable', 'tidyr', 'dplyr')


#### Item a) ----------------------------------
nmx <- read.csv('Trabalho1/3anmx.csv') %>%
  rename_all(~c("Faixa_Etaria", "Masculino", "Feminino", "Total")) %>%
  select(c(Faixa_Etaria, Masculino, Feminino)) %>%
  pivot_longer(cols = c(Masculino, Feminino)) %>%
  rename_all(~c("Faixa_Etaria", "Sexo", "nMx")) %>%
  mutate(Faixa_Etaria = factor(Faixa_Etaria, levels = unique(Faixa_Etaria)))

ggplot(nmx, aes(x = Faixa_Etaria, y = nMx,
                colour = Sexo, group = Sexo)) +
  geom_line(size = 1.1) +
  geom_point(size = 2, stroke = 0) +
  xlab("Faixa Etária") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position="bottom")


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

## Faz com que o print mostre a primeira coluna como valores inteiros
tabua_masculino$`$x$` <- factor(tabua_masculino$`$x$`, levels = tabua_masculino$`$x$`)
tabua_feminino$`$x$` <- factor(tabua_feminino$`$x$`, levels = tabua_feminino$`$x$`)

print(xtable(tabua_masculino, caption = "Tábua de vida para o sexo masculino, em 2019, no estado de São Paulo."),
      sanitize.text.function=function(x){x},
      latex.environments = "center",
      caption.placement = "top",
      include.rownames=FALSE)
print(xtable(tabua_feminino, caption = "Tábua de vida para o sexo feminino, em 2019, no estado de São Paulo."),
      sanitize.text.function=function(x){x},
      latex.environments = "center",
      caption.placement = "top",
      include.rownames=FALSE)


### Grafico do lx e nqx
indicador_hash <- tibble(
  "lx" = "Masculino",
  "lx2" =  "Feminino",
  "nqx" = "Masculino",
  "nqx2" =  "Feminino",
)
lx_tab <- tabua %>%
  select(x, n, lx, lx2) %>%
  pivot_longer(cols = c(lx, lx2)) %>%
  mutate(name = unname(unlist(indicador_hash[name]))) %>%
  rename_all(~ c("x", "n", "Sexo", "lx"))

ggplot(data = lx_tab, aes(x = x, y = lx, colour = Sexo)) +
  geom_line(size = 1.1) +
  geom_point(size = 2, stroke = 0) +
  scale_y_continuous(breaks = seq(40000,100000,10000),
                     labels = format(seq(40000,100000,10000), scientific = FALSE)) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  xlab("Idade (anos)")

nqx_tab <- tabua %>%
  select(x, n, nqx, nqx2) %>%
  pivot_longer(cols = c(nqx, nqx2)) %>%
  mutate(name = unname(unlist(indicador_hash[name]))) %>%
  rename_all(~ c("x", "n", "Sexo", "nqx"))

ggplot(data = nqx_tab, aes(x = x, y = nqx, colour = Sexo)) +
  geom_line(size = 1.1) +
  geom_point(size = 2, stroke = 0) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  xlab("Idade (anos)")


filter(lx)