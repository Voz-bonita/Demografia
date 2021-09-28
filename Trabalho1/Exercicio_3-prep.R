pacman::p_load('read.dbc', 'foreign', 'dplyr', 'purrr', 'kableExtra', 'ggplot2')

#### Item d) ----------------------------------

### Capítulos do CID-10
## Agrupamentos
caps_df <- data.frame(
  "Grupo" = paste("Grupo", seq(1,7), sep = " "),
  "Capitulos" = c(paste("I", "X", "XVIII", sep = ", "),
                  paste("II", "III", "IV", sep = ", "),
                  paste("V", "VI", "VII", "VIII", sep = ", "),
                  paste("IX", "XI", "XII", sep = ", "),
                  paste("XIII", "XIV", "XV", sep = ", "),
                  paste("XVI", "XVII", "XIX", sep = ", "),
                  paste("XX", "XXI", "**", sep = ", "))
)
kbl(caps_df, booktabs = T,
    caption = "\\label{tab:CID10gp}Agrupamento dos capítulos da CID-10") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)


## Relação com Covid
# covid <- c(1, 10, 18)
cid10hash <- data.frame(matrix(ncol = 26, nrow = 1))

## Alfabeto completo
names(cid10hash) <- map_chr(seq(65, 65+25), intToUtf8)
cid10hash[c("A","B","J","R")] <- "Grupo 1"
cid10hash[c("C", "D", "E")] <- "Grupo 2"
cid10hash[c("F", "G", "H")] <- "Grupo 3"
cid10hash[c("I", "K", "L")] <- "Grupo 4"
cid10hash[c("M", "N", "O")] <- "Grupo 5"
cid10hash[c("P", "Q")] <- "Grupo 6"
cid10hash[c("S", "T", "V", "W", "X", "Y", "Z", "U")] <- "Grupo 7"

sexohash <- data.frame(
  "0" = "Feminino",
  "1" = "Masculino",
  "2" = "Feminino"
)

anos <- seq(2015, 2020)
n_anos <- length(anos)
obitos <- data.frame(matrix(ncol = 4, nrow = 0)) %>%
  rename_all( ~ c("Sexo", "Causa Base", "Obitos", "Ano"))

for (i in 1:n_anos) {
  base <- read.dbc(paste0("Trabalho1/SIM/DOSP", anos[i], ".dbc"))

  ## Gambiarra para obter os grupos
  base$CAUSABAS <- levels(base$CAUSABAS)[as.numeric(base$CAUSABAS)] %>%
    map_chr(~ cid10hash[[substr(.x, 1, 1)]])

  base$SEXO <- map_chr(base$SEXO, ~ sexohash[[.x]])

  agrupado <- base %>%
    group_by(SEXO) %>%
    select(CAUSABAS, SEXO) %>%
    count(CAUSABAS) %>%
    rename_all( ~ c("Sexo", "Causa Base", "Obitos")) %>%
    mutate(Ano = anos[i])
  obitos <- rbind(obitos, agrupado)
}

write.csv(obitos, 'Trabalho1/Obitos_CID10.csv', row.names=FALSE)