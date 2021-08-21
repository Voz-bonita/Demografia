pacman::p_load(LexisPlotR, dplyr, ggplot2, purrr, randomcoloR)

#### Questao 1
mes <- "01" # Janeiro
dia <- "01" # Primeiro
anos <- as.character(seq(2008,2013,1))
datas <- as.Date(map_chr(anos, ~paste(.x,mes,dia, sep = "-")))
N <- length(anos)-1

labels01 <- c(525, 522, 559, 499, 506)
labels14 <- c(73, 69, 71, 71, 65)

# set.seed nao funcionou
# cores14 <- randomColor(count = N, luminosity="dark")
#cores01 <- randomColor(count = N, luminosity="light")
cores14 <- c("#bc5912", "#9e013d", "#037a09", "#068744", "#0b428e")
cores01 <- c("#b2f78f", "#fcf99c", "#b5e3fc", "#a2b8ef", "#c1ff7f")

lexis <- lexis_grid(year_start = 2008-5, year_end = 2014, age_start = 0, age_end = 5) +
    labs(x = "Anos", y = "Idade")
for (i in 1:N) {
  lexis <- lexis +
    annotate(geom = "rect", fill = cores14[i], alpha = 0.7,
             xmin = datas[i], xmax = datas[i+1],
             ymin = 1, ymax = 5) +
    annotate(geom = "text", label = labels14[i],
             x = datas[i] + 366/2, # Meio do ano
             y = (5+1)/2) +
    annotate(geom = "rect", fill = cores01[i], alpha = 0.7,
             xmin = datas[i], xmax = datas[i+1],
             ymin = 0, ymax = 1) +
    annotate(geom = "text", label = labels01[i],
             x = datas[i] + 366/2,
             y = (0+1)/2)
}

lexis


### Questao 2a
rm(list = ls())
## 1999
obt99 <- c(463,138,34,23,19,10,11,10,7,4)
1 - sum(obt99)/26498

## 2000
obt00 <- c(487,100,28,13,9,15,9,10,9,1)
1 - sum(obt00)/26205

## 2001
obt01 <- c(486,73,28,25,12,7,7,11,6,8)
1 - sum(obt01)/27050

## 2002
obt02 <- c(455,81,24,14,10,6,9,6,8,10)
1 - sum(obt02)/26274

### Questao 2b
## 1999 - 2006
(463+138)/26498
(487+100)/26205
(486+73)/27050
(455+81)/26274
(433+61)/26053
(421+57)/25673
(392+75)/26368
(385+65)/36028


### Questao 3
rm(list = ls())
obitos <- c(12500,4058,1023,854,
            410,300,274,221,186)
idades <- seq(0.25,4.25,0.5)
N <- length(obitos)

ano_interesse <- as.Date("1995-01-01") + 366/2 # Meio do ano
align <- 30 # Distorcao para separar os anos

lexis <- lexis_grid(year_start = 1991, year_end = 1997, age_start = 0, age_end = 5) %>%
  lexis_year(year = 1995) +
  labs(x = "Anos", y = "Idade")

for (i in 1:N) {
  lexis <- lexis +
    annotate(geom = "text", label = obitos[i],
             x = ano_interesse + align,
             y = idades[i])
  align <- -align
}

lexis


### Questao 4
rm(list = ls())
idades <- seq(12.5,52.5,5)
anos <- as.character(seq(2014-45,2014,5))
n_filhos <- c(3,317,537,393,223,131,63,17,2)
N <- length(idades)

dia <- "01"
mes <- "01"
datas <- as.Date(map_chr(anos, ~paste(.x, mes, dia, sep = "-")))
lexis <- lexis_grid(year_start = 2014-45, year_end = 2014, age_start = 10, age_end = 55) %>%
  lexis_cohort(cohort = 2014-45) +
  theme(axis.text.x = element_text(angle = -20)) +
  scale_x_continuous(breaks = datas) +
  scale_y_continuous(breaks = seq(10,55,5)) +
  xlab("Anos") + ylab("Idade")

for (i in 1:N) {
  lexis <- lexis +
    annotate(geom = "text", label = n_filhos[i],
             x = datas[i] + 365.25*2.5, y = idades[i])
}
