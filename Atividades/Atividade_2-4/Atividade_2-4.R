pacman::p_load(LexisPlotR, dplyr, ggplot2, purrr, randomcoloR)

#### Questao 1
mes <- "01" # Janeiro
dia <- "01" # Primeiro
anos <- as.character(seq(2008,2013,1))
datas <- as.Date(map_chr(anos, ~paste(.x,mes,dia, sep = "-")))
N <- length(anos)-1

labels01 <- c(525, 522, 559, 499, 506)
labels14 <- c(73, 69, 71, 71, 65)

cores14 <- randomColor(count = N, luminosity="dark")
cores01 <- randomColor(count = N, luminosity="light")


lexis <- lexis_grid(year_start = 2008-5, year_end = 2014, age_start = 0, age_end = 5)
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

lexis + ggsave("Atividades/Atividade_2-4/Q1.png")

### Questao 2a
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



