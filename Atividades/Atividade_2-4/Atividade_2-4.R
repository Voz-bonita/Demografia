pacman::p_load(LexisPlotR, dplyr, ggplot2, purrr, randomcoloR)


mes <- "01" # Janeiro
dia <- "01" # Primeiro
anos <- as.character(seq(2008,2013,1))
datas <- as.Date(map_chr(anos, ~paste(.x,mes,dia, sep = "-")))
N <- length(anos)-1

labels01 <- c(525, 522, 559, 499, 506)
labels14 <- c(73, 69, 71, 71, 65)

cores14 <- randomColor(count = N, luminosity="dark")
cores01 <- randomColor(count = N, luminosity="light")


lexis <- lexis_grid(year_start = 2008-4, year_end = 2014, age_start = 0, age_end = 5)
for (i in 1:N) {
  lexis <- lexis +
    annotate(geom = "rect", fill = cores14[i], alpha = 0.7,
             xmin = datas[i], xmax = datas[i+1],
             ymin = 1, ymax = 4) +
    annotate(geom = "text", label = labels14[i],
             x = datas[i] + 366/2, # Meio do ano
             y = (4+1)/2) +
    annotate(geom = "rect", fill = cores01[i], alpha = 0.7,
             xmin = datas[i], xmax = datas[i+1],
             ymin = 0, ymax = 1) +
    annotate(geom = "text", label = labels01[i],
             x = datas[i] + 366/2,
             y = (0+1)/2)
}

lexis + ggsave("Atividades/Atividade_2-4/Q1.png")
