# Pirámide Población ----

## Usando: `pyramid` ----

### Datos
Edad <- c(
  "0 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29","30 a 34",
  "35 a 39","40 a 44","45 a 49","50 a 54","55 a 59","60 a 64","65 a 69",
  "70 a 74","75 a 79","80 a 84","85 y más")

Hombres <- c(
  1555605,1705574,1848218,1970530,1983553,1835158,1649783,1560417,
  1308328,1245829,1213908,1057242,840134,639772,457772,325224,205428,168018)

Mujeres <- c(
  1482176,1629666,1762366,1881725,1956735,1857016,1700746,1656227,
  1436336,1400272,1382470,1223557,984516,750320,546647,405409,281348,256392)

datos <- base::data.frame(
  H1 = base::round(Hombres/1000, 0),
  M1 = base::round(Mujeres/1000, 0))

### Gráfico
pyramid::pyramid(
  data = datos,
  Llab = "Hombres",
  Rlab = "Mujeres",
  Clab = "Edad",
  main = "Población de Colombia 2018 \n (en miles)",
  Lcol = "green", 
  Rcol = "cyan", 
  Cgap = 0.5
)

## Usando: `apyramid` y `ggplot2` ----

### Datos
linelist <- base::readRDS(
  file = "data/linelist_cleaned.rds"
)

### Gráfico
apyramid::age_pyramid(
  data = linelist,
  age_group = "age_cat5",
  split_by = "gender",
  show_midpoint = FALSE, # TRUE: Para mostrar los medios en cada intervalo
  proportional = FALSE # TRUE: Para mostrar porcentajes
) +
  ggplot2::theme_minimal() +
  ggplot2::scale_fill_manual(
    values = c("#5679FD", "#D294FE"),
    labels = c("m" = "Male", "f" = "Female")) +
  ggplot2::labs(
    y = "Percent of all cases",
    x = "Age categories",
    fill = "Gender",
    caption = "My data source and caption here",
    title = "Title of my plot",
    subtitle = "Subtitle with \n a second line...") +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text = ggplot2::element_text(size = 10, face = "bold"),
    axis.title = ggplot2::element_text(size = 12, face = "bold"))

## Usando: `ggplot2` - Opción 1 ----

### Datos
url <- 'https://www.populationpyramid.net/api/pp/76/2019/?csv=true'
data <- utils::read.csv(url)

data <- 
  data |> 
  tidyr::pivot_longer(
    names_to = 'Gender', 
    values_to = 'Population', 
    cols = 2:3) |> 
  dplyr::mutate(
    PopPerc = dplyr::case_when(
      Gender == 'M' ~ base::round(Population/base::sum(Population)*100,2),
      TRUE ~ - base::round(Population/base::sum(Population)*100,2)),
    signal = dplyr::case_when(
      Gender == 'M' ~ 1,
      TRUE ~ -1),
    Age = base::factor(Age, levels = base::unique(Age), ordered = TRUE))

### Gráfico
data |> 
ggplot2::ggplot() +
  ggplot2::geom_bar(
    mapping = ggplot2::aes(x = Age, y = PopPerc, fill = Gender), stat = 'identity') +
  ggplot2::geom_text(
    mapping = ggplot2::aes(
      x = Age, 
      y = PopPerc + signal * .3, 
      label = base::abs(PopPerc))) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(name = '', values = c('darkred', 'steelblue')) +
  ggplot2::scale_y_continuous(
    breaks = base::seq(-10,10,1),
    labels = function(x){base::paste(base::abs(x), '%')}) +
  ggplot2::labs(
    x = '',
    y = 'Population (%)',
    title = 'Population Pyramid of Brazil',
    subtitle = base::paste(
      'Total resident population in 2019:', 
      base::format(base::sum(data$Population), decimal.mark = '.')),
    caption = 'Source: PopulationPyramid.net') +
  cowplot::theme_cowplot() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(vjust = .5),
    panel.grid.major.y = ggplot2::element_line(
      color = 'lightgray', 
      linetype = 'dashed'),
    legend.position = 'top',
    legend.justification = 'center')

## Usando: `ggplot2` - Opción 2 ----
### Datos
df <- base::data.frame(
  Type = base::sample(c('Male', 'Female', 'Female'), 1000, replace=TRUE),
  Age = sample(18:60, 1000, replace=TRUE))

AgesFactor <- base::ordered(
  base::cut(
    df$Age, breaks = c(18, base::seq(20,60,5)),
    include.lowest = TRUE))

df$Age <- AgesFactor

### Gráfico
gg <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = Age))

gg.male <- gg + 
  ggplot2::geom_bar(
    data = base::subset(df, Type == 'Male'),
    mapping = ggplot2::aes(
      y = ..count../base::sum(ggplot2::after_stat(count)), 
      fill = Age)) +
  ggplot2::scale_y_continuous(name = '', labels = scales::percent) + 
  ggplot2::theme(
    legend.position = 'none',
    axis.title.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(size = 11.5),
    plot.margin = grid::unit(c(0.1,0.2,0.1,-.1), "cm"),
    axis.ticks.y = ggplot2::element_blank(), 
    axis.text.y = ggplot2::theme_bw()$axis.text.y) + 
  ggplot2::ggtitle("Male") + 
  ggplot2::coord_flip()    

gg.female <- gg + 
  ggplot2::geom_bar(
    data = base::subset(df, Type == 'Female'), 
    mapping = ggplot2::aes(
      y = ..count../base::sum(ggplot2::after_stat(count)), 
      fill = Age)) +
  ggplot2::scale_y_continuous(
    name = '', 
    labels = scales::percent, 
    trans = 'reverse') + 
  ggplot2::theme(
    legend.position = 'none',
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(), 
    plot.title = ggplot2::element_text(size = 11.5),
    plot.margin = grid::unit(c(0.1,0,0.1,0.05), "cm")) + 
  ggplot2::ggtitle("Female") + 
  ggplot2::coord_flip() + 
  ggplot2::ylab("Age")

gridExtra::grid.arrange(
  gg.female,
  gg.male,
  widths = c(0.4,0.6),
  ncol = 2
)

