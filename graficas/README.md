# Gráfico de series

## Gráfico de barras

<img src="img/barras_1.jpeg" alt="Barras" width="400"/>

```r
# Datos
ppp.DF <- base::data.frame(
  country = pwt10::pwt10.0$country, 
  isocode = pwt10::pwt10.0$isocode,
  year = pwt10::pwt10.0$year, 
  gdp = pwt10::pwt10.0$rgdpe,
  pc.GDP = (pwt10::pwt10.0$rgdpe / pwt10::pwt10.0$pop) / 1000.0)

pib2019 <- 
  ppp.DF |> 
  dplyr::filter(year == 2019) |> 
  dplyr::select(country, year, gdp, pc.GDP)

# colourCount = base::length(base::unique(pib2019$country))
colourCount = 15
getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(15, "RdYlBu"))

# Gráfico
pib2019 |> 
  dplyr::slice_head(n = 15) |> 
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = stats::reorder(country, gdp, sum), 
      y = gdp, 
      fill = country)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Los 15 países con mayor PIB para 2019",
    x = "",
    y = "PIB real (PPP) en millones de dólares de 2017",
    caption = "Fuente: Elaboración propia con información de Penn World Table 10.0") +
  ggplot2::theme(
    plot.caption = ggplot2::element_text(hjust = 0, size = 9),
    plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "black", 
      fill = NA, 
      linewidth = 1),
    axis.title.x = ggplot2::element_text(size = 6),
    axis.text.x = ggplot2::element_text(
      angle = 0,
      vjust = 0.2,
      hjust = 0.5, 
      colour = "black"),
    axis.text.y = ggplot2::element_text(
      hjust = 1,
      colour = "black",
      size = 8)) + 
  ggplot2::scale_y_continuous(label = scales::comma) +
  ggplot2::scale_fill_manual(values = getPalette(colourCount)) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(label = scales::comma(base::round(gdp,-1))),
    size = 3, 
    hjust = 1, 
    nudge_x = -0.5, 
    vjust = -0.5)
```

## Gráfico de líneas

<img src="img/lineas_1.jpeg" alt="Lineas" width="400"/>

```r
# Datos
ppp.DF <- base::data.frame(
  country = pwt10::pwt10.0$country, 
  isocode = pwt10::pwt10.0$isocode,
  year = pwt10::pwt10.0$year, 
  gdp = pwt10::pwt10.0$rgdpe,
  pc.GDP = (pwt10::pwt10.0$rgdpe / pwt10::pwt10.0$pop) / 1000.0)

PIBpc <- 
  ppp.DF |>
  dplyr::mutate(
    Diff_year = year - dplyr::lag(year),
    Diff_growth = pc.GDP - dplyr::lag(pc.GDP),
    Rate_percent = (Diff_growth / Diff_year)/pc.GDP * 100) |> 
  dplyr::filter(isocode %in% c("CHN", "USA", "MEX", "IND", "PER"))

# Gráfico
PIBpc |> 
  ggplot2::ggplot(mapping = ggplot2::aes(x = year, y = pc.GDP, color = country)) + 
  ggplot2::geom_line(size = 1, na.rm = T) +
  ggplot2::scale_color_brewer(
    name = " ",
    palette = "RdYlBu",
    direction = -1) +
  ggplot2::theme(
    plot.caption = ggplot2::element_text(hjust = 0, size = 9),
    plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text.x = ggplot2::element_text(
      angle = 0, vjust = 1, hjust = 1, colour = "black"),
    legend.position = c(0.2, 0.7),
    legend.title = ggplot2::element_blank(),
    legend.background = ggplot2::element_rect(fill = NA, colour = 1),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "black", 
      fill = NA, 
      size = 1),
    axis.text.y = ggplot2::element_text(hjust = 0, colour = "black", size = 8))+
  ggplot2::labs(
    title = "Evolución del PIB per cápita real (PPA)",
    subtitle = "Miles de dólares de 2017",
    caption = "Fuente: Elaboración propia con información de Penn World Table 10.0") +
  ggplot2::xlab("") +
  ggplot2::ylab("")+
  ggplot2::scale_y_continuous(label = scales::comma)
```

## Gráfico de variación

<img src="img/variacion_1.jpeg" alt="Variación 1" width="400"/>

```r
# Datos
TDCP <- 
  PIBpc |> 
  dplyr::filter(isocode == "MEX") |> 
  {\(x) 
    (
      (x$pc.GDP[x$year == 2019] / x$pc.GDP[x$year == 1950])**(1/base::nrow(x)) - 1
    ) *100
  }()

texto.PIBpc <- 
  base::paste0(
    'La tasa de crecimiento medio del PIB per cápita en',
    '<br><br>',
    '<span style="color:#4575b4;">',
    '**México** ',
    '</span>',
    'para el periodo 1950-2019 es de ', 
    '<span style="color:#d73027;">',
    '**1.96%**',
    '</span>')

# Gráfico
PIBpc |> 
  dplyr::filter(isocode == "MEX") |> 
  ggplot2::ggplot(mapping = ggplot2::aes(x = year, y = Rate_percent)) +
  ggplot2::geom_line(cex = 0.8, col = "#4575b4") +
  ggplot2::geom_hline(yintercept = 0, size = 0.8, col = "black") +
  ggplot2::theme(
    plot.caption = ggplot2::element_text(hjust = 0, size = 9),
    plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12,face = "bold"),
    legend.position = "none",
    legend.title = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
    axis.text.x = ggplot2::element_text(
      angle = 0, vjust = 1, hjust = 1, colour = "black"),
    axis.text.y = ggplot2::element_text(
      hjust = 0, colour = "black", size = 8)) +
  ggplot2::labs(
    title = "Variación anual del PIB real per cápita de México",
    caption = "Fuente: Elaboración propia con información de Penn World Table 10.0") +
  ggplot2::xlab("") +
  ggplot2::ylab("Puntos porcentuales") +
  ggplot2::geom_hline(
    yintercept = 1.96, 
    size = 0.8, 
    col = "#d73027",
    linetype = 'dashed') +
  ggplot2::annotate(
    geom = ggtext::GeomRichtext,
    x = 1970, 
    y = 11,
    label = texto.PIBpc,
    label.colour = NA, 
    fill = NA,
    fontface = "bold",
    color = "black",
    size = 2.5)
```

# Gráfico de Lollipop

## Opción 1

<img src="img/lollipop_1.jpeg" alt="Lollipop 1" width="400"/>

```r
# Datos
name <- base::letters[1:10]
score <- c(20,35,37,39,35,22,28,29,32,38)
df <- base::data.frame(name, score)

# Gráfico
df |> 
  ggplot2::ggplot(mapping = ggplot2::aes(x = name, y = score, color = name)) +
  ggplot2::geom_point(size = 6) +
  ggplot2::geom_segment(
    mapping = ggplot2::aes(x = name, xend = name, y = 0, yend = score), 
    size = 2) +
  ggplot2::theme_bw() + 
  ggplot2::labs(
    title = "Gráfico de lollipop",
    X = "Nombre del estudiante",
    y = "Score")
```

## Opción 2

<img src="img/lollipop_2.jpeg" alt="Lollipop 2" width="400"/>

```r
# Datos
city <- c("Delhi", "Mumbai", "Kolkata", "Naggur", "Kapurthala")
max <- c(35,32,37,41,37)
min <- c(26,25,26,29,26)
lp2 <- base::data.frame(city, max, min)
lp2 <- tidyr::gather(lp2, key = "category", value = "Temp", 2:3)

# Gráfico
ggplot2::ggplot(
  data = lp2,
  mapping = ggplot2::aes(
    x = city,
    y = Temp,
    color = category)) +
  ggplot2::geom_point(size = 10) +
  ggplot2::geom_segment(
    mapping = ggplot2::aes(
      x = city,
      xend = city, 
      y = 30,
      yend = Temp)) +
  ggplot2::theme_bw() +
  ggplot2::coord_flip() +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 30))
```

## Opción 3

<img src="img/lollipop_3.jpeg" alt="Lollipop 3" width="400"/>

```r
# Datos
msleep <- ggplot2::msleep

# Gráfico
msleep |> 
  dplyr::group_by(order) |>
  dplyr::summarise(mean_sleep = base::mean(sleep_total)) |> 
  dplyr::mutate(order = forcats::fct_reorder(order, mean_sleep)) |> 
  ggplot2::ggplot(mapping = ggplot2::aes(x = order, y = mean_sleep)) +
  ggplot2::geom_point(
    size = 6,
    colour = "orange") +
  ggplot2::geom_segment(
    mapping = ggplot2::aes(
      x = order,
      y = base::mean(msleep$sleep_total),
      xend = order,
      yend = mean_sleep),
    colour = "grey") +
  ggplot2::geom_hline(
    yintercept = base::mean(msleep$sleep_total),
    colour = "grey",
    size = 1) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::labs(
    title="Sueño de tiempo promedio de mamíferos por orden",
    x="",
    y="Horas")
```

# Gráficos financieros

## Gráfico de líneas

<img src="img/financieras_lineas.jpeg" alt="Líneas" width="400"/>

```r
# Datos
quantmod::getSymbols(Symbols = "AAPL", src="yahoo", from="2021-01-02")

# Gráfico
quantmod::chartSeries(
  x = `AAPL`, 
  type = "line",
  subset = '2022',
  theme = quantmod::chartTheme('white'))
```

## Gráfico de barras

<img src="img/financieras_barras.jpeg" alt="Barras" width="400"/>

```r
# Datos
quantmod::getSymbols(Symbols = "AAPL", src="yahoo", from="2021-01-02")

# Gráfico
quantmod::chartSeries(
  x = `AAPL`,
  type = "bar",
  subset = '2022',
  theme = quantmod::chartTheme('white'))
```

## Gráfico de velas

<img src="img/finacieras_velas.jpeg" alt="Velas" width="400"/>

```r
# Datos
quantmod::getSymbols(Symbols = "AAPL", src="yahoo", from="2021-01-02")

# Gráfico
quantmod::chartSeries(
  x = `AAPL`,
  subset = '2021-05::2022-02',
  up.col = 'green',
  down.col = 'black',
  theme = quantmod::chartTheme('white'))
```

## Incluir: Media Movil

<img src="img/financieras_media_movil.jpeg" alt="Media Movil" width="400"/>

```r
# Datos
quantmod::getSymbols(Symbols = "AAPL", src="yahoo", from="2021-01-02")

# Gráfico
quantmod::chartSeries(
  x = `AAPL`,
  subset = "last 10 months",
  theme = quantmod::chartTheme('white'))

quantmod::addSMA()
quantmod::addSMA(n = 20, on = 1, col = "green")
quantmod::addSMA(n = 50, on = 1, col = "blue")
quantmod::addSMA(n = 200, on = 1, col = "red")
quantmod::addEMA(n = 30, on = 1, col = "orange")
```

## Incluir: Bandas de Bollinger

<img src="img/financieras_bollinger.jpeg" alt="Bollinger" width="400"/>

```r
# Datos
quantmod::getSymbols(Symbols = "AAPL", src="yahoo", from="2021-01-02")

# Gráfico
quantmod::chartSeries(
  x = `AAPL`,
  subset = '2021-05::2022-02',
  theme = quantmod::chartTheme('white'))

quantmod::addBBands(n = 20, sd = 2)
```

## Incluir: Momentos

<img src="img/finacieras_momentos.jpeg" alt="Momentos" width="400"/>

```r
# Datos
quantmod::getSymbols(Symbols = "AAPL", src="yahoo", from="2021-01-02")

# Gráfico
quantmod::chartSeries(
  x = `AAPL`,
  subset = '2021-05::2022-02',
  theme = quantmod::chartTheme('white'))

quantmod::addMomentum(n = 10)
```


# Gráfico de pirámide población

## Usando: `pyramid`

<img src="img/piramide_1.jpeg" alt="Pirámide Poblacional" width="400"/>

```r
# Datos
Edad <- c(
  "0 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29","30 a 34","35 a 39",
  "40 a 44","45 a 49","50 a 54","55 a 59","60 a 64","65 a 69","70 a 74",
  "75 a 79","80 a 84","85 y más")

Hombres <- c(
  1555605,1705574,1848218,1970530,1983553,1835158,1649783,1560417,1308328,
  1245829,1213908,1057242,840134,639772,457772,325224,205428,168018)

Mujeres <- c(
  1482176,1629666,1762366,1881725,1956735,1857016,1700746,1656227,1436336,
  1400272,1382470,1223557,984516,750320,546647,405409,281348,256392)

datos <- base::data.frame(
  H1 = base::round(Hombres/1000, 0),
  M1 = base::round(Mujeres/1000, 0))

# Gráfico
pyramid::pyramid(
  data = datos,
  Llab = "Hombres",
  Rlab = "Mujeres",
  Clab = "Edad",
  main = "Población de Colombia 2018 \n (en miles)",
  Lcol = "green", 
  Rcol = "cyan", 
  Cgap = 0.5)
```

## Usando: `apyramid` y `ggplot2`

<img src="img/piramide_2.jpeg" alt="Pirámide Poblacional" width="400"/>

```r
# Datos
linelist <- base::readRDS(
  file = "data/linelist_cleaned.rds"
)

# Gráfico
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
```

## Usando: `ggplot2` - Opción 1

<img src="img/piramide_3.jpeg" alt="Pirámide Poblacional" width="400"/>

```r
# Datos
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

# Gráfico
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
```

## Usando: `ggplot2` - Opción 2

<img src="img/piramide_4.jpeg" alt="Pirámide Poblacional" width="400"/>

```r
# Datos
df <- base::data.frame(
  Type = base::sample(c('Male', 'Female', 'Female'), 1000, replace=TRUE),
  Age = sample(18:60, 1000, replace=TRUE))

AgesFactor <- base::ordered(
  base::cut(
    df$Age, breaks = c(18, base::seq(20,60,5)),
    include.lowest = TRUE))

df$Age <- AgesFactor

# Gráfico
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
```