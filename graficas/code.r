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
  file = "graficas/linelist_cleaned.rds"
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




#INFORMACION DE BRAZIL
# Load packages
library(ggplot2)
library(tidyverse)

# Load data
url <- 'https://www.populationpyramid.net/api/pp/76/2019/?csv=true'
data <- read.csv(url)
head(data)

# Manipulating data
data <- data%>%
  pivot_longer(names_to = 'Gender', values_to = 'Population', cols = 2:3) %>%
  mutate(PopPerc=case_when(Gender=='M'~round(Population/sum(Population)*100,2),
                           TRUE~-round(Population/sum(Population)*100,2)),
         signal=case_when(Gender=='M'~1,
                          TRUE~-1))
head(data)
levels(data$Age)

data$Age <- factor(data$Age,levels=unique(data$Age),ordered=TRUE)

# Plotting
ggplot(data)+
  geom_bar(aes(x=Age,y=PopPerc,fill=Gender),stat='identity')+
  geom_text(aes(x=Age,y=PopPerc+signal*.3,label=abs(PopPerc)))+
  coord_flip()+
  scale_fill_manual(name='',values=c('darkred','steelblue'))+
  scale_y_continuous(breaks=seq(-10,10,1),
                     labels=function(x){paste(abs(x),'%')})+
  labs(x='',y='Population (%)',
       title='Population Pyramid of Brazil',
       subtitle=paste('Total resident population in 2019:', format(sum(data$Population),big.mark='.')),
       caption='Source: PopulationPyramid.net')+
  cowplot::theme_cowplot()+
  theme(axis.text.x=element_text(vjust=.5),
        panel.grid.major.y = element_line(color='lightgray',linetype='dashed'),
        legend.position = 'top',
        legend.justification = 'center')


# generando data frame



library(ggplot2)
library(plyr)
library(gridExtra)

## The Data
df <- data.frame(Type = sample(c('Male', 'Female', 'Female'), 1000, replace=TRUE),
                 Age = sample(18:60, 1000, replace=TRUE))

AgesFactor <- ordered(cut(df$Age, breaks = c(18,seq(20,60,5)), 
                          include.lowest = TRUE))

df$Age <- AgesFactor

## Plotting
gg <- ggplot(data = df, aes(x=Age))

gg.male <- gg + 
  geom_bar( data=subset(df,Type == 'Male'), 
            aes( y = ..count../sum(..count..), fill = Age)) +
  scale_y_continuous('', labels = scales::percent) + 
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        plot.title = element_text(size = 11.5),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        axis.ticks.y = element_blank(), 
        axis.text.y = theme_bw()$axis.text.y) + 
  ggtitle("Male") + 
  coord_flip()    

gg.female <-  gg + 
  geom_bar( data=subset(df,Type == 'Female'), 
            aes( y = ..count../sum(..count..), fill = Age)) +
  scale_y_continuous('', labels = scales::percent, 
                     trans = 'reverse') + 
  theme(legend.position = 'none',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        plot.title = element_text(size = 11.5),
        plot.margin=unit(c(0.1,0,0.1,0.05),"cm")) + 
  ggtitle("Female") + 
  coord_flip() + 
  ylab("Age")

## Plutting it together
grid.arrange(gg.female,
             gg.male,
             widths=c(0.4,0.6),
             ncol=2
)
