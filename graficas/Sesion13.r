
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

colourCount = base::length(base::unique(pib2019$country))
getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(15, "RdYlBu"))

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


CHN.DF<- ppp.DF %>% filter(isocode  == "CHN")
USA.DF<- ppp.DF %>% filter(isocode  == "USA")
MEX.DF<- ppp.DF %>% filter(isocode  == "MEX")
IND.DF<- ppp.DF %>% filter(isocode  == "IND")
PERU.DF<- ppp.DF %>% filter(isocode  == "PER")

PIBpc<-rbind(CHN.DF,USA.DF,MEX.DF,IND.DF,PERU.DF)
head(PIBpc)

#Tenemos un dataframe de 5 columnas con 350 filas, 
#una columna para todos los países, otra para los años y otra para los
#valores de la serie que nos interesan. Sin embargo, de la forma en que
#está acomodado este dataframe no nos es posible graficar una gráfica de 
#líneas con ggplot2, necesitamos un dataframe con columnas para cada país. 
#Por lo tanto procedemos con lo siguiente:

pib.pc<-data.frame(dcast(PIBpc, year ~ country, value.var='pc.GDP'))
head(pib.pc)

#De esta manera se han ido las columnas isocode y gdp,
#pues solo queremos el pc.GDP de cada país, asimismo,
#ahora cada país tiene su propia columna. 
#Ya transformado nuestro cuadro, procedemos a graficar.

pibpc.plot<-ggplot(data=pib.pc, aes(x=year))+
  geom_line(aes(y=China, color = 'China'), cex = 1)+
  geom_line(aes(y=India, color = 'India'), cex = 1)+
  geom_line(aes(y=Peru, color = 'Peru'), cex = 1)+  
  geom_line(aes(y=Mexico, color = 'México'),cex = 1)+
  geom_line(aes(y=United.States.of.America, color = 'Estados Unidos'),cex = 1)+
  scale_colour_brewer(name=" ",
                      palette="RdYlBu",
                      direction = -1)+
  theme(plot.caption=element_text(hjust=0,size=9),
        plot.title=element_text(hjust=0.5, size=14,face="bold"),
        plot.subtitle =element_text(hjust=0.5, size=12,face="bold"),
        axis.text.x=element_text(angle =0,vjust =1,
                                 hjust=1, colour = "black"),
        legend.position=c(0.2,0.7),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA,
                                         colour = 1),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y = element_text(hjust = 0,
                                   colour = "black",
                                   size = 8))+
  labs(title="Evolución del PIB per cápita real (PPA)",
       subtitle = "Miles de dólares de 2017",
       caption="Fuente: Elaboración propia con información de Penn World Table 10.0")+
  xlab(" ")+
  ylab("")+
  scale_y_continuous(label=comma)
pibpc.plot


#Vamos a calcular la tasa de crecimiento del PIB per cápita para México.
#El resultado será una serie de 1951 a 2019. 1950 no cuenta porque es el
#año inicial.


MEX.TDC=MEX.DF%>%
  mutate(Diff_year = year - lag(year),  # Restar los años tiempo en caso de que haya brechas (año actual - año anterior)
         Diff_growth = pc.GDP - lag(pc.GDP), # Restar al valor presente del pib pc el valor inmediato anterior
         Rate_percent = (Diff_growth / Diff_year)/pc.GDP * 100) # TDC en %)
head(MEX.TDC)

#grafico de la tasa de crecimiento
MEX.TDC.plot<-ggplot(data=MEX.TDC,
                     aes(x=year, y=Rate_percent))+
  geom_line(cex = 0.8, col="#4575b4")+
  geom_hline(yintercept = 0, size=0.8, col="black")+
  theme(plot.caption=element_text(hjust=0,size=9),
        plot.title=element_text(hjust=0.5, size=14,face="bold"),
        plot.subtitle =element_text(hjust=0.5, size=12,face="bold"),
        legend.position="none",
        legend.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.x=element_text(angle =0,vjust =1,
                                 hjust=1, colour = "black"),
        axis.text.y = element_text(hjust = 0,
                                   colour = "black",
                                   size = 8)
        
  )+
  labs(title="Variación anual del PIB real per cápita de México",
       caption="Fuente: Elaboración propia con información de Penn World Table 10.0")+
  xlab(" ")+
  ylab("Puntos porcentuales")
MEX.TDC.plot


#TASA DE CRECIMIENTO PROMEDIO (TDCP)
V_i<-MEX.DF%>%
  filter(year==1950)%>%
  select(pc.GDP)

V_f<-MEX.DF%>%
  filter(year==2019)%>%
  select(pc.GDP)

n<-length(MEX.DF$year)

#En promedio, cada año el ingreso per cápita de México creció un 1.96% en 70 años.

TDCP=((V_f/V_i)^(1/n)-1)*100
TDCP

#Podemos agregar esto al gráfico anterior.
texto.PIBpc <- 'La tasa de crecimiento medio del PIB per cápita en <br><br><span style="color:#4575b4;">**México**</span> para el periodo 1950-2019 es de <span style="color:#d73027;">**1.96%**</span>'

MEX.TDC.plot+
  geom_hline(yintercept = 1.96, size=0.8, col="#d73027",linetype = 'dashed')+
  annotate(geom = "richtext",
           x = 1970, y = 11,
           label = texto.PIBpc,
           label.colour = NA, fill = NA,
           fontface = "bold",
           color = "black",
           size = 2.5)






#-----------------------------------------
#          FUNCIÓN GATHER 
#-----------------------------------------

#Crear un data frame
data<- data.frame(player=c('A', 'B', 'C', 'D'),
                  year1=c(12, 15, 19, 19),
                  year2=c(22, 29, 18, 12))

data


library(tidyr)

gather(data, key="year", value="points", 2:3)



#-----------------------------------------
#          FUNCIÓN SPREAD 
#-----------------------------------------
data2 <- data.frame(player=rep(c('A', 'B'), each=4),
                    year=rep(c(1, 1, 2, 2), times=2),
                    stat=rep(c('points', 'assists'), times=4),
                    amount=c(14, 6, 18, 7, 22, 9, 38, 4))

data2
library(tidyr)

spread(data2, key=stat, value=amount)

#-----------------------------------------
#          FUNCIÓN SEPARATE 
#-----------------------------------------

data3 <- data.frame(player=c('A', 'A', 'B', 'B', 'C', 'C'),
                    year=c(1, 2, 1, 2, 1, 2),
                    stats=c('22-2', '29-3', '18-6', '11-8', '12-5', '19-2'))

library(tidyr)

separate(data3, col=stats, into=c('points', 'assists'), sep='-')

#-----------------------------------------
#          FUNCIÓN UNITE 
#-----------------------------------------
data4 <- data.frame(player=c('A', 'A', 'B', 'B', 'C', 'C'),
                    year=c(1, 2, 1, 2, 1, 2),
                    points=c(22, 29, 18, 11, 12, 19),
                    assists=c(2, 3, 6, 8, 5, 2))

library(tidyr)
unite(data4, col='points-assists', c('points', 'assists'), sep='-')

#-----------------------------------------
#          GRÁFICO DE LOLLIPOP (EJEMPLO 1)
#-----------------------------------------

name =letters[1:10]
score=c(20,35,37,39,35,22,28,29,32,38)

df=data.frame(name,score)
library(ggplot2)

# 1) Graficamos los puntos

ggplot(data=df,aes(x=name,y=score))+
  geom_point()
#2) Coloreamos cada punto con un color diferente
ggplot(data=df,aes(x=name,y=score,color=name))+
  geom_point()
#3) Agrandamos los puntos a un tamaño más grande
ggplot(data=df,aes(x=name,y=score,color=name))+
  geom_point(size=6)
#4) Agregamos los segmentos que tengan la altura de la variable numérica 

ggplot(data=df,aes(x=name,y=score,color=name))+
  geom_point(size=6)+
  geom_segment(aes(x=name,xend=name,y=0,yend=score))

#5) Aumentamos el grosor de los segmentos
ggplot(data=df,aes(x=name,y=score,color=name))+
  geom_point(size=6)+
  geom_segment(aes(x=name,xend=name,y=0,yend=score),size=2)

#6) Adicionamos el título y el nombre de los ejes
ggplot(data=df,aes(x=name,y=score,color=name))+
  geom_point(size=6)+
  geom_segment(aes(x=name,xend=name,y=0,yend=score),size=2)+
  theme_bw()+
  labs(title="Gráfico de lollipop",X="Nombre del estudiante",y="Score") 


#-----------------------------------------
#          GRÁFICO DE LOLLIPOP (EJEMPLO 2)
#-----------------------------------------

#Comparación de dos variables usando lollipop plot

city=c("Delhi","Mumbai","Kolkata","Naggur","Kapurthala")
max=c(35,32,37,41,37) #Temperatura máxima de la ciudad
min=c(26,25,26,29,26) #Temperatura de la ciudad

lp2=data.frame(city,max,min)
lp2

#install.packages("tidyr")
#library(tidyr)
lp2=gather(lp2, key="category", value="Temp", 2:3)
lp2

#graficar los puntos del lillipop

ggplot(lp2,aes(x=city,y=Temp,color=category))+
  geom_point(size=10)

#Adicionando los segmentos
ggplot(lp2,aes(x=city,y=Temp,color=category))+
  geom_point(size=10)+
  geom_segment(aes(x=city,xend=city,y=30,yend=Temp))+
  theme_bw()

#Invirtiendo el gráfico 
ggplot(lp2,aes(x=city,y=Temp,color=category))+
  geom_point(size=10)+
  geom_segment(aes(x=city,xend=city,y=30,yend=Temp))+
  theme_bw()+
  coord_flip() 


#Adicionando una línea de intercepto
ggplot(lp2,aes(x=city,y=Temp,color=category))+
  geom_point(size=10)+
  geom_segment(aes(x=city,xend=city,y=30,yend=Temp))+
  theme_bw()+
  coord_flip()+
  geom_hline(aes(yintercept=30))


#-----------------------------------------
#          GRÁFICO DE LOLLIPOP (EJEMPLO 3)
#-----------------------------------------
#---BASE DE MAMÍFEROS

library(tidyverse)
library(forcats)

theme_set(theme_bw()+
            theme(panel.grid=element_blank()))

#data()
view(msleep)
#?geom_segment
summary(msleep)
glimpse(msleep)

names(msleep)

msleep %>%
  group_by(order) %>%
  summarise(mean_sleep=mean(sleep_total))  %>%
  mutate(order=fct_reorder(order,mean_sleep)) %>%
  ggplot(aes(x=order,y=mean_sleep))+
  geom_point(size=6,
             colour="orange")+
  geom_segment(aes(x=order,
                   y=mean(msleep$sleep_total),
                   xend=order,
                   yend=mean_sleep),
               colour="grey")+
  geom_hline(yintercept=mean(msleep$sleep_total),
             colour="grey",
             size=1)+
  theme(axis.text.x=element_text(angle=90))+
  labs(title="Sueño de tiempo promedio de mamíferos por orden",
x="",
y="Horas")


#-----------------------------------------
#          GRÁFICO DE SERIES FINANCIERAS
#-----------------------------------------
## Paquetería

install.packages("quantmod")
install.packages("TTR")


library(TTR)
library(quantmod)


## Cargando data de activos financieros desde Yahoo
#Los archivos XTS SON DATOS DE TIPO SERIES DE TIEMPO

#BITCOIN VS DOLAR
crypto = "BTC-USD"
getSymbols(crypto, src="yahoo", from="2021-01-02")

cryptos = c("BTC-USD","ETH-USD","ADA-USD")
getSymbols(cryptos, src="yahoo", from="2021-01-02")

#ACCIONES
stock = c("AAPL")
getSymbols(stock, src="yahoo", from="2021-01-02")

stock_list=c("AAPL","GOOG")
getSymbols(stock_list, src="yahoo", from="2021-01-02")

head(`BTC-USD`,n=3)

class(`BTC-USD`)

## Gráfico de líneas de las acciones de FB: Muestra el precio y volumen transado
#chartSeries(FB, type="line",subset='2022',theme=chartTheme('white'))

## Gráfico de líneas de las acciones de apple: Muestra el precio y volumen transado
chartSeries(`AAPL`, type="line",subset='2022',theme=chartTheme('white'))

## Gráfico de barras: Muestra precio de cierre, de apertura, máximo, mínimo y volumen.

chartSeries(`AAPL`,
            type="bar",
            subset='2022',
            theme=chartTheme('white'))

## Gráfico de velas: igual que el gráfico anterior

chartSeries(`AAPL`,
            type="candlesticks",
            subset='2022',
            up.col = 'green',
            down.col = 'black',
            theme=chartTheme('white'))

chartSeries(`AAPL`,
            subset='2021-05::2022-02',
            theme=chartTheme('white')
)

chartSeries(`AAPL`,
            subset="last 10 months",
            theme=chartTheme('white')
)

## Medias móviles

addSMA(n=20,on=1,col = "green")

addSMA(n=50,on=1,col = "blue")

addSMA(n=200,on=1,col = "red")

addEMA(n=30,on=1,col = "orange")

## Bandas de Bollinger

chartSeries(`AAPL`,
            subset='2021-05::2022-02',
            theme=chartTheme('white'))

addBBands(n=20,sd=2)

## Momentum

chartSeries(`AAPL`,
            subset='2021-05::2022-02',
            theme=chartTheme('white'))

addMomentum(n=10) #toma el precio base de hace 10 periodos


###########################


#-----------------------------------------------------
#          GRÁFICOS DEMOGRÁFICOS (PIRÁMIDE POBLACIONAL)
#-----------------------------------------------------
#Los datos corresponden a la población colombiana
#registrados en el censo de población y vivienda de 2018


Edad<-c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29","30 a 34",
        "35 a 39","40 a 44","45 a 49","50 a 54","55 a 59","60 a 64","65 a 69",
        "70 a 74","75 a 79","80 a 84","85 y más")

Hombres<-c(1555605,1705574,1848218,1970530,1983553,1835158,1649783,1560417,
           1308328,1245829,1213908,1057242,840134,639772,457772,325224,205428,168018)

Mujeres<-c(1482176,1629666,1762366,1881725,1956735,1857016,1700746,1656227,
           1436336,1400272,1382470,1223557,984516,750320,546647,405409,281348,256392)
datos<-data.frame(Edad,Hombres,Mujeres)
      
library(pyramid)
H1<-round(Hombres/1000,0)

M1<-round(Mujeres/1000,0)

datos<-data.frame(H1,M1,Edad)

#Gráfico de la pirámide poblacional
pyramid(datos,
    Llab="Hombres",
    Rlab="Mujeres",
    Clab="Edad",
    main="Población de Colombia 2018 \n (en miles)",
    Lcol="green", Rcol="cyan", Cgap=0.5)

     

pacman::p_load(rio,       # para importar datos
               here,      # para localizar archivos
               tidyverse, # para limpiar, manejar y graficar los datos (incluye el paquete ggplot2)
               apyramid,  # un paquete dedicado a crear pirámides de edad
               janitor,   # tablas y limpieza de datos
               stringr)   # trabajar con cadenas para títulos, subtítulos, etc.

           
#Importar datos
#Para empezar, importamos la lista de casos limpia de una 
#epidemia de ébola simulada. 

# importar linelist de casos 
getwd()
linelist <- readRDS("linelist_cleaned.rds")
names(linelist)
View(linelist)
str(linelist)

#Limpieza
library (janitor)

linelist %>% 
  tabyl(age_cat5, gender)

#También realizamos un histograma rápido de la columna 
#age para asegurarnos de que está limpia y correctamente
#clasificada:

hist(linelist$age)

#Grafico de la pirámide poblacional
library(apyramid)
apyramid::age_pyramid(data = linelist,
                      age_group = "age_cat5",
                      split_by = "gender")
#porcentajes
apyramid::age_pyramid(data = linelist,
                      age_group = "age_cat5",
                      split_by = "gender",
                      proportional = TRUE)
#split by
apyramid::age_pyramid(data = linelist,
                      age_group = "age_cat5",
                      split_by = "hospital")  

#valores faltantes
apyramid::age_pyramid(data = linelist,
                      age_group = "age_cat5",
                      split_by = "gender",
                      na.rm = FALSE)  # mostrar pacientes sin edad o sexo

#Proporciones, colores y estética

apyramid::age_pyramid(
  data = linelist,
  age_group = "age_cat5",
  split_by = "gender",
  proportional = TRUE,              # muestra porcentajes, no conteos
  show_midpoint = FALSE,            # elimina la línea del punto medio de la barra
  #pal = c("orange", "purple")     # puede especificar colores alternativos aquí (pero no etiquetas)
)+                 
  
  # comandos adicionales de ggplot
  theme_minimal()+                               # simplifica el fondo
  scale_fill_manual(                             # especificar colores Y etiquetas
    values = c("orange", "purple"),              
    labels = c("m" = "Male", "f" = "Female"))+
  labs(y = "Percent of all cases",              # observa que los labs x e y se intercambian
       x = "Age categories",                          
       fill = "Gender", 
       caption = "My data source and caption here",
       title = "Title of my plot",
       subtitle = "Subtitle with \n a second line...")+
  theme(
    legend.position = "bottom",                          # leyenda en la parte inferior
    axis.text = element_text(size = 10, face = "bold"),  # fuentes/tamaños
    axis.title = element_text(size = 12, face = "bold"))
