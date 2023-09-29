---
title: "MEU_2023_AIU2_TP2"
author: "Agustina Carbonari"
date: "2023-09-22"
output: html_document
---
  
## Instrumentos de Análisis Urbanos II - TP2: 
  
#En este proyecto, utilizaremos la misma base de datos que en el Trabajo Práctico 1.
#A diferencia de ese trabajo, en lugar de centrarnos en un solo barrio, analizaremos todas las Áreas de Protección Histórica (APH) en la Ciudad Autónoma de Buenos Aires (CABA) para obtener conclusiones a nivel ciudad.

library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(viridis)

#Para llevar a cabo este proyecto, primero activamos las bibliotecas necesarias y adquirimos los datos públicos proporcionados por organismos gubernamentales de la Ciudad Autónoma de Buenos Aires. Estos datos se obtuvieron de la siguiente fuente: https://data.buenosaires.gob.ar/dataset/

data <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/secretaria-de-desarrollo-urbano/areas-proteccion-historica/areas-de-proteccion-historica.csv")
summary(data)
dim (data)

#Empleamos summary con el propósito de obtener una comprensión detallada acerca de la estructura y organización de la base de datos. La base de datos presenta las Áreas de Protección Histórica (APH) de la Ciudad Autónoma de Buenos Aires, organizándolas según su dirección, barrio y comuna correspondiente. Además, proporciona información sobre el número de APH, el tipo de denominación, la catalogación, el nivel de protección, el estado actual y la normativa legal a la cual está sujeta cada área.
#A su vez, utilizando la funcion dim comprobamos que la base cuenta con 149564 filas y 22 columnas.   

#Ahora que conocemos la información contenida en el dataset, procederemos a limpiar y transformar los datos,




## Análisis de los Diferentes Tipos de Áreas de Protección Histórica (APH) por Barrio en la Ciudad Autónoma de Buenos Aires

data_APH <- data %>% 
select(BARRIOS,PROTECCION)

colnames(data_APH)

#Comenzamos por limpiar la base de datos, descartando las columnas que no tenian relevancia para el analisis y revisamos los nombres de las columnas seleccionadas

unique(data_APH$PROTECCION)

#Luego, visualizamos los distintos tipos de protecciones que presentan las APH en los 48 barrios de CABA. Cada una de estas categorías de protección tiene un enfoque y alcance específicos para la preservación del patrimonio histórico en la ciudad:¨

#DESESTIMADO: la APH no cuenta con medidas de protección específicas o que se han desestimado las medidas existentes.
#CAUTELAR: protege temporalmente un bien o área mientras se realizan evaluaciones más detalladas o se toman decisiones sobre su preservación.
#GENERAL: medidas generales que pueden aplicarse a un rango amplio de bienes o áreas, sin especificaciones detalladas.
#ESTRUCTURAL:  medidas que se centran en la estructura física de los bienes o áreas, como edificios históricos.
#INTEGRAL: abordan la preservación de múltiples aspectos de un bien o área histórica, incluyendo su valor arquitectónico, cultural y ambiental.
#ESPECIAL: diseñadas específicamente para bienes o áreas con características particulares que requieren atención especial.
#GENERAL LEY 4464: medidas establecidas por la Ley 4464 de la Ciudad de Buenos Aires, abordan diversos aspectos de la preservación del patrimonio histórico y cultural.

protecciones_por_barrio <- data_APH %>%
  group_by(BARRIOS, PROTECCION) %>%
  filter(BARRIOS != "") %>% 
  filter(PROTECCION != "") %>% 
  count() %>%
  rename(cantidad = n)
View(protecciones_por_barrio)

#Calculamos la cantidad de cada tipo de proteccion en los barrios de CABA. De esta manera, observamos que por ejemplo el barrio de Recoleta cuenta con 1.0425 APH CON PROTECCION de tipo DESESTIMADO, 618 CAUTELAR, 249 GENERAL, 65 ESTRUCTURAL, 3 INTEGRAL y ninguna ESPECIAL o GENERAL LEY 4464

ggplot(protecciones_por_barrio, aes(x = reorder(PROTECCION, -cantidad), y = cantidad)) +
  geom_bar(stat = "identity", fill = "#4c92b5") +
  labs(title = "Tipos de Protecciones APH en CABA",
       x = "Protección del APH", y = "Cantidad",
       caption = "FUENTE: BA Data") +
  theme_minimal() +
  coord_flip() +  # Girar las etiquetas del eje x
  theme(plot.title = element_text(hjust = 0.5))  # Centrar el título

#Por último, realizamos un gráfico de barras para poder visualizar fácilmente esta información.
#Se puede concluir que que el tipo de protección predominante dentro de las Áreas de Protección Histórica (APH) en los barrios de la Ciudad Autónoma de Buenos Aires (CABA) es el denominado "DESESTIMADO". Este hallazgo sugiere la importancia de considerar una revisión y el fortalecimiento de las medidas de protección en toda la ciudad.

(APH_BARRIOS <- data_APH %>% 
    group_by(PROTECCION, BARRIOS) %>%
    filter(BARRIOS != "") %>% 
    filter(PROTECCION != "") %>% 
    summarise(Cantidad = n ()))

APH_BARRIOS <- APH_BARRIOS %>%
  arrange(BARRIOS)

ggplot(APH_BARRIOS) +
  geom_bar(aes(x = PROTECCION, weight = Cantidad), width = 0.3, fill = "#FFB3C6") +
  labs(title = "PROTECCIONES POR BARRIO",
       x = "PROTECCION",
       y = "BARRIOS",
       caption = "FUENTE: BA Data") +
  facet_wrap(~BARRIOS, scales = "free_x") +
  theme(
    plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"),
    panel.background = element_rect(fill = "gray100", colour = "gray100", linewidth = 2, linetype = "solid"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", colour = "gray80"),
    panel.grid.minor = element_line(linewidth = 0.25, linetype = "dashed", colour = "gray90"),
    title = element_text(size = 12, face = "bold"),
    plot.caption = element_text(face = "italic", colour = "gray35", size = 5),
    axis.text.x = element_text(size = 4),  # Ajusta el tamaño de las etiquetas del eje X
    axis.text.y = element_text(size = 7),  # Ajusta el tamaño de las etiquetas del eje Y
    axis.title.x = element_text(size = 10),  # Ajusta el tamaño del título del eje X
    axis.title.y = element_text(size = 10)  # Ajusta el tamaño del título del eje Y
  )

#Para enriquecer el gráfico anterior, incorporamos la variable 'BARRIOS' para observar la distribución espacial de las Áreas de Protección Histórica (APH) en la ciudad. 
#Esta representación geográfica nos permite identificar la concentración y los tipos de Áreas de Protección Histórica (APH) en cada barrio. 
#Para ello, transofmamos la base de datos, agrupando la información según tipo de proteccion y los barrios a los que pertenecen. A su vez, calculamos la cantidad para cada caso.




## Análisis de la Cantidad Total de Áreas de Protección Histórica (APH) por Barrio en la Ciudad Autónoma de Buenos Aires.

protecciones_totales_por_barrio <- APH_BARRIOS %>%
  group_by(BARRIOS) %>%
  summarise(ProteccionesTotales = sum(Cantidad))
protecciones_totales_por_barrio <- protecciones_totales_por_barrio %>%
  arrange(ProteccionesTotales)

ggplot(protecciones_totales_por_barrio, aes(x = reorder(BARRIOS, -ProteccionesTotales), y = ProteccionesTotales)) +
  geom_bar(stat = "identity", fill = "#4c92b5") +
  labs(title = "Protecciones APH por Barrio en CABA",
       x = "Barrio", y = "Cantidad de Protecciones",
       caption = "FUENTE: BA Data") +
  theme_minimal() +
  coord_flip() +  # Girar las etiquetas del eje x
  theme(plot.title = element_text(hjust = 0.5))  # Centrar el título

#Finalmente, transofmamos nuevamente la base de datos para averiguar las protecciones totales por barrio.
#De esta manera, podemos observar con mayor detalle aquellos barrios que exhiben una presencia destacada de estas áreas de protección histórica (como es el caso de Flores con 7853, de los cuales 7225 son de tipo DESESTIMADO, 411 CAUTELAR, 186 GENERAL y 31 ESTRUCTURAL), así como aquellos en los que su presencia es más limitada (Como es el caso de Puerto Madero con tan solo 27, de los cuales 20 son de tipo DESESTIMADO, 4 ESTRUCTURAL, 2 GENERAL y 1 CAUTELAR).




## Análisis de la distribución espacial de las Áreas de Protección Histórica (APH) en los distintos barrios de en la Ciudad Autónoma de Buenos Aires.

data_barrios <- st_read("databarrios/barrios_wgs84.shp")
summary(data_barrios)
dim (data_barrios)

#Comenzamos por importar la base de datos de los barrios de CABA desde BA Data. Luego, empleamos summary y la funcion dim para obtener una comprensión detallada de la estructura y organización de la base de datos nueva. Comprobamos que la base cuenta con 48 filas y 6 columnas.

proteccion_barrios <- data_APH %>%
  group_by(BARRIOS) %>%
  filter(BARRIOS != "0") %>%
  summarise(cantidad = n())

#Luego, trabajamos sobre la base de datos para unificar ambas bases (data_APH y data_barrios) a partir de una columna en común y obtener un único dataframe que permita su visualización.

proteccion_barrios <- proteccion_barrios %>%
  rename(BARRIO = BARRIOS)

#Determinamos la cantidad de protecciones por barrio. Asimismo, para contar con una columna en común de iguales propiedades,
#Renombramos la columna "BARRIOS" como "BARRIO" para que coincida con el otro dataframe.

(barrios_geometria <- data_barrios %>% 
    select(BARRIO, geometry))
(proteccion_barrios_mapa1 <- left_join(proteccion_barrios, barrios_geometria, by = "BARRIO") %>% 
    st_as_sf())
class(proteccion_barrios_mapa1) 

#Unimos ambas base de datos, las transformamos a formato sf y #verificamos la transformación

proteccion_barrios_mapa1 <- left_join(proteccion_barrios_mapa1, resultado_max[, c("BARRIO", "Tipo_Predominante")], by = "BARRIO")

# Unimos el dataframe proteccion_barrios_mapa1 con los resultados

breaks <- c(0, 1299, 4071, 6424, 7853, 6555)
labels <- c("0-1299", "1300-4071", "4072-6424", "6425-7853", "7854-6555")
proteccion_barrios_mapa1$cantidad_discreta <- cut(proteccion_barrios_mapa1$cantidad, breaks = breaks, labels = labels)

colores_personalizados <- c("#FFE5EC", "#FFC2D1", "#FFB3C6", "#FF8FAB", "#FB6F92")
ggplot(proteccion_barrios_mapa1) +
  geom_sf(aes(fill = cantidad_discreta)) +
  geom_sf_label(aes(label = BARRIO), size = 2) +
  scale_fill_manual(values = colores_personalizados) +
  labs(title = "PROTECCION POR BARRIO",
       fill = "Cantidad de Protecciones",
       caption = "FUENTE: BA Data") +
  guides(fill = guide_legend(title.position = "top", ncol = 2)) +
  theme(plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"),
        panel.background = element_rect(fill = "gray100", colour = "gray100", linewidth = 2, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", colour = "gray80"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = "dashed", colour = "gray90"),
        title = element_text(size = 12, face = "bold"),
        legend.key.size = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 5),
        plot.caption = element_text(face = "italic", colour = "gray35", size = 5))

#Finalmente, definimos los límites y etiquetas para las categorías. Luego agrupamos la variable 'cantidad' en categorías discretas y definimos una paleta de colores para nuestro grafico.




## Análisis de la Distribución Espacial de las Áreas de Protección Histórica (APH) según el Tipo de Protección e Identificación de los Barrios con la Mayor Cantidad de Casos en la Ciudad Autónoma de Buenos Aires

resultado <- data_APH %>%
  group_by(PROTECCION, BARRIOS) %>%
  summarise(cantidad = n()) %>%
  ungroup()  #Quitamos agrupación para calcular el máximo por cada tipo de protección

resultado_max <- resultado %>%
  group_by(PROTECCION) %>%
  filter(cantidad == max(cantidad)) %>%
  ungroup()
View(resultado_max)

#Identificamos para cada tipo de protección, cual era el barrio con la mayor cantidad de casos
#DESESTIMADO: FLORES
#CAUTELAR: BALVANERA
#GENERAL: SAN TELMO
#ESTRUCTURAL: MONSERRAT
#INTEGRAL: MONSERRAT
#ESPECIAL: PALERMO
#GENERAL LEY 4464: SAN TELMO

colnames(resultado_max) <- c("Tipo_Predominante", "BARRIO", "Cantidad")

#Renombrmosa las columnas para que coincidan con el dataframe proteccion_barrios_mapa1

ggplot(proteccion_barrios_mapa1) +
  geom_sf(aes(fill = Tipo_Predominante)) +
  geom_sf_label(aes(label = BARRIO), size = 1 +
  scale_fill_viridis_d() +  # Utiliza una paleta de colores discreta
  labs(title = "Protección Predominante por Barrio",
       fill = "Tipo de Protección Predominante",
       caption = "FUENTE: BA Data") +
  guides(fill = guide_legend(title.position = "top", ncol = 2)) +
  theme(plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"),
        panel.background = element_rect(fill = "gray100", colour = "gray100", linewidth = 2, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", colour = "gray80"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = "dashed", colour = "gray90"),
        title = element_text(size = 12, face = "bold"),
        legend.key.size = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 5),
        plot.caption = element_text(face = "italic", colour = "gray35", size = 5))




## En resumen, este proyecto destaca la importancia de considerar una revisión de las medidas de protección, especialmente en áreas "DESESTIMADAS," e identifica los barrios más relevantes en función de su tipo de protección predominante. 
## Lo cual podría contribuir de manera significativa a la planificación urbana y a los esfuerzos de preservación del patrimonio histórico de la Ciudad Autónoma de Buenos Aires








