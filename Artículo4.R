#EJECUTAR ESTE SCRIP DE R EN COMBINACION CON EL SCRIPT DE PYTHON 'ARTÍCULO'

library(sf)
library(igraph)
library(htmltools)
library(leaflet)
library(dplyr)
library(readr)
library(mapview)
library(ggplot2)
library(netmap) # spatial tools to interact with ggplot2
library(progress)# para el progreso de las operaciones
library(plotrix)#para gradiente de color
library(ggspatial)
library(rgeos)

df <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\datacalderon_mapa.csv')
df2 <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\datanieto_mapa.csv')
df3 <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\dataobrador_mapa.csv')

mapa_mexico <- st_read('C:\\Users\\PC\\Desktop\\Narco\\entropía\\extras\\estado\\mexico_estados.shp')
path = 'C:\\Users\\PC\\Desktop\\Narco\\entropía\\articulo'

# Definir una lista de colores
colores <- c("#813E3E", "gray40", "#42D78C", "yellow", "purple", "orange", 'cyan', '#FA9CFA', 'black')

# Crear un vector para almacenar el color asignado a cada pareja única
parejas_unicas <- unique(df$Parejas)
parejas_colores <- setNames(colores[seq_along(parejas_unicas)], parejas_unicas)

# Agregar una nueva columna con los colores correspondientes
df <- df %>%
  mutate(color = parejas_colores[Parejas])

# Convertir la base de datos de eventos a un objeto sf
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

# Crear el mapa con ggplot2
ggplot() +
  geom_sf(data = mapa_mexico, fill = "white", color = "black") + 
  geom_sf(data = df_sf, aes(size = evento, color = Parejas), alpha = 0.5) +
  scale_color_manual(values = df$color) +
  scale_size_continuous(range = c(2, 10)) +
  labs(title = "Mapa de eventos en México - Sexenio de Calderón (2006-2012)",
       subtitle = "Tamaño de puntos proporcional a la cantidad de eventos",
       color = "Parejas",
       size = "Cantidad de eventos") +
  theme(
         panel.background = element_rect(fill = "lightblue",
                                         colour = "lightblue",
                                         size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                         colour = "white"), 
         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                         colour = "white"), legend.text = element_text(size = 6)) +
  theme(legend.position = "right") +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering)
  ggsave(filename='mapa_calderon.png', path = path, width = 8, height = 5, device='tiff', dpi=500)
###############################################################################################
  ##########################################################################################
  ##########################################################################################
  ##########################################################################################
  
  # Definir una lista de colores
  colores2 <- c("#00083D", "#FA9CFA", "#42D78C", "#775A3C", "red", "orange", 'purple', 'cyan', 'yellow', 'black')
  
  # Crear un vector para almacenar el color asignado a cada pareja única
  parejas_unicas2 <- unique(df2$Parejas)
  parejas_colores2 <- setNames(colores2[seq_along(parejas_unicas2)], parejas_unicas2)
  
  # Agregar una nueva columna con los colores correspondientes
  df2 <- df2 %>%
    mutate(color = parejas_colores2[Parejas])
  
  # Convertir la base de datos de eventos a un objeto sf
  df2_sf <- st_as_sf(df2, coords = c("longitude", "latitude"), crs = 4326)
  
  # Crear el mapa con ggplot2
  ggplot() +
    geom_sf(data = mapa_mexico, fill = "white", color = "black") + 
    geom_sf(data = df2_sf, aes(size = evento, color = Parejas), alpha = 0.5) +
    scale_color_manual(values = df2$color) +
    scale_size_continuous(range = c(2, 10)) +
    labs(title = "Mapa de eventos en México - Sexenio de Peña Nieto (2012-2018)",
         subtitle = "Tamaño de puntos proporcional a la cantidad de eventos",
         color = "Parejas",
         size = "Cantidad de eventos") +
    theme(
      panel.background = element_rect(fill = "lightblue",
                                      colour = "lightblue",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"), legend.text = element_text(size = 6)) +
    theme(legend.position = "right") +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering)
  ggsave(filename='mapa_peña.png', path = path, width = 8, height = 5, device='tiff', dpi=500)
  
  ###############################################################################################
  ##########################################################################################
  ##########################################################################################
  ##########################################################################################
  
  # Definir una lista de colores
  colores3 <- c("#00083D", "red", "green", "#775A3C", "orange", "#44A0AB", 'yellow', '#06471A', 'purple')
  
  # Crear un vector para almacenar el color asignado a cada pareja única
  parejas_unicas3 <- unique(df3$Parejas)
  parejas_colores3 <- setNames(colores3[seq_along(parejas_unicas3)], parejas_unicas3)
  
  # Agregar una nueva columna con los colores correspondientes
  df3 <- df3 %>%
    mutate(color = parejas_colores3[Parejas])
  
  # Convertir la base de datos de eventos a un objeto sf
  df3_sf <- st_as_sf(df3, coords = c("longitude", "latitude"), crs = 4326)
  
  # Crear el mapa con ggplot2
  ggplot() +
    geom_sf(data = mapa_mexico, fill = "white", color = "black") + 
    geom_sf(data = df3_sf, aes(size = evento, color = Parejas), alpha = 0.5) +
    scale_color_manual(values = df3$color) +
    scale_size_continuous(range = c(2, 10)) +
    labs(title = "Mapa de eventos en México - Periodo de López Obrador (2018-2022)",
         subtitle = "Tamaño de puntos proporcional a la cantidad de eventos",
         color = "Parejas",
         size = "Cantidad de eventos") +
    theme(
      panel.background = element_rect(fill = "lightblue",
                                      colour = "lightblue",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"), legend.text = element_text(size = 6)) +
    theme(legend.position = "right") +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering)
  ggsave(filename='mapa_obrador.png', path = path, width = 8, height = 5, device='tiff', dpi=500)
  
  
  ##########################################################################################
  ##########################################################################################
  ##########################################################################################
  ##########################################################################################
  #INGLESSSS
  ##########################################################################################
  ##########################################################################################
  ##########################################################################################
  ##########################################################################################
  
  
  df <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\datacalderon_mapa.csv')
  df2 <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\datanieto_mapa.csv')
  df3 <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\dataobrador_mapa.csv')
  
  mapa_mexico <- st_read('C:\\Users\\PC\\Desktop\\Narco\\entropía\\extras\\estado\\mexico_estados.shp')
  path = 'C:\\Users\\PC\\Desktop\\Narco\\entropía\\articulo\\Ingles'
  
  # Definir una lista de colores
  colores <- c("#813E3E", "gray40", "#42D78C", "yellow", "purple", "orange", 'cyan', '#FA9CFA', 'black')
  
  # Crear un vector para almacenar el color asignado a cada pareja única
  parejas_unicas <- unique(df$Parejas)
  parejas_colores <- setNames(colores[seq_along(parejas_unicas)], parejas_unicas)
  
  # Agregar una nueva columna con los colores correspondientes
  df <- df %>%
    mutate(color = parejas_colores[Parejas])
  
  # Convertir la base de datos de eventos a un objeto sf
  df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  
  # Crear el mapa con ggplot2
  ggplot() +
    geom_sf(data = mapa_mexico, fill = "white", color = "black") + 
    geom_sf(data = df_sf, aes(size = evento, color = Parejas), alpha = 0.5) +
    scale_color_manual(values = df$color) +
    scale_size_continuous(range = c(2, 10)) +
    labs(title = "Map of events in Mexico: Calderón's six-year term (2006-2012)",
         subtitle = "Point size proportional to the number of events",
         color = "Cartel couple",
         size = "Number of events") +
    theme(
      panel.background = element_rect(fill = "lightblue",
                                      colour = "lightblue",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"), legend.text = element_text(size = 6)) +
    theme(legend.position = "right") +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering)
  ggsave(filename='mapa_calderon.png', path = path, width = 8, height = 5, device='tiff', dpi=300)
  
  
  ##########################################################################################
  ##########################################################################################
  ##########################################################################################
  
  # Definir una lista de colores
  colores2 <- c("#00083D", "#FA9CFA", "#42D78C", "#775A3C", "red", "orange", 'purple', 'cyan', 'yellow', 'black')
  
  # Crear un vector para almacenar el color asignado a cada pareja única
  parejas_unicas2 <- unique(df2$Parejas)
  parejas_colores2 <- setNames(colores2[seq_along(parejas_unicas2)], parejas_unicas2)
  
  # Agregar una nueva columna con los colores correspondientes
  df2 <- df2 %>%
    mutate(color = parejas_colores2[Parejas])
  
  # Convertir la base de datos de eventos a un objeto sf
  df2_sf <- st_as_sf(df2, coords = c("longitude", "latitude"), crs = 4326)
  
  # Crear el mapa con ggplot2
  ggplot() +
    geom_sf(data = mapa_mexico, fill = "white", color = "black") + 
    geom_sf(data = df2_sf, aes(size = evento, color = Parejas), alpha = 0.5) +
    scale_color_manual(values = df2$color) +
    scale_size_continuous(range = c(2, 10)) +
    labs(title = "Map of events in Mexico: Peña's six-year term (2006-2012)",
         subtitle = "Point size proportional to the number of events",
         color = "Cartel couple",
         size = "Number of events") +
    theme(
      panel.background = element_rect(fill = "lightblue",
                                      colour = "lightblue",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"), legend.text = element_text(size = 6)) +
    theme(legend.position = "right") +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering)
  ggsave(filename='mapa_peña.png', path = path, width = 8, height = 5, device='tiff', dpi=300)
  
  
  ##########################################################################################
  ##########################################################################################
  ##########################################################################################
  
  # Definir una lista de colores
  colores3 <- c("#00083D", "red", "green", "#775A3C", "orange", "#44A0AB", 'yellow', '#06471A', 'purple')
  
  # Crear un vector para almacenar el color asignado a cada pareja única
  parejas_unicas3 <- unique(df3$Parejas)
  parejas_colores3 <- setNames(colores3[seq_along(parejas_unicas3)], parejas_unicas3)
  
  # Agregar una nueva columna con los colores correspondientes
  df3 <- df3 %>%
    mutate(color = parejas_colores3[Parejas])
  
  # Convertir la base de datos de eventos a un objeto sf
  df3_sf <- st_as_sf(df3, coords = c("longitude", "latitude"), crs = 4326)
  
  # Crear el mapa con ggplot2
  ggplot() +
    geom_sf(data = mapa_mexico, fill = "white", color = "black") + 
    geom_sf(data = df3_sf, aes(size = evento, color = Parejas), alpha = 0.5) +
    scale_color_manual(values = df3$color) +
    scale_size_continuous(range = c(2, 10)) +
    labs(title = "Map of events in Mexico: Obrador period (2018-2022)",
         subtitle = "Point size proportional to the number of events",
         color = "Cartel couple",
         size = "Number of events") +
    theme(
      panel.background = element_rect(fill = "lightblue",
                                      colour = "lightblue",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"), legend.text = element_text(size = 6)) +
    theme(legend.position = "right") +
    annotation_scale(location = "bl") +
    annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_fancy_orienteering)
  ggsave(filename='mapa_obrador.png', path = path, width = 8, height = 5, device='tiff', dpi=300)
  
  
  
  
  
  
