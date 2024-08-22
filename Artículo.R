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
###########################################################################################
###########################################################################################
###########################################################################################
##########ESTE PRIMER SCRIPT ES PARA EL MAPA POR ESTADOS
#Leemos la base de datos y el mapa por estados de México
df <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\dataframe_mex_ev.csv')
mapa_mexico <- st_read('C:\\Users\\PC\\Desktop\\Narco\\entropía\\extras\\estado\\mexico_estados.shp')
path = 'C:\\Users\\PC\\Desktop\\Narco\\entropía\\articulo'
mapa_eventos <- merge(mapa_mexico, df, by.x = "nombre_igg", by.y = "Estado", all.x = TRUE)
m <- ggplot() +
  geom_sf(data = mapa_eventos, aes(fill = Eventos)) +
  scale_fill_gradient(low = "green", high = "red") +
  #scale_fill_gradientn(colors = c("green", "blue", "red"), na.value = "white")+
  #ESTE ES EL NUEVO TEMA QUE PONE AZUL CIELO EL MAR  
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) +
  labs(title = "Eventos totales en México por estado (2004-2022)") 
m
ggsave(filename='Eventos_totales_estado.png', path = path, width = 8, height = 5, device='tiff', dpi=300)
###########################################################################################
###########################################################################################
###########################################################################################
##########ESTE PRIMER SCRIPT ES PARA EL MAPA DE EVENTOS POR MUNICIPIO
df2 <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\mapa_municipios_tot.csv')
mapa_mexico <- st_read('C:\\Users\\PC\\Desktop\\Narco\\entropía\\extras\\estado\\mexico_estados.shp')
path = 'C:\\Users\\PC\\Desktop\\Narco\\entropía\\articulo'

# Convertir el dataframe a un objeto sf
df2_sf <- st_as_sf(df2, coords = c("longitude", "latitude"), crs = st_crs(mapa_mexico))
# Graficar el mapa base de México
ggplot() +
  geom_sf(data = mapa_mexico) +
  coord_sf() +
# Añadir los puntos con tamaño proporcional a la columna 'evento'
geom_sf(data = df2_sf, aes(size = evento), color = "red", alpha = 0.6) +
  scale_size_continuous(range = c(1, 10)) + # Ajustar el rango del tamaño del punto según sea necesario
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) +
  labs(title = "Eventos totales en México (2004-2022)",
       size = "Eventos")

ggsave(filename='Eventos_totales_municipio.png', path = path, width = 8, height = 5, device='tiff', dpi=300)
###########################################################################################
###########################################################################################
###########################################################################################
########## ESTE PRIMER SCRIPT ES PARA CLUSTERING
#df3 <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\filtro en turno.csv')
#df4 <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\filtro en turno2.csv')
#df5 <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\filtro en turno3.csv')
#df6 <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\filtro en turno4.csv')
#lista_colores <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\lista colores.csv')

#mapa_mexico <- st_read('C:\\Users\\PC\\Desktop\\Narco\\entropía\\extras\\estado\\mexico_estados.shp')
#path = 'C:\\Users\\PC\\Desktop\\Narco\\entropía\\articulo\\cluster'                                                                   
#nombre_variable <- names(sort(table(df3$cart_a), decreasing = TRUE))[1]
#año_variable <- df3$date[1]
# Convertir la fecha a tipo de dato fecha
#año_variable <- as.Date(año_variable, format = "%Y-%m-%d")
# Extraer el año y almacenarlo en una variable única
#año_variable <- format(año_variable, "%Y")
#df3$zona <- "CJNG"
#df4$zona <- "C. Sinaloa"
#df5$zona <- "C. Juarez"
#df6$zona <- "C. Golfo"

#gg <- ggplot() +
#  geom_sf(data = mapa_mexico) +
#  coord_sf() +
#  theme( #Esto es para el fondo azul bonito
#    panel.background = element_rect(fill = "lightblue",
#                                    colour = "lightblue",
#                                    size = 0.5, linetype = "solid"),
#    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                    colour = "white"), 
#    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                    colour = "white")) +
#      labs(title = paste("Zonas de control de ", nombre_variable, "en", año_variable)) 
#gg <- gg +
#  geom_point(data = df3, aes(x = jitter(longitude, factor = 0), y = jitter(latitude, factor = 150), size = eventos, fill = color), shape = 21, alpha=0.8) +
#  geom_point(data = df4, aes(x = longitude, y = latitude, size = eventos, fill = color), shape = 21, alpha=0.5) +
#  geom_point(data = df5, aes(x = jitter(longitude, factor = 50), y = jitter(latitude, factor = 50), size = eventos, fill = color), shape = 21, alpha=0.3) +
#  geom_point(data = df6, aes(x = jitter(longitude, factor = 0), y = jitter(latitude, factor = 50), size = eventos, fill = color), shape = 21, alpha=0.3) +
#  scale_size_continuous(range = c(1, 10), limits = c(0, 82)) +  # Ajustar el rango del tamaño de los discos según tus datos

#  scale_color_manual(values = unique(lista_colores$color), 
#                     labels = unique(lista_colores$cartel)) +
  
#  scale_size_continuous(name = "Eventos")
# Construir el nombre del archivo con la variable especial
#nombre_archivo <- paste("Zonas_control", año_variable, "VIEJO.png", sep = "")
# Guardar la imagen utilizando ggsave
#gg
#ggsave(filename=nombre_archivo, path = path, width = 8, height = 5, device='tiff', dpi=300)

###########################################################################################
###########################################################################################
###########################################################################################
#INGLES
###########################################################################################
###########################################################################################
###########################################################################################
##########ESTE PRIMER SCRIPT ES PARA EL MAPA POR ESTADOS
#Leemos la base de datos y el mapa por estados de México
df_aux <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\dataframe_mex_ev.csv')
names(df_aux)[names(df_aux) == 'Estado'] <- 'State'
names(df_aux)[names(df_aux) == 'Eventos'] <- 'Events'

mapa_mexico <- st_read('C:\\Users\\PC\\Desktop\\Narco\\entropía\\extras\\estado\\mexico_estados.shp')
path = 'C:\\Users\\PC\\Desktop\\Narco\\entropía\\articulo\\Ingles'
mapa_eventos <- merge(mapa_mexico, df_aux, by.x = "nombre_igg", by.y = "State", all.x = TRUE)
m <- ggplot() +
  geom_sf(data = mapa_eventos, aes(fill = Events)) +
  scale_fill_gradient(low = "green", high = "red") +
  #scale_fill_gradientn(colors = c("green", "blue", "red"), na.value = "white")+
  #ESTE ES EL NUEVO TEMA QUE PONE AZUL CIELO EL MAR  
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) +
  labs(title = "Total events in Mexico by state (2004-2022)") 
m
ggsave(filename='Eventos_totales_estado.png', path = path, width = 8, height = 5, device='tiff', dpi=300)
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
##########ESTE PRIMER SCRIPT ES PARA EL MAPA DE EVENTOS POR MUNICIPIO
df2_aux <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\mapa_municipios_tot.csv')
names(df2_aux)[names(df2_aux) == 'evento'] <- 'Event'

mapa_mexico <- st_read('C:\\Users\\PC\\Desktop\\Narco\\entropía\\extras\\estado\\mexico_estados.shp')
path = 'C:\\Users\\PC\\Desktop\\Narco\\entropía\\articulo\\Ingles'

# Convertir el dataframe a un objeto sf
df2_aux_sf <- st_as_sf(df2_aux, coords = c("longitude", "latitude"), crs = st_crs(mapa_mexico))
# Graficar el mapa base de México
ggplot() +
  geom_sf(data = mapa_mexico) +
  coord_sf() +
  # Añadir los puntos con tamaño proporcional a la columna 'evento'
  geom_sf(data = df2_aux_sf, aes(size = Event), color = "red", alpha = 0.6) +
  scale_size_continuous(range = c(1, 10)) + # Ajustar el rango del tamaño del punto según sea necesario
  theme(
    panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) +
  labs(title = "Total events in Mexico (2004-2022)",
       size = "Events")

ggsave(filename='Eventos_totales_municipio.png', path = path, width = 8, height = 5, device='tiff', dpi=300)
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
# EL ULTIMO SCRIPT NO SE UTILIZA, ENTONCES NO HAGO VERSIÓN EN ESPAÑOL