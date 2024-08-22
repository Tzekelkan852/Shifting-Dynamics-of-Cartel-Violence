#EJECUTAR ESTE SCRIP DE R EN COMBINACION CON EL SCRIPT DE PYTHON 'CORRELACIONES_ARTÍCULO'

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

df <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\mapa_municipios_tot_definitivo.csv')
mapa_muni <- st_read('C:\\Users\\PC\\Desktop\\Narco\\entropía\\extras\\municipios\\muni_2018gw.shp')
path = 'C:\\Users\\PC\\Desktop\\Narco\\entropía\\articulo'

mapa_eventos <- merge(mapa_muni, df, by.x = "NOM_MUN", by.y = "Municipio", all.x = TRUE)
#mapa_eventos <- merge(mapa_muni, df, by.x = c("NOM_MUN", "NOM_ENT"), by.y = c("Municipio", "Estado"), all.x = TRUE)

m <- ggplot() +
  geom_sf(data = mapa_eventos, aes(fill = Eventos_log), size = 0.15) +
  #scale_fill_gradient(low = "green", high = "red") +
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
  labs(title = "Eventos totales en México por municipio (2004-2022) en escala logarítmica") 
m
ggsave(filename='Eventos_totales_municipio_new.png', path = path, width = 8, height = 5, device='tiff', dpi=500)



################################################################################################
################################################################################################
################################################################################################
################################################################################################
#inlgessssss
################################################################################################
################################################################################################
################################################################################################
################################################################################################

df_aux <- read.csv('C:\\Users\\PC\\Desktop\\Narco\\entropía\\rstudio\\mapa_municipios_tot_definitivo.csv')
names(df_aux)[names(df_aux) == 'Municipio'] <- 'Municipality'
names(df_aux)[names(df_aux) == 'Eventos_log'] <- 'Events_log'


mapa_muni <- st_read('C:\\Users\\PC\\Desktop\\Narco\\entropía\\extras\\municipios\\muni_2018gw.shp')
path = 'C:\\Users\\PC\\Desktop\\Narco\\entropía\\articulo\\Ingles'

mapa_eventos <- merge(mapa_muni, df_aux, by.x = "NOM_MUN", by.y = "Municipality", all.x = TRUE)
#mapa_eventos <- merge(mapa_muni, df, by.x = c("NOM_MUN", "NOM_ENT"), by.y = c("Municipio", "Estado"), all.x = TRUE)

m <- ggplot() +
  geom_sf(data = mapa_eventos, aes(fill = Events_log), size = 0.15) +
  #scale_fill_gradient(low = "green", high = "red") +
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
  labs(title = "Total events in Mexico by municipality (2004-2022) on logarithmic scale") 
m
ggsave(filename='Eventos_totales_municipio_new.png', path = path, width = 8, height = 5, device='tiff', dpi=300)

################################################################################################
################################################################################################
################################################################################################
################################################################################################
names(df_aux)[names(df_aux) == 'Muertes_log'] <- 'Deaths_log'

mapa_eventos <- merge(mapa_muni, df_aux, by.x = "NOM_MUN", by.y = "Municipality", all.x = TRUE)
#mapa_eventos <- merge(mapa_muni, df, by.x = c("NOM_MUN", "NOM_ENT"), by.y = c("Municipio", "Estado"), all.x = TRUE)

m <- ggplot() +
  geom_sf(data = mapa_eventos, aes(fill = Deaths_log), size = 0.15) +
  #scale_fill_gradient(low = "green", high = "red") +
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
  labs(title = "Total deaths in Mexico by municipality (2004-2022) on logarithmic scale") 
m
ggsave(filename='Muertes_totales_municipio_new.png', path = path, width = 8, height = 5, device='tiff', dpi=300)

