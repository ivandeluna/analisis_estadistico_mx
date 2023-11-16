# Relacionar Marco geoestadítico y Tabla AGEB Censo 2020

# Cargar librerías
library(tidyverse)
library(sf)
library(sp)
library(maptools)
library(compare)
library(raster)
library(dplyr)
library(mapview)
library(plotly)
library(leaflet)
library(rgdal)

# Polígonos de manzana
coah.m <- read_sf("05_coahuiladezaragoza/conjunto_de_datos/05m.shp") # Leer como SF pero no se guarda como SpatialPolygonsDataFrame
coah.m1 <- readOGR(dsn = "05_coahuiladezaragoza/conjunto_de_datos", layer = "05m") # Leer como OGR se guarda como SpatialPolygonsDataFrame

# Estadísticas por manzana de los Censos 2020
coa.ageb <- read_csv("ageb05_2020.csv")

# Checar las tablas
head(coa.ageb$MUN)
head(coa.ageb$CVEGEO)
head(coah.m$CVEGEO)
head(coah.m1)

# Checar variables y su data type
str(coa.ageb)
str(coah.m)
str(coah.m1)

# Crear una variable para la tabla de datos del censo para construir la Clave Geoestadística (CVEGEO)

coa.ageb <- coa.ageb %>% mutate(CVEGEO = paste0(coa.ageb$ENTIDAD,
                                                 coa.ageb$MUN,
                                                 coa.ageb$LOC,
                                                 coa.ageb$AGEB,
                                                 coa.ageb$MZA))


# Convertir a factores para poder hacer el join/merge
coa.ageb$CVEGEO <- factor(coa.ageb$CVEGEO) # La tabla el censo
coah.m$CVEGEO <- factor(coah.m$CVEGEO) # El SF que no se polígono
coah.m1@data$CVEGEO <- factor(coah.m1@data$CVEGEO) # El que si es polígono

# Unir la tabla de estadística a la tabla del shapefile
coa.s <- sp::merge(coah.m, coa.ageb, by="CVEGEO") 
coa.s2 <- sp::merge(coa.ageb, coah.m, by= "CVEGEO")
coa.s3 <- sp::merge(coah.m1, coa.ageb, by = "CVEGEO") # Este si es polígono

coa_sf <- st_as_sf(coa.s3) # Convertir a sf para poderlo filtrar como tibble


# Generar mapas usando varios df

# Con SF
p <- coah.m %>% filter(CVE_MUN == "035") %>% 
  ggplot() + 
  geom_sf(fill = "antiquewhite")

# Con el SpatialPolygonsDataFrame convertido a SF
p <- coa_sf %>% filter(CVE_MUN == "035") %>% 
  ggplot() +
  geom_sf(aes(fill = POBTOT)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Generar mapa interactivo
ggplotly(p) %>% highlight("plotly_hover",
                          selected = attrs_selected(line = list(color = "white")))


# Guardar el SpatialPolygonsDataFrame en shape
writeOGR(coa.s3, ".", "coahuila_shp", driver = "ESRI Shapefile")
