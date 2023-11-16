library(dplyr)
library(stringr)
library(tidygeocoder)

direcciones <- tibble::tribble(~name, ~addr,
                              "Prueba1", "Viena 560, 27268 Torreon Residencial, Torreon, Coahuila, Mexico",
                              "Prueba2", "C. de las Golondrinas 394, Prolongacion los Nogales, Torreon, Coahuila",
                              "Prueba3", "P. del Tecnologico 421, La Rosita, Amp La Rosita, 27258 Torreon, Coahuila"
                              )
lat_longs <- direcciones %>% geocode(addr, method = 'arcgis', lat = latitude, long = longitude)

lat_longs$latitude
lat_longs$longitude


###
prueba1 <- cuentas %>% 
  filter(ruta == "1-05-10")

head(prueba1)

prueba1$direccion <- str_c(prueba1$calle, " ", prueba1$numero, ", ", prueba1$colonia, ", Torreon, Coahuila, Mexico")

prueba1$direccion

dir_prueba1 <- prueba1 %>% geocode(direccion, method = 'arcgis', lat = latitude, long = longitude)

dir_prueba1$latitude
dir_prueba1$longitude

head(dir_prueba1)

write.csv(dir_prueba1, "dir_prueba3.csv")


### Loop

sector1_01 <- cuentas_ord %>% 
  filter(!is.na(ruta)) %>%
  filter(ruta != "NA") %>%
  filter(subsistema == "1") %>% 
  filter(sector == "01") %>% 
  mutate(direccion = str_c(calle, " ", numero, ", ", colonia, ", Torreon, Coahuila, Mexico")) 

sector1_01$direccion = str_c(sector1_01$calle, " ",
                             sector1_01$numero, ", ",
                             sector1_01$colonia, ", Torreon, Coahuila, Mexico")

sector1_01 <- sector1_01 %>% geocode(direccion,
                          method = 'arcgis',
                          lat = latitude,
                          long = longitude,
                          limit = 1)


write.csv(sector1_01, "sector1_01.csv")

# Rutas de Z.E. 1-24, 1-32, 2-31, 2-32

## Plot points

library(rgdal)
library(sf)
library(mapview)
library(ggplot2)
library(ggmap)
library(leaflet)
library(leaflet.extras)


sector1_10_sf <- st_as_sf(sector1_01, coords = c('longitude','latitude'))

sector1_10_sf <- st_set_crs(sector1_01, crs = 4326)

#Plot it:

ggplot(prueba_sf) + 
  geom_sf(aes(color = ruta)) + mapview()


leaflet(sector1_01) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, 
                   popup = ~direccion)

pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'purple', 'orange'),
  domain = sector1_10$ruta)

sector109 <- leaflet(sector1_09) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
             popup = ~direccion, color = ~pal(ruta))
  
# Exportar html
library(htmlwidgets)
htmlwidgets::saveWidget(sector109,
                        "sector109.html", 
                        selfcontained = T)
