getwd()

setwd("C:\\Users\\Niko\\Desktop\\Programacion_avanzada\\Practicas\\Practica3\\datos")

# vamos a hacer un breve ejemplo de cosas que vamos a ver en el tema 3 y 4 

library(sf)
library(dplyr)
library(mapview)
library(fastmap)
library(tmap)

## ejemplo 1: datos vectoriales de tipo polÃ­gono ####

## leer datos vectoriales

bayarea <- read_sf("backup-counties-bayarea.shp")

str(bayarea)

glimpse(bayarea)

view(bayarea)

## hacemos un grÃ¡fico del mapa

x11()
plot(bayarea)
names(bayarea)

## hacemos un grÃ¡fico sencillo con tmap

tm_shape(bayarea) +
  tm_polygons(col = "county", lwd = 2, lty = 3) +
  tm_layout("San Francisco") ## tÃ­tulo


## calcular los centroides del polÃ­gono

bayarea_cent <- st_centroid(bayarea)

mapview(list(bayarea, bayarea_cent))

setwd("C:\\Users\\Niko\\Desktop\\Programacion_avanzada\\Practicas\\Practica3\\Limites_provinciales\\BDLJE_Completo\\recintos_provinciales_inspire_peninbal_etrs89")

peninsula <- read_sf("recintos_provinciales_inspire_peninbal_etrs89.shp") ## se puede utilizar tambien st_read, pero la otra es mas recomendable

x11()
plot(peninsula, max.plot = 1) ## procesamiento muy pesado
mapview(peninsula)
glimpse(peninsula)

st_write(peninsula, "output\\provincia.shp")

class(peninsula)
head(peninsula)

## obtener la geometría del archibo vectorial

geo <- st_geometry(peninsula)

plot(geo, max.plot = 1)

## obtenemos la proyección y la cambiamos

st_crs(peninsula)

peninsula_proj <- st_transform(peninsula, crs = 32618)
st_crs(peninsula_proj)

## ejemplo 3: con datos vectoriales tipo puntos ####
## leemos nuestro archivo de campo en el que hemos tomados datos espaciales
## plotcode Madrid txt

setwd("C:\\Users\\Niko\\Desktop\\Programacion_avanzada\\Practicas\\Practica3\\datos")

madrid <- read.table("Plotcode_madrid.txt", sep = "\t", header = TRUE)
madrid
typeof(madrid)
names(madrid)
str(madrid)
dim(madrid)

summary(madrid) ## ver si hay NA, etc, hacerlo siempre para ver la calidad y estructura de los datos

madrid$FCCARB[is.na(madrid$FCCARB)] <- 0

madrid <- na.omit(madrid) ## eliminar observaciones con cualquier valor como na
## se eliminan de todo el dataframe cualquier columna con valor na

summary(madrid)

## vamos a seleccionar todas las parelas donde la cobertura sea mayor al 10% 
## (i.e. definidas como bosque)

bosque <- madrid[madrid$FCCARB > 10,]
bosque
summary(bosque)

## ahora pasamos de un data frame a un archivo veectorial
## para ello tenemos que saver CY = latitud y CX = longitud

sbosque <-  bosque %>%  ## ctrl + shift + m shortcut para la pipa!!
  st_as_sf(coords = c("CX", "CY"), crs = 25830) ## se convierte a un sf con un determinado sistema de coordenadas

plot(sbosque)

st_geometry(sbosque) %>% plot(col = "blue")
st_geometry(sbosque) %>% plot(col = "blue", pch = 16)
st_geometry(sbosque) %>% plot(col = "blue", pch = 16, cex = 0.2)

st_write(sbosque, "tratamiento_madrid_sbosque.shp")

## ejemplo 4: datos de tipo polígono
## install.packages("devtools")
## install.packages("naturalearth")
## devtools::install_github("ropensci/rnaturalearth", force = TRUE) sinofunciona el anterior, que es lo mas comun


library(devtools)
library(rnaturalearth)

countries <- ne_countries(returnclass = "sf")

plot(countries, max.plot = 1)

## hacemos un mapa con los paises

tm_shape(shp = countries) +
  tm_polygons(col = "name", border.col = "grey",
              title = "Paises", lwd = 2, lty =3) +
  tm_layout(main.title = "Mapa del mundo",
            main.title.position = "center")
