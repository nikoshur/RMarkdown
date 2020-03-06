library(devtools)

library(sf)
library(dplyr)
library(mapview)
library(fastmap)
library(tmap)

library(naturalearth)


# mapa de los paises
countries <- ne_countries(returnclass = "sf")
tm_shape(shp = countries) +
  tm_polygons (col = "red", border.col = "grey", title = "Paises", lwd = 2, lty = 3) +
  tm_layout (main.title = "Mapa del mundo", main.title.position = "center")


# mapa de españa
sp_c <- ne_countries(returnclass = "sf", country = 'spain')
tm_shape(shp = sp_c) +
  tm_polygons (col = "red")


# lineas de costa
coast <- ne_coastline(returnclass = "sf")
x11()
st_geometry(coast) %>% plot (col = "blue")

# gestión de datos
glimpse(countries)
names(countries)

europe <- countries %>% 
  select ("name_sort", "subregion", "continent", "lastcensus", "geometry") %>% 
  filter (subregion == "Southern Europe")
# si quisiera eliminar la geometria tendria que usar la st_drop_geometry
x11()
plot (europe)

########## Error muy comun, funciones de paquetes con el mismo nombre, para ello hay que usar por ejemplo
########## dplyr::select [nombre del paquete :: nombre de la funcion]

europe2 <- countries %>% 
  select ("geometry") %>% 
# si quisiera eliminar la geometria tendria que usar la st_drop_geometry
  
st_geometry (europe) %>% plot()
x11()
plot (europe2)


# hacemos una agrupacion por subregion
europa_subr <- europe %>% 
  group_by (subregion) %>% 
  summarize(last_census = mean(lastcensus))

europa_subr

europe <- countries %>% 
  select ("name_sort", "subregion", "continent", "lastcensus", "geometry") %>% 
  filter (subregion == "Southern Europe")

mapa_mas_bonito <- europa_subr
tm_shape(shp = mapa_mas_bonito)+
  tm_polygons(col = "lastcensus", border.col = "grey")
  tm_layout(main.title = 'Subregion sur de Europa')


x11()
plot(mapa_mas_bonito, pal = c("red", "yellow"))

# extraer el sistema de coordenadas
st_crs (europa_subr)

# obtener las coordenadas de la extensión del mapa 
europa_subr %>% st_bbox()

# convertir la extensión en un polígono
poly <- st_as_sfc (st_bbox(europa_subr))

library(raster)

setwd("C:\\Users\\Niko\\Desktop\\Programacion_avanzada\\Practicas\\Practica3\\datos")

single <- raster("PNOA_MDT200_ETRS89_HU30_Madrid.asc")

multi <- brick("LT05_L1TP_201032_20080820_20180116_01_T1_MB_7BANDAS.tif")

class(single)
class(multi)
multi
crs(multi)
extent(multi)

# escribir el archivo de salida


writeRaster(single, "elevacion.tif")
writeRaster(multi, "multi_tif.tif")

plot(single)

# 

disk <- file.size ("LT05_L1TP_201032_20080820_20180116_01_T1_MB_7BANDAS.tif")
objeto <- object.size(multi)

disk
objeto
disk/objeto ## el objeto de R es mucho menor!!

x11()
tm_shape(single) + tm_raster()

## vamos a ver la extensión del polígono

extent(multi)

# extension <- c(xmin, xmax, ymin, ymax)
ext_ras <- c(304185, 547215, 4356885, 4573515)
ext_pol <- c(385000, 437215, 4445000, 4473515)
x11()
plot(ext_ras, col = "red")
points(ext_pol, col = "blue", add = TRUE)

new <- crop(multi, ext_pol)
plot(new)
dev.off()
