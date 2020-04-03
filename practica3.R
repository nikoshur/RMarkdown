library(sf)
library(dplyr)
library(raster)
library(rgdal)
library(maptools)
library(jsonlite)
library(mapview)
library(fastmap)
library(tmap)
library(rnaturalearth) # para la base de datos vectoriales de paises


detach(package:sf)
detach(package:dplyr)
detach(package:raster)
detach(package:rgdal)
detach(package:maptools)
detach(package:jsonlite)
detach(package:mapview)
detach(package:fastmap)
detach(package:tmap)
detach(package:rnaturalearth)

library(devtools)

library(sf)

library(dplyr)

library(mapview)
library(fastmap)
library(tmap)

library(rnaturalearth)


setwd(""
andalucia <- ne_states(returnclass = "sf", country = 'spain') %>%
  dplyr::select("region", "geometry") %>% 
  filter(region == "Andalucía")



precip_actual <- chelsa_climate(period = "present",variable = 'prec', meses = 1:12)

precip_futura_CCSM4 <- chelsa_climate(period = "future", variable = "prec", meses = 1:12,
                                      scenario_string = "rcp45", model_string = "CCSM4", future_years = "2041-2060")

precip_futura_ACCESS_0 <- chelsa_climate(period = "future", variable = "prec", meses = 1:12,
                                         scenario_string = "rcp45", model_string = "ACCESS1-0", future_years = "2041-2060")

tmax_actual <- chelsa_climate(period = "present", variable = 'tmax', meses = 1:12)

tmax_futura_CCSM4 <- chelsa_climate(period = "future", variable = "tmax", meses = 1:12,
                                    scenario_string = "rcp45", model_string = "CCSM4", future_years = "2041-2060")

tmax_futura_ACCESS_0 <- chelsa_climate(period = "future", variable = "tmax", meses = 1:12,
                                       scenario_string = "rcp45", model_string = "ACCESS1-0", future_years = "2041-2060")



precip_actual_crop <- crop(precip_actual, andalucia)
precip_futura_CCSM4_crop <- crop(precip_futura_CCSM4, andalucia)
precip_futura_ACCESS_0_crop <- crop(precip_futura_ACCESS_0, andalucia)

tmax_actual_crop <- crop(tmax_actual, andalucia)
tmax_futura_CCSM4_crop <- crop(tmax_futura_CCSM4, andalucia)
tmax_futura_ACCESS_0_crop <- crop(tmax_futura_ACCESS_0, andalucia)



precip_actual_mask <- mask(precip_actual_crop, andalucia)
precip_futura_CCSM4_mask <- mask(precip_futura_CCSM4_crop, andalucia)
precip_futura_ACCESS_0_mask <- mask(precip_futura_ACCESS_0_crop, andalucia)

tmax_actual_mask <- mask(tmax_actual_crop, andalucia)
tmax_futura_CCSM4_mask <- mask(tmax_futura_CCSM4_crop, andalucia)
tmax_futura_ACCESS_0_mask <- mask(tmax_futura_ACCESS_0_crop, andalucia)



media_actual <- calc(tmax_actual_mask, fun = mean)
media_ccsm4 <-calc(tmax_futura_CCSM4_mask, fun = mean)
media_access <- calc(tmax_futura_ACCESS_0_mask, fun = mean)

x11()
plot(precip_actual_mask)
x11()
plot(precip_futura_CCSM4_mask)
x11()
plot(precip_futura_ACCESS_0_mask)


x11()
plot(precip_actual_crop)
x11()
plot(precip_futura_CCSM4_crop)
x11()
plot(precip_futura_ACCESS_0_crop)

precip_futura_CCSM4_mask[precip_futura_CCSM4_mask < 0 ] <- NA


x11()
plot(precip_actual)
x11()
plot(precip_futura_CCSM4)
x11()
plot(precip_futura_ACCESS_0)






res(p_anual)

st_crs(asd)

str(asd)

glimpse(precip_actual)

View(precip_actual)

names(precip_actual)

tm_shape(p_anual) + tm_raster() +
  tm_shape(andalucia) + tm_borders(col = "red", lwd = 3)

x11()
plot(p_anual_mask)

glimpse(tmax_futura)
cropp <- crop(tmax_futura, andalucia)
asd <- calc(cropp, fun = mean)
glimpse(asd)

plot(tmax_futura)
