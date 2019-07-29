

# install.packages(c("tidyverse","glue", "rgdal", "stplanr", "tmap", "mapview"))

library(glue)
library(sf)
library(rgdal)
library(tidyverse)
library(stplanr)      # geographic transport data package
library(tmap)         # visualization package (see Chapter 8)


# Definiciones Iniciales --------------------------------------------------

options(scipen=999) # Sacar notacion cientifica

# setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/12_Conferencias/Junta Anual CEDEUS 2019/Gravitacional")
setwd("C:/Users/rtruf/Desktop/Monica/Junta Anual CEDEUS 2019/Gravitacional")


# Definir origenes y destinos ---------------------------------------------

# Leer orígenes (localidades)
origenes <- st_read("Input/manzanas", stringsAsFactors = F ) %>% 
  rename_all(tolower) %>% 
  transmute(
    geo_code = manzent_i,
    pob_tot = total_pers,
    area_AV_m2 = 0
  ) 

# Leer punto destino
destinos <- st_read("Input/aavv") %>% 
  rename_all(tolower) %>% 
  transmute(
    geo_code = str_c("AV", as.character(row_number())),
    pob_tot = 0L,
    area_AV_m2 = 0
  ) 

# Calcular area - Areas verdes en m2
area_AV_m2 <- st_area(destinos) # unidades en m2
destinos$area_AV_m2 <- as.numeric(area_AV_m2) # Agregarlo como variable

# separar en parques y plazas
plazas <- destinos %>% 
  filter(area_AV_m2 < 10000)

parques <- destinos %>% 
  filter(area_AV_m2 >= 10000)

# Unir origenes y destinos - plazas ---------------------------------------

# Unir plazas y mzn en unica base y sacar centroide
centroides <- origenes %>% 
  rbind(plazas) %>% 
  st_centroid() %>% 
  # select(geo_code, pob_tot) %>%  # Seleccionar solo geo-codigo y poblacion total
  as("Spatial") # Transformar shape a spatial points data frame
  
# Plotear
centroides %>% st_as_sf() %>% ggplot() + geom_sf(color = "red")

# Visualizar tabla datos
as.data.frame(centroides)

# Network analysis --------------------------------------------------------

# Proceso siguiendo: https://geocompr.robinlovelace.net/transport.html 

# 1. Armar una matriz origen destino --------------------------------------
dest <- plazas %>% 
  st_set_geometry(NULL) %>% 
  select(geo_code) %>% 
  mutate(count = NA) %>% 
  spread(geo_code, count) 

# Agregar celdas vacias para luego juntar tablas
dest[nrow(dest)+nrow(origenes)-1,] <- NA
  
# Matriz origen - destino
flow <- origenes %>% 
  st_set_geometry(NULL) %>% 
  bind_cols(dest) %>% 
  gather("AV1":"AV99", key = "destino", value = "value") %>% 
  transmute(
    code_orig = geo_code,
    code_dest = destino,
    pob = pob_tot
  )


# 2. Crear lineas rectas entre origen y destino ---------------------------

desire_lines <- od2line(flow = flow, zones=centroides)

desire_lines %>% st_as_sf() %>% mapview::mapview() # Visualizar lineas


# 3. Transformar a ruta ---------------------------------------------------

ruta <- line2route(desire_lines, route_fun = route_osrm) # Funcion ruta route_osrm baja datos de http://project-osrm.org/

# Filtrar distancias menores a 500m
ruta_final <- ruta %>% 
  st_as_sf() %>% 
  st_set_geometry(NULL) %>% 
  filter(distance <= 500)


# 4. Cálculo CAP CARGA -------------------------------------------------------

# Calcular capacidad de carga DESTINOS
DEST_m2_hab <- ruta_final %>% 
  group_by(code_dest, area_AV_m2) %>% 
  summarise(
    pob_AV = sum(pob, na.rm = TRUE)
    ) %>% 
  ungroup() %>% 
  mutate(
    m2_hab_dest = area_AV_m2/pob_AV
  ) %>% 
  select(code_dest, m2_hab_dest)
  
# Llevar m2 AV a origenes
ORIG_m2_hab <- ruta_final %>%
  left_join(DEST_m2_hab, by = "m2_hab_dest") %>% 
  group_by(code_orig) %>% 
  summarise(
    m2_AV_hab = sum(m2_hab_dest, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  rename(geo_code = code_orig)


# Llevar a manzanas originales --------------------------------------------

# Left join capacidad de carga
mzn_final <- origenes %>% 
  left_join(ORIG_m2_hab, by = "geo_code")

# Plotear
mapview::mapview(mzn_final, zcol = "m2_AV_hab", at = seq(0, 10, 2), legend = TRUE)
  
    