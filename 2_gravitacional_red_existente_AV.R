

# install.packages(c("tidyverse","glue", "rgdal", "stplanr", "tmap", "mapview"))

library(glue)
library(sf)
library(rgdal)
library(tidyverse)
library(stplanr)      # geographic transport data package
library(tmap)         # visualization package (see Chapter 8)



  
# Definiciones Iniciales --------------------------------------------------

options(scipen=999) # Sacar notacion cientifica

setwd("C:/Users/CEDEUS 18/Documents/CEDEUS/Monica - 2018/12_Conferencias/Junta Anual CEDEUS 2019/Gravitacional")
# setwd("C:/Users/rtruf/Desktop/Monica/Junta Anual CEDEUS 2019/Gravitacional")

# DESCOMENTAR EN EL EJERCICIO
# ######## INTRODUCIR NUEVAS AREAS VERDES ########
# 
# # PARAMETROS AJUSTABLES
# n_grupo <- "G1"
# area_AV_m2 <- 10800 # Agregar area m2
# coord_y <- -73.115
# coord_x <- -36.844
# 
# # NO CAMBIAR
# geo_code <- str_c("AV", n_grupo) # crear con ID con nombre grupo
# pob_tot <- 0L # Dejar tal cual en 0
# 
# # crear df
# df_new <- data.frame(geo_code, pob_tot, area_AV_m2, coord_y, coord_x)  

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

# Leer red
red_local <- st_read("Input/RED") %>% 
  rename_all(tolower)  %>% 
  as("Spatial") %>% # Transformar shape a spatial points data frame
  SpatialLinesNetwork()


# Agregar nuevas areas verdes propuestas ---------------------------------
# DESCOMENTAR EN EL EJERCICIO
# 
# # Transformar nuevas AV a spatial data
# geom_new <- st_as_sf(x = df_new,                         
#                  coords = c("coord_y", "coord_x"),
#                  crs = st_crs(destinos)) 
# 
# # Visualizar - chequear ubicacion correcta
# mapview::mapview(geom_new)
# 
# # Agregar a destinos
# destinos <- destinos %>% 
#   rbind(geom_new)

# ITERACION ---------------------------------------------------------------

isodistancias <- c(5000, 500)
# test
# iso <- 5000

for (iso in isodistancias) { 
  
  areas_verdes <- destinos %>% 
    filter(
      if (iso == 500) {
        area_AV_m2 < 10000
      } else {
        area_AV_m2 >= 10000
      }
    )
  
  # Network analysis --------------------------------------------------------
  
  # Proceso siguiendo: https://geocompr.robinlovelace.net/transport.html 
  
  # 0. Unir origenes y destinos - plazas ---------------------------------------
  
  # Unir plazas y mzn en unica base y sacar centroide
  centroides <- origenes %>% 
    rbind(areas_verdes) %>% 
    st_centroid() %>% 
    as("Spatial") # Transformar shape a spatial points data frame
    
  # Plotear
  centroides %>% st_as_sf() %>% ggplot() + geom_sf(color = "red")
  
  # Visualizar tabla datos
  as.data.frame(centroides)
  
  # 1. Armar una matriz origen destino --------------------------------------
  dest <- areas_verdes %>% 
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
    gather(-c(1:4), key = "destino", value = "value") %>% 
    transmute(
      code_orig = geo_code,
      code_dest = destino,
      pob = pob_tot
    )
  
  
  # 2. Crear lineas rectas entre origen y destino ---------------------------
  
  desire_lines <- od2line(flow = flow, zones=centroides)
  
  desire_lines_filt <- desire_lines %>% 
    st_as_sf() 
  
  desire_lines_filt <- desire_lines_filt %>% 
    mutate(
      dist_recta = as.numeric(st_length(desire_lines_filt))
      ) %>% 
    filter(dist_recta < (2*iso)) %>% 
    as("Spatial")
  
  # 3. Transformar a ruta ---------------------------------------------------
  
  # ruta <- line2route(desire_lines_filt, route_fun = route_osrm) # Funcion ruta route_osrm baja datos de http://project-osrm.org/
  
  ruta <- route_local(network, from = NULL, to = NULL, l = desire_lines_filt)
  
  ruta <- ruta %>% st_as_sf() %>% rename(dist_ruta = distance)
  
  # Filtrar distancias menores a isodistancia (500m plazas o 5000m parques)
  ruta_final <- desire_lines_filt %>% 
    st_as_sf() %>% 
    st_set_geometry(NULL) %>% 
    bind_cols(ruta) %>% 
    filter(dist_ruta <= iso & !is.na(dist_ruta))
  
  # Guardar ruta para corroborar
  ruta_final %>% st_write(glue("Output/ruta_{iso}_local.shp"), delete_layer = TRUE)
  
  
  # 4. Cálculo CAP CARGA -------------------------------------------------------
  
  ruta_final <- ruta_final %>% st_set_geometry(NULL)
  destinos_data <- destinos %>% st_set_geometry(NULL) %>% rename(code_dest = geo_code) %>% select(-pob_tot)
  
  # Calcular capacidad de carga DESTINOS
  DEST_m2_hab <- ruta_final %>% 
    left_join(destinos_data, by = "code_dest") %>% 
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
    left_join(DEST_m2_hab, by = "code_dest") %>% 
    group_by(code_orig) %>% 
    summarise(
      m2_AV_hab = sum(m2_hab_dest, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    rename(geo_code = code_orig) 

  # Guardar mzn para corroborar
  ORIG_m2_hab %>% saveRDS(glue("Output/temp_m2_AV_{iso}.Rds"))
  

  ###### FIN ITERACION #######    
  
}


# 5. Llevar a manzanas originales --------------------------------------------

parques_m2hab <- readRDS("Output/temp_m2_AV_5000.Rds") %>% rename(pqe_m2_AV_hab = m2_AV_hab)
plazas_m2hab <- readRDS("Output/temp_m2_AV_500.Rds") %>% rename(pza_m2_AV_hab = m2_AV_hab)

# Left join capacidad de carga
mzn_final <- origenes %>% 
  # left_join(plazas_m2hab, by = "geo_code") %>% 
  left_join(parques_m2hab, by = "geo_code") %>% 
  select(-area_AV_m2) %>% 
  filter(pob_tot != 0) %>% 
  mutate(
    m2_AV_hab = pqe_m2_AV_hab, #+ pza_m2_AV_hab,
    m2_AV_hab = if_else(is.na(m2_AV_hab), 0, m2_AV_hab)
  )

# Guardar mzn para corroborar
mzn_final %>% st_write("Output/mzn_areas_verdes_total_0.shp", delete_layer = TRUE)

# Visualización -----------------------------------------------------------

# Plotear
areas_verdes_cent <- destinos %>% st_centroid() %>% select(-pob_tot)

# Plotear todo lo mayor a 20m2/hab como 20
mzn_plot <- mzn_final %>% 
  mutate(
  m2_AV_hab = if_else(m2_AV_hab >20, 20, m2_AV_hab)
)

# Visualizar mapa interactivo
mapview::mapview(areas_verdes_cent, col.regions = "green") +
  mapview::mapview(mzn_plot, zcol = "m2_AV_hab", at = seq(0, 20, 5), legend = TRUE) 
  


