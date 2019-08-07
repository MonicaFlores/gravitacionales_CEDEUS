library(tidyverse)
library(sf)
library(glue)

# Leer shape AV y agregar sitios eriazos

setwd("~/Desktop/AV/aavv_accesibilidad/San Pedro/input")

shp_base <- st_read("aavv/aavv_centroide.shp") %>%
  select(SUP_TOTAL_, AVID)


# GRUPO1 (ejemplo) --------------------------------

grupo <- "G1"
seleccion <- c("12", "25", "28")

# Leer sitios eriazos y filtrar elegidos
sitios <- st_read("eriazos") %>%
  # st_transform(5361) %>%
  filter(IDSI %in% paste0("SI100", seleccion)) %>% # Ejemplo sitios Grupo1
  transmute(SUP_TOTAL_ = as.numeric(st_area(.)),
         AVID = IDSI) 

sitios_cent <- st_centroid(sitios)  %>% 
  st_transform(st_crs(shp_base))

# Unir nuevos sitios
av_G1 <- rbind(shp_base, sitios_cent)

# separar en parques y plazas
av_G1_pqe <- av_G1 %>% filter(SUP_TOTAL_>=10000)
av_G1_pza <- av_G1 %>% filter(SUP_TOTAL_<10000)


# GRUPO4 --------------------------------

grupo <- "G4"
seleccion <- c("02", "04", "11")

# Leer sitios eriazos y filtrar elegidos
sitios <- st_read("eriazos") %>%
  # st_transform(5361) %>%
  filter(IDSI %in% paste0("SI100", seleccion)) %>% # Ejemplo sitios Grupo1
  transmute(SUP_TOTAL_ = as.numeric(st_area(.)),
            AVID = IDSI) 

sitios_cent <- st_centroid(sitios)  %>% 
  st_transform(st_crs(shp_base))

# Unir nuevos sitios
av_G4 <- rbind(shp_base, sitios_cent)

# separar en parques y plazas
av_G4_pqe <- av_G4 %>% filter(SUP_TOTAL_>=10000)
av_G4_pza <- av_G4 %>% filter(SUP_TOTAL_<10000)

# guardar
av_G4 %>% st_write(glue("{grupo}/av_G4.shp"), delete_layer = TRUE)
av_G4_pqe %>% st_write(glue("{grupo}/av_G4_pqe.shp"), delete_layer = TRUE)
av_G4_pza %>% st_write(glue("{grupo}/av_G4_pza.shp"), delete_layer = TRUE)
  

# GRUPO1 --------------------------------

grupo <- "G1"
seleccion <- c("02", "01", "07")

# Leer sitios eriazos y filtrar elegidos
sitios <- st_read("eriazos") %>%
  # st_transform(5361) %>%
  filter(IDSI %in% paste0("SI100", seleccion)) %>% # Ejemplo sitios Grupo1
  transmute(SUP_TOTAL_ = as.numeric(st_area(.)),
            AVID = IDSI) 

sitios_cent <- st_centroid(sitios)  %>% 
  st_transform(st_crs(shp_base))

# Unir nuevos sitios
av_G1 <- rbind(shp_base, sitios_cent)

# separar en parques y plazas
av_G1_pqe <- av_G1 %>% filter(SUP_TOTAL_>=10000)
av_G1_pza <- av_G1 %>% filter(SUP_TOTAL_<10000)

# guardar
av_G1 %>% st_write(glue("{grupo}/av_G1.shp"), delete_layer = TRUE)
av_G1_pqe %>% st_write(glue("{grupo}/av_G1_pqe.shp"), delete_layer = TRUE)
av_G1_pza %>% st_write(glue("{grupo}/av_G1_pza.shp"), delete_layer = TRUE)

