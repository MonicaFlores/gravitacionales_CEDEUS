library(tidyverse)
library(sf)
library(glue)

# Leer shape AV y agregar sitios eriazos

setwd("~/Desktop/AV/aavv_accesibilidad/San Pedro")

mzn_tasa_0 <- st_read("input/G0_base/output") %>%
  select(geo_code = MANZENT_I,  m2_AV_0 = AVERDE) 


# Tasa de cambio GRUPO 1 (ejemplo) --------------------------------------------------

# grupo <- "G1"
# 
# mzn_tasa_G1 <- st_read("output/{grupo}.shp") %>% 
#   select(geo_code = MANZENT_I,  m2_AV_G1 = AVERDE) %>% 
#   st_set_geometry(NULL)
# 
# # Cálculo tasa de cambio
# av_join_G1 <- mzn_tasa_0 %>% 
#   left_join(mzn_tasa_G1, by="geo_code") %>% 
#   mutate(
#     m2_avp_cambio = m2_AV_G1 - m2_AV_0,
#     m2_avp_pctc = if_else(m2_AV_0==0, 0, ((m2_AV_G1 - m2_AV_0)/ m2_AV_0)*100)
#   ) 
# 
# # mapview::mapview(av_join_G1, zcol = "m2_avp_cambio", at = seq(0, 10, 2), legend = TRUE)
# 
# # Guardar
# av_join_G1 %>% st_write(glue("tasa_cambio/{grupo}/av_G4.shp"), delete_layer = TRUE)


# Tasa de cambio GRUPO 4  --------------------------------------------------

grupo <- "G4"

mzn_tasa_G4 <- st_read(glue("tasa_cambio/{grupo}/final_total.shp")) %>% 
  select(geo_code = MANZENT_I,  m2_AV_G4 = AVERDE) %>% 
  st_set_geometry(NULL)

# Cálculo tasa de cambio
av_join_G4 <- mzn_tasa_0 %>% 
  left_join(mzn_tasa_G4, by="geo_code") %>% 
  mutate(
    m2_avp_cambio = m2_AV_G4 - m2_AV_0,
    m2_avp_pctc = if_else(m2_AV_0==0, 0, ((m2_AV_G4 - m2_AV_0)/ m2_AV_0)*100)
  ) 

# mapview::mapview(av_join_G4, zcol = "m2_avp_cambio", at = seq(0, 10, 2), legend = TRUE)

# Guardar
av_join_G4 %>% st_write(glue("tasa_cambio/{grupo}/av_G4.shp"), delete_layer = TRUE)


# Tasa de cambio GRUPO 1  --------------------------------------------------

grupo <- "G1"

mzn_tasa_G1 <- st_read(glue("tasa_cambio/{grupo}/Final_Total.shp")) %>% 
  select(geo_code = MANZENT_I,  m2_AV_G1 = AVERDE) %>% 
  st_set_geometry(NULL)

# Cálculo tasa de cambio
av_join_G1 <- mzn_tasa_0 %>% 
  left_join(mzn_tasa_G1, by="geo_code") %>% 
  mutate(
    m2_avp_cambio = m2_AV_G1 - m2_AV_0,
    m2_avp_pctc = if_else(m2_AV_0==0, 0, ((m2_AV_G1 - m2_AV_0)/ m2_AV_0)*100)
  ) 


# Guardar
av_join_G1 %>% st_write(glue("tasa_cambio/{grupo}/av_G1.shp"), delete_layer = TRUE)
