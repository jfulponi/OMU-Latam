library(tidyverse)
library(sf)
library(lubridate)
library(osmdata)
library(osrm)
library(leaflet)

## Seteo parámetros

ciudad <- "Porto Alegre"
abrev <- "por"
folder <- "porto"
pop_d <- "ciudades_bid/rio/population_bra_southeast_2018-10-01.csv"
pop_d2 <- "ciudades_bid/rio/population_bra_southwest_2018-10-01.csv"
pop_5_d <- "ciudades_bid/rio/BRA_children_under_five_2019-06-01.csv"
pop_60_d <- "ciudades_bid/rio/BRA_elderly_60_plus_2019-06-01.csv"
level_osm <- "8"

## Busco cada uno de los archivos de Facebook (Movement between Administrative Regions y Movement between Tiles)

adm_files <- list.files(paste0("ciudades_bid/", folder, "/adm"), full.names = T)
tiles_files <- list.files(paste0("ciudades_bid/", folder, "/tiles"), full.names = T)

### CHEQUEO EL FORMATO DE DATES

str_sub(adm_files[1], start =42, end =-10)

start_date <- 42

## Se arma un BBOX 

bbox2 <- read.csv(tiles_files[1])[,1] %>%
  st_as_sfc() %>% st_bbox()

bbox <- opq(bbox = bbox2)

## Se crean desde OSM líneas que definan partidos

boundaries <- bbox %>%
  add_osm_feature(key = 'admin_level', value = level_osm) %>% 
  osmdata_sf %>% unique_osmdata 

municipalities <- boundaries$osm_multipolygons

municipalities <- st_transform(municipalities %>%  select(name) %>% 
                                 rename(partido=name), st_crs(4326))


## CHEQUEO LA INTEGRALIDAD DE MUNICIPALITIES
## Si dan todas TRUE, se pasa a la siguiente línea
## Si da alguna FALSE, hay que hacer coincidir los nombres de los polígonos
## de OSM con los nombres de Facebook (por ejemplo, tildes o espacios)

sort(unique(read.csv(adm_files[1])$start_polygon_name))%in%sort(municipalities$partido)


#Se lee el archivo de population y se transforma a SF
pop_hd <- read_csv(pop_d) %>% bind_rows(read_csv(pop_d2)) %>%   # Lee el archivo de población, en este caso, FB dividió a Brasil en dos y por eso son dos files
  filter(longitude > bbox2[1], latitude > bbox2[2],             # Filtra según el bbox de lat y lon para que no ocupe mucho procesamiento (son archivos enormes)
         longitude < bbox2[3], latitude < bbox2[4]) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) #se convierte a SF con CRS

intersect_pop <- st_drop_geometry(st_join(municipalities, pop_hd))  # Se joinea la población (en puntos) con los límites administrativos, se descarta la geometría

rm(list=c("pop_hd"))

pop_5 <- read_csv(pop_5_d) %>% 
  filter(longitude > bbox2[1], latitude > bbox2[2],
         longitude < bbox2[3], latitude < bbox2[4]) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))

intersect_pop_5 <- st_join(municipalities,pop_5)
intersect_pop_5 <- st_drop_geometry(intersect_pop_5)

rm(list=c("pop_5"))

pop_60 <- read_csv(pop_60_d) %>% 
  filter(longitude > bbox2[1], latitude > bbox2[2],
         longitude < bbox2[3], latitude < bbox2[4]) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) 

intersect_pop_60 <- st_join(municipalities,pop_60) %>% st_drop_geometry()

rm(list=c("pop_60"))

## Se agrupa por partido y se suma la población general, mayored de 60 y menores de 5

intersect_pop <- intersect_pop %>%
  group_by(partido) %>% 
  summarise(pobl = sum(as.numeric(population_2020))) 

intersect_pop_60 <- intersect_pop_60 %>%
  group_by(partido) %>% 
  summarise(pobl_60 = sum(as.numeric(population))) 

intersect_pop_5 <- intersect_pop_5 %>%
  group_by(partido) %>% 
  summarise(pobl_5 = sum(as.numeric(population))) 

intersect_pop <- intersect_pop %>%
  left_join(intersect_pop_5) %>% 
  left_join(intersect_pop_60) %>% 
  mutate(pobl = pobl - pobl_5 - pobl_60) %>%  # Se resta la población que hay que quitar del análisis
  dplyr::select(partido, pobl)

rm(list=c("intersect_pop_5", "intersect_pop_60"))

# Comienza el loop entre cada uno de los archivos

distancias_medias <- list()
viajes_adm_final <- list()

for (i in 1:length(adm_files)){
  
  mov_adm <- read.csv(adm_files[i])
  
  colnames(intersect_pop)[1] <- "pop"
  
  expansion <- intersect_pop %>% 
    rename(start_polygon_name = pop) %>% 
    # right_join(fuzzy %>% rename(start_polygon_name = mov)) %>% 
    left_join(mov_adm %>% group_by(start_polygon_name) %>%
                summarise(n_crisis = sum(n_crisis, na.rm = T))) %>% 
                filter(!is.na(n_crisis)) %>% 
    mutate(fact_expansion = pobl/n_crisis) 
  
  mov_tiles <- read.csv(tiles_files[i])
  
  viajes_cortos <- mov_tiles %>% 
    filter(start_quadkey == end_quadkey) %>% 
    group_by(start_polygon_name, end_polygon_name) %>% 
    summarise(viajes_cortos = sum(n_crisis, na.rm=T)) %>% 
    right_join(expansion) %>% 
    mutate(viajes_cortos_expandidos = viajes_cortos*fact_expansion)
  
  viajes_adm_final[[i]] <- mov_adm %>% 
    left_join(expansion %>% dplyr::select(start_polygon_name, 
                                          fact_expansion)) %>% 
    mutate(viajes = n_crisis*fact_expansion) %>% 
    left_join(viajes_cortos %>% 
                dplyr::select(start_polygon_name,
                              end_polygon_name,
                              viajes_cortos_expandidos)) %>% 
    mutate(viajes_cortos_expandidos = ifelse(is.na(viajes_cortos_expandidos), 
                                             0, viajes_cortos_expandidos), 
           viajes = viajes - viajes_cortos_expandidos,
           date = as.Date(str_sub(adm_files[i], start = start_date, end =-10)))
  
  mov_intrapart <- mov_tiles %>%
    filter(start_polygon_name == end_polygon_name,
           start_quadkey != end_quadkey) %>% 
    mutate(n_crisis = data.table::nafill(n_crisis, fill = 0),
           length_km = data.table::nafill(length_km, fill = 0)) %>% 
    group_by(start_polygon_name, end_polygon_name) %>% 
    #  summarise(distancia_intrapartido = stats::weighted.mean(length_km, n_crisis)) 
    summarise(distancia_intrapartido = if_else(
      is.na(stats::weighted.mean(length_km, n_crisis)), 
      mean(length_km), 
      stats::weighted.mean(length_km, n_crisis))) 
  
  distancias_medias[[i]] <- viajes_adm_final[[i]] %>%
    mutate(viajes = data.table::nafill(viajes, fill = 0),
           n_crisis = data.table::nafill(n_crisis, fill = 0)) %>% 
    left_join(mov_intrapart %>% dplyr::select(start_polygon_name,
                                              end_polygon_name,
                                              distancia_intrapartido)) %>% 
    mutate(length_km = if_else(is.na(distancia_intrapartido), length_km, distancia_intrapartido)) %>% 
    group_by(start_polygon_name, date) %>% 
    summarise(distancia_media = if_else(
      is.na(stats::weighted.mean(length_km, viajes)), 
      mean(length_km), 
      stats::weighted.mean(length_km, viajes))) %>% 
    filter(!is.nan(distancia_media))
    
  print(i)
}

viajes_adm_final <- bind_rows(viajes_adm_final)
distancias_medias <- bind_rows(distancias_medias) 


write_csv(viajes_adm_final %>% mutate(city = ciudad), paste0("viajes_", abrev, ".csv"))
write_csv(distancias_medias %>% mutate(city = ciudad), paste0("distancias_medias_", abrev, ".csv"))
st_write(municipalities %>% select(partido) %>% 
           filter(partido %in% unique(distancias_medias$start_polygon_name)) %>% 
           mutate(city = ciudad), paste0("geo_", abrev, ".geojson"))
