library(tidyverse)
datos_mun <- read_csv("Z:/Procesamiento/Trabajo/lab_microdatos/data/datos_mun.csv")

datos_mun <- datos_mun %>%
  mutate(cve_edo = stringr::str_sub(string = ubica_geo, start = 1, end = 2),
         cve_mun = stringr::str_sub(string = ubica_geo, start = 3, end = -1))

cpv_vivienda <- "Z:/Procesamiento/Insumos/Censo de Población y Vivienda/VIVIENDA10_"
cpv_personas <- "Z:/Procesamiento/Insumos/Censo de Población y Vivienda/PERSONA10_"
out <- "Z:/Procesamiento/Trabajo/CPV/"

guarda_sub <- function(i){
  edo <- str_pad(string = i, width = 2, pad = '0')
  ruta_viv <- paste0(cpv_vivienda, edo, ".csv")
  ruta_per <- paste0(cpv_personas, edo, ".csv")
  
  cat(sprintf("\n\nLeyendo datos de entidad %s \n\n", edo))
  viviendas <- read.csv(ruta_viv)
  colnames(viviendas) <- str_to_upper(colnames(viviendas))
  # variables de base personas
  personas <- read.csv(ruta_per)
  colnames(personas) <- str_to_upper(colnames(personas))
  
  viviendas <- viviendas %>% select(ID_VIV, ENT, MUN, AGEB, LOC, PISOS, DISAGU, SERSAN, DRENAJE, AUTOPROP, 
                                    CELULAR, INTERNET, NUMPERS, TAM_LOC)
  personas <- personas %>% select(ID_VIV, MUN, SEXO, EDAD, PARENT, HLENGUA, NIVACAD, CONACT)
  
  # checar que exista directorio de estado o crearlo
  path <- paste0(out, edo, '/')
  if(!dir.exists(path)){
    dir.create(path, showWarnings = TRUE, recursive = FALSE, mode = "777")
  }
  
  # corremos por municipio
  muns <- datos_mun %>% filter(cve_edo == edo) %>% select(cve_mun)
  
  r <- map(.x= 1:nrow(muns), .f = function(j){
    mun <- muns$cve_mun[j]
    path <- paste0(out, edo, '/', mun, '/')
    cat(sprintf("\t Escribiendo archivos de municipio %s\n", paste0(edo,mun)))
    if(!dir.exists(path)){
      dir.create(path, showWarnings = TRUE, recursive = FALSE, mode = "777")
    }
    viv_mun <- viviendas %>% filter(MUN == parse_integer(mun))
    per_mun <- personas %>% filter(MUN == parse_integer(mun))
    write_csv(x = viv_mun, path = paste0(path, 'viviendas.csv'))
    write_csv(x = per_mun, path = paste0(path, 'personas.csv'))
    return(T)
  })
  return(r)
}

estados <- 1:32
resultado <- map(.x = estados, .f = guarda_sub)
Reduce(sum,Reduce(c, resultado))
