library(tidyverse)
options(warn = -1)

fecha <- "2018-11-26"
dir <- paste0("Z:/Resultados/CPV-", fecha, "/LM883-CPV-", fecha, "-Resultados/")

zz <- file(paste0(dir,"log.txt"), open = "wt")
sink(zz)
sink(zz, type = "message")

### Funciones para recodificar datos, predecir y hacer reportes
setwd(dir)
datos_mun <- read_csv("Z:/Procesamiento/Trabajo/lab_microdatos/data/datos_mun.csv")
datos_mun <- datos_mun %>%
  mutate(cve_edo = stringr::str_sub(string = ubica_geo, start = 1, end = 2),
         cve_mun = stringr::str_sub(string = ubica_geo, start = 3, end = -1))
source("recodificar-datos.R")
source("prediccion.R")
source("reportes-faltantes.R")

# ######## Primer estado y municipio se hace a mano para revisar 
# ### Aqui leemos datos por estado
# # variables de base viviendas
# dummy_viviendas <- foreign::read.dbf('../../datos/DUMMY_VIVIENDAS.dbf', 
#   as.is = TRUE)
# # variables de base personas
# dummy_personas <- foreign::read.dbf('../../datos/DUMMY_PERSONAS.dbf', 
#   as.is = TRUE)
# 
# # corremos por municipio
# muns <- unique(dummy_viviendas$MUN)
# # Primera vez correr para SOLO UN MUNICIPIO
# mun <- muns[1]
# datos_viviendas_mun <- filter(dummy_viviendas, MUN == mun)
# datos_personas_mun <- filter(dummy_personas, MUN == mun)
# # dependiendo de memoria borrar bases estatales 
# rm(dummy_viviendas, dummy_personas)
# # crea matriz de diseño
# mat <- preparar_datos(datos_vivienda = datos_viviendas_mun, 
#   datos_persona = datos_personas_mun)
# # predice por municipio y escribe las simulaciones para el municipio
# predice_mun(mat, path_save = "../salidas/")
# 
# # Correr por estado
# procesa_edo(path_viviendas = '../../datos/DUMMY_VIVIENDAS.dbf', 
#   path_personas = '../../datos/DUMMY_PERSONAS.dbf', path_salida = "../salidas/")
# 
# ### reportes faltantes (por estado?)
# # reportes de faltantes en base viviendas por AGEB y municipio
# reporte_faltantes_viviendas(path_viviendas = '../../datos/DUMMY_VIVIENDAS.dbf', 
#   "faltantes_ageb.csv")
# # reportes de faltantes en base personas por AGEB y municipio 
# reporte_faltantes_personas(path_personas = '../../datos/DUMMY_PERSONAS.dbf',
#   "faltantes_personas_ageb.csv")


# Corremos por municipio

out <- "Z:/Procesamiento/Trabajo/CPV/"
ruta_out <- dir

corre_edo <- function(i){
  edo <- str_pad(string = i, width = 2, pad = '0')
  muns <- datos_mun %>% filter(cve_edo == edo) %>% select(cve_mun)
  
  # Crear directorio si no existe
  path_edo <- paste0(ruta_out,edo,'/')
  if(!dir.exists(path_edo)){
    dir.create(path_edo, showWarnings = TRUE, recursive = FALSE, mode = "777")
  }
  cat(sprintf("Entidad %s \n\n", edo))
  r <- map(.x= 1:nrow(muns), .f = function(j){
    mun <- muns$cve_mun[j]
    cat(sprintf("\t Procesando municipio: %s\n", paste0(edo,mun)))
    path <- paste0(out, edo, '/', mun, '/')
    datos_viviendas_mun <- read_csv(paste0(path,"viviendas.csv"),
                                    col_types = "iiiciiiiiiiiii", progress = F)
    path_faltantes_v <- paste0(path_edo,edo,mun,"_faltantes_ageb.csv")
    reporte_faltantes_viviendas(datos_viviendas = datos_viviendas_mun, 
                                archivo_reporte = path_faltantes_v)
    cat(sprintf("\t \t \t %s municipio: %s\n", "Faltantes viviendas", paste0(edo,mun)))
    datos_personas_mun <- read_csv(paste0(path,"personas.csv"),
                                   col_types = "iiiiiiii", progress = F)
    ubic_ageb <- datos_viviendas_mun %>% select(ID_VIV,ENT,MUN,AGEB)
    datos_personas_mun <- datos_personas_mun %>%
      left_join(ubic_ageb, by = c("ID_VIV","MUN"))
    path_faltantes_p <- paste0(path_edo,edo,mun,"_faltantes_personas_ageb.csv")
    cat(sprintf("\t \t \t %s municipio: %s\n", "Faltantes personas", paste0(edo,mun)))
    reporte_faltantes_personas(datos_personas = datos_personas_mun,
                               archivo_reporte = path_faltantes_p)
    
    mat <- tryCatch(
      {
        message(sprintf("\n\t \t \t Started %s.", Sys.time()))
        cat(sprintf("\t \t \t %s municipio: %s\n", "Model matrix", paste0(edo,mun)))
        start <- Sys.time()
        mat <- preparar_datos(datos_vivienda = datos_viviendas_mun, 
                              datos_persona = datos_personas_mun)
        end <- Sys.time()
        z <- difftime(end, start, units="secs")
        message(sprintf("\t \t \t Processing time: %.2f secs.", 
                        round(as.numeric(z),2)))
        mat
      },
      error = function(cond) {
        message(sprintf("\t \t \t Error computing model matrix for municipio: %s.", 
                        paste0(edo,mun)))
        print(cond)
        return(NA)
      },
      finally={
        message(sprintf("\t \t \t Finished %s.\n", Sys.time()))
      }
    )
    
    # Limpiamos memoria
    rm(datos_viviendas_mun, datos_personas_mun)
    
    if(all(!is.na(mat))){
      t <- tryCatch(
        {
          message(sprintf("\n\t \t \t Started %s.", Sys.time()))
          cat(sprintf("\t \t \t %s municipio: %s\n", "Predicciones", 
                      paste0(edo,mun)))
          start <- Sys.time()
          if(nrow(mat$x_hogar) <= 0) stop("Model matrix has zero rows.")
          t <- system.time(predice_mun(mat, path_save = paste0(path_edo,edo,mun,".csv")))
          t
        },
        error=function(cond) {
          message(sprintf("\t \t \t Error computing predictions for municipio: %s.", 
                          paste0(edo,mun)))
          print(cond)
          return(NA)
        },
        finally={
          message(sprintf("\t \t \t Finished %s.\n", Sys.time()))
        }
      )
    }
    
    return(as.numeric(t["elapsed"]))
  })
  
  return(r)
}

estados <- 24:32
tiempos <- map(.x = estados, .f = corre_edo)
Reduce(sum,Reduce(c, tiempos))

sink(type = "message")
sink()
