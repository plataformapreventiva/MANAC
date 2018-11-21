library(tidyverse)

### Funciones para recodificar datos, predecir y hacer reportes
source("recodificar-datos.R")
source("prediccion.R")
source("reportes-faltantes.R")

######## Primer estado y municipio se hace a mano para revisar 
### Aquí leemos datos por estado
# variables de base viviendas
dummy_viviendas <- foreign::read.dbf('../../datos/DUMMY_VIVIENDAS.dbf', 
  as.is = TRUE)
# variables de base personas
dummy_personas <- foreign::read.dbf('../../datos/DUMMY_PERSONAS.dbf', 
  as.is = TRUE)

# corremos por municipio
muns <- unique(dummy_viviendas$MUN)
# Primera vez correr para SOLO UN MUNICIPIO
mun <- muns[1]
datos_viviendas_mun <- filter(dummy_viviendas, MUN == mun)
datos_personas_mun <- filter(dummy_personas, MUN == mun)
# dependiendo de memoria borrar bases estatales 
rm(dummy_viviendas, dummy_personas)
# crea matriz de diseño
mat <- preparar_datos(datos_vivienda = datos_viviendas_mun, 
  datos_persona = datos_personas_mun)
# predice por municipio y escribe las simulaciones para el municipio
predice_mun(mat, path_save = "../salidas/")

# Correr por estado
procesa_edo(path_viviendas = '../../datos/DUMMY_VIVIENDAS.dbf', 
  path_personas = '../../datos/DUMMY_PERSONAS.dbf', path_salida = "../salidas/")

### reportes faltantes (por estado?)
# reportes de faltantes en base viviendas por AGEB y municipio
reporte_faltantes_viviendas(path_viviendas = '../../datos/DUMMY_VIVIENDAS.dbf', 
  "faltantes_ageb.csv")
# reportes de faltantes en base personas por AGEB y municipio 
reporte_faltantes_personas(path_personas = '../../datos/DUMMY_PERSONAS.dbf',
  "faltantes_personas_ageb.csv")


