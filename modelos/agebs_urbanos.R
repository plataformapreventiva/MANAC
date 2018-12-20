library(tidyverse)
library(curl)
library(foreign)

pre <- 'http://www.beta.inegi.org.mx/contenidos/programas/ccpv/2010/microdatos/iter/ageb_manzana/'
post <- '_2010_ageb_manzana_urbana_dbf.zip'
estados <- c('01_aguascalientes', '02_baja_california', 
  '03_baja_california_sur', '04_campeche', '05_coahuila', '06_colima',
  '07_chiapas','08_chihuahua','09_distrito_federal',
  '10_durango','11_guanajuato','12_guerrero','13_hidalgo','14_jalisco','15_mexico',
  '16_michoacan','17_morelos','18_nayarit','19_nuevo_leon','20_oaxaca','21_puebla',
  '22_queretaro','23_quintana_roo','24_san_luis_potosi','25_sinaloa','26_sonora',
  '27_tabasco','28_tamaulipas','29_tlaxcala','30_veracruz','31_yucatan','32_zacatecas')

agebs <- map_df(.x = estados, .f = function(edo){
  url <- paste0(pre,edo,post)
  destino <- paste0(edo,'.zip')
  curl_download(url = url, destfile = paste0('data/censo/', destino),
    quiet = TRUE, mode = 'w')
  system(paste0('unzip -o data/censo/', destino, ' -d data/censo/',edo))
  edo_cve <- str_sub(edo, 1, 2)
  datos <- read.dbf(paste0('data/censo/', edo, '/RESAGEBURB_', edo_cve, 
    'DBF10.dbf'), as.is = TRUE) %>%
    as_tibble() %>%
    filter(MZA == '000' & AGEB != '0000') %>%
    select(-MZA)
  datos
})

# write_rds(agebs, "data_output/agebs.rds")
agebs <- read_rds("data_output/agebs.rds")
glimpse(agebs)
agebs_sub <- agebs %>% 
  select(ENTIDAD:POBFEM, TOTHOG, VIVPAR_HAB, TVIVPARHAB, OCUPVIVPAR, PROM_OCUP,
    HOGJEF_F, PHOGJEF_F, VPH_PISODT, VPH_PISOTI, VPH_AGUAFV, VPH_AGUAFV,
    VPH_EXCSA, VPH_DRENAJ, VPH_NODREN, VPH_C_SERV, VPH_SNBIEN, VPH_AUTOM, 
    VPH_CEL, VPH_INTER, POCUPADA, PDESOCUP, P3YM_HLI, PHOG_IND, P15YM_SE, 
    P15PRI_IN, P15PRI_CO, P15SEC_CO, P18YM_PB, GRAPROES)

# VIVPAR_HAB: Viviendas particulares habitadas
# TVIVPARHAB: Total de viviendas particulares habitadas
# OCUPVIVPAR: Ocupantes en viviendas particulares habitadas
# PROM_OCUP: Promedio de ocupantes en viviendas particulares habitadas
# HOGJEF_M: Hogares censales con jefatura masculina
# HOGJEF_F: Hogares censales con jefatura femenina
# VPH_PISODT: Viviendas particulares habitadas con piso de material diferente de tierra
# VPH_PISOTI: Viviendas particulares habitadas con piso de tierra
# VPH_AGUADV: Viviendas particulares habitadas que disponen de agua entubada en el ámbito de la vivienda
# VPH_AGUAFV: Viviendas particulares habitadas que no disponen de agua entubada en el ámbito de la vivienda
# VPH_EXCSA: Viviendas particulares habitadas que disponen de excusado o sanitario
# VPH_DRENAJ: Viviendas particulares habitadas que disponen de drenaje
# VPH_NODREN: Viviendas particulares habitadas que no disponen de drenaje
# VPH_C_SERV: Viviendas particulares habitadas que disponen de luz eléctrica, agua entubada de la red pública y drenaje
# VPH_SNBIEN: Viviendas particulares habitadas sin ningún bien
# VPH_AUTOM: Viviendas particulares habitadas que disponen de automóvil o camioneta
# VPH_CEL: Viviendas particulares habitadas que disponen de teléfono celular
# VPH_INTER: Viviendas particulares habitadas que disponen de internet
# POCUPADA: Población ocupada. Personas de 12 a 130 años de edad que trabajaron o que no trabajaron pero sí tenían trabajo en la semana de referencia.
# PDESOCUP: Población desocupada. Personas de 12 a 130 años de edad que no tenían trabajo, pero buscaron trabajo en la semana de referencia.
# P3YM_HLI: Población de 3 años y más que habla alguna lengua indígena
# PHOG_IND: Población en hogares censales indígenas
# P15YM_SE: Población de 15 años y más sin escolaridad
# P15PRI_IN: Población de 15 años y más con primaria incompleta
# P15PRI_CO: Población de 15 años y más con primaria completa
# P15SEC_CO: Población de 15 años y más con secundaria completa
# P18YM_PB: Población de 18 años y más con educación pos-básica
# GRAPROES: Grado promedio de escolaridad
