library(foreign)
library(tidyverse)
library(rstan)

rm(list = ls())

# 1. Se recodifican los casos que corresponden a "Blanco por pase".
# 2. Se asigna NA a los casos que corresponden a "No especificado".
# 3. Se generan las variables de acuerdo a las características de la 
#    base "enigh_final" creada por Andreu.
# 4. Se genera la base "datos_limpios_viviendas" como lo hizo Tere para el modelo.

# variables de base viviendas
viviendas <- read.dbf('C:/Users/H11765/Documents/Proyecto/Censos/datos_censos/DUMMY_VIVIENDAS.dbf')
#viviendas <- read.dbf('C:/Users/Amanda29/Documents/Proyecto/Censos/datos_censos/DUMMY_VIVIENDAS.dbf')

viviendas <- viviendas %>%
  select(ID_VIV, ENT, MUN, AGEB, PISOS,
         DISAGU, SERSAN, DRENAJE, AUTOPROP, CELULAR, 
         INTERNET, NUMPERS, TAMLOC4)

viviendas <- viviendas %>%
  mutate(id_viv = ID_VIV,
         ubica_geo = paste0(ENT, MUN),
         entidad = ENT, 
         municipio = MUN, 
         ageb = AGEB,
         pisos = ifelse(is.na(PISOS), '0', ifelse(PISOS == '9', NA, PISOS)),
         dis_agua = ifelse(is.na(DISAGU), '0', ifelse(DISAGU == '9', NA, DISAGU)),
         excus = ifelse(is.na(SERSAN), '0', ifelse(SERSAN == '9', NA, SERSAN)),
         drenaje = ifelse(is.na(DRENAJE), '0', ifelse(DRENAJE == '9', NA, DRENAJE)),
         automovil = ifelse(is.na(AUTOPROP), '0', ifelse(AUTOPROP == '9', NA, AUTOPROP)),
         servicio_celular =  ifelse(is.na(CELULAR), '0', ifelse(CELULAR == '9', NA, CELULAR)),
         servicio_internet = ifelse(is.na(INTERNET), '0', ifelse(INTERNET == '9', NA, INTERNET)),
         tam_hog = NUMPERS,
         tam_loc = ifelse(TAMLOC4 == '01', 4, ifelse(TAMLOC4 == '02', 3, 
                   ifelse(TAMLOC4 == '03', 2, ifelse(TAMLOC4 == '04', 1, 0))))) %>%
  select(id_viv, ubica_geo, entidad, municipio, ageb, pisos, 
         dis_agua, excus, drenaje, automovil, servicio_celular, 
         servicio_internet, tam_hog, tam_loc)

# variables de base personas
personas <- read.dbf('C:/Users/H11765/Documents/Proyecto/Censos/datos_censos/DUMMY_PERSONAS.dbf')
#personas <- read.dbf('C:/Users/Amanda29/Documents/Proyecto/Censos/datos_censos/DUMMY_PERSONAS.dbf')
                     
personas <- personas %>%
  select(ID_VIV, SEXO, EDAD, PARENT, HLENGUA, NIVACAD, CONACT)         

personas <- personas %>%
    mutate(id_viv = ID_VIV,
           sexo = ifelse(SEXO == 3, 2, SEXO),
           edad = as.integer(EDAD),
           niv_acad = ifelse(is.na(NIVACAD), 0, ifelse(NIVACAD == '00', 1, 
                      ifelse(NIVACAD == '01', 2, ifelse(NIVACAD == '02', 3, 
                      ifelse(NIVACAD == '03', 4, ifelse(NIVACAD == '04', 5,
                      ifelse(NIVACAD %in% c('05', '09'), 6, 
                      ifelse(NIVACAD %in% c('06', '07', '08'), 7,
                      ifelse(NIVACAD == '10', 8, ifelse(NIVACAD == '11', 9,
                      ifelse(NIVACAD == '12', 10, NA))))))))))),
           ocupado = ifelse(edad > 13 & CONACT %in% c('1', '2'), 1, 0)) %>%
    group_by(id_viv) %>%
    summarise(jefe_sexo = first((sexo-1)*(PARENT == '01')),
              n_ocup = sum(ocupado, na.rm = T),
              maxnved = max(niv_acad, na.rm = T),
              indigena = 1*any(HLENGUA == 1, na.rm = T)) %>%
    select(id_viv, jefe_sexo, n_ocup, maxnved, indigena) %>%
    ungroup()

# unión variables viviendas y personas
base <- left_join(viviendas, personas, by = 'id_viv')
rm('viviendas', 'personas')

# agregar variable geo_id
cat_mun <- read_csv('I:/Censos/cat_mun.csv')
base <- left_join(base, cat_mun, by = 'ubica_geo')

# explorar valores faltantes
faltantes <- DataExplorer::plot_missing(base)

# eliminar valores faltantes
base <- base %>%
  na.omit()

# preparar datos 
datos_limpios_vivienda <- base %>%
  mutate(jefe_sexo = jefe_sexo,
         pisos = as.numeric(pisos != 1),
         dis_agua = as.numeric(dis_agua == 1),
         excus = as.numeric(excus == 1),
         drenaje = as.numeric(drenaje != 5),
         automovil = as.numeric(automovil == 1),
         servicio_celular =  as.numeric(servicio_celular == 3),
         servicio_internet = as.numeric(servicio_internet == 1),
         tam_hog = log(1 + as.numeric(tam_hog)),
         n_ocup = log(1 + n_ocup),
         max_ed = log(1 + maxnved)) %>%
         #max_ed = log(maxnved),
         #max_ed = ifelse(is.na(max_ed), 1, max_ed)) %>%
  select(id_viv, geo_id, entidad, municipio, ageb,
         jefe_sexo, pisos, dis_agua, excus, drenaje, 
         automovil, servicio_celular, servicio_internet, tam_hog, n_ocup,
         max_ed, tam_loc, indigena)
rm('base', 'cat_mun')

# preparar matriz para simulaciones del ingreso
x_vivienda <- model.matrix(~ jefe_sexo + pisos + dis_agua + excus + 
                          drenaje + servicio_celular + servicio_internet + automovil + tam_hog +
                          n_ocup + max_ed + n_ocup * max_ed + factor(tam_loc) + indigena, 
                          data = datos_limpios_vivienda)
x_vivienda <- x_vivienda[, colnames(x_vivienda) != "(Intercept)"]

#########################
## Cálculo del ingreso ##
#########################

# extraer parámetros obtenidos con el modelo
load("fit.RData")
ext_fit <- extract(fit)
beta_mun <- ext_fit$beta_mun
beta_0 <- ext_fit$beta_0
beta_pos <- ext_fit$beta
sigma_pos <- ext_fit$sigma
rm('ext_fit', 'fit')

# función para estimar el ingreso
gen_quantities_r <- function(x_vivienda2, municipio){
  reg_pred <- beta_0 + apply(x_vivienda2 * beta_pos, 1, sum) + beta_mun[,municipio]
  log_ing <- apply(reg_pred, 1, function(x) rnorm(nrow(x_vivienda2), mean = x, sd = sigma_pos))
  return(log_ing)
}

# estimacion del ingreso por municipio
level_tam_loc <- unique(datos_limpios_vivienda$tam_loc)
municipios <- unique(datos_limpios_vivienda$geo_id)
log_ing_mun_final <- NULL

for(i in 1:length(municipios)){
  municipio <- municipios[i]
  datos_limpios_vivienda1 <- datos_limpios_vivienda %>%
    filter(geo_id == municipio)
  x_vivienda1 <- model.matrix(~ jefe_sexo + pisos + dis_agua + excus + 
                    drenaje + servicio_celular + servicio_internet + automovil + tam_hog +
                    n_ocup + max_ed + n_ocup * max_ed + factor(tam_loc, levels=level_tam_loc) + indigena, 
                    data = datos_limpios_vivienda1)
  x_vivienda1 <- x_vivienda1[, colnames(x_vivienda1) != "(Intercept)"]
  pred_log_ing_mun <- rep(0,  nrow(beta_0))
  for(j in 1:nrow(x_vivienda1)){
    x_vivienda2 <- matrix(x_vivienda1[j,], nrow = nrow(x_vivienda1), ncol =  nrow(beta_0), byrow = TRUE) 
    pred_log_ing <- gen_quantities_r(x_vivienda2, municipio)
    pred_log_ing_mun <- pred_log_ing_mun + pred_log_ing
  }
  pred_log_ing_mun <- c(municipio, nrow(x_vivienda1), pred_log_ing_mun)
  log_ing_mun_final <- rbind(log_ing_mun_final, pred_log_ing_mun)
}

write_csv(x = log_ing_mun_final, path = 'resultados_mun.csv')