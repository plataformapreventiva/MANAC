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
viviendas <- read.dbf('../datos_censos/DUMMY_VIVIENDAS.dbf')

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
personas <- read.dbf('../datos_censos/DUMMY_PERSONAS.dbf')

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
            n_ocup = sum(ocupado),
            maxnved = max(niv_acad),
            indigena = 1*any(HLENGUA == 1)) %>%
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
   select(id_viv, geo_id, entidad, municipio, ageb,
         jefe_sexo, pisos, dis_agua, excus, drenaje, 
         automovil, servicio_celular, servicio_internet, tam_hog, n_ocup,
         max_ed, tam_loc, indigena)
rm('base', 'cat_mun')

################################################################################################

# Calculo del ingreso por vivienda

# preparar matriz para simulaciones del ingreso
x_vivienda <- model.matrix(~ jefe_sexo + pisos + dis_agua + excus + 
                             drenaje + servicio_celular + servicio_internet + automovil + tam_hog +
                             n_ocup + max_ed + n_ocup * max_ed + factor(tam_loc) + indigena, 
                           data = datos_limpios_vivienda)
x_vivienda <- x_vivienda[, colnames(x_vivienda) != "(Intercept)"]


# extraer parámetros obtenidos con el modelo
load("fit.RData")
ext_fit <- extract(fit)
beta_mun <- apply(ext_fit$beta_mun, 2, mean)
beta_0 <- ext_fit$beta_0
beta_pos <- apply(ext_fit$beta, 2, mean)
sigma_pos <- ext_fit$sigma
log_reps <- apply(ext_fit$log_reps, 2, mean)

# función para estimar el ingreso
gen_quantities_r <- function(x_vivienda, muni){
  reg_pred <- mean(beta_0) + sum(x_hogar * beta_pos) + beta_mun[muni]
  sigma_pred <- sample(sigma_pos, 1)
  log_ing <- rnorm(1, reg_pred, sigma_pred)
  return(log_ing)
}

# se obtienen las estimaciones del ingreso para el conjunto de prueba
set.seed(1234)
pred_log_ing <- rep(0, nrow(x_vivienda)) 
for(i in 1:x_vivienda){
  muni <- datos_limpios_vivienda$geo_id[i]
  pred_log_ing[i] <- gen_quantities_r(x_vivienda[i,], muni)
}
