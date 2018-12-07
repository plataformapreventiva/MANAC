#!/usr/bin/env Rscript
### Leer datos censo
load(file = "Z:/Procesamiento/Trabajo/lab_microdatos/data/constantes_centrar.RData")

# recibe datos de vivienda del censo
recodificar_viviendas <- function(datos_viviendas) {
  viv_tamloc <- datos_viviendas %>%
    group_by(ENT,MUN,AGEB,LOC) %>%
    summarise(n_habitantes = sum(as.integer(NUMPERS))) %>%
    mutate(TAMLOC4 = ifelse(n_habitantes < 2500, "01",ifelse(n_habitantes <= 14999, "02",
                                                             ifelse(n_habitantes <= 99999, "03", "04")))) %>%
    select(-n_habitantes)
  vivienda_recod <- datos_viviendas %>%
    select(ID_VIV, ENT, MUN, LOC, AGEB, PISOS, DISAGU, SERSAN, DRENAJE, AUTOPROP, 
           CELULAR, INTERNET, NUMPERS) %>%
    left_join(viv_tamloc, by = c("ENT","MUN","AGEB","LOC")) %>%
    select(-LOC) %>%
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
           tam_hog = as.numeric(NUMPERS),
           tam_loc = ifelse(TAMLOC4 == '01', 4, ifelse(TAMLOC4 == '02', 3, 
                                                       ifelse(TAMLOC4 == '03', 2, ifelse(TAMLOC4 == '04', 1, 0)))))
  vivienda_recod # %>%
  # select(id_viv, ubica_geo, entidad, municipio, ageb, pisos, dis_agua,
  # excus, drenaje, automovil, servicio_celular, servicio_internet, tam_hog, tam_loc)
}

recodificar_personas <- function(datos_personas) {
  datos_personas %>%
    select(ID_VIV, SEXO, EDAD, PARENT, HLENGUA, NIVACAD, CONACT) %>%
    mutate_if(is.factor, as.character) %>% 
    mutate(id_viv = ID_VIV,
      sexo = ifelse(SEXO == 3, 2, 1),
      edad = as.integer(EDAD),
      niv_acad = case_when(
        is.na(NIVACAD) ~ 0,
        NIVACAD == 0 ~ 1,
        NIVACAD == 1 ~ 2,
        NIVACAD == 2 ~ 3,
        NIVACAD == 3 ~ 4,
        NIVACAD == 4 ~ 5,
        NIVACAD %in% c(5, 9) ~ 6,
        NIVACAD %in% 6:8 ~ 7,
        NIVACAD == 10 ~ 8,
        NIVACAD == 11 ~ 9,
        NIVACAD == 12 ~ 10,
        TRUE ~ NA_real_
    ),
    ocupado = ifelse(edad > 13 & CONACT %in% 1:2, 1, 0)) %>%
  group_by(id_viv) %>%
  summarise(jefe_sexo = sum((sexo - 1) * (PARENT == 1)),
    n_ocup = sum(ocupado),
    maxnved = max(niv_acad, na.rm = TRUE),
    maxnved = ifelse(maxnved == Inf | maxnved == -Inf, NA, maxnved),
    indigena = 1 * any(HLENGUA == 1, na.rm = TRUE)) %>%
  select(id_viv, jefe_sexo, n_ocup, maxnved, indigena) %>%
  ungroup()
}

crear_matrices <- function(datos){
  datos_limpios_hogar <- datos %>%
    mutate(
      AGEB = as.character(AGEB),
      AGEB = str_pad(AGEB,4,'left',0),
      id = paste0(str_pad(ENT,2,'left','0'), str_pad(MUN,3,'left','0'), '-', AGEB),
      jefe_sexo = jefe_sexo,
      pisos = as.numeric(pisos != 1),
      dis_agua = as.numeric(dis_agua == 1),
      excus = as.numeric(excus == 1),
      drenaje = as.numeric(drenaje!=5),
      servicio_celular =  as.numeric(servicio_celular == 1),
      servicio_internet = as.numeric(servicio_internet == 1),
      automovil = as.numeric(automovil == 1),
      tam_hog = tam_hog - media_tam_hog,
      n_ocup = n_ocup - media_tam_hog,
      max_ed = maxnved - media_maxnved, 
      tam_loc = factor(tam_loc, levels = 1:4)
    )
  x_hogar <- model.matrix(~ jefe_sexo + pisos + dis_agua + excus + 
      drenaje + servicio_celular + servicio_internet + automovil + tam_hog +
      n_ocup + max_ed + n_ocup * max_ed + tam_loc + indigena, 
    data = datos_limpios_hogar)
  x_hogar <- x_hogar[, colnames(x_hogar) != "(Intercept)"]
  list(ids = datos_limpios_hogar$id, x_hogar = x_hogar)
}

preparar_datos <- function(datos_vivienda, datos_persona) {
  viviendas_recod <- recodificar_viviendas(datos_viviendas = datos_vivienda)
  personas_recod <- recodificar_personas(datos_personas = datos_persona)
  viviendas_personas <- left_join(viviendas_recod, personas_recod, 
    by = "id_viv") %>% 
    na.omit()
  matrices <- crear_matrices(datos = viviendas_personas)
}


## Notas, 
## si HLENGUA es no especificado o NA asignamos cero
## si maxnved en NA ignoramos
