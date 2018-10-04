### Leer datos censo
load(file = "../data/constantes_centrar.RData")

# recibe datos de vivienda del censo
recodificar_viviendas <- function(datos_viviendas) {
  vivienda_recod <- datos_viviendas %>%
      select(ID_VIV, ENT, MUN, AGEB, PISOS, DISAGU, SERSAN, DRENAJE, AUTOPROP, 
      CELULAR, INTERNET, NUMPERS, TAMLOC4) %>%
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
        ifelse(TAMLOC4 == '03', 2, ifelse(TAMLOC4 == '04', 1, 0))))) # %>%
  # select(id_viv, ubica_geo, entidad, municipio, ageb, pisos, dis_agua, 
  # excus, drenaje, automovil, servicio_celular, servicio_internet, tam_hog, tam_loc)
}

recodificar_personas <- function(datos_personas) {
  datos_personas %>%
    select(ID_VIV, SEXO, EDAD, PARENT, HLENGUA, NIVACAD, CONACT)  %>%
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
    maxnved = max(niv_acad, na.rm = TRUE),
    maxnved = ifelse(maxnved == Inf | maxnved == -Inf, NA, maxnved),
    indigena = 1 * any(HLENGUA == 1, na.rm = TRUE)) %>%
  select(id_viv, jefe_sexo, n_ocup, maxnved, indigena) %>%
  ungroup()
}

crear_matrices <- function(datos){
  datos_limpios_hogar <- datos %>%
    mutate(
      id = paste0(ENT, MUN, AGEB),
      jefe_sexo = jefe_sexo,
      pisos = as.numeric(pisos != 1),
      dis_agua = as.numeric(dis_agua == 1),
      excus = as.numeric(excus == 1),
      drenaje = as.numeric(drenaje!=5),
      servicio_celular =  as.numeric(servicio_celular == 1),
      servicio_internet = as.numeric(servicio_internet == 1),
      automovil = as.numeric(automovil >= 1),
      tam_hog = tam_hog - media_tam_hog,
      n_ocup = n_ocup - media_tam_hog,
      max_ed = maxnved - media_maxnved, 
      tam_loc = factor(tam_loc, levels = 1:4)
    )
  x_hogar <- model.matrix(~ jefe_sexo + pisos + dis_agua + excus + 
      drenaje + servicio_celular + servicio_internet + automovil + tam_hog +
      n_ocup + max_ed + n_ocup * max_ed + factor(tam_loc) + indigena, 
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
  matrices <- crear_matrices(viviendas_personas)
}


## Notas, 
## si HLENGUA es no especificado o NA asignamos cero
## si maxnved en NA ignoramos
