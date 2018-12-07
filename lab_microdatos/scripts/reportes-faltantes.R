# escribe tablas donde para cada var_agrupar (AGEB o municipio) se reporta el 
# número de faltantes por variable y el número de hogares con al menos un 
# faltante.

reporte_faltantes <- function(datos_viviendas, archivo_reporte) {
  faltantes_var <- datos_viviendas  %>% 
    group_by(AGEB) %>% 
    summarise_all(funs(falta = sum(is.na(.))))
  datos_viviendas$viviendas_na <- apply(datos_viviendas, 1, function(x) 
    sum(is.na(x)) > 0)
  faltantes_viviendas <- datos_viviendas %>% 
    group_by(AGEB) %>% 
    summarise(n_viviendas_na = sum(viviendas_na), n_viviendas = n())
  tab_faltantes <- faltantes_var %>% 
    left_join(faltantes_viviendas, by = "AGEB")
  write_csv(tab_faltantes, path = archivo_reporte)
}

# variables de base viviendas
reporte_faltantes_viviendas <- function(datos_viviendas, archivo_reporte) {
  viviendas <- datos_viviendas %>%
    select(ID_VIV, ENT, MUN, AGEB, PISOS, DISAGU, SERSAN, DRENAJE, AUTOPROP, 
      CELULAR, INTERNET, NUMPERS) %>%
    mutate(AGEB = as.character(AGEB),
           AGEB = str_pad(AGEB,4,'left',0))
  reporte_faltantes(datos_viviendas = viviendas, archivo_reporte)
}

# variables de base personas
reporte_faltantes_personas <- function(datos_personas, archivo_reporte) {
  personas_recod <- datos_personas %>%
    select(ID_VIV, AGEB, SEXO, EDAD, PARENT, HLENGUA, NIVACAD, CONACT) %>%
    mutate_if(is.factor, as.character) %>% 
    mutate(AGEB = as.character(AGEB),
           AGEB = str_pad(AGEB,4,'left',0),
           id_viv = ID_VIV,
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
    summarise(AGEB = first(AGEB),
              jefe_sexo = first((sexo-1)*(PARENT == 1)),
              n_ocup = sum(ocupado),
              maxnved = max(niv_acad),
              indigena = 1 * any(HLENGUA == 1, na.rm = TRUE)) %>%
    ungroup()
  reporte_faltantes(datos_vivienda = personas_recod, archivo_reporte)
}
