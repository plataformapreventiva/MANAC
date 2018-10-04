# escribe tablas donde para cada var_agrupar (AGEB o municipio) se reporta el 
# número de faltantes por variable y el número de hogares con al menos un 
# faltante.
reporte_faltantes <- function(datos_viviendas, var_agrupar,
  archivo_reporte) {
  faltantes_var <- datos_viviendas %>% 
    group_by(!!enquo(var_agrupar)) %>% 
    summarise_all(funs(falta = sum(is.na(.)))) 
  datos_viviendas$viviendas_na <- apply(datos_viviendas, 1, function(x) 
    sum(is.na(x)) > 0)
  faltantes_viviendas <- datos_viviendas %>% 
    group_by(!!enquo(var_agrupar)) %>% 
    summarise(n_viviendas_na = sum(viviendas_na), n_viviendas = n())
  tab_faltantes <- faltantes_var %>% 
    left_join(faltantes_viviendas)
  write_csv(tab_faltantes, path = archivo_reporte)
}

# variables de base viviendas
reporte_faltantes_viviendas <- function(datos_viviendas, var_agrupar, 
  archivo_reporte = "../salidas/faltantes.csv") {
  datos_viviendas <- foreign::read.dbf(path_viviendas)
  viviendas <- datos_viviendas %>%
    select(ID_VIV, ENT, MUN, AGEB, PISOS, DISAGU, SERSAN, DRENAJE, AUTOPROP, 
      CELULAR, INTERNET, NUMPERS, TAMLOC4)
  reporte_faltantes(viviendas, !!enquo(var_agrupar), archivo_reporte)
}

# variables de base personas
reporte_faltantes_personas <- function(path_personas, var_agrupar,
  archivo_reporte = "../salidas/faltantes.csv") {
  datos_personas <- foreign::read.dbf(path_personas)
  personas_recod <- datos_personas %>%
    select(MUN, AGEB, ID_VIV, SEXO, EDAD, PARENT, HLENGUA, NIVACAD, CONACT) %>%
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
    summarise(
      AGEB = first(AGEB), 
      MUN = first(MUN),
      jefe_sexo = first((sexo-1)*(PARENT == '01')),
      n_ocup = sum(ocupado),
      maxnved = max(niv_acad),
      indigena = 1 * any(HLENGUA == 1)) %>%
    select(AGEB, MUN, id_viv, jefe_sexo, n_ocup, maxnved, indigena) %>%
    ungroup()
  reporte_faltantes(datos_vivienda = personas_recod, !!enquo(var_agrupar),
    archivo_reporte)
}
