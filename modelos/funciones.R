## Funciones de limpieza u otras funciones generales para 
## los modelos de ingreso y carencias para el MANAC

# Recibe: 
# datos_enigh: los datos enigh y un vector con los hogares seleccionados en la muestra 
# de entrenamiento identificados por la varibale hogares_id
# Regresa en forma de lista:
# datos_modelo: lista para usar directamente en Stan, define datos del modelo.
# ind_mun: índice de clave de municipio e índice correspondiente en modelo.
# covs_mun: incluir covariables en efecto de municipio
preparar_datos <- function(datos_enigh, in_sample_ids, covs_mun = TRUE){
  datos_limpios_hogar <- datos_enigh %>%
    mutate(
      jefe_sexo = jefe_sexo,
      pisos = as.numeric(pisos != 1),
      dis_agua = as.numeric(dis_agua == 1),
      excus = as.numeric(excus == 1),
      drenaje = as.numeric(drenaje!=5),
      servicio_celular =  as.numeric(servicio_celular == 1),
      servicio_internet = as.numeric(servicio_internet == 1),
      automovil = as.numeric(automovil == 1),
      tam_hog = tam_hog - mean(tam_hog),
      n_ocup = n_ocup - mean(n_ocup),
      max_ed = maxnved - mean(maxnved),
      tam_loc = factor(tam_loc, levels = 1:4),
      # max_ed = ifelse(is.na(max_ed), 1, max_ed), 
      in_sample = hogar_id %in% in_sample_ids
    ) %>% 
    arrange(desc(in_sample))
  x_hogar <- model.matrix(~ jefe_sexo + pisos + dis_agua + excus + 
                            drenaje + servicio_celular + servicio_internet + automovil + tam_hog +
                            n_ocup + max_ed + n_ocup * max_ed + tam_loc,# + indigena, 
                          data = datos_limpios_hogar)
  x_hogar <- x_hogar[, colnames(x_hogar) != "(Intercept)"]
  if (covs_mun) {
    x_mun <- model.matrix(~ factor(tam_mun) * factor(grado_marg), 
                          data = datos_mun)
    x_mun <- x_mun[, colnames(x_mun) != "(Intercept)"]
  } else {
    x_mun <- model.matrix(~ 1 + factor(grado_marg), data = datos_mun)  
    x_mun <- x_mun[, colnames(x_mun) != "(Intercept)"]
  }
  ind_mun <- select(datos_mun, ubica_geo, in_sample_mun, ubica_geo_int)
  datos_limpios_hogar <- datos_limpios_hogar %>% 
    left_join(ind_mun, by = "ubica_geo")
  n <- nrow(x_hogar)
  n_mun <- nrow(x_mun)
  datos_modelo <- list(
    n = n,
    n_mun = n_mun,
    mh = ncol(x_hogar),
    mm = ncol(x_mun),
    ingreso = datos_limpios_hogar$ingcor,
    x_hogar = x_hogar,
    x_municipio = x_mun,
    municipio = datos_limpios_hogar$ubica_geo_int 
  )
  return(list(datos_modelo = datos_modelo, ind_mun = ind_mun, 
              datos_hogar = datos_limpios_hogar, 
              log_ingreso = log(datos_limpios_hogar$ingcor + 1)))
}

get_num_personas_carencias <- function(enigh_train, datos_limpios_hogar){
  datos_limpios_hogar %>% 
    left_join(enigh_train %>% select(hogar_id, ic_alimentacion),
              by = "hogar_id") %>%
    mutate(n_personas = round(total_personas * ic_seguridad_social), 0) %>%
    select(hogar_id, total_personas, n_personas)
}

