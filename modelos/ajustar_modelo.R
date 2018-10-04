library(tidyverse)
library(rstan)

# leer datos ENIGH
enigh_2010 <- read_csv("../datos/enigh_final.csv")

# leer datos municipio y crear variables para modelo
datos_mun <- read_csv("../datos/tabla_municipios.csv") %>% 
  rename(ubica_geo = cve_muni) %>% 
  mutate(grado_marg = case_when(grado_marginacion == "Muy alto" ~ 1,
    grado_marginacion == "Alto" ~ 2, 
    grado_marginacion == "Medio" ~ 3, 
    grado_marginacion == "Bajo" ~ 4, 
    grado_marginacion == "Muy bajo" ~ 5), 
    tam_mun = floor(tamloc_mediana), 
    ubica_geo_int = as.numeric(as.factor(ubica_geo)))


# constantes para centrar
media_tam_hog <- mean(enigh_2010$tam_hog, na.rm = TRUE)
media_n_ocup <- mean(enigh_2010$n_ocup, na.rm = TRUE)
media_maxnved <- mean(enigh_2010$maxnved, na.rm = TRUE)

save(media_tam_hog, media_n_ocup, media_maxnved, 
  file = "../lab_microdatos/data/constantes_centrar.RData")

mod_ingreso <- stan_model(file = "src/ingreso.stan")

preparar_datos <- function(datos_enigh, covs_mun = TRUE){
  enigh_vars <- datos_enigh %>% 
    select(hogar_id, ubica_geo, jefe_sexo, pisos, dis_agua, excus, drenaje,
      servicio_celular, servicio_internet, automovil, tam_hog, n_ocup, conapo,
      tam_loc, ingcor, maxnved, indigena) %>% 
    na.omit() %>% 
    filter(ingcor > 0) %>% 
    left_join(datos_mun)
  datos_limpios_hogar <- enigh_vars %>%
    mutate(
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
  if (covs_mun) {
    x_mun <- model.matrix(~ factor(tam_mun) * factor(grado_marg), 
      data = datos_mun)
    x_mun <- x_mun[, colnames(x_mun) != "(Intercept)"]
  } else {
    x_mun <- model.matrix(~ 1 + factor(grado_marg), data = datos_mun)  
    x_mun <- x_mun[, colnames(x_mun) != "(Intercept)"]
  }
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
  return(list(datos_modelo = datos_modelo, 
    datos_hogar = datos_limpios_hogar, 
    log_ingreso = log(datos_limpios_hogar$ingcor + 1)))
}

datos <- preparar_datos(datos_enigh = sample_n(enigh_2010, 200), covs_mun = FALSE)

fit <- sampling(mod_ingreso, data = datos$datos_modelo, chains = 2,
  cores = 2, iter = 700, warmup = 400, control=list(max_treedepth=13))

extract_fit <- extract(fit, permuted = TRUE)
beta_mun <- extract_fit$beta_mun
beta_0 <- extract_fit$beta_0 
beta <- extract_fit$beta
sigma <- extract_fit$sigma

save(beta_mun, beta_0, beta, sigma, 
  file = "../lab_microdatos/data/simulaciones_parametros.RData")

