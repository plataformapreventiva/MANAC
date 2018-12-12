library(tidyverse)
library(rstan)
library(doSNOW)
library(foreach)
library(parallel)

# leer datos ENIGH
enigh_2010 <- read_csv("datos/enigh_raw.csv")

set.seed(182791)
in_sample_mun_ids <- sample(unique(enigh_2010$ubica_geo), 800, replace = F)

in_sample_ids <- enigh_2010 %>%
  filter(ubica_geo %in% in_sample_mun_ids) %>% 
  sample_n(10000) %>% 
  pull(hogar_id)

enigh_train <- enigh_2010 %>%
  filter(hogar_id %in% in_sample_ids)

mod_carencia <- stan_model(file = "modelos/src/carencia_binomial.stan")

# leer datos municipio y crear variables para modelo
datos_mun <- read_csv("datos/tabla_municipios.csv") %>% 
  rename(ubica_geo = cve_muni) %>% 
  mutate(grado_marg = case_when(grado_marginacion == "Muy alto" ~ 1,
                                grado_marginacion == "Alto" ~ 2, 
                                grado_marginacion == "Medio" ~ 3, 
                                grado_marginacion == "Bajo" ~ 4, 
                                grado_marginacion == "Muy bajo" ~ 5), 
         tam_mun = floor(tamloc_mediana), 
         ubica_geo_int = as.numeric(as.factor(ubica_geo)))

# Cargar constantes para centrar
load("lab_microdatos/data/simulaciones_parametros.RData")

preparar_datos <- function(datos_enigh, covs_mun = TRUE){
  datos_limpios_hogar <- datos_enigh %>%
    #filter(ingcor > 0) %>%
    select(hogar_id, ubica_geo, jefe_sexo, pisos, dis_agua, excus, drenaje,
           servicio_celular, servicio_internet, automovil, tam_hog, n_ocup, conapo,
           ic_seguridad_social, total_personas, tam_loc, ingcor, maxnved) %>%
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
      tam_loc = factor(tam_loc, levels = 1:4)
    ) %>% 
    left_join(datos_mun, by = "ubica_geo")
  f <- ~ jefe_sexo + pisos + dis_agua + excus + 
    drenaje + servicio_celular + servicio_internet + automovil + tam_hog +
    n_ocup + max_ed + n_ocup * max_ed + tam_loc
  vars <- str_split(as.character(f)[2], '\\+')
  vars <- map_chr(vars[[1]], function(x){ifelse(str_detect(x, '\\*'), '', str_squish(x))})
  datos_hogar_completos <- datos_limpios_hogar %>% 
    dplyr::select(one_of(c("hogar_id","ubica_geo_int","id",
                           "ic_seguridad_social", "total_personas",vars))) %>%
    na.omit
  x_hogar <- model.matrix(f, data = datos_hogar_completos)
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
  datos_carencia <- datos_hogar_completos %>% 
    select(hogar_id, total_personas, ic_seguridad_social) %>%
    mutate(n_personas = round(total_personas * ic_seguridad_social), 0) %>%
    select(hogar_id, total_personas, n_personas)
  datos_modelo <- list(
    n = n,
    n_mun = n_mun,
    mh = ncol(x_hogar),
    mm = ncol(x_mun),
    x_hogar = x_hogar,
    x_municipio = x_mun,
    municipio = datos_hogar_completos$ubica_geo_int,
    n_personas = datos_carencia$total_personas,
    n_carencia = datos_carencia$n_personas
  )
  return(list(datos_modelo = datos_modelo, 
              datos_hogar = datos_limpios_hogar))
}

# Ajuste de parametros completos: 26 minutos
datos <- preparar_datos(datos_enigh = enigh_2010, covs_mun = FALSE)

mh <- datos$datos_modelo$mh
mm <- datos$datos_modelo$mm
inits <- list(beta_0 = 0, beta = rnorm(n = mh), beta_mun_raw = rnorm(n = 2457),
              sigma_mun = 0.1, alpha = rnorm(n = mm), rho = 0.3)

fit <- sampling(mod_carencia, data = datos$datos_modelo, chains = 1, 
                cores = 1, iter = 800, warmup = 400, control=list(max_treedepth=12))#,
                #init_r = 3,
                #init = inits)

save(fit, file = "fit_c_seg_soc.RData")

extract_fit <- extract(fit, permuted = TRUE)
beta_mun <- extract_fit$beta_mun
beta_0 <- extract_fit$beta_0
beta <- extract_fit$beta
sigma <- extract_fit$sigma
rho <- extract_fit$rho

save(beta_mun, beta_0, beta, sigma, rho,
     file = "../lab_microdatos/data/simulaciones_parametros.RData")







