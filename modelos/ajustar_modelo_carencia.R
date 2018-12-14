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

load("lab_microdatos/data/constantes_centrar.RData")
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


preparar_datos <- function(datos_enigh, covs_mun = TRUE, sample_size = 2000){
  datos_limpios <- datos_enigh %>%
    #filter(ingcor > 0) %>%
    select(hogar_id, ubica_geo, jefe_sexo, pisos, dis_agua, excus, drenaje,
      servicio_celular, servicio_internet, automovil, tam_hog, n_ocup, conapo,
      ic_seguridad_social, total_personas, tam_loc, ingcor, maxnved) %>%
      #, indigena) %>%
    na.omit() %>% 
    left_join(datos_mun, by = "ubica_geo") %>% 
    mutate(
      jefe_sexo = jefe_sexo,
      pisos = as.numeric(pisos != 1),
      dis_agua = as.numeric(dis_agua == 1),
      excus = as.numeric(excus == 1),
      drenaje = as.numeric(drenaje != 5),
      servicio_celular =  as.numeric(servicio_celular == 1),
      servicio_internet = as.numeric(servicio_internet == 1),
      automovil = as.numeric(automovil >= 1),
      tam_hog = tam_hog - media_tam_hog,
      n_ocup = n_ocup - media_n_ocup,
      max_ed = maxnved - media_maxnved,
      tam_loc = factor(tam_loc, levels = 1:4), 
      n_carencia = total_personas * ic_seguridad_social
    )
  in_sample_ids <- sample(datos_limpios$hogar_id, size = sample_size)
  datos_limpios_hogar <- filter(datos_limpios, hogar_id %in% in_sample_ids)
  x_hogar <- model.matrix(~ jefe_sexo + pisos + dis_agua + excus + 
      drenaje + servicio_celular + servicio_internet + automovil + tam_hog +
      n_ocup + max_ed + n_ocup * max_ed + factor(tam_loc), # + indigena, 
    data = datos_limpios_hogar)
  x_hogar <- x_hogar[, colnames(x_hogar) != "(Intercept)"]
  if (covs_mun) {
    x_mun <- model.matrix(~ factor(tam_mun, levels = c(1:3, 5)) * 
      factor(grado_marg, levels = 1:5), data = datos_mun)
    x_mun <- x_mun[, colnames(x_mun) != "(Intercept)"]
  } else {
    x_mun <- model.matrix(~ 1 + factor(grado_marg, levels = 1:5), 
      data = datos_mun)  
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
    municipio = datos_limpios_hogar$ubica_geo_int,
    n_personas = datos_limpios_hogar$total_personas,
    n_carencia = datos_limpios_hogar$n_carencia
  )
  return(list(datos_modelo = datos_modelo, 
    datos_hogar = datos_limpios, in_sample_ids = in_sample_ids))
}

# Ajuste de parametros completos: 26 minutos
datos <- preparar_datos(datos_enigh = enigh_2010, covs_mun = FALSE, 
  sample_size = 10000)

mh <- datos$datos_modelo$mh
mm <- datos$datos_modelo$mm
inits <- list(beta_0 = 0, beta = rnorm(n = mh), beta_mun_raw = rnorm(n = 2457),
              sigma_mun = 0.1, alpha = rnorm(n = mm), rho = 0.3)

fit <- sampling(mod_carencia, data = datos$datos_modelo, chains = 2, 
  iter = 800, warmup = 400, cores = 4)

save(fit, file = "fit_2.RData")


n_carencia <- datos$datos_modelo$n_carencia

shinystan::launch_shinystan(fit)

posterior <- extract(fit, permuted = TRUE)

library(bayesplot)
p <- mcmc_trace(posterior,  pars = c("alpha", "beta"), n_warmup = 0,
  facet_args = list(nrow = 2, labeller = label_parsed, scales = "free_y"))
p + facet_text(size = 15)

rhats_betas <- rhat(fit, pars = "beta")
mcmc_rhat(rhats_betas) + ggtitle("R-hat betas") + yaxis_text(hjust = 1)




