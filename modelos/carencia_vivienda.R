# Modelo de carencias 

library(tidyverse)
library(rstan)
library(emdbook)

enigh_2010 <- read_csv("datos/base_final.csv")

mod_carencia <- stan_model(file = "modelos/src/carencia_binomial.stan")

datos_mun <- read_csv("datos/tabla_municipios.csv") %>% 
  rename(ubica_geo = cve_muni) %>% 
  mutate(grado_marg = case_when(grado_marginacion == "Muy alto" ~ 1,
                                grado_marginacion == "Alto" ~ 2, 
                                grado_marginacion == "Medio" ~ 3, 
                                grado_marginacion == "Bajo" ~ 4, 
                                grado_marginacion == "Muy bajo" ~ 5), 
         tam_mun = floor(tamloc_mediana), 
         ubica_geo_int = as.numeric(as.factor(ubica_geo)))

# cargar medias para centrar
load("lab_microdatos/data/constantes_centrar.RData")

# cargar funciones para preparar los datos
source("modelos/funciones.R")

# Crear y probar código para estimar resultados en una muestra.
# 1. Separar los datos ENIGH en conjuntos de entrenamiento y 
# prueba.
# 2. Ajustar el modelo con covariables actuales.
# 3. Extraer información (simulaciones de los parametros) 
# necesaria para estimar en datos nuevos.
# 3.1 Verificar resultados estimando dentro de Stan y extrayendo 
# coeficientes y estimando fuera de Stan.

# seleccionar muestra (municipios y hogares) en dos:
#  1. Seleccionamos 500 municipios.
#  2. Seleccionamos 10,000 hogares dentro de los municipios del paso 1
#     el tamaño de muestra varía a lo largo de los municipios.
set.seed(182791)
in_sample_mun_ids <- sample(unique(enigh_2010$ubica_geo.y), 500)

enigh_2010 <- enigh_2010 %>% 
  mutate(ubica_geo = ubica_geo.y,
         tam_loc = tam_loc.y,
         ingcor = ingcor.y)

in_sample_ids <- enigh_2010 %>%
  filter(ubica_geo %in% in_sample_mun_ids) %>% 
  sample_n(10000) %>% 
  pull(hogar_id)

enigh_train <- enigh_2010 %>%
  filter(hogar_id %in% in_sample_ids)

datos_mun <- datos_mun %>%
  mutate(in_sample_mun = ubica_geo %in% in_sample_mun_ids)

datos <- preparar_datos(datos_enigh = enigh_train, in_sample_ids, covs_mun = FALSE)

num_carencias <- get_num_personas_carencias(enigh_train = enigh_train, 
                                            datos_limpios_hogar = datos$datos_hogar)


datos_train <- list(n = ,
              n_mun = ,
              mh = ,
              mm = ,
              x_hogar = ,
              x_municipio = ,
              municipio = datos$datos_modelo$municipio,
              n_personas = num_carencias_train_x$total_personas,
              n_carencia = num_carencias_train_x$n_personas)

# Ajuste de parametros: 35 minutos
fit <- sampling(mod_carencia, data = datos_train, chains = 3, 
                cores = 12, iter = 800, warmup = 400, control=list(max_treedepth=12))

# Operaciones matriciales para hacer la predicción
# convertir a matrices para hacer las estimaciones
sims_alpha <- as.matrix(fit, pars = c("alpha")); dim(sims_alpha)
sims_sigma_mun <- as.matrix(fit, pars = c("sigma_mun")); dim(sims_sigma_mun)
sims_beta_mun_raw <- as.matrix(fit, pars = c("beta_mun_raw")); dim(sims_beta_mun_raw)
sims_beta <- as.matrix(fit, pars = c("beta")); dim(sims_beta)
sims_beta0 <- as.matrix(fit, pars = c("beta_0")); dim(sims_beta0)
sims_rho <- as.matrix(fit, pars = c("rho")); dim(sims_rho)

# Predecir para entrenamiento

# Individuo 1 stan
ind_1_stan <- as.numeric(as.matrix(fit, pars = "reg_prob[1]"))

# Individuo 1
ind_hog_1 <- x_train_hogar[1,]; length(ind_hog_1)
ind_mun_1 <- x_mun_train[1,]; length(ind_mun_1)
ind_mun_ind_1 <- municipios_train$geo_id[1]
ind_n_pers_1 <- num_carencias_train_x$total_personas[1]
# beta_mun = beta_mun_raw * sigma_mun + x_municipio * alpha
beta_mun <- sims_beta_mun_raw[,ind_mun_ind_1] * sims_sigma_mun + sims_alpha %*% ind_mun_1
# reg_prob = inv_logit(beta_0 + x_hogar * beta + beta_mun[municipio]) ;
eta <- sims_beta0 + sims_beta %*% ind_hog_1 + beta_mun
reg_prob <- plogis(eta)
# a = reg_prob * ((1-rho)/rho);
a <- reg_prob * ((1 - sims_rho)/sims_rho)
# b = a .* (1.0 - reg_prob)./reg_prob;
b <- a * ((1 - reg_prob)/reg_prob)
prob = a / (a + b)
theta = a + b

beta_binomial_sim <- function(i){
  p <- prob[i]
  th <- theta[i]
  as.integer(rbetabinom(n = 1, prob = p, theta = th, size = ind_n_pers_1))
}

y_sim <- map_int(.x = 1:length(prob), .f = beta_binomial_sim)
table(y_sim)




