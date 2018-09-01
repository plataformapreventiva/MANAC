# !diagnostics off

library(tidyverse)
library(rstan)
library(emdbook)

enigh_2010 <- read_csv("datos/base_final.csv")

# Modelo de carencias 
mod_carencia <- stan_model(file = "modelos/src/carencia_binomial.stan")

set.seed(123456)
enigh_muestra <- sample_n(enigh_2010, 20000)

# Crear y probar código para estimar resultados en una muestra.
# 1. Separar los datos ENIGH en conjuntos de entrenamiento y 
# prueba.
# 2. Ajustar el modelo con covariables actuales.
# 3. Extraer información (simulaciones de los parametros) 
# necesaria para estimar en datos nuevos.
# 3.1 Verificar resultados estimando dentro de Stan y extrayendo 
# coeficientes y estimando fuera de Stan.

enigh_train <- sample_frac(enigh_muestra, size = 0.52)
enigh_test <- setdiff(enigh_muestra, enigh_train)

limpia_datos <- function(enigh_crudo){
  aux <- enigh_crudo %>%
    select(hogar_id, ubica_geo=ubica_geo.y, jefe_sexo, pisos, dis_agua, excus, 
           drenaje, servicio_celular, servicio_internet, automovil, 
           tam_hog, n_ocup, conapo, tam_loc=tam_loc.y, ingcor=ingcor.y, maxnved) %>%
    ## quitar faltantes ########
    drop_na() %>%
    mutate(
      #geo_id = as.numeric(factor(ubica_geo)),
      jefe_sexo = jefe_sexo,
      pisos = as.numeric(pisos != 1),
      dis_agua = as.numeric(dis_agua == 1),
      excus = as.numeric(excus == 1),
      drenaje = as.numeric(drenaje!=5),
      servicio_celular =  as.numeric(servicio_celular == 1),
      servicio_internet = as.numeric(servicio_internet == 1),
      automovil = as.numeric(automovil == 1),
      tam_hog = log(1+tam_hog),
      n_ocup = log(1+n_ocup),
      max_ed = log(maxnved)) %>%
    mutate(max_ed = ifelse(is.na(max_ed), 1, max_ed)) %>%
    mutate(max_ed_faltante = as.numeric(is.na(max_ed)),
           max_ed = ifelse(max_ed <= 0, 0, max_ed))
  
  aux %>% # Codificacion de niveles en covariables de municipio
    mutate(conapo = as.integer(conapo),
           tam_loc = as.integer(tam_loc),
           # codificacion misma y no nuevos niveles
           conapo = factor(x = conapo, levels = 1:5, labels = 1:5),
           tam_loc = factor(x = tam_loc, levels = 1:4, labels = 1:4))
}

get_num_personas_carencias <- function(enigh_limpio){
  aux <- enigh_train %>% select(hogar_id, ic_asalud, total_personas)
  enigh_limpio %>% left_join(aux, by = "hogar_id") %>%
    mutate(n_personas = round(total_personas * ic_asalud), 0) %>%
    select(hogar_id, total_personas, n_personas)
}

datos_limpios_train_x <- limpia_datos(enigh_train)
datos_limpios_test_x <- limpia_datos(enigh_test)

# Codificacion de claves de municipios
enigh_mun_train <- datos_limpios_train_x %>%
  mutate(geo_id = as.numeric(factor(ubica_geo))) %>%
  select(ubica_geo, geo_id) %>%
  distinct() %>%
  left_join(datos_limpios_train_x, by = "ubica_geo") %>%
  select(ubica_geo, geo_id, conapo, tam_loc) %>%
  group_by(ubica_geo, geo_id) %>%
  summarise(conapo = first(conapo),
            tam_loc = first(tam_loc))

enigh_mun_test <- datos_limpios_test_x %>%
  filter(!(ubica_geo %in% enigh_mun_train$ubica_geo)) %>%
  mutate(geo_id = as.numeric((factor(ubica_geo))) + nrow(enigh_mun_train)) %>%
  select(ubica_geo, geo_id) %>%
  distinct() %>%
  left_join(datos_limpios_test_x, by = "ubica_geo") %>%
  select(ubica_geo, geo_id, conapo, tam_loc) %>%
  group_by(ubica_geo, geo_id) %>%
  summarise(conapo = first(conapo),
            tam_loc = first(tam_loc))

municipios_train <- datos_limpios_train_x %>%
  left_join(enigh_mun_train, by = "ubica_geo") %>%
  select(ubica_geo, geo_id)

municipios_test <- datos_limpios_test_x %>%
  left_join(enigh_mun_test, by = "ubica_geo") %>%
  select(ubica_geo, geo_id)

# Obtener variables de carencia y numero de personas en el hogar
num_carencias_train_x <- get_num_personas_carencias(datos_limpios_train_x)
num_carencias_test_x <- get_num_personas_carencias(datos_limpios_test_x)

# Matrices para el modelo
f <- ~ - 1 + jefe_sexo + pisos + dis_agua + excus + drenaje + 
  servicio_celular + servicio_internet + automovil + 
  tam_hog + n_ocup + max_ed + max_ed_faltante + n_ocup*max_ed
x_train_hogar <- model.matrix(f, data = datos_limpios_train_x) # 10,175 hogares
x_test_hogar <- model.matrix(f, data = datos_limpios_test_x) # 9,380 hogares


# Matrices de covariables de municipio
x_mun_train <- model.matrix(~ -1 + tam_loc * conapo, data = enigh_mun_train)
x_mun_test <- model.matrix(~ -1 + tam_loc * conapo, data = enigh_mun_test)


datos_train <- list(n = nrow(x_train_hogar),
              n_mun = nrow(x_mun_train),
              mh = ncol(x_train_hogar),
              mm = ncol(x_mun_train),
              x_hogar = x_train_hogar,
              x_municipio = x_mun_train,
              municipio = municipios_train$geo_id,
              n_personas = num_carencias_train_x$total_personas,
              n_carencia = num_carencias_train_x$n_personas)

# Ajuste de parametros: 35 minutos
fit <- sampling(mod_carencia, data = datos_train, chains = 3, 
                cores = 12, iter = 800, warmup = 400, control=list(max_treedepth=12))

# shinystan
shinystan::launch_shinystan(fit)

# Resumen 
rstan::summary(fit, pars = c("beta", "alpha", "beta_0", "sigma_mun"))$summary

# Extraer datos de parámetros para ajuste del modelo
sims_muestra <- fit@sim$samples

append_sims_params <- function(k){
  mat <- sims_muestra[[k]] %>% purrr::transpose()
  toma_parametros <- function(i){
    x <- mat[[i]]
    nombres <- names(x)
    tibble(parametro = nombres,
           cadena = i,
           valor = map_dbl(.x = x, .f = c))
  }
  map_df(.x = 1:length(mat), .f = toma_parametros)
}

# número de cadenas en las simulaciones
nchains <- length(sims_muestra)

# data frame con estimaciones
params_sims_df <- map_df(.x = 1:nchains, .f = append_sims_params)
fit_ss <- extract(fit, permuted = TRUE) 

# convertir a matrices para hacer las estimaciones
sims_alpha <- as.matrix(fit, pars = c("alpha")); dim(sims_alpha)
sims_sigma_mun <- as.matrix(fit, pars = c("sigma_mun")); dim(sims_sigma_mun)
sims_beta_mun_raw <- as.matrix(fit, pars = c("beta_mun_raw")); dim(sims_beta_mun_raw)
sims_beta <- as.matrix(fit, pars = c("beta")); dim(sims_beta)
sims_beta0 <- as.matrix(fit, pars = c("beta_0")); dim(sims_beta0)
sims_rho <- as.matrix(fit, pars = c("rho")); dim(sims_rho)

# Predecir para entrenamiento

# Individuo 1
ind_hog_1 <- x_train_hogar[1,]; length(ind_hog_1)
ind_mun_1 <- x_vmun_train[1,]; length(ind_mun_1)
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

beta_binomial_sim <- function(i, N){
  p <- prob[i]
  th <- theta[i]
  rbetabinom(n = 1, prob = p, theta = th, size = N)
}

y_sim <- map_int(.x = length(prob), .f = beta_binomial_sim, N = sims_beta_mun_raw)




aux <- (t(sims_beta_mun_raw) %*% sims_sigma_mun) 
aux2 <- (x_mun_train %*% t(sims_alpha))








eta <- beta0+beta1*x  ## predictor lineal
prob <- plogis(eta)   ## logística inversa
y <- rbetabinom(k, prob=prob, size=n, theta=6) # simulaciones de beta binomial







