# Modelo de carencias 

library(tidyverse)
library(rstan)
library(emdbook)

enigh_2010 <- read_csv("datos/enigh_final.csv")

mod_carencia <- stan_model(file = "modelos/src/carencia_binomial.stan")

datos_mun <- read_csv("lab_microdatos/data/datos_mun.csv") %>% 
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


# enigh_2010 <- enigh_2010 %>% 
#   mutate(ubica_geo = ubica_geo.y,
#          tam_loc = tam_loc.y,
#          ingcor = ingcor.y)
set.seed(182791)
in_sample_mun_ids <- sample(unique(enigh_2010$ubica_geo), 500)

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
                                            datos_hogar = datos$datos_hogar)


datos_train <- list(n = datos$datos_modelo$n,
                    n_mun = datos$datos_modelo$n_mun,
                    mh = datos$datos_modelo$mh,
                    mm = datos$datos_modelo$mm,
                    x_hogar = datos$datos_modelo$x_hogar,
                    x_municipio = datos$datos_modelo$x_municipio,
                    municipio = datos$datos_modelo$municipio,
                    n_personas = num_carencias$total_personas,
                    n_carencia = num_carencias$n_personas)

# Ajuste de parametros: 25 minutos
fit <- sampling(mod_carencia, data = datos_train, chains = 3, 
                cores = 12, iter = 800, warmup = 400, control=list(max_treedepth=12))
save(fit, file = "fit_c_seg_soc.RData")

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

# Predicciones matriciales
extract_fit <- extract(fit, permuted = TRUE)
beta_mun <- extract_fit$beta_mun
beta_0 <- extract_fit$beta_0 
beta <- extract_fit$beta
sigma <- extract_fit$sigma
rho <- extract_fit$rho

save(beta_mun, beta_0, beta, sigma, 
     file = "../lab_microdatos/data/simulaciones_parametros.RData")

# reg_prob = inv_logit(beta_0 + x_hogar * beta + beta_mun[municipio]) ;
n_pred <- nrow(x_hogar)
n_sims <- length(beta_0)
beta_0_mat <- matrix(beta_0, nrow = n_pred, ncol = n_sims, 
                     byrow = TRUE)
beta_mun_mat <- matrix(t(beta_mun)[municipio, ], nrow = n_pred, ncol = n_sims, 
                       byrow = TRUE)
eta <- beta_0_mat + x_hogar %*% t(beta) + beta_mun_mat
invlogit <- function(x){
  exp(x)/(1+exp(x))
}
reg_prob <- invlogit(eta)
# a = reg_prob * ((1-rho)/rho);
rho_mat <- matrix(rho, nrow = n_pred, ncol = n_sims, byrow = T)
a <- reg_prob / ((1 - rho_mat)/rho_mat)
# b = a .* (1.0 - reg_prob)./reg_prob;
b <- a * ((1 - reg_prob)/reg_prob)
mu = a / (a + b)
M = a + b

rbbinom <- function(n, size, mu, M){
  alpha <- mu * M
  beta <- M * (1 - mu)
  theta <- rbeta(n, alpha, beta)
  k <- rbinom(n=n, size=size, theta)
  k
}

# sims <- map(.x = 1:n_pred, .f = function(x){
#   rbbinom(n = n_sims, size = num_carencias$total_personas[x], mu = mu[x,], M = M[x,])
# })
# 
# sims <- Reduce(f = c, x = sims)
# sims <- matrix(sims, nrow = n_pred, ncol = n_sims)

# Paralelizacion
library(doSNOW)
library(foreach)
library(parallel)
numCores <- detectCores()
cl<-makeCluster(8) 
registerDoSNOW(cl)

sims <- matrix(nrow = n_pred, ncol = n_sims)
foreach(i=1:n_pred, .combine = "rbind") %dopar% {
  rbbinom(n = n_sims, size = num_carencias$total_personas[x], mu = mu[x,], M = M[x,])
}

stopCluster(cl)


colnames(sims_agebs) <- paste0('sim_',1:n_sims)

