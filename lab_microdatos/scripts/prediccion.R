load("../data/simulaciones_parametros.RData")
datos_mun <- read_csv("../data/datos_mun.csv")

# sería bueno instalar fs para que los paths funcionen en cualquier OS
predice_mun <- function(mat, path_save = "../salidas/") {
  x_hogar <- mat$x_hogar
  mun <- mat$ids[1] %>% substr(1, 5)
  municipio <- datos_mun$ubica_geo_int[datos_mun$ubica_geo == mun]
  n_pred <- nrow(x_hogar)
  n_sims <- length(beta_0)
  beta_0_mat <- matrix(beta_0, nrow = n_pred, ncol = n_sims, 
    byrow = TRUE) 
  beta_mun_mat <- matrix(t(beta_mun)[municipio, ], nrow = n_pred, ncol = n_sims, 
    byrow = TRUE)
  medias <- beta_0_mat + x_hogar %*% t(beta) + beta_mun_mat
  sims <- apply(medias, 1, function(x) rnorm(n_sims, mean = x, sd = sigma))
  simulaciones_ageb <- data.frame(ageb = mat$ids, sims = t(sims)) %>% 
    gather(n_sim, sim_ingreso, -ageb) %>% 
    mutate(sim_ingreso = exp(sim_ingreso)) %>% 
    group_by(ageb, n_sim) %>% 
    summarise(n = n(), ingreso_medio = mean(sim_ingreso))
  write_csv(simulaciones_ageb, path = paste0(path_save, mun, ".csv"))
}

procesa_mun <- function(viviendas, personas, mun, path_salida = "../salidas/") {
  viviendas_mun <- filter(viviendas, MUN == mun)
  personas_mun <- filter(personas, MUN == mun)
  mat <- preparar_datos(datos_vivienda = viviendas_mun, 
    datos_persona = personas_mun)
  predice_mun(mat, path_save = path_salida)
}

procesa_edo <- function(path_viviendas, path_personas, 
  path_salida = "../salidas/") {
  edo_viviendas <- foreign::read.dbf(path_viviendas)
  edo_personas <- foreign::read.dbf(path_personas)
  muns <- unique(edo_viviendas$MUN)
  map(muns, ~procesa_mun(viviendas = edo_viviendas, personas = edo_personas, 
    mun = ., path_save = path_salida))
}
