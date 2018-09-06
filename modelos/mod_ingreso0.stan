data {
  int n; //n�mero de hogares
  int n_mun; // n�mero de municipios
  int mh; //n�mero de covariables nivel hogar
  int mm; //n�mero de covariables nivel municipio
  real ingreso[n];
  matrix[n, mh] x_hogar;
  matrix[n_mun, mm] x_municipio;
  int municipio[n];
  int n_f; //n�mero total de hogares
  matrix[n_f, mh] x_hogar_f;
  int municipio_f[n_f];
  vector[n_f] in_sample;
}

transformed data {
  vector[n] log_ingreso;
  log_ingreso = log(1. + to_vector(ingreso));
}

parameters {
  real beta_0;
  vector[mh] beta;
  real<lower=0> sigma;
  vector[n_mun] beta_mun_raw;
  real<lower=0> sigma_mun;
  vector[mm] alpha;
}

transformed parameters {
  vector[n] reg_pred;
  vector[n_mun] beta_mun;
  beta_mun = beta_mun_raw * sigma_mun + x_municipio * alpha;
  reg_pred = beta_0 + x_hogar * beta + beta_mun[municipio];
}

model {
  log_ingreso ~ normal(reg_pred, sigma);
  beta_0 ~ normal(10, 2);
  beta ~ normal(0, 1);
  sigma ~ normal(0,1);
  beta_mun_raw ~ normal(0, 1);
  sigma_mun ~ normal(0, 1);
  alpha ~ normal(0, 1);
}

generated quantities {
  vector[n] log_reps;
  for(i in 1:n){
    log_reps[i] = normal_rng(reg_pred[i], sigma);
  }
}
