data {
  // datos observados
  int n; //número de hogares
  int n_mun; // número de municipios
  int mh; //número de covariables nivel hogar
  int mm; //número de covariables nivel municipio
  real ingreso[n];
  matrix[n, mh] x_hogar;
  int municipio[n];
  matrix[n_mun, mm] x_municipio;
  
  // datos todos los hogares
  int N; // número total de hogares
  matrix[N, mh] x_todos_hogar; // covariables todos los hogares
  int todos_municipio[N];
  vector[N] in_sample_hogar; // indicadora hogar en muestra
  
  // datos todos municipios
  int N_mun; // numero total de municipios
  matrix[N_mun, mm] x_todos_municipio;
  vector[N_mun] in_sample_mun;
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
  vector[N_mun] beta_out;
  vector[N] log_ingreso_out;
  for(i in 1:n){
    log_reps[i] = normal_rng(reg_pred[i], sigma);
  }
  // parámetros para todos los municipios
  for(j in 1:N_mun){
    if(in_sample_mun[j]==1){
      beta_out[j] = beta_mun[j];
    } else {
      beta_out[j] = normal_rng(dot_product(x_todos_municipio[j,], alpha), sigma_mun);
    }
  }
  for(k in 1:N){
    if(in_sample_hogar[k]==1){
      log_ingreso_out[k] = log_ingreso[k];
    } else {
      log_ingreso_out[k] = normal_rng(beta_0 + x_todos_hogar[k, ] * beta + beta_out[todos_municipio[k]], sigma);
    }
  }
}

