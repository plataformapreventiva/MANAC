data {
  int n; //número de hogares
  int n_mun; // número de municipios
  int mh; //número de covariables nivel hogar
  int mm; //número de covariables nivel municipio

  matrix[n, mh] x_hogar;
  int municipio[n];
  matrix[n_mun, mm] x_municipio;
  int n_personas[n];
  int n_carencia[n];
}

transformed data {
 
}

parameters {
  real beta_0;
  vector[mh] beta;
  //real<lower=0> sigma;
  vector[n_mun] beta_mun_raw;
  real<lower=0> sigma_mun;
  vector[mm] alpha;
  real<lower=0,upper=1> rho;
}

transformed parameters {
  vector<lower=0,upper=1>[n] reg_prob;
  vector[n_mun] beta_mun;
  vector<lower=0>[n] a;
  vector<lower=0>[n] b;
  
  beta_mun = beta_mun_raw * sigma_mun + x_municipio * alpha;
  reg_prob = inv_logit(beta_0 + x_hogar * beta + beta_mun[municipio]) ;
  a = reg_prob * ((1-rho)/rho);
  b = a .* (1.0 - reg_prob)./reg_prob;
}

model {
  // nivel hogar, por ejemplo acceso a salud
  n_carencia ~ beta_binomial(n_personas, a, b);
  beta_0 ~ normal(0, 2);
  beta ~ normal(0, 2);
  //sigma ~ normal(0,1);
  beta_mun_raw ~ normal(0, 1);
  sigma_mun ~ normal(0, 1);
  alpha ~ normal(0, 2);
  rho ~ beta(1, 1);
  
}

generated quantities {
 //vector[n] num_reps ;

 //for(i in 1:n){
 //  num_reps[i] = beta_binomial_rng(n_personas[i], a[i], b[i]);
 //}
}
