data {
  int<lower=1> Nages;
  int<lower=1> Nyears;
  int<lower=1> Ncohorts;
  int<lower=1> N_cov;
  array[Nages, Ncohorts] int cohort_id;
  matrix[Nages, Nyears] laa_obs; //input length-at-age data
  array[2] real sigma_o_prior; //observation error prior
  array[2] real sigma_p_prior; //process error prior
  int<lower=0, upper=1> est_cohort_effects; //boolean if cohort effects should be estimated
  int<lower=0, upper=1> est_year_effects; //boolean if year effects should be estimated
  int<lower=0, upper=1> est_init_effects; //boolean if initial size effects should be estimated
  int<lower=0, upper=1> est_cov_effects; //boolean if temperature effects should be estimated
  int<lower=0, upper=Ncohorts> N_eta_c; //Number of initial size effects
  int<lower=0, upper=Ncohorts> N_gamma_y; //Number of year effects
  int<lower=0, upper=Ncohorts> N_delta_c; //Number of cohort effects
  int<lower=0> n_proc_error;
  array[N_cov] real cov_effect; //data for enviromental covariates
}

parameters {
  real<lower=-0.99, upper=0.99> beta; //autocorrelation coefficient
  real beta_e; //coefficient for cohort_effect_cov
  array[1-est_init_effects] real X0; //starting size for initial age
  real<lower=0> sigma_p; //process error
  real<lower=0> sigma_o; //observation error
  vector[N_eta_c] eta_c_raw; //raw initial size effect
  vector[N_gamma_y] gamma_y_raw; //raw year effect
  vector[N_delta_c] delta_c_raw; // raw cohort effect
  matrix[Nages, Nyears] laa_mis; //imputed values for missing lengths-at-age
  vector[n_proc_error] pro_error_raw; //raw process error

  array[est_init_effects] real<lower=0> eta_c_sd;
  array[est_cohort_effects] real<lower=0> delta_c_sd;
  array[est_year_effects] real<lower=0> gamma_y_sd;
}
transformed parameters {
  matrix[Nages, Ncohorts] xaa;
  vector[N_delta_c] delta_c;
  vector[N_gamma_y] gamma_y;
  vector[N_eta_c] eta_c;
  vector[Ncohorts] lambda_c; //temperature effect for initial size
  vector[Ncohorts] lambda_y; //temperature effect for year
  xaa = rep_matrix(0, Nages, Ncohorts); // initialize at 0
  matrix[Nages, Nyears] laa;

  // non-centered parameters:
  if (est_init_effects) eta_c = eta_c_raw * eta_c_sd[1];
  if (est_year_effects){
      gamma_y = gamma_y_raw * gamma_y_sd[1];
    if (est_cov_effects){
      lambda_y[1] = 0;
      for(i in 2:N_cov){
        lambda_y[i] = beta_e * cov_effect[i-1];

      }
      for(i in (N_cov+1):Ncohorts){
        lambda_y[i] = 0;
      }
    }
  }
   if (est_cohort_effects){
     delta_c = delta_c_raw * delta_c_sd[1];
  }

  for (y in 1:Ncohorts) {
    if (!est_init_effects) xaa[1, y] = X0[1];

  if (est_init_effects){
      xaa[1, y] = eta_c[y];
    if(est_cov_effects){
    if(y<(Ncohorts-N_cov+1)){
      lambda_c[y] = 0;
    }
    if(y>=(Ncohorts-N_cov+1)){
      lambda_c[y] = beta_e * cov_effect[y-(Ncohorts-N_cov)];
      xaa[1, y] = xaa[1,y] + lambda_c[y];

    }
      }
    }
  }

    for(i in 1:Nages){
    for(y in 1:Nyears){
      if (laa_obs[i,y]==999.0){
        laa[i, y] = laa_mis[i, y];
      } else {
        laa[i, y] = laa_obs[i, y];
      }
    }
  }
  {
    int ii;
    ii = 0;
    for (y in 2:Ncohorts) {
      for (i in 2:Nages) {
        if (cohort_id[i-1, y-1] != 999) { // 999 = magic number for NA
          ii = ii + 1;
          // xaa[i,y] = beta * xaa[i-1, y-1] + pro_error_raw[ii] * sigma_p;
          xaa[i,y] = beta * xaa[i-1, y-1] + pro_error_raw[ii] * sigma_p;
          if (est_cohort_effects){
            xaa[i,y] = xaa[i,y] + delta_c[cohort_id[i,y]];
          }
          if (est_year_effects) {
            xaa[i,y] = xaa[i,y] + gamma_y[y];
            if (est_cov_effects){
              xaa[i,y] = xaa[i,y] + lambda_y[y];
            }
          }
        }
      }
    }
  }
}
model {
  pro_error_raw ~ normal(0, sigma_p);

  sigma_p ~ lognormal(sigma_p_prior[1], sigma_p_prior[2]);
  sigma_o ~ lognormal(sigma_o_prior[1], sigma_o_prior[2]);
  // pro_error_raw ~ std_normal();
  to_vector(laa_mis) ~ normal(0, sigma_o);
  to_vector(laa) ~ normal(to_vector(xaa[1:Nages, Nages:Ncohorts]), sigma_o);
  // sigma_p ~ normal(0, 1);
  beta ~ std_normal();
  beta_e ~ std_normal();
  if (est_year_effects) {
    gamma_y_raw ~ std_normal();
    gamma_y_sd ~ normal(0, 1);
  }
  if (est_cohort_effects) {
    delta_c_raw ~ std_normal();
    delta_c_sd ~ normal(0, 1);
  }
  if (est_init_effects) {
    eta_c_raw ~ std_normal();
    eta_c_sd ~ normal(0, 1);
  }
   if (!est_init_effects) {
     X0[1] ~ normal(0, sigma_p);
   }
}


generated quantities {
  matrix[Nages, Nyears] laa_postpred;
  matrix[Nages, Nyears] log_lik;
  for (i in 1:Nages) {
    for (y in 1:Nyears) {
      laa_postpred[i, y] = normal_rng(xaa[i, y + (Nages - 1)], sigma_o);
      log_lik[i,y] = normal_lpdf(laa[i,y] | xaa[i,y], sigma_o);
    }
  }

}
