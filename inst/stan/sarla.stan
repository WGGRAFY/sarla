data {
  int<lower=1> Nages;
  int<lower=1> Nyears;
  int<lower=1> Ncohorts;
  int<lower=1> N_cov;
  array[Nages, Ncohorts] int cohort_id;
  matrix[Nages, Nyears] laa_obs;
  array[2] real sigma_o_prior;
  array[2] real sigma_p_prior;
  int<lower=0, upper=1> est_cohort_effects;
  int<lower=0, upper=1> est_year_effects;
  int<lower=0, upper=1> est_init_effects;
  int<lower=0, upper=1> est_cov_effects;
  int<lower=0, upper=Ncohorts> N_eta_c;
  int<lower=0, upper=Ncohorts> N_gamma_y;
  int<lower=0, upper=Ncohorts> N_delta_c;
  int<lower=0> n_proc_error;
  array[N_cov] real cov_effect;
}

parameters {
  real<lower=-0.99, upper=0.99> beta;
  real<lower=-0.99, upper=0.99> beta_e; //coefficient for cohort_effect_cov
  array[1-est_init_effects] real X0;
  real<lower=0> sigma_p;
  real<lower=0> sigma_o;
  vector[N_eta_c] eta_c_raw;
  vector[N_gamma_y] gamma_y_raw;
  vector[N_delta_c] delta_c_raw;
  vector[N_cov] lambda_raw;

  vector[n_proc_error] pro_error_raw;

  array[est_init_effects] real<lower=0> eta_c_sd;
  array[est_cohort_effects] real<lower=0> delta_c_sd;
  array[est_year_effects] real<lower=0> gamma_y_sd;
  array[est_cov_effects] real<lower=0> lambda_sd;
}
transformed parameters {
  matrix[Nages, Ncohorts] xaa;
  vector[N_delta_c] delta_c;
  vector[N_gamma_y] gamma_y;
  vector[N_eta_c] eta_c;
  vector[Ncohorts] lambda_c;
  vector[Ncohorts] lambda_y;
  xaa = rep_matrix(0, Nages, Ncohorts); // initialize at 0
  matrix[Nages, Nyears] laa;
  matrix[Nages, Nyears] laa_mis;
  laa_mis = rep_matrix(0, Nages, Nyears);

  // non-centered parameters:

  if (est_init_effects) eta_c = eta_c_raw * eta_c_sd[1];
  if (est_year_effects){
      gamma_y = gamma_y_raw * gamma_y_sd[1];
    if (est_cov_effects){
      for(i in 2:Nyears){
        lambda_y[i] = beta_e * cov_effect[i-1] + lambda_raw[i]*lambda_sd[1];
      }
    }
  }
   if (est_cohort_effects){
     delta_c = delta_c_raw * delta_c_sd[1];
  if (est_cov_effects){
    for(i in 1:(Ncohorts-N_cov+1)){
      lambda_c[i] = 0;
    }
    for(i in (Ncohorts-N_cov+1):Ncohorts){
      lambda_c[i] = beta_e * cov_effect[i-(Ncohorts-N_cov)] +
      lambda_raw[i-(Ncohorts-N_cov)] * lambda_sd[1];
    }
}
  }

  for (y in 1:Ncohorts) {
    if (!est_init_effects) xaa[1, y] = X0[1];
    if (est_init_effects) xaa[1, y] = eta_c[y];
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
            if (est_cov_effects){
              xaa[i,y] = xaa[i,y] + lambda_c[cohort_id[i,y]];
            }
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
  // pro_error_raw ~ std_normal();
  to_vector(laa) ~ normal(to_vector(xaa[1:Nages, Nages:Ncohorts]), sigma_o);
  // sigma_p ~ normal(0, 1);
  sigma_p ~ lognormal(sigma_p_prior[1], sigma_p_prior[2]);
  sigma_o ~ lognormal(sigma_o_prior[1], sigma_o_prior[2]);
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
   if(est_cov_effects) {
     lambda_raw ~ std_normal();
     lambda_sd ~ normal(0,1);
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
