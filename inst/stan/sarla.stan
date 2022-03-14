data {
  int<lower=1> Nages;
  int<lower=1> Nyears;
  int<lower=1> Ncohorts;
  int cohort_id[Nages, Ncohorts];
  matrix[Nages, Nyears] laa_obs;
  real sigma_o_prior[2];
  int<lower=0, upper=1> est_cohort_effects;
  int<lower=0, upper=1> est_year_effects;
  int<lower=0, upper=1> est_init_effects;
  int<lower=0, upper=Ncohorts> N_eta_c;
  int<lower=0, upper=Ncohorts> N_gamma_y;
  int<lower=0, upper=Ncohorts> N_delta_c;
  int<lower=0> n_proc_error;
}
parameters {
  real<lower=-0.99, upper=0.99> beta;
  vector[n_proc_error] pro_error_raw;
  real X0[1-est_init_effects];
  real<lower=0> sigma_p;
  real<lower=0> sigma_o;
  vector[N_eta_c] eta_c_raw;
  vector[N_gamma_y] gamma_y_raw;
  vector[N_delta_c] delta_c_raw;
  real<lower=0> eta_c_sd[est_init_effects];
  real<lower=0> delta_c_sd[est_cohort_effects];
  real<lower=0> gamma_y_sd[est_year_effects];
  real laa_mis[N_mis];
}
transformed parameters {
  matrix[Nages, Ncohorts] xaa;
  vector[N_delta_c] delta_c;
  vector[N_gamma_y] gamma_y;
  vector[N_eta_c] eta_c;
  xaa = rep_matrix(0, Nages, Ncohorts); // initialize at 0
  matrix[Nages, Nyears] laa;
  matrix[Nages, Nyears] laa_mis;
  laa_mis = rep_matrix(0, Nages, Nyears);

  for(i in 1:Nages){
    for(y in 1:Nyears){
      if(laa_obs[i,y]==999.0){
        laa = laa_mis[i,y];
      } else{
        laa = laa_obs[i,y];
      }
    }
  }

  // non-centered parameters:
  if (est_cohort_effects) delta_c = delta_c_raw * delta_c_sd[1];
  if (est_init_effects) eta_c = eta_c_raw * eta_c_sd[1];
  if (est_year_effects) gamma_y = gamma_y_raw * gamma_y_sd[1];

  for (y in 1:Ncohorts) {
    if (!est_init_effects) xaa[1, y] = X0[1];
    if (est_init_effects) xaa[1, y] = eta_c[y];
  }
  {
    int ii;
    ii = 0;
    for (y in 2:Ncohorts) {
      for (i in 2:Nages) {
        if (cohort_id[i-1, y-1] != 999) { // 999 = magic number for NA
          ii = ii + 1;
          xaa[i,y] = beta * xaa[i-1, y-1] + pro_error_raw[ii];
          if (est_cohort_effects) xaa[i,y] = xaa[i,y] + delta_c[cohort_id[i,y]];
          if (est_year_effects) xaa[i,y] = xaa[i,y] + gamma_y[y];
        }
      }
    }
  }
}
model {
  pro_error_raw ~ normal(0, sigma_p);
  // pro_error_raw ~ std_normal();
  to_vector(laa) ~ normal(to_vector(xaa[1:Nages, Nages:Ncohorts]), sigma_o);
  sigma_p ~ normal(0, 1);
  sigma_o ~ lognormal(sigma_o_prior[1], sigma_o_prior[2]);
  beta ~ std_normal();
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
