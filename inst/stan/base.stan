data {
  int<lower=1> Nages;
  int<lower=1> Nyears;
  matrix[Nages, Nyears] laa;
  real sigma_o_prior[2];
}
parameters {
  real<lower=-1, upper=1> beta;
  matrix[Nages, Nyears] pro_error_raw;
  real<lower=0> sigma_p;
  real<lower=0> sigma_o;
  vector[Nyears - 1] gamma_y;
}
transformed parameters {
  matrix[Nages, Nyears] xaa;
//  matrix[Nages, Nyears] obs_error_raw;
  for (i in 1:Nages) {
    for (y in 1:Nyears) {
      if (i == 1 || y == 1) {
        xaa[i, y] = pro_error_raw[i, y];
      } else {
        xaa[i, y] = beta * xaa[i - 1, y - 1] +
                    sigma_p * pro_error_raw[i, y];
      }
    }
  }
  for (i in 2:Nages) {
    for (y in 2:Nyears) {
      xaa[i, y] = xaa[i, y] + gamma_y[y - 1];
    }
  }
  // non-centered observation error:
  // for (i in 1:Nages) {
  //   for (y in 1:Nyears) {
  //     // laa[i, y] = xaa[i, y] - sigma_o * obs_error_raw[i, y];
  //     // (laa[i, y] - xaa[i, y]) / -sigma_o = obs_error_raw[i, y];
  //     obs_error_raw[i, y] = (laa[i, y] - xaa[i, y]) / -sigma_o;
  //   }
  // }
}
model {
  to_vector(pro_error_raw) ~ std_normal();
  to_vector(laa) ~ normal(to_vector(xaa), sigma_o);
  // sigma_o ~ student_t(3, 0, 1);
  sigma_o ~ lognormal(sigma_o_prior[1], sigma_o_prior[2]);
  sigma_p ~ student_t(3, 0, 1);
  beta ~ std_normal();
  gamma_y ~ std_normal();
}
generated quantities {
  matrix[Nages, Nyears] laa_postpred;
  matrix[Nages, Nyears] log_lik;
  for (i in 1:Nages) {
    for (y in 1:Nyears) {
      laa_postpred[i, y] = normal_rng(xaa[i, y], sigma_o);
      log_lik[i, y] = normal_lpdf(laa[i, y] | xaa[i, y], sigma_o);
    }
  }
}
