data {
  int<lower=1> Nages;
  int<lower=1> Nyears;
  matrix[Nages, Nyears] laa;
}
parameters {
  real<lower=-1, upper=1> beta;
  matrix[Nages, Nyears] eps;
  real<lower=0> sigma_p;
  real<lower=0> sigma_o;
  vector[Nyears - 1] gamma_y;
}
transformed parameters {
  matrix[Nages, Nyears] xaa;
  for (i in 1:Nages) {
    for (y in 1:Nyears) {
      if (i == 1 || y == 1) {
        xaa[i, y] = eps[i, y];
      } else {
        xaa[i, y] = beta * xaa[i - 1, y - 1] + gamma_y[y - 1] +
                    sigma_p * eps[i, y];
      }
    }
  }
  for (i in 1:Nages) {
    for (y in 2:Nyears) {
      xaa[i, y] = xaa[i, y] + gamma_y[y - 1];
    }
  }
}
model {
  to_vector(eps) ~ std_normal();
  to_vector(laa) ~ normal(to_vector(xaa), sigma_o);
  sigma_o ~ student_t(3, 0, 2);
  sigma_p ~ student_t(3, 0, 2);
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
