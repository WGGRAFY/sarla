//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// The input data is a matrix 'laa' of dimensions Nages*Nyears
data {
  int<lower=0> Nages;
  int<lower=0> Nyears;
  matrix[Nages, Nyears] laa;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta;
  real xaa_0;
  matrix[Nages,Nyears] xaa;
  real<lower=0> sigma_p;
  real<lower=0> sigma_o;
  real<lower=0> sigma_0;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

  xaa[1,1] ~ normal(0,sigma_0);

  for (i in 1:Nages) {
    for(y in 1:Nyears){
      xaa[i,y] ~ normal(beta*xaa[i-1,y-1], sigma_p);
      laa[i,y] ~ normal(xaa[i,y], sigma_o);
    }
  }
}
