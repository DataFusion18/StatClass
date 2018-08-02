data{
  int<lower=0> L;
  vector[2] X[L];
}

parameters{
  vector[2] mu;
  real<lower=0> sig1;
  real<lower=0> sig2;
  real<lower=-1,upper=1> rho;
}

transformed parameters{
  cov_matrix[2] Sig;
  Sig[1,1] = sig1^2;
  Sig[2,2] = sig2^2;
  Sig[1,2] = sig1 * sig2 * rho;
  Sig[2,1] = sig1 * sig2 * rho;
}

model{
  //likelihood
  X ~ multi_normal(mu,Sig);
  // prior
  mu[1] ~ normal(0,300);
  mu[2] ~ normal(0,200);
  sig1 ~ cauchy(0,5);
  sig2 ~ cauchy(0,5);
  rho ~ uniform(-1,1);
}
