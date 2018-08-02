data{
  int<lower=0> L;
  int<lower=0> N[L];
  int<lower=0> X[L];
}

parameters{
  real<lower=0,upper=1> theta;
}

model{
  X ~ binomial(N,theta);
}
