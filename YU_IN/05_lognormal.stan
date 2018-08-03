data{
  int<lower=0> N;
  real X[N];
  real Upper;
  real Lower;
}

parameters{
  real mu;
  real<lower=0> sig;
}

model{
  // likelihood
  X ~ lognormal(mu,sig);
  //prior
  mu ~ uniform(Lower,Upper);
  sig ~ cauchy(0,5);
}

generated quantities{
  real pred1;
  pred1 = lognormal_rng(mu,sig);
}
