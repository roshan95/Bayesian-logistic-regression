data {
  int<lower = 0> N;
  int<lower = 0> K;
  matrix[N, K] X;
  int<lower = 0, upper = 1> y[N];
}

parameters {
  vector[K] beta;
  real<lower=0> sigma;

}

model {
  beta ~ normal(0, 5);
  sigma ~ normal(0, 1);
  y ~ bernoulli_logit(X * beta);
}

generated quantities {
  int<lower = 0, upper = 1> y_rep[N];
  for (i in 1:N) {
    y_rep[i] = bernoulli_logit_rng(X[i] * beta);
  }
}