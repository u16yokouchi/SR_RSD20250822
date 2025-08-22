data {
  int <lower=1> J; // n_examinee
  int <lower=1> T; // n_time
  int <lower=0> K; // n_score
  int <lower=1> N; // n_samples
  int <lower=1> I;
  int <lower=1, upper=J> ExamineeID[N];
  int <lower=1, upper=T> TimeID[N];
  int <lower=1, upper=I> ETinter[N];
  int <lower=0, upper=K> X[N];
}

transformed data {
  vector[K] c = cumulative_sum(rep_vector(1, K)) - 1;
}

parameters {
  vector[J] theta;
  real Hier_Time;
  real Hier_alpha_t;
  real<lower=0> sigma_Time;
  real<lower=0> sigma_at;
  real<lower=0> alpha_i[I];
  real<lower=0> alpha_t[T];
  vector[T] beta_t;
  vector[K-2] beta_k;
  vector[I] beta_i;
}

transformed parameters {
  vector[K-1] category_est;
  vector[K] category_prm;
  category_est[1:(K-2)] = beta_k;
  category_est[K-1] = -1 * sum(beta_k);
  category_prm = cumulative_sum(append_row(0, category_est));
}

model {
  alpha_i~lognormal(0,1);
  Hier_Time~normal(0,3);
  sigma_Time~gamma(10,10);
  sigma_at~gamma(10,10);
  theta ~ normal(0, 1);
  beta_t ~ normal(Hier_Time, sigma_Time);
  alpha_t~normal(Hier_alpha_t,sigma_at);
  beta_i ~ normal(0, 1);
  category_est ~ normal(0, 1);

  for (n in 1:N) {
    X[n] ~ categorical_logit(1.7*alpha_t[TimeID[n]]*alpha_i[ETinter[n]]*(c*(theta[ExamineeID[n]]-beta_t[TimeID[n]])-beta_i[ETinter[n]]-category_prm));
  }
}

generated quantities {
  vector[N] log_lik;
  int X_pre[N];
  vector[N] residuals;
  for (n in 1:N) {
    log_lik[n] = categorical_logit_log(X[n],1.7*alpha_t[TimeID[n]]*alpha_i[ETinter[n]]*(c*(theta[ExamineeID[n]]-beta_t[TimeID[n]])-beta_i[ETinter[n]]-category_prm));
    X_pre[n] = categorical_rng(softmax(1.7*alpha_t[TimeID[n]]*alpha_i[ETinter[n]]*(c*(theta[ExamineeID[n]]-beta_t[TimeID[n]])-beta_i[ETinter[n]]-category_prm)));
    residuals[n] = X[n] - X_pre[n];
  }
}
