data {
    int<lower=1> N; // observations
    int<lower=1> N_pop; // individuals
    int id[N];  // id index to identify individuals
    int<lower=1> N_caste; // castes

    int<lower=0> canvas_size[N]; // number of canvas size gestures
    
    int caste[N];
    vector[N_pop] age;
    vector[N_pop] duration;

}
transformed data {
  real delta = 1e-9; // nugget or jitter to ensure a positive definite matrix (stabilize the numerical calculations) before Cholesky decomposition
}
parameters {
  real<lower=0> phi;
  real alpha;
  vector[N_caste] z_caste;
  real<lower=0> group_beta_caste_sd;
  real beta_age;
  real beta_duration;
  vector[N_pop] z_artist;
  real<lower=0> artist_sigma;
}
transformed parameters{
    vector[N] mu;
    vector[N_caste] beta_caste = group_beta_caste_sd * z_caste;
    vector[N_pop] a_artist = artist_sigma * z_artist;
    
    // Compute mu
   for (i in 1:N) {
        mu[i] = alpha + a_artist[id[i]] + beta_caste[caste[i]] + beta_age*age[id[i]] + beta_duration * duration[id[i]];
    }
    
    mu = exp(mu);
    
}
model {
  z_artist ~ normal(0, 1); 
  z_caste ~ std_normal();
  alpha ~ normal(1, 2);
  group_beta_caste_sd ~ normal(0.5, 1);
  phi ~ normal(1.5, 3);
  beta_age ~ std_normal();
  beta_duration ~ std_normal();
  artist_sigma ~ normal(0, 0.5);

  canvas_size ~ neg_binomial_2(mu, phi);
}
generated quantities {
  vector[N] log_lik;
  vector[N] mu1;
  int<lower=0> y2[N];
  
  for (i in 1:N) {
      mu1[i] = alpha + a_artist[id[i]] + beta_caste[caste[i]] + beta_age*age[id[i]] + beta_duration * duration[id[i]];
  }
  mu1 = exp(mu1);

  y2 = neg_binomial_2_rng(mu1, phi);
  for (i in 1:N){
    log_lik[i] = neg_binomial_2_lpmf(canvas_size[i] | mu[i], phi);
  }
}
