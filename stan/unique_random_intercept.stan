data {
    int<lower=1> N; // observations
    int<lower=1> N_pop; // individuals
    int id[N];  // id index to identify individuals
    int<lower=1> N_caste; // castes

    int<lower=0> unique[N]; // number of unique gestures
    
    int caste[N];
    vector[N_pop] age;
    vector[N_pop] duration;
}
parameters {
  real alpha;
  real beta_age;
  real beta_duration;
  vector[N_caste] z_caste;
  real<lower=0> group_beta_caste_sd;
  real<lower=0> artist_sigma;
  vector[N_pop] z_artist;
}
transformed parameters{
    vector[N] lambda;
    vector[N_caste] beta_caste = group_beta_caste_sd * z_caste;
    vector[N_pop] a_artist = artist_sigma * z_artist;

    // Compute lambda
   for (i in 1:N) {
        lambda[i] = alpha + a_artist[id[i]] + beta_caste[caste[i]] + beta_age*age[id[i]] + beta_duration * duration[id[i]];
    }
    
    lambda = exp(lambda);
    
}
model {
  // If a variable is declared with a lower bound of zero, then assigning it a normal prior in a Stan model produces the same effect as providing a properly truncated half-normal prior. The truncation at zero need not be specified as Stan only requires the density up to a proportion. https://github.com/stan-dev/stan/releases/download/v2.16.0/stan-reference-2.16.0.pdf --> under Truncated Priors
  z_artist ~ normal(0, 1); 
  beta_age ~ std_normal();
  beta_duration ~ std_normal();
  z_caste ~ std_normal();
  alpha ~ normal(1, 2);
  group_beta_caste_sd ~ normal(0.5, 1);
  artist_sigma ~ normal(0, 0.5);

  unique ~ poisson(lambda);
}
generated quantities {
  vector[N] log_lik;
  vector[N] lambda1;
  int<lower=0> y2[N];
  
  for (i in 1:N) {
      lambda1[i] = alpha + a_artist[id[i]] + beta_caste[caste[i]] + beta_age*age[id[i]] + beta_duration * duration[id[i]];
  }
  lambda1 = exp(lambda1);

  y2 = poisson_rng(lambda1);
  for (i in 1:N){
    log_lik[i] = poisson_lpmf(unique[i] | lambda[i]);
  }
}
