data {
    int<lower=1> N; // observations
    int<lower=1> N_pop; // individuals
    int<lower=1> N_caste; // castes
    int id[N];  // id index to identify individuals
    int caste[N];
    real density[N];
    vector[N_pop] age;
    vector[N_pop] duration;
}
parameters {
  real<lower=0> sigma;
  vector[N_caste] z_caste;
  real alpha;
  real <lower=0> group_beta_caste_sd;
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
}

model {
  z_artist ~ normal(0, 1); 
  sigma ~ normal(0.5, 0.5);
  z_caste ~ std_normal();
  alpha ~ normal(0.5, 1);
  group_beta_caste_sd ~ normal(0.5, 0.5);
  beta_age ~ std_normal();
  beta_duration ~ std_normal();
  artist_sigma ~ normal(0, 0.5);

  density ~ lognormal(mu, sigma);
}
generated quantities {
  vector[N] log_lik;
  real y2[N];
  vector[N] mu1;
  
  for (i in 1:N) {
      mu1[i] = alpha + a_artist[id[i]] + beta_caste[caste[i]] + beta_age*age[id[i]] + beta_duration * duration[id[i]];
  }

  for(i in 1:N){
  y2[i] = lognormal_rng(mu[i], sigma);
  }
  
  for (i in 1:N){
    log_lik[i] = lognormal_lpdf(density[i] | mu[i], sigma);
  }
}

