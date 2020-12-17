functions {
  real trunc_normal_lpdf(real y, real mu, real sigma, real lb, real ub){
    real out;
    
    out  = normal_lpdf(y | mu, sigma);
    out -= log_diff_exp(normal_lcdf(ub | mu, sigma),
                        normal_lcdf(lb | mu, sigma));
                        
    return out;
  }
  real trunc_normal_rng(real mu, real sigma, real lb, real ub){
    real p1 = normal_cdf(lb, mu, sigma);  // cdf with lower bound
    real p2 = normal_cdf(ub, mu, sigma);  // cdf with upper bound
    real u = uniform_rng(p1, p2);
    return (sigma * inv_Phi(u)) + mu;  // inverse cdf
  }
}
data {
    int<lower=1> N; // observations
    int<lower=1> N_pop; // individuals
    int id[N];  // id index to identify individuals
    int caste[N];
    real entropy[N];
    int<lower=1> N_caste; // castes
    vector[N_pop] age;
    vector[N_pop] duration;

}
parameters {
  real<lower=0> sigma;
  real alpha;
  vector[N_pop] z_artist;
  real<lower=0> artist_sigma;
  real beta_age;
  real beta_duration;
  vector[N_caste] z_caste;
  real <lower=0> group_beta_caste_sd;

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
  sigma ~ normal(0.5, 1);
  alpha ~ normal(1, 2);
  artist_sigma ~ normal(0, 0.5);
  group_beta_caste_sd ~ normal(0.5, 0.5);
  beta_age ~ std_normal();
  beta_duration ~ std_normal();
  z_caste ~ std_normal();
  
  for (i in 1:N){
    entropy[i] ~ normal(mu[i], sigma) T[0, ];
  }
}
generated quantities {
  real y2[N];
  vector[N] mu1;
  
  for (i in 1:N) {
      mu1[i] = alpha + a_artist[id[i]] + beta_caste[caste[i]] + beta_age*age[id[i]] + beta_duration * duration[id[i]];
  }
  
  for(i in 1:N){
  y2[i] = trunc_normal_rng(mu[i], sigma, 0, positive_infinity());
  }
}
