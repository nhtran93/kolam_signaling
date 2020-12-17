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
    int<lower=1> N_caste; // castes
    int id[N];  // id index to identify individuals
    real gini[N];
    
    int caste[N];
    vector[N_pop] age;
    vector[N_pop] duration;
}
parameters {
  real<lower=0> sigma;
  vector[N_caste] z_caste;
  real alpha;
  real<lower=0>group_beta_caste_sd;
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
  sigma ~ normal(0.5, 1);
  z_caste ~ std_normal();
  alpha ~ normal(1, 2);
  group_beta_caste_sd ~ normal(0.5, 1);
  beta_age ~ std_normal();
  beta_duration ~ std_normal();
  artist_sigma ~ normal(0, 0.5);

  for (i in 1:N){
    gini[i] ~ normal(mu[i], sigma) T[0, 1];
  }
}
generated quantities {
  vector[N] log_lik;
  real y2[N];
  vector[N] mu1;
  
  for (i in 1:N) {
      mu1[i] = alpha + a_artist[id[i]] + beta_caste[caste[i]] + beta_age*age[id[i]] + beta_duration * duration[id[i]];
  }
  
  for(i in 1:N){
  y2[i] = trunc_normal_rng(mu[i], sigma, 0, 1);
  }
  
  for (i in 1:N){
    log_lik[i] = trunc_normal_lpdf(gini[i] | mu[i], sigma, 0, 1);
  }
}
