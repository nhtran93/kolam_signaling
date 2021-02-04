rm(list = ls())
library(rstan)
library(rethinking)
library(parallel)
set.seed(152)

# Make sure to create a directory where the fitted MCMC samples will be saved.
dir.create("samples")

# You will have to manually change the following to TRUE, if you want to fit the model:
density_model <- FALSE
gini_model <- FALSE
unique_model <- FALSE
canvas_size_model <- FALSE
entropy_model <- FALSE

################## Density Model ##################
if (density_model) {
  load(file = "data/kolam_data_share.RData")
  df$jathi_standard_all <- as.integer(as.factor(df$jathi))
  
  data_list <- list(N_pop = length(unique(df$artist_id)), N = nrow(df),
                    id =  as.integer(as.factor(df$artist_id)),
                    density = df$density,
                    caste = df$jathi_standard_all, 
                    N_caste = length(unique(df$jathi_standard_all)),
                    age = (df[!duplicated(df$artist_id), "age"] - mean(df[!duplicated(df$artist_id), "age"]))/sd(df[!duplicated(df$artist_id), "age"]),
                    duration = (df[!duplicated(df$artist_id), "duration_practice"] - mean(df[!duplicated(df$artist_id), "duration_practice"]))/sd(df[!duplicated(df$artist_id), "duration_practice"])
  )
  
  Niter <- 6000
  Nchains <- 4
  Ncores <- parallel::detectCores()
  samples_density <- stan(file = "stan/density_random_intercept.stan",
                          data = data_list, chains = Nchains, iter = Niter,
                          warmup = 1500,
                          control = list(adapt_delta = 0.99, max_treedepth = 18),
                          cores = Ncores, seed = 12) 
  
  params <- extract.samples(samples_density)
  params_density <- list(mu = params$mu,
                         sigma = params$sigma,
                         artist_sigma = params$artist_sigma,
                         a_artist = params$a_artist,
                         alpha = params$alpha,
                         beta_age = params$beta_age,
                         beta_duration = params$beta_duration,
                         beta_caste = params$beta_caste,
                         group_sd_caste = params$group_beta_caste_sd,
                         z_caste_sd = params$z_caste,
                         post_unique = params$y2
                         )
  
  save(params_density, samples_density, file = "samples/params_density_intercept.RData")
  
  rm(samples_density, data_list, params_density, df, params)
}

################## Gini Model ##################
if (gini_model) {
  load(file = "data/kolam_data_share.RData")
  df$jathi_standard_all <- as.integer(as.factor(df$jathi))
  df1 <- df[-which(is.na(df$gesture_gini)), ] # exclude Gini with NA
  
  data_list <- list(N_pop = length(unique(df1$artist_id)), N = nrow(df1),
                    id =  as.integer(as.factor(df1$artist_id)),
                    gini = df1$gesture_gini,
                    caste = df1$jathi_standard_all, 
                    N_caste = length(unique(df1$jathi_standard_all)),
                    age = (df1[!duplicated(df1$artist_id), "age"] - mean(df1[!duplicated(df1$artist_id), "age"]))/sd(df1[!duplicated(df1$artist_id), "age"]),
                    duration = (df1[!duplicated(df1$artist_id), "duration_practice"] - mean(df1[!duplicated(df1$artist_id), "duration_practice"]))/sd(df1[!duplicated(df1$artist_id), "duration_practice"])
  )
  
  Niter <- 6000
  Nchains <- 4
  Ncores <- parallel::detectCores()
  samples_gini <- stan("stan/gini_random_intercept.stan",
                       data = data_list, chains = Nchains, iter = Niter,
                       warmup = 1500,
                       control = list(adapt_delta = 0.99, max_treedepth = 15),
                       cores = Ncores, seed = 1234) 
  
  params <- extract.samples(samples_gini)
  params_gini <- list(mu = params$mu,
                      sigma = params$sigma,
                      artist_sigma = params$artist_sigma,
                      a_artist = params$a_artist,
                      alpha = params$alpha,
                      beta_age= params$beta_age,
                      beta_duration = params$beta_duration,
                      beta_caste = params$beta_caste,
                      group_sd_caste = params$group_beta_caste_sd,
                      z_caste_sd = params$z_caste,
                      post_unique = params$y2
                      )
  
  save(params_gini, samples_gini, file = "samples/params_gini_intercept.RData")
  
  rm(samples_gini, data_list, params_gini, df, df1, params)
}


################## Unique Model ##################
if (unique_model) {
  load(file = "data/kolam_data_share.RData")
  df$jathi_standard_all <- as.integer(as.factor(df$jathi))
  
  data_list <- list(N_pop = length(unique(df$artist_id)), N = nrow(df),
                    id =  as.integer(as.factor(df$artist_id)),
                    unique = df$n_unique_gestures,
                    caste = df$jathi_standard_all, 
                    N_caste = length(unique(df$jathi_standard_all)),
                    age = (df[!duplicated(df$artist_id), "age"] - mean(df[!duplicated(df$artist_id), "age"]))/sd(df[!duplicated(df$artist_id), "age"]),
                    duration = (df[!duplicated(df$artist_id), "duration_practice"] - mean(df[!duplicated(df$artist_id), "duration_practice"]))/sd(df[!duplicated(df$artist_id), "duration_practice"])
  )
  
  Niter <- 6000
  Nchains <- 4
  Ncores <- parallel::detectCores()
  samples_unique <- stan(file = "stan/unique_random_intercept.stan",
                         data = data_list, chains = Nchains, iter = Niter,
                         warmup = 1500,
                         control = list(adapt_delta = 0.99, max_treedepth = 15),
                         cores = Ncores, seed = 123) 
  
  params <- extract.samples(samples_unique)
  params_unique <- list(lambda = params$lambda,
                        artist_sigma = params$artist_sigma,
                        a_artist = params$a_artist,
                        alpha = params$alpha,
                        beta_age= params$beta_age,
                        beta_duration = params$beta_duration,
                        beta_caste = params$beta_caste,
                        group_sd_caste = params$group_beta_caste_sd,
                        z_caste_sd = params$z_caste,
                        post_unique = params$y2
                        )
  
  save(params_unique, samples_unique, file = "samples/params_unique_intercept.RData")
  
  rm(samples_unique, data_list, params_unique, df, params)
}


################## Canvas Size Model ##################
if (canvas_size_model) {
  load(file = "data/kolam_data_share.RData")
  df$jathi_standard_all <- as.integer(as.factor(df$jathi))
  
  data_list <- list(N_pop = length(unique(df$artist_id)), N = nrow(df),
                    id =  as.integer(as.factor(df$artist_id)),
                    canvas_size = df$size,
                    caste = df$jathi_standard_all, 
                    N_caste = length(unique(df$jathi_standard_all)),
                    age = (df[!duplicated(df$artist_id), "age"] - mean(df[!duplicated(df$artist_id), "age"]))/sd(df[!duplicated(df$artist_id), "age"]),
                    duration = (df[!duplicated(df$artist_id), "duration_practice"] - mean(df[!duplicated(df$artist_id), "duration_practice"]))/sd(df[!duplicated(df$artist_id), "duration_practice"])
  )
  
  Niter <- 6000
  Nchains <- 4
  Ncores <- parallel::detectCores()
  samples_size <- stan(file = "stan/size_random_intercept.stan",
                       data = data_list, chains = Nchains, iter = Niter,
                       warmup = 1500,
                       control = list(adapt_delta = 0.99, max_treedepth = 15),
                       cores = Ncores, seed = 123) 
  
  params <- extract.samples(samples_size)
  params_size <- list(mu = params$mu,
                      artist_sigma = params$artist_sigma,
                      a_artist = params$a_artist,
                      alpha = params$alpha,
                      beta_age = params$beta_age,
                      beta_duration = params$beta_duration,
                      beta_caste = params$beta_caste,
                      group_sd_caste = params$group_beta_caste_sd,
                      z_caste_sd = params$z_caste,
                      phi = params$phi, 
                      post_unique = params$y2
                      )
  
  save(params_size, samples_size, file = "samples/params_size_intercept.RData")
  
  rm(samples_size, data_list, params_size, df, params)
}

################## Entropy Model ##################
if (entropy_model) {
  load(file = "data/kolam_data_share.RData")
  df$jathi_standard_all <- as.integer(as.factor(df$jathi))
  
  data_list <- list(N_pop = length(unique(df$artist_id)), N = nrow(df),
                    id =  as.integer(as.factor(df$artist_id)),
                    entropy = df$entropy,
                    caste = df$jathi_standard_all, 
                    N_caste = length(unique(df$jathi_standard_all)),
                    age = (df[!duplicated(df$artist_id), "age"] - mean(df[!duplicated(df$artist_id), "age"]))/sd(df[!duplicated(df$artist_id), "age"]),
                    duration = (df[!duplicated(df$artist_id), "duration_practice"] - mean(df[!duplicated(df$artist_id), "duration_practice"]))/sd(df[!duplicated(df$artist_id), "duration_practice"])
  )
  
  Niter <- 6000
  Nchains <- 4
  Ncores <- parallel::detectCores()
  samples_entropy <- stan(file = "stan/entropy_random_intercept.stan",
                       data = data_list, chains = Nchains, iter = Niter,
                       warmup = 1500,
                       control = list(adapt_delta = 0.99, max_treedepth = 18),
                       cores = Ncores, seed = 123) 
  
  params <- extract.samples(samples_entropy)
  params_entropy <- list(mu = params$mu,
                         artist_sigma = params$artist_sigma,
                         a_artist = params$a_artist,
                         alpha = params$alpha,
                         beta_age = params$beta_age,
                         beta_duration = params$beta_duration,
                         beta_caste = params$beta_caste,
                         group_sd_caste = params$group_beta_caste_sd,
                         z_caste_sd = params$z_caste,
                         sigma = params$sigma, 
                         post_unique = params$y2
                         )
  
  save(params_entropy, samples_entropy, file = "samples/params_size_intercept.RData")
  
  rm(samples_entropy, data_list, params_entropy, df, params)
}

