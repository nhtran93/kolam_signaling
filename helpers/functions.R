library(rethinking)
library(dplyr)
library(gtools)
library(ks)
library(concaveman)
library(igraph)
library(kernelgon)
library(RColorBrewer)

############## Functions for the 
choose_max <- function(n_combos, k) {
  n <- 1
  while (n_combos > choose(n - 1 + k, k)) {
    n <- n + 1
  }
  n <- n - 1
  choose(n - 1 + k, k)
  return(n)
}

gini <- function(x) {
  x <- unlist(x)
  x <- sort(x, decreasing = TRUE)
  p <- x/sum(x)
  n <- length(p)
  i <- 1:n
  1 - 2 * (sum(p * i) - 1)/(n - 1)
}

sim_categorical <- function(n_bins, n_rows = 10000, beta = 1) {
  n_grid <- choose_max(n_rows, n_bins)
  grid <- seq(0.0001, 1, length.out = n_grid) # entropy doesn't like 0's
  grid <- grid^beta / ((1 - grid)^beta + grid^beta)
  sim <- combinations(n = n_grid, r = n_bins, v = grid, repeats.allowed = TRUE)
  sim <- sim / apply(sim, 1, sum)
  colnames(sim) <- paste0("p", 1:n_bins)
  return(sim)
}

# simulates approximately n_rows distributions from test grid n_variant_vec
sim_gini_entropy <- function(n_variant_vec, n_rows = 100000, beta = 1) {
  n_rows_each <- floor(n_rows/length(n_variant_vec))
  d <- data.frame()
  for (i in 1:length(n_variant_vec)) {
    n <- n_variant_vec[i]
    sim <- sim_categorical(n_bins = n, n_rows = n_rows_each, beta = beta)
    sim_gini <- apply(sim, 1, gini)
    sim_entropy <- apply(sim, 1, shannon)
    d_sim <- data.frame(n = n, gini = sim_gini, entropy = sim_entropy)
    d <- bind_rows(d, d_sim)
  }
  return(d)
}

# returns the (approximate) gini for a given maximum entropy H and n
g_H_max <- function(H, n) {
  a <- exp(0.51390628)
  b <- 2/(2 + a) + a/(n + a)
  ((n - exp(H))/(n - 1))^(1/(b+1))
}

# returns the (approximate) maximum entropy for a given g and n
H_g_max <- function(g, n, exp = FALSE) {
  a <- exp(0.51390628)
  b <- 2/(2 + a) + a/(n + a)
  out <- log(n - (n - 1) * g^(b + 1))
  if (exp) out <- exp(out)
  return(out)
}

# possible colour palettes
blues <- c(
  "#DEEBF7",
  "#9ECAE1",
  "#4292C6",
  "#08519C",
  "#08306B"
)

oranges <- c(
  "#FFEDA0",
  "#FED976",
  "#FEB24C",
  "#FD8D3C",
  "#FC4E2A"
)

# aquamarine <- c(
#   "#B9F6DE",
#   "#54EEC0",
#   "#00C991",
#   "#009C76",
#   "#126D5C"
# )
# 
# rose_to_cyan <- c(
#   "#FF254D",
#   "#EC0071",
#   "#C74D86",
#   "#886B9C",
#   "#2F9390"
# )

shannon <- function(p, base = exp(1), exp = FALSE){
  p <- p/sum(p)
  output <- sum(p*log(1/p, base))
  if (exp) output <- base^output
  output
}

gradient_maker <- function(start=NA, stop=NA, cols=c("darkorange", "white", "darkcyan"), vis=FALSE, n=1000){
  if(is.na(start) | is.na(stop)) stop("need to specify start and stop points on a numerical scale")
  colfunc <- colorRampPalette(cols)
  color.list <- colfunc(n)
  color.locations <- seq(start, stop, length=n)
  names(color.locations) <- color.list
  if(vis==TRUE) plot(color.locations, rep(1, n), col=color.list, pch="|", ylim=c(0.9, 1.1), cex=5)
  return(color.locations)
}

data_gradient <- function(data, colors=c("darkorange", "white", "darkcyan"), my.start=NA, my.stop=NA){
  if(is.na(my.start)) my.start <- min(data, na.rm=TRUE)
  if(is.na(my.stop)) my.stop <- max(data, na.rm=TRUE)
  my.gradient <- gradient_maker(start=my.start, stop=my.stop, cols=colors)
  if(any(data > max(my.gradient), na.rm=T) | any(data < min(my.gradient), na.rm=T)) warning("data is not within gradient range")
  data.colors <- rep(NA, length(data))
  for(i in 1:length(data)){
    if(!is.na(data[i])) data.colors[i] <- names(my.gradient)[which.min(abs(data[i]-my.gradient))]
  }
  data.colors
}

col_alpha <- function (acol, alpha = 0.2){
  acol <- col2rgb(acol)
  acol.red <- acol["red",]/255
  acol.green <- acol["green",]/255
  acol.blue <- acol["blue",]/255
  acol <- mapply(function(red, green, blue, alphas) rgb(red, green, blue, alphas), acol.red, acol.green, acol.blue, alpha)
  return(as.character(acol))
}

# numerical solver for the maximum entropy probability 
# distribution of n categories, holding gini constant
H_max <- function(g, n, exp = FALSE) {
  
  objective <- function(x) {
    n <- length(x)
    log(n) - sum(x * log(1/x))
  }
  
  constraints <- function(x) {
    c(gini(x), sum(x))
  }
  
  lower <- rep(0, n)
  upper <- 1/c(1:n)
  
  init <- sort(runif(n), decreasing = TRUE)
  init <- init/sum(init)
  
  res <- Rsolnp::solnp(
    pars = init,          # initial objective values
    fun = objective,      # objective, to minimize
    eqfun = constraints,  # equality constraints
    eqB = c(g, 1),        # the equality boundaries for each constraint
    LB = lower,
    UB = upper
  )
  
  out <- log(n) - objective(res$pars)
  if (exp) out <- exp(out)
  return(out)
  
}
