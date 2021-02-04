rm(list = ls())
library(rethinking)
library(ggplot2)
library(msm)
library(ggpubr)
library(tidyverse)

load(file = "samples/params_density.RData")
load(file = "samples/params_canvas_size.RData")
load(file = "samples/params_gini.RData")
load(file = "samples/params_unique.RData")
load(file = "samples/params_entropy.RData")

######################## 2 Outcomes Plots  SHORT ###################
artist_gini_mean <- apply(sapply(1:ncol(params_gini$a_artist), function(k) params_gini$a_artist[, k] + params_gini$alpha), 2, mean)
artist_unique_mean <- apply(sapply(1:ncol(params_unique$a_artist), function(k) exp(params_unique$a_artist[, k] + params_unique$alpha)), 2, mean)
artist_density_mean <- apply(sapply(1:ncol(params_density$a_artist), function(k) params_density$a_artist[, k] + params_density$alpha), 2, mean)
artist_size_mean <- apply(sapply(1:ncol(params_size$a_artist), function(k) exp(params_size$a_artist[, k] + params_size$alpha)), 2, mean)
artist_entropy_mean <- apply(sapply(1:ncol(params_entropy$a_artist), function(k) params_entropy$a_artist[, k] + params_entropy$alpha), 2, mean)
artist_mean <- cbind(1:192, artist_gini_mean, artist_unique_mean,
                     artist_density_mean, artist_size_mean, artist_entropy_mean)
artist_mean <- data.frame(artist_mean)
colnames(artist_mean) <- c("artist","gini_offset", "unique_offset",
                           "density_offset", "size_offset", "entropy_offset")

artist_mean_long <- pivot_longer(data = artist_mean, cols = -artist, names_to = "model", values_to = "offset")

plot_size1 <- ggplot(artist_mean, aes(x = size_offset, y = 1-gini_offset)) + 
  geom_point(aes(color = entropy_offset), size = 2) +
  xlab("Canvas Size") + ylab("Evenness") +
  theme_classic() + 
  labs(color = "Entropy") +
  guides(color = guide_colorbar(title.vjust = 0.8)) +
  theme(legend.position='none', legend.title=element_text(size=20),
        legend.text=element_text(size=15), axis.text=element_text(size=20),
        axis.title=element_text(size=25),
        plot.title = element_text(size = 20, face = "bold"),
        legend.key.width = unit(1.5,"cm")) +
  ggtitle("A") + 
  scale_fill_gradient(limits = c(0, 2), breaks = c(1.1, 1.2, 1.3))  +
  annotate(geom = "text",
           label = paste("r =", round(cor((1-artist_gini_mean), artist_size_mean), 3)),
           x = 8.7, y = 0.65, size = 6)

plot_size2 <- ggplot(artist_mean, aes(x = unique_offset, y = size_offset)) + 
  geom_point(aes(color = entropy_offset), size = 2) +
  ylab("Canvas Size") +
  xlab("Richness") +
  theme_classic() + 
  scale_x_continuous(breaks = seq(4.43, 4.47, by=.02),
                     limits = c(4.42, 4.47), expand = expansion(mult = 0.1)) +
  labs(color = "Entropy") +
  guides(color = guide_colorbar(title.vjust = 0.8)) +
  theme(legend.position='none', legend.title=element_text(size=20),
        legend.text=element_text(size=15), axis.text=element_text(size=20),
        axis.title=element_text(size=25),
        plot.title = element_text(size = 20, face = "bold"),
        legend.key.width = unit(1.5,"cm")) +
  ggtitle("C") + 
  scale_fill_gradient(limits = c(0, 2), breaks = c(1.1, 1.2, 1.3)) +
  annotate(geom = "text",
           label = paste("r =", round(cor(artist_unique_mean, artist_size_mean), 3)),
           x = 4.465, y = 9.1, size = 6) 


g_H_max_0.7 <- function(x) {
  a <- exp(0.51390628)
  b <- 2/(2 + a) + a/(x + a)
  1- (((x - exp(0.7))/(x - 1))^(1/(b+1)))
}
g_H_max_1.15 <- function(x) {
  a <- exp(0.51390628)
  b <- 2/(2 + a) + a/(x + a)
  1- (((x - exp(1.15))/(x - 1))^(1/(b+1)))
}
g_H_max_1.2 <- function(x) {
  a <- exp(0.51390628)
  b <- 2/(2 + a) + a/(x + a)
  1- (((x - exp(1.2))/(x - 1))^(1/(b+1)))
}
g_H_max_1.3 <- function(x) {
  a <- exp(0.51390628)
  b <- 2/(2 + a) + a/(x + a)
  1- (((x - exp(1.3))/(x - 1))^(1/(b+1)))
}

g_H_max_1.4 <- function(x) {
  a <- exp(0.51390628)
  b <- 2/(2 + a) + a/(x + a)
  1- (((x - exp(1.4))/(x - 1))^(1/(b+1)))
}
g_H_max_1.6 <- function(x) {
  a <- exp(0.51390628)
  b <- 2/(2 + a) + a/(x + a)
  1- (((x - exp(1.6))/(x - 1))^(1/(b+1)))
}

plot_gini1 <- ggplot(artist_mean, aes(x = unique_offset, y = 1 - gini_offset)) + 
  geom_point(aes(color = entropy_offset), size = 2) +
  ylab("Evenness") + xlab("Richness") +
  theme_classic() +
  #xlim(c(4.40, 4.50)) + 
  scale_x_continuous(breaks = seq(4.43, 4.47, by=.02),
                     limits = c(4.42, 4.47), expand = expansion(mult = 0.1)) +
  ylim(c(0.40, 0.65)) +
  labs(color = "Entropy") +
  guides(color = guide_colorbar(title.vjust = 0.8)) +
  theme(legend.position='none', legend.title=element_text(size=20),
        legend.text=element_text(size=15), axis.text=element_text(size=20),
        axis.title=element_text(size=25),
        plot.title = element_text(size = 20, face = "bold"),
        legend.key.width = unit(1.5,"cm")) +
  ggtitle("B") + 
  scale_fill_gradient(limits = c(0, 2), breaks = c(1.1, 1.2, 1.3)) +
  annotate(geom = "text",
           label = paste("r =", round(cor((1-artist_gini_mean), artist_unique_mean), 3)),
           x = 4.465, y = 0.65, size = 6)

cairo_ps(file = "output/posterior_intercepts.eps", 
         fallback_resolution = 1000, width = 15, height = 7)
dev.off()
# png(filename = "output/posterior_intercepts.png",
#     width = 15, height = 7, units = "in", res = 800)
ggarrange(plot_size1, plot_gini1, plot_size2,
          common.legend = TRUE,
          ncol = 3, nrow = 1, legend="bottom")
dev.off()


##################### Plot BETA ##################### 
####### Observational Sigma
df_sigma <- data.frame(model = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)
# Canvas Size
df_sigma[1, "model"] <- "canvas size"
df_sigma[1, "mean"] <- mean(exp(params_size$alpha) + (exp(params_size$alpha)^2)/params_size$phi)
df_sigma[1, "lower_HPDI"] <- mean(round(exp(params_size$alpha) + (exp(params_size$alpha)^2)/HPDI(params_size$phi, prob = .90)[1], 2))
df_sigma[1, "upper_HPDI"] <- mean(round(exp(params_size$alpha) + (exp(params_size$alpha)^2)/HPDI(params_size$phi, prob = .90)[2], 2)) 
# Gini
df_sigma[2, "model"] <- "evenness"
df_sigma[2, "mean"] <- mean(params_gini$sigma)
df_sigma[2, "lower_HPDI"] <- round(HPDI(params_gini$sigma, prob = .90)[1], 2)
df_sigma[2, "upper_HPDI"] <- round(HPDI(params_gini$sigma, prob = .90)[2], 2) 
# Unique Gestures
df_sigma[3, "model"] <- "richness"
df_sigma[3, "mean"] <- mean(exp(params_unique$alpha))
df_sigma[3, "lower_HPDI"] <- round(HPDI(exp(params_unique$alpha), prob = .90)[1], 2)
df_sigma[3, "upper_HPDI"] <- round(HPDI(exp(params_unique$alpha), prob = .90)[2], 2) 
# Density
df_sigma[4, "model"] <- "density"
df_sigma[4, "mean"] <- mean(params_density$sigma)
df_sigma[4, "lower_HPDI"] <- round(HPDI(params_density$sigma, prob = .90)[1], 2)
df_sigma[4, "upper_HPDI"] <- round(HPDI(params_density$sigma, prob = .90)[2], 2) 
# Entropy
df_sigma[5, "model"] <- "entropy"
df_sigma[5, "mean"] <- mean(params_entropy$sigma)
df_sigma[5, "lower_HPDI"] <- round(HPDI(params_entropy$sigma, prob = .90)[1], 2)
df_sigma[5, "upper_HPDI"] <- round(HPDI(params_entropy$sigma, prob = .90)[2], 2) 


##### Artist Sigma
df_artist_sigma <- data.frame(model = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)
# Canvas Size
df_artist_sigma[1, "model"] <- "canvas size"
df_artist_sigma[1, "mean"] <- mean(params_size$artist_sigma)
df_artist_sigma[1, "lower_HPDI"] <- round(HPDI(params_size$artist_sigma, prob = .90)[1], 2)
df_artist_sigma[1, "upper_HPDI"] <- round(HPDI(params_size$artist_sigma, prob = .90)[2], 2) 
# Gini
df_artist_sigma[2, "model"] <- "evenness"
df_artist_sigma[2, "mean"] <- mean(params_gini$artist_sigma)
df_artist_sigma[2, "lower_HPDI"] <- round(HPDI(params_gini$artist_sigma, prob = .90)[1], 2)
df_artist_sigma[2, "upper_HPDI"] <- round(HPDI(params_gini$artist_sigma, prob = .90)[2], 2) 
# Unique Gestures
df_artist_sigma[3, "model"] <- "richness"
df_artist_sigma[3, "mean"] <- mean(params_unique$artist_sigma)
df_artist_sigma[3, "lower_HPDI"] <- round(HPDI(params_unique$artist_sigma, prob = .90)[1], 2)
df_artist_sigma[3, "upper_HPDI"] <- round(HPDI(params_unique$artist_sigma, prob = .90)[2], 2) 
# Density
df_artist_sigma[4, "model"] <- "density"
df_artist_sigma[4, "mean"] <- mean(params_density$artist_sigma)
df_artist_sigma[4, "lower_HPDI"] <- round(HPDI(params_density$artist_sigma, prob = .90)[1], 2)
df_artist_sigma[4, "upper_HPDI"] <- round(HPDI(params_density$artist_sigma, prob = .90)[2], 2) 
# Entropy
df_artist_sigma[5, "model"] <- "entropy"
df_artist_sigma[5, "mean"] <- mean(params_entropy$artist_sigma)
df_artist_sigma[5, "lower_HPDI"] <- round(HPDI(params_entropy$artist_sigma, prob = .90)[1], 2)
df_artist_sigma[5, "upper_HPDI"] <- round(HPDI(params_entropy$artist_sigma, prob = .90)[2], 2) 


#### Jathi Sigma
df_jathi <- data.frame(model = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)
# Canvas Size
df_jathi[1, "model"] <- "canvas size"
df_jathi[1, "mean"] <- mean(params_size$group_sd_caste)
df_jathi[1, "lower_HPDI"] <- round(HPDI(params_size$group_sd_caste, prob = .90)[1], 2)
df_jathi[1, "upper_HPDI"] <- round(HPDI(params_size$group_sd_caste, prob = .90)[2], 2) 
# Gini
df_jathi[2, "model"] <- "evenness"
df_jathi[2, "mean"] <- mean(params_gini$group_sd_caste)
df_jathi[2, "lower_HPDI"] <- round(HPDI(params_gini$group_sd_caste, prob = .90)[1], 2)
df_jathi[2, "upper_HPDI"] <- round(HPDI(params_gini$group_sd_caste, prob = .90)[2], 2) 
# Unique Gestures
df_jathi[3, "model"] <- "richness"
df_jathi[3, "mean"] <- mean(params_unique$group_sd_caste)
df_jathi[3, "lower_HPDI"] <- round(HPDI(params_unique$group_sd_caste, prob = .90)[1], 2)
df_jathi[3, "upper_HPDI"] <- round(HPDI(params_unique$group_sd_caste, prob = .90)[2], 2) 
# Density
df_jathi[4, "model"] <- "density"
df_jathi[4, "mean"] <- mean(params_density$group_sd_caste)
df_jathi[4, "lower_HPDI"] <- round(HPDI(params_density$group_sd_caste, prob = .90)[1], 2)
df_jathi[4, "upper_HPDI"] <- round(HPDI(params_density$group_sd_caste, prob = .90)[2], 2) 
# Entropy
df_jathi[5, "model"] <- "entropy"
df_jathi[5, "mean"] <- mean(params_entropy$group_sd_caste)
df_jathi[5, "lower_HPDI"] <- round(HPDI(params_entropy$group_sd_caste, prob = .90)[1], 2)
df_jathi[5, "upper_HPDI"] <- round(HPDI(params_entropy$group_sd_caste, prob = .90)[2], 2) 


#### Beta Age
df_beta_age <- data.frame(model = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)
# Canvas Size
df_beta_age[1, "model"] <- "canvas size"
df_beta_age[1, "mean"] <- mean(params_size$beta_age)
df_beta_age[1, "lower_HPDI"] <- round(HPDI(params_size$beta_age, prob = .90)[1], 2)
df_beta_age[1, "upper_HPDI"] <- round(HPDI(params_size$beta_age, prob = .90)[2], 2) 
# Gini
df_beta_age[2, "model"] <- "evenness"
df_beta_age[2, "mean"] <- mean(params_gini$beta_age)
df_beta_age[2, "lower_HPDI"] <- round(HPDI(params_gini$beta_age, prob = .90)[1], 2)
df_beta_age[2, "upper_HPDI"] <- round(HPDI(params_gini$beta_age, prob = .90)[2], 2) 
# Unique Gestures
df_beta_age[3, "model"] <- "richness"
df_beta_age[3, "mean"] <- mean(params_unique$beta_age)
df_beta_age[3, "lower_HPDI"] <- round(HPDI(params_unique$beta_age, prob = .90)[1], 2)
df_beta_age[3, "upper_HPDI"] <- round(HPDI(params_unique$beta_age, prob = .90)[2], 2) 
# Density
df_beta_age[4, "model"] <- "density"
df_beta_age[4, "mean"] <- mean(params_density$beta_age)
df_beta_age[4, "lower_HPDI"] <- round(HPDI(params_density$beta_age, prob = .90)[1], 2)
df_beta_age[4, "upper_HPDI"] <- round(HPDI(params_density$beta_age, prob = .90)[2], 2) 
# Entropy
df_beta_age[5, "model"] <- "entropy"
df_beta_age[5, "mean"] <- mean(params_entropy$beta_age)
df_beta_age[5, "lower_HPDI"] <- round(HPDI(params_entropy$beta_age, prob = .90)[1], 2)
df_beta_age[5, "upper_HPDI"] <- round(HPDI(params_entropy$beta_age, prob = .90)[2], 2) 


### Beta Duration
df_beta_duration <- data.frame(model = NA, mean = NA, lower_HPDI = NA, upper_HPDI = NA)
# Canvas Size
df_beta_duration[1, "model"] <- "canvas size"
df_beta_duration[1, "mean"] <- mean(params_size$beta_duration)
df_beta_duration[1, "lower_HPDI"] <- round(HPDI(params_size$beta_duration, prob = .90)[1], 2)
df_beta_duration[1, "upper_HPDI"] <- round(HPDI(params_size$beta_duration, prob = .90)[2], 2) 
# Gini
df_beta_duration[2, "model"] <- "evenness"
df_beta_duration[2, "mean"] <- mean(params_gini$beta_duration)
df_beta_duration[2, "lower_HPDI"] <- round(HPDI(params_gini$beta_duration, prob = .90)[1], 2)
df_beta_duration[2, "upper_HPDI"] <- round(HPDI(params_gini$beta_duration, prob = .90)[2], 2) 
# Unique Gestures
df_beta_duration[3, "model"] <- "richness"
df_beta_duration[3, "mean"] <- mean(params_unique$beta_duration)
df_beta_duration[3, "lower_HPDI"] <- round(HPDI(params_unique$beta_duration, prob = .90)[1], 2)
df_beta_duration[3, "upper_HPDI"] <- round(HPDI(params_unique$beta_duration, prob = .90)[2], 2) 
# Density
df_beta_duration[4, "model"] <- "density"
df_beta_duration[4, "mean"] <- mean(params_density$beta_duration)
df_beta_duration[4, "lower_HPDI"] <- round(HPDI(params_density$beta_duration, prob = .90)[1], 2)
df_beta_duration[4, "upper_HPDI"] <- round(HPDI(params_density$beta_duration, prob = .90)[2], 2) 
# Entropy
df_beta_duration[5, "model"] <- "entropy"
df_beta_duration[5, "mean"] <- mean(params_entropy$beta_duration)
df_beta_duration[5, "lower_HPDI"] <- round(HPDI(params_entropy$beta_duration, prob = .90)[1], 2)
df_beta_duration[5, "upper_HPDI"] <- round(HPDI(params_entropy$beta_duration, prob = .90)[2], 2) 
df_beta_age$model <- factor(df_beta_age$model,
                            levels = rev(c("canvas size", "evenness",
                                           "richness", "density", "entropy")))
df_beta_duration$model <- factor(df_beta_duration$model,
                                 levels = rev(c("canvas size", "evenness",
                                                "richness", "density", "entropy")))
df_jathi$model <- factor(df_jathi$model,
                         levels = rev(c("canvas size", "evenness",
                                        "richness", "density", "entropy")))
df_artist_sigma$model <- factor(df_artist_sigma$model,
                                levels = rev(c("canvas size", "evenness",
                                               "richness", "density", "entropy")))
df_beta_age$param <- "beta age"
df_beta_duration$param <- "beta practice duration"
df_jathi$param <- "caste sigma"
df_artist_sigma$param <- "artist sigma"
df_combined <- rbind(df_beta_age, df_beta_duration, df_artist_sigma, df_jathi)
df_combined$param <- factor(df_combined$param,
                            levels = c("beta age", "beta practice duration", "artist sigma", "caste sigma"))

df_combined$panel <- c(rep("beta coefficients", 10), rep("variation", 10))
df_combined$panel <- factor(df_combined$panel,
                            levels = c("beta coefficients", "variation"))

coefficient_plots <- ggplot(data = df_combined,
                            aes(x = model, y = mean, ymin = lower_HPDI, ymax = upper_HPDI)) +
  geom_pointrange(aes(color = param), size= 1, position = position_dodge(width = 1/2)) +
  #ylim(c(-0.5, 0.5)) +
  #ggtitle("beta age") +
  geom_hline(yintercept = 0, lty = 2, color = "red") +  # add a dotted line at x=2 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Model") + ylab("mean (90% HPDI)") +
  theme_bw() + # use a white background
  facet_grid(. ~ panel, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.title.x = element_text(hjust = 0.5),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        panel.spacing = unit(2, "lines"),
        legend.text=element_text(size=15),
        strip.text.x = element_text(size = 15)) +
  scale_color_brewer(palette="Paired")

png(filename = "output/coefficients_random_plot.png",
    width = 10, height = 4, units = "in", res = 800)
dev.off()
cairo_ps(file = "output/coefficients_random_plot.eps", 
         onefile = TRUE, fallback_resolution = 800, width = 10, height = 4)
print(coefficient_plots)
dev.off()

############################# Visual MCMC Diagnostics ################################
png(filename = "output/trace_unique_intercept.png",
    width = 8, height = 5, units = "in", res = 800)
traceplot(samples_unique, pars = c("alpha", "artist_sigma",
                                   "group_beta_caste_sd", 
                                   "beta_age", "beta_duration"))
dev.off()

png(filename = "output/rank_unique_intercept.png",
    width = 8, height = 5, units = "in", res = 800)
trankplot(samples_unique, pars = c("alpha", "artist_sigma",
                                   "group_beta_caste_sd", 
                                   "beta_age", "beta_duration"))
dev.off()

png(filename = "output/pairs_unique_intercept.png",
    width = 6, height = 6, units = "in", res = 800)
pairs(samples_unique, pars = c("alpha", "artist_sigma",
                               "group_beta_caste_sd", 
                               "beta_age", "beta_duration"))
dev.off()

png(filename = "output/trace_density_intercept.png",
    width = 8, height = 5, units = "in", res = 800)
traceplot(samples_density, pars = c("alpha", "artist_sigma",
                                    "group_beta_caste_sd", "beta_age",
                                    "beta_duration", "sigma"))
dev.off()

png(filename = "output/rank_density_intercept.png",
    width = 8, height = 5, units = "in", res = 800)
trankplot(samples_density, pars = c("alpha", "artist_sigma",
                                    "group_beta_caste_sd", "beta_age", "beta_duration", "sigma"))
dev.off()

png(filename = "output/pairs_density_intercept.png",
    width = 6, height = 6, units = "in", res = 800)
pairs(samples_density, pars = c("alpha", "artist_sigma",
                                "group_beta_caste_sd",
                                "beta_age", "beta_duration", "sigma"))
dev.off()

png(filename = "output/trace_size_intercept.png",
    width = 8, height = 5, units = "in", res = 800)
traceplot(samples_size, pars = c("alpha", "artist_sigma",
                                 "group_beta_caste_sd", 
                                 "phi",
                                 "beta_age", "beta_duration"))
dev.off()

png(filename = "output/rank_size_intercept.png",
    width = 8, height = 5, units = "in", res = 800)
trankplot(samples_size, pars = c("alpha", "artist_sigma",
                                 "group_beta_caste_sd", 
                                 "phi",
                                 "beta_age", "beta_duration"))
dev.off()

png(filename = "output/pairs_size_intercept.png",
    width = 6, height = 6, units = "in", res = 800)
pairs(samples_size, pars = c("alpha", "artist_sigma",
                             "group_beta_caste_sd", 
                             "phi",
                             "beta_age", "beta_duration"))
dev.off()

png(filename = "output/trace_gini_intercept.png",
    width = 8, height = 5, units = "in", res = 800)
traceplot(samples_gini, pars = c("alpha", "artist_sigma",
                                 "group_beta_caste_sd", 
                                 "sigma",
                                 "beta_age", "beta_duration"))
dev.off()

png(filename = "output/rank_gini_intercept.png",
    width = 8, height = 5, units = "in", res = 800)
trankplot(samples_gini, pars = c("alpha", "artist_sigma",
                                 "group_beta_caste_sd", 
                                 "sigma",
                                 "beta_age", "beta_duration"))
dev.off()

png(filename = "output/pairs_gini_intercept.png",
    width = 6, height = 6, units = "in", res = 800)
pairs(samples_gini, pars = c("alpha", "artist_sigma",
                             "group_beta_caste_sd", 
                             "sigma",
                             "beta_age", "beta_duration"))
dev.off()

png(filename = "output/trace_entropy_intercept.png",
    width = 8, height = 5, units = "in", res = 800)
traceplot(samples_entropy, pars = c("alpha", "artist_sigma",
                                    "group_beta_caste_sd", 
                                    "sigma",
                                    "beta_age", "beta_duration"))
dev.off()

png(filename = "output/rank_entropy_intercept.png",
    width = 8, height = 5, units = "in", res = 800)
trankplot(samples_entropy, pars = c("alpha", "artist_sigma",
                                    "group_beta_caste_sd", 
                                    "sigma",
                                    "beta_age", "beta_duration"))
dev.off()

png(filename = "output/pairs_entropy_intercept.png",
    width = 6, height = 6, units = "in", res = 800)
pairs(samples_entropy, pars = c("alpha", "artist_sigma",
                                "group_beta_caste_sd", 
                                "sigma",
                                "beta_age", "beta_duration"))
dev.off()

