rm(list = ls())
source("helpers/functions.R")

dir.create(path = "output/")

my_u <- c(2, 3, 5, 8, 12)

d <- sim_gini_entropy(my_u, n_rows = 100000, beta = 1.5)

n_unique_color <- data_gradient(2:12, colors = brewer.pal(9, "Set1")[1:length(my_u)])
d$n_unique_color <- n_unique_color[d$n - 1]
d$entropy_color <- data_gradient(d$entropy,
                                 colors = rev(brewer.pal(9, "Blues"))[1:length(my_u)])

d$exp_entropy <- exp(d$entropy)
d$even <- (d$exp_entropy - 1)/(d$n - 1)
d$gamma <- 1 - d$even

png("output/entropy_envelope.png", res = 300, height = 6, width = 7, units = "in")
plot(1, 1, xlim = c(0, 1), ylim = log(c(1, max(my_u))), type = "n",
     ylab = "Entropy", xlab = "Evenness", main = "")
hs <- 2:15
for (i in my_u) {
  tar <- which(d$n == i)
  points(1 - d$gini[tar], d$entropy[tar],
         col = col_alpha(d$n_unique_color[tar], 0.1), pch = 20)
}
text(.9, .8, "n = 2")
text(.9, 1.2, "n = 3")
text(.9, 1.7, "n = 5")
text(.9, 2.15, "n = 8")
text(.7, 2.48, "n = 12")
for (n in my_u) {
  g <- seq(0.001, 0.9999, by = 0.005)
  maxent <- sapply(g, H_max, n = n, exp = FALSE)
  points(1 - g, maxent, type = "l", col = "black", lwd = 2)
}
dev.off()



