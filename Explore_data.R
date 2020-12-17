rm(list = ls())
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggridges)
library(kolam)
library(plot3D)

dir.create(path = "output/")

gini <- function(x){
  n <- length(x)
  D.2 <- sapply(1:n, function(z) sum(abs(x[z] - x)))
  TD.2 <- sum(D.2)
  TI.2 <- 2*(n-1)*sum(x)
  TD.2/TI.2
}


shannon <- function(p){
  -sum(p*log(p))
}


df <- read.csv(file = "data/kolam_data_share.csv",
               stringsAsFactors = FALSE)
colnames(df)[colnames(df) == 'pulli'] <- "size"

# Create path for the figures
dir.create(path = "output/")
############ Example Kolams with sequences ############
png(filename = "output/kolam_transcription.png",
    width = 10, height = 7, units = "in", res = 1000)
par(mfrow = c(1, 2), mar = c(7, 0, 0, 2))
# Kolam 1
plotLoop(c("o4", "o1", "o4", "o1", "o4", "o1", "o4", "o1"))
mtext(text = "sequence: 
o4 o1 o4 o1
o4 o1 o4 o1", side = 1, line = 2, cex = 1.1)
# Kolam 2
plotLoop(c("o4", "o1", "o3", "o2", "o3", "o2", "o3", "o2", "o3", "o1"),
         xStart = 2.0, yStart = 1.5, headingStart = 45)
plotLoop(c("o2", "o1", "o2", "o1", "o2", "o1", "o2", "o1"),
         xStart = 1.5, yStart = 1.0, headingStart = 135, add = TRUE,
         col = "black")
mtext(text = paste("sequence 1:
o4 o1 o3 o2 o3 o2 o3 o2 o3 o1"), line = 2, side = 1.2)
mtext(text = paste("sequence 2:
o2 o1 o2 o1 o2 o1 o2 o1"), line = 4, side = 1.2, col = "black")
dev.off()


########## Examples of Kolams with different Entropy, Sequence Length, Unique Gestures, Gini-Coefficient ##########
# Make dataframe of all the yamls
kolam_combined <- data.frame()
yaml_files <- list.files("data/",
                         pattern = "*.yaml", full.names = TRUE)
for (i in 1:length(yaml_files)) {
  kolam_combined <- rbind(kolam_combined, data.frame(read_yaml(file = yaml_files[i]),
                                                     stringsAsFactors = FALSE))
}
colnames(kolam_combined)[colnames(kolam_combined) == 'pulli'] <- "size"
# Compute indices
for (i in 1:nrow(kolam_combined)) {
  sequence <- extract_sequence(kolam_combined[i, "sequence"])
  p <- table(sequence)/sum(table(sequence))
  kolam_combined[i, "entropy"] <- round(shannon(p), 2)
}
kolam_combined$entropy_exponentiated <- exp(kolam_combined$entropy)
kolam_combined$total_gestures <- unlist(lapply(kolam_combined$sequence,
                                               function(x){length(extract_sequence(x))}))
# Compute Density
kolam_combined$density <- kolam_combined$total_gestures/(kolam_combined$size^2)
# Compute unique gestures
kolam_combined$sequence %>% gsub("\\[|\\]", "", .) %>%
  gsub("\"", "", .) %>% gsub("^ ", "", .) %>%
  strsplit(" ") %>% lapply(function(z) length(unique(z))) %>%
  unlist() -> kolam_combined$n_unique_gestures
# Compute Gini
kolam_combined$sequence %>% gsub("\\[|\\]", "", .) %>%
  gsub("\"", "", .) %>% gsub("^ ", "", .) %>%
  strsplit(" ") %>% lapply(function(z) gini(table(z))) %>%
  unlist() -> kolam_combined$gesture_gini

# Plot
postscript(file = "output/kolam_examples.eps",
           onefile = TRUE, horizontal = FALSE, width = 10, height = 5)

#png(filename = "output/kolam_examples.png", width = 6, height = 8, units = "in", res = 800)
par(mfrow = c(2, 2), oma = c(1, 0, 0, 0), mar = c(9, 0, 0, 0))
# Kolam 1
kolam1 <- loadKolam(read_yaml(file = "data/kolam1.yaml"))
plotKolam(kolam1, arrow = FALSE, loop_col = FALSE, cex = 0.2)
mtext(paste("Entropy = ",
            round(kolam_combined[which(kolam_combined$png_hash == kolam1$hash), "entropy"], 2)),
      side = 1, line = 0.5, cex = 1.3)
mtext(paste("Total Gestures = ",
            kolam_combined[which(kolam_combined$png_hash == kolam1$hash), "total_gestures"]),
      side = 1, line = 2, cex = 1.3)
mtext(paste("Richness = ",
            kolam_combined[which(kolam_combined$png_hash == kolam1$hash), "n_unique_gestures"]),
      side = 1, line = 3.5, cex = 1.3)
mtext(paste("Evenness = ",
            1-kolam_combined[which(kolam_combined$png_hash == kolam1$hash), "gesture_gini"]),
      side = 1, line = 5, cex = 1.3)
mtext(paste("Density = ",
            round(kolam_combined[which(kolam_combined$png_hash == kolam1$hash), "density"], 2)),
      side = 1, line = 6.5, cex = 1.3)
mtext(paste("Canvas Size = ",
            kolam1$pulli),
      side = 1, line = 8, cex = 1.3)
# Kolam 4
kolam4 <- loadKolam(read_yaml(file = "data/kolam4.yaml"))
plotKolam(kolam4, arrow = FALSE, loop_col = FALSE, cex = 0.2)
mtext(paste("Entropy = ",
            round(kolam_combined[which(kolam_combined$png_hash == kolam4$hash), "entropy"], 2)),
      side = 1, line = 0.5, cex = 1.3)
mtext(paste("Total Gestures = ",
            kolam_combined[which(kolam_combined$png_hash == kolam4$hash), "total_gestures"]),
      side = 1, line = 2, cex = 1.3)
mtext(paste("Richness = ",
            kolam_combined[which(kolam_combined$png_hash == kolam4$hash), "n_unique_gestures"]),
      side = 1, line = 3.5, cex = 1.3)
mtext(paste("Evenness = ",
            1-round(kolam_combined[which(kolam_combined$png_hash == kolam4$hash), "gesture_gini"], 2)),
      side = 1, line = 5, cex = 1.3)
mtext(paste("Density = ",
            round(kolam_combined[which(kolam_combined$png_hash == kolam4$hash), "density"], 2)),
      side = 1, line = 6.5, cex = 1.3)
mtext(paste("Canvas Size = ",
            kolam4$pulli),
      side = 1, line = 8, cex = 1.3)
# Kolam 3
kolam3 <- loadKolam(read_yaml(file = "data/kolam3.yaml"))
plotKolam(kolam3, arrow = FALSE, loop_col = FALSE, cex = 0.2)
mtext(paste("Entropy = ",
            round(kolam_combined[which(kolam_combined$png_hash == kolam3$hash), "entropy"], 2)),
      side = 1, line = 0.5, cex = 1.3)
mtext(paste("Total Gestures = ",
            kolam_combined[which(kolam_combined$png_hash == kolam3$hash), "total_gestures"]),
      side = 1, line = 2, cex = 1.3)
mtext(paste("Richness = ",
            kolam_combined[which(kolam_combined$png_hash == kolam3$hash), "n_unique_gestures"]),
      side = 1, line = 3.5, cex = 1.3)
mtext(paste("Evenness = ",
            1-round(kolam_combined[which(kolam_combined$png_hash == kolam3$hash), "gesture_gini"], 2)),
      side = 1, line = 5, cex = 1.3)
mtext(paste("Density = ",
            round(kolam_combined[which(kolam_combined$png_hash == kolam3$hash), "density"], 2)),
      side = 1, line = 6.5, cex = 1.3)
mtext(paste("Canvas Size = ",
            kolam3$pulli),
      side = 1, line = 8, cex = 1.3)
# Kolam 2
kolam2 <- loadKolam(read_yaml(file = "data/kolam2.yaml"))
plotKolam(kolam2, arrow = FALSE, loop_col = FALSE, cex = 0.2)
mtext(paste("Entropy = ",
            round(kolam_combined[which(kolam_combined$png_hash == kolam2$hash), "entropy"], 2)),
      side = 1, line = 0.5, cex = 1.3)
mtext(paste("Total Gestures = ",
            kolam_combined[which(kolam_combined$png_hash == kolam2$hash), "total_gestures"]),
      side = 1, line = 2, cex = 1.3)
mtext(paste("Richness = ",
            kolam_combined[which(kolam_combined$png_hash == kolam2$hash), "n_unique_gestures"]),
      side = 1, line = 3.5, cex = 1.3)
mtext(paste("Evenness = ",
            1-round(kolam_combined[which(kolam_combined$png_hash == kolam2$hash), "gesture_gini"], 2)),
      side = 1, line = 5, cex = 1.3)
mtext(paste("Density = ",
            round(kolam_combined[which(kolam_combined$png_hash == kolam2$hash), "density"], 2)),
      side = 1, line = 6.5, cex = 1.3)
mtext(paste("Canvas Size = ",
            kolam2$pulli),
      side = 1, line = 8, cex = 1.3)
dev.off()

#########################  evenness richness tradeoff ####################
source(file = "helpers/functions.R")
set.seed(152)

df <- read.csv("data/kolam_data_share.csv", stringsAsFactors = FALSE)

png("output/evenness_richness_tradeoff.jpeg", res = 800,
     height = 5, width = 5, units = "in")

par(mar = c(5, 4.1, 1, 1))
# brighter #9ECAE1 #C6DBEF
plot(jitter(df$n_unique_gestures), 1 - df$gesture_gini,
     col = col_alpha("#9ECAE1", 0.5), pch = 20, xlim = c(2, 15), ylim = c(0, 1),
     ylab = "Evenness", xlab = "Richness")

hs <- log(2:15)
for (i in 1:length(hs)) curve(1 - g_H_max(H = hs[i], x),
                              add = TRUE, lty = 5, lwd = 1.5, col = "grey55")

size <- 2:25
# mean Gini Index & Unique Number of Gestures by canvas size
x <- tapply(df$n_unique_gestures, df$size,  function(z) mean(z, na.rm = TRUE))
y <- tapply(df$gesture_gini, df$size, function(z) mean(z, na.rm = TRUE))

dat <- data.frame(x, 1 - y)
colnames(dat) <- c("x", "y")
# kernelgon(dat, border = NA, col = col_alpha(blues[1], 0.5), prob = 0.99)
kernelgon(dat, border = NA, col = col_alpha(oranges[2], 0.5), prob = 0.90)
kernelgon(dat, border = NA, col = col_alpha(oranges[4], 0.5), prob = 0.75)
kernelgon(dat, border = NA, col = col_alpha(oranges[5], 0.5), prob = 0.50)

# Gradient Text
text(15, 0.07, round(hs[1], 1), cex = 0.8, col = "grey55")
text(15, 0.12, round(hs[2], 1), cex = 0.8, col = "grey55")
text(15, 0.16, round(hs[3], 1), cex = 0.8, col = "grey55")
text(15, 0.21, round(hs[4], 1), cex = 0.8, col = "grey55")
text(15, 0.26, round(hs[5], 1), cex = 0.8, col = "grey55")
text(15, 0.32, round(hs[6], 1), cex = 0.8, col = "grey55")
text(15, 0.38, round(hs[7], 1), cex = 0.8, col = "grey55")
text(15, 0.44, round(hs[8], 1), cex = 0.8, col = "grey55")
text(15, 0.5, round(hs[9], 1), cex = 0.8, col = "grey55")
text(15, 0.58, round(hs[10], 1), cex = 0.8, col = "grey55")
text(15, 0.65, round(hs[11], 1), cex = 0.8, col = "grey55")
text(15, 0.75, round(hs[12], 1), cex = 0.8, col = "grey55")
text(15, 0.87, round(hs[12], 1), cex = 0.8, col = "grey55")


# Divide data into 4 size categories
df$size_cat <- case_when(
  df$size <= 4 ~ 1,
  df$size %in% 5:6 ~ 2,
  df$size %in% 7:8 ~ 3,
  df$size %in% 9:11 ~ 4,
  df$size >= 13 ~ 5
)

size <- 1:5
x <- tapply(df$n_unique_gestures, df$size_cat,  function(z) mean(z, na.rm = TRUE))
y <- tapply(df$gesture_gini, df$size_cat, function(z) mean(z, na.rm = TRUE))
points(x, 1 - y, type = "b", col = "black", cex = 1:5)

dev.off()