# Load libraries
library(rstanarm)
library(RColorBrewer)

# Read data
data <- read.csv("Data/eggdata.csv", header = T)
# Add total column
data$Total <- data$X50.weight + data$Remaining
data$Count <- NA
for(i in 1:nrow(data)){
    data$Count[i] <- data$Total[i] / (data$X50.weight[i] / 50)
}
data <- subset(data, Count > 0)
data$Count <- round(data$Count)
# Remove week1
data <- subset(data, Week %in% c("AO.2", "AO.3", "AO.4", "AO.5", "AO.6", "AO.7", "OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"))
head(data)
# Set seed
SEED <- 20171208

## AO egg count
# fixed + random intercept
stan.fit1 <- stan_glmer(Count ~ Density + (1 | Week), data = subset(data, Type == "AO"), family = neg_binomial_2, prior = normal(0, 1, scale = 1), prior_intercept = normal(0, 1, scale = 10000), seed = SEED, iter = 5000, chains = 4, cores = 4, control = list(adapt_delta = 0.99))
print(summary(stan.fit1), digits = 4)
# Dimensions?
dim(as.matrix(stan.fit1))
write(as.matrix(stan.fit1), file = "Data/eggAOfit.csv")

## OA egg count
# fixed + random intercept
stan.fit2 <- stan_glmer(Count ~ Density + (1 | Week), data = subset(data, Type == "OA"), family = neg_binomial_2, prior = normal(0, 1, scale = 1), prior_intercept = normal(0, 1, scale = 100000), seed = SEED, iter = 5000, chains = 4, cores = 4)
print(summary(stan.fit2), digits = 4)
write(as.matrix(stan.fit2), file = "Data/eggOAfit.csv")

# Use model to predict
nd2 <- expand.grid(Density = c(0:150), Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"))
prednd2 <- posterior_predict(stan.fit2, newdata = nd2)
# Dimensions?
dim(prednd2)
write(prednd2, file = "Data/eggOApredictions.csv")

# colors
darkcols <- brewer.pal(6, "Dark2")

png(filename = "Eggs.png", width = 1000)
# Summary of results
par(mfrow = c(1, 2))
# AO
plot(NA, xlim = c(0, 150), ylim = c(0, 400), xaxt = "n", yaxt = "n", bty = "n", cex.lab = 1.3, xlab = "Density", ylab = "Counts")
axis(1, seq(0, 150, 30), tcl = -.5, pos = 0)
axis(1, seq(0, 150, 10), tcl = -.3, labels = F, pos = 0)
axis(2, seq(0, 400, 100), tcl = -.5, las = 1)
points(Count ~ Density, data = subset(data, Week == "AO.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "AO.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "AO.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "AO.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "AO.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "AO.7"), col = darkcols[6], pch = 16, cex = 1.2)
mtext(side = 3, c("(a) Egg count, AO"), adj = 0, cex = 1.3)
# OA
plot(NA, xlim = c(0, 150), ylim = c(0, 400), xaxt = "n", yaxt = "n", bty = "n", cex.lab = 1.3, xlab = "Density", ylab = "")
axis(1, seq(0, 150, 30), tcl = -.5, pos = 0)
axis(1, seq(0, 150, 10), tcl = -.3, labels = F, pos = 0)
axis(2, seq(0, 400, 100), tcl = -.5, las = 1)
for(i in 1:200){
    points(c(prednd2[i, 1:151]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[1]))/ 255, alpha = .1))
    points(c(prednd2[i, 152:302]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[2]))/ 255, alpha = .1))
    points(c(prednd2[i, 303:453]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[3]))/ 255, alpha = .1))
    points(c(prednd2[i, 454:604]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[4]))/ 255, alpha = .1))
    points(c(prednd2[i, 605:755]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[5]))/ 255, alpha = .1))
    points(c(prednd2[i, 756:906]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[6]))/ 255, alpha = .1))
}
points(Count ~ Density, data = subset(data, Week == "OA.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "OA.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "OA.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "OA.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "OA.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "OA.7"), col = darkcols[6], pch = 16, cex = 1.2)
legend("bottomright", c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5", "Block 6"), pch = 16, col = darkcols, inset = .05)
mtext(side = 3, c("(b) Egg count, OA"), adj = 0, cex = 1.3)

###### Preserved for loo model comparison (done in a previous version)
## # Now compare models
## model1 <- loo(stan.fit1, cores = 4)
## model2 <- loo(stan.fit2, cores = 4)
## # Negative values mean first model is better
## print(compare(model1, model2), digits = 4)
