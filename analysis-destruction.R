# Load libraries
library(rstanarm)
library(RColorBrewer)

# colors
darkcols <- brewer.pal(6, "Dark2")

# Read data
data <- read.csv("Data/destructiondata.csv", header = T)
# Add total column
data$Total <- NA
for(i in 1:nrow(data)){
    data$Total[i] <- sum(data$Larvae[i], data$Pupae[i], data$Adults[i], na.rm = T)
}
# Remove odd replicates
## Too few individuals (OA.1, Density = 6, Rep = 3; OA.4, Density = 50, Rep = 2)
fewrows <- c(234, 372)
data <- data[-fewrows, ]
data <- subset(data, Week %in% c("AO.2", "AO.3", "AO.4", "AO.5", "AO.6", "AO.7", "OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"))
head(data)

# Set seed
SEED <- 20171208

###### Life Stages

# Day 22
day22 <- subset(data, Day == 22 & Total <= 70)

## AO Pupae vs Larvae
### Fixed effect: density + random intercept
stan.fit1 <- stan_glmer(cbind(Pupae, Larvae) ~ Density + (1 | Week), data = subset(day22, Type == "AO"), family = binomial("logit"), prior = normal(0, 1, scale = 1), prior_intercept = normal(0, 1, scale = 1000), seed = SEED, iter = 5000, chains = 4, cores = 4)
print(summary(stan.fit1), digits = 4)
# Save results
## Dimensions?
dim(as.matrix(stan.fit1))
write(as.matrix(stan.fit1), file = "Data/day22AOfit.csv")

## OA Pupae vs Larvae
### Fixed effect: density + random intercept
stan.fit2 <- stan_glmer(cbind(Pupae, Larvae) ~ Density + (1 | Week), data = subset(day22, Type == "OA"), family = binomial("logit"), prior = normal(0, 1, scale = 1), prior_intercept = normal(0, 1, scale = 100), seed = SEED, iter = 5000, chains = 4, cores = 4)
print(summary(stan.fit2), digits = 4)
# Save results
write(as.matrix(stan.fit2), file = "Data/day22OAfit.csv")

# Use models to predict
nd1 <- expand.grid(Density = c(0:150), Week = c("AO.2", "AO.3", "AO.4", "AO.5", "AO.6", "AO.7"), Pupae = 0, Larvae = 1000)
prednd1 <- posterior_predict(stan.fit1, newdata = nd1)
nd2 <- expand.grid(Density = c(0:150), Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"), Pupae = 0, Larvae = 1000)
prednd2 <- posterior_predict(stan.fit2, newdata = nd2)
# Dimensions?
dim(prednd1)
# Save predictions
write(prednd1, file = "Data/day22AOpredictions.csv")
write(prednd2, file = "Data/day22OApredictions.csv")

png(filename = "Day22.png", width = 1000)
# Summary of results
par(mfrow = c(1, 2))
# AO
plot(NA, xlim = c(0, 150), ylim = c(0, 1), xaxt = "n", yaxt = "n", bty = "n", cex.lab = 1.3, xlab = "Density", ylab = "Proportion in older stage")
axis(1, seq(0, 150, 30), tcl = -.5, pos = 0)
axis(1, seq(0, 150, 10), tcl = -.3, labels = F, pos = 0)
axis(2, seq(0, 1, .1), tcl = -.5, las = 1)
for(i in 1:100){
    points(c(prednd1[i, 1:151] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[1]))/ 255, alpha = .1))
    points(c(prednd1[i, 152:302] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[2]))/ 255, alpha = .1))
    points(c(prednd1[i, 303:453] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[3]))/ 255, alpha = .1))
    points(c(prednd1[i, 454:604] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[4]))/ 255, alpha = .1))
    points(c(prednd1[i, 605:755] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[5]))/ 255, alpha = .1))
    points(c(prednd1[i, 756:906] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[6]))/ 255, alpha = .1))
}
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.7"), col = darkcols[6], pch = 16, cex = 1.2)
mtext(side = 3, c("(a) Day 22, AO"), adj = 0, cex = 1.3)
# OA
plot(NA, xlim = c(0, 150), ylim = c(0.5, 1), xaxt = "n", yaxt = "n", bty = "n", cex.lab = 1.3, xlab = "Density", ylab = "")
axis(1, seq(0, 150, 30), tcl = -.5, pos = 0.5)
axis(1, seq(0, 150, 10), tcl = -.3, labels = F, pos = 0.5)
axis(2, seq(0.5, 1, .1), tcl = -.5, las = 1)
for(i in 1:100){
    points(c(prednd2[i, 1:151] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[1]))/ 255, alpha = .1))
    points(c(prednd2[i, 152:302] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[2]))/ 255, alpha = .1))
    points(c(prednd2[i, 303:453] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[3]))/ 255, alpha = .1))
    points(c(prednd2[i, 454:604] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[4]))/ 255, alpha = .1))
    points(c(prednd2[i, 605:755] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[5]))/ 255, alpha = .1))
    points(c(prednd2[i, 756:906] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[6]))/ 255, alpha = .1))
}
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.7"), col = darkcols[6], pch = 16, cex = 1.2)
legend("bottomright", c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5", "Block 6"), pch = 16, col = darkcols, inset = .05)
mtext(side = 3, c("(b) Day 22, OA"), adj = 0, cex = 1.3)
dev.off()

# Day 29
day29 <- subset(data, Day == 29 & Total <= 70)

## AO Pupae vs Adults
### Fixed effect: density + random intercept
stan.fit3 <- stan_glmer(cbind(Adults, Pupae) ~ Density + (1 | Week), data = subset(day29, Type == "AO"), family = binomial("logit"), prior = normal(0, 1, scale = 1), prior_intercept = normal(0, 1, scale = 1000), seed = SEED, iter = 5000, chains = 4, cores = 4)
print(summary(stan.fit3), digits = 4)
# Save results
write(as.matrix(stan.fit3), file = "Data/day29AOfit.csv")

## OA Pupae vs Larvae
### Fixed effect: density + random intercept
stan.fit4 <- stan_glmer(cbind(Adults, Pupae) ~ Density + (1 | Week), data = subset(day29, Type == "OA"), family = binomial("logit"), prior = normal(0, 1, scale = 1), prior_intercept = normal(0, 1, scale = 1000), seed = SEED, iter = 5000, chains = 4, cores = 4)
print(summary(stan.fit4), digits = 4)
# Save results
write(as.matrix(stan.fit4), file = "Data/day29OAfit.csv")

# Predict
nd2 <- expand.grid(Density = c(0:150), Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"), Adults = 0, Pupae = 1000)
prednd2 <- posterior_predict(stan.fit4, newdata = nd2)
# Save
write(prednd2, file = "Data/day29OApredictions.csv")

png(filename = "Day29.png", width = 1000)
# Summary of results
par(mfrow = c(1, 2))
# AO
plot(NA, xlim = c(0, 150), ylim = c(0, 1), xaxt = "n", yaxt = "n", bty = "n", cex.lab = 1.3, xlab = "Density", ylab = "Proportion in older stage")
axis(1, seq(0, 150, 30), tcl = -.5, pos = 0)
axis(1, seq(0, 150, 10), tcl = -.3, labels = F, pos = 0)
axis(2, seq(0, 1, .1), tcl = -.5, las = 1)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.7"), col = darkcols[6], pch = 16, cex = 1.2)
mtext(side = 3, c("(a) Day 29, AO"), adj = 0, cex = 1.3)
# OA
plot(NA, xlim = c(0, 150), ylim = c(0.5, 1), xaxt = "n", yaxt = "n", bty = "n", cex.lab = 1.3, xlab = "Density", ylab = "")
axis(1, seq(0, 150, 30), tcl = -.5, pos = .5)
axis(1, seq(0, 150, 10), tcl = -.3, labels = F, pos = 0.5)
axis(2, seq(0.5, 1, .1), tcl = -.5, las = 1)
for(i in 1:100){
    points(c(prednd2[i, 1:151] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[1]))/ 255, alpha = .1))
    points(c(prednd2[i, 152:302] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[2]))/ 255, alpha = .1))
    points(c(prednd2[i, 303:453] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[3]))/ 255, alpha = .1))
    points(c(prednd2[i, 454:604] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[4]))/ 255, alpha = .1))
    points(c(prednd2[i, 605:755] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[5]))/ 255, alpha = .1))
    points(c(prednd2[i, 756:906] / 1000) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[6]))/ 255, alpha = .1))
}
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.7"), col = darkcols[6], pch = 16, cex = 1.2)
legend("bottomright", c("Block 1", "Block 2", "Block 3", "Block 4", "Block 5", "Block 6"), pch = 16, col = darkcols, inset = .05)
mtext(side = 3, c("(b) Day 29, OA"), adj = 0, cex = 1.3)
dev.off()


## ###### Hatched versus Unhatched
## # Subset day 22 data for hatched versus unhatched test (and remove odd replicates)
## day22 <- subset(data, Day == 22 & Total <= 50)
## # `Hatched` is number of either pupae or larvae
## day22$Hatched <- day22$Larvae + day22$Pupae
## # `Unhatched` is 50 - Hatched
## day22$Unhatched <- 50 - day22$Hatched

## ## AO hatched vs unhatched
## # Let's try random intercept only
## stan.fit1 <- stan_glmer(cbind(Hatched, Unhatched) ~ (1 | Week), data = subset(day22, Type == "AO"), family = binomial("logit"), prior_intercept = normal(0, 1, scale = 10), seed = SEED, iter = 5000, control = list(adapt_delta = 0.99), chains = 4, cores = 4)
## print(summary(stan.fit1), digits = 4)
## ### Fixed effect: density + random intercept
## stan.fit2 <- stan_glmer(cbind(Hatched, Unhatched) ~ Density + (1 | Week), data = subset(day22, Type == "AO"), family = binomial("logit"), prior = normal(0, 1, scale = 1), prior_intercept = normal(0, 1, scale = 1000), seed = SEED, iter = 5000, control = list(adapt_delta = 0.99), chains = 4, cores = 4)
## print(summary(stan.fit2), digits = 4)
## # And compare models using leave one out package
## model1 <- loo(stan.fit1, cores = 4)
## model2 <- loo(stan.fit2, cores = 4)
## # Negative values mean first model is better
## print(compare(model1, model2), digits = 4)

## ## OA hatched vs unhatched
## # Let's try random intercept only
## stan.fit1 <- stan_glmer(cbind(Hatched, Unhatched) ~ (1 | Week), data = subset(day22, Type == "OA"), family = binomial("logit"), prior_intercept = normal(0, 1, scale = 10), seed = SEED, iter = 5000, control = list(adapt_delta = 0.99), chains = 4, cores = 4)
## print(summary(stan.fit1), digits = 4)
## ### Fixed effect: density + random intercept
## stan.fit2 <- stan_glmer(cbind(Hatched, Unhatched) ~ Density + (1 | Week), data = subset(day22, Type == "OA"), family = binomial("logit"), prior = normal(0, 1, scale = 1), prior_intercept = normal(0, 1, scale = 1000), seed = SEED, iter = 5000, control = list(adapt_delta = 0.99), chains = 4, cores = 4)
## print(summary(stan.fit2), digits = 4)
## # And compare models using leave one out package
## model1 <- loo(stan.fit1, k_threshold = 0.7, cores = 4)
## model2 <- loo(stan.fit2, k_threshold = 0.7, cores = 4)
## # Negative values mean first model is better
## print(compare(model1, model2), digits = 4)

## ## End result is that Density is not significant and random intercept model is better for AO and OA
