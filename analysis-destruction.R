# Load libraries
library(rstanarm)

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
stan.fit1 <- stan_glmer(cbind(Pupae, Larvae) ~ Density + (1 | Week),
                        data = subset(day22, Type == "AO"),
                        family = binomial("logit"),
                        prior = normal(0, scale = 2.5),
                        prior_intercept = normal(0, scale = 10),
                        seed = SEED,
                        iter = 5000,
                        chains = 4,
                        cores = 4)
print(summary(stan.fit1), digits = 4)
# Save results
write(as.matrix(stan.fit1), file = "Data/day22AOfit.csv")

## OA Pupae vs Larvae
### Fixed effect: density + random intercept
stan.fit2 <- stan_glmer(cbind(Pupae, Larvae) ~ Density + (1 | Week),
                        data = subset(day22, Type == "OA"),
                        family = binomial("logit"),
                        prior = normal(0, scale = 2.5),
                        prior_intercept = normal(0, scale = 10),
                        seed = SEED,
                        iter = 5000,
                        chains = 4,
                        cores = 4)
print(summary(stan.fit2), digits = 4)
# Save results
write(as.matrix(stan.fit2), file = "Data/day22OAfit.csv")

# Use models to predict
nd1 <- expand.grid(Density = c(0:150), Week = c("AO.2", "AO.3", "AO.4", "AO.5", "AO.6", "AO.7"), Pupae = 0, Larvae = 1000)
prednd1 <- posterior_linpred(stan.fit1, newdata = nd1, transform = TRUE)
nd2 <- expand.grid(Density = c(0:150), Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"), Pupae = 0, Larvae = 1000)
prednd2 <- posterior_linpred(stan.fit2, newdata = nd2, transform = TRUE)
# Save predictions
write(prednd1, file = "Data/day22AOpredictions.csv")
write(prednd2, file = "Data/day22OApredictions.csv")


# Day 29
day29 <- subset(data, Day == 29 & Total <= 70)

## AO Pupae vs Adults
### Fixed effect: density + random intercept
stan.fit3 <- stan_glmer(cbind(Adults, Pupae) ~ Density + (1 | Week),
                        data = subset(day29, Type == "AO"),
                        family = binomial("logit"),
                        prior = normal(0, scale = 2.5),
                        prior_intercept = normal(0, scale = 10),
                        seed = SEED,
                        iter = 5000,
                        chains = 4,
                        cores = 4)
print(summary(stan.fit3), digits = 4)
# Save results
write(as.matrix(stan.fit3), file = "Data/day29AOfit.csv")

## OA Pupae vs Larvae
### Fixed effect: density + random intercept
stan.fit4 <- stan_glmer(cbind(Adults, Pupae) ~ Density + (1 | Week),
                        data = subset(day29, Type == "OA"),
                        family = binomial("logit"),
                        prior = normal(0, scale = 2.5),
                        prior_intercept = normal(0, scale = 10),
                        seed = SEED,
                        iter = 5000,
                        chains = 4,
                        cores = 4)
print(summary(stan.fit4), digits = 4)
# Save results
write(as.matrix(stan.fit4), file = "Data/day29OAfit.csv")

# Predict
nd2 <- expand.grid(Density = c(0:150), Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"), Adults = 0, Pupae = 1000)
prednd2 <- posterior_linpred(stan.fit4, newdata = nd2, transform = TRUE)
# Save
write(prednd2, file = "Data/day29OApredictions.csv")

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
