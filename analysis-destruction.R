# Load libraries
library(rstanarm)
# Set seed
SEED <- 20171208

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

########### Day 22
day22 <- subset(data, Day == 22 & Total < 70)

## AO Pupae vs Larvae
### Fixed effect: density + random intercept
stan.fit1 <- stan_glmer(cbind(Pupae, Total - Pupae) ~ Density + (1 | Week),
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
write(as.matrix(stan.fit1), file = "Data/day22AO-fit.csv")

## OA Pupae vs Larvae
### Fixed effect: density + random intercept
stan.fit2 <- stan_glmer(cbind(Pupae, Total - Pupae) ~ Density + (1 | Week),
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
write(as.matrix(stan.fit2), file = "Data/day22OA-fit.csv")

# Use models to predict
## Obtain posterior draws of linear predictor removing the stock id random effect
prednd1 <- posterior_linpred(stan.fit1, re.form = ~0, transform = TRUE, newdata = data.frame(Density = c(0:150)))
prednd2 <- posterior_linpred(stan.fit2, re.form = ~0, transform = TRUE, newdata = data.frame(Density = c(0:150)))
# Save predictions
write(prednd1, file = "Data/day22AO-predictions.csv")
write(prednd2, file = "Data/day22OA-predictions.csv")


########### Day 29
day29 <- subset(data, Day == 29 & Total < 70)

## AO Pupae vs Adults
### Fixed effect: density + random intercept
stan.fit3 <- stan_glmer(cbind(Adults, Total - Adults) ~ Density + (1 | Week),
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
write(as.matrix(stan.fit3), file = "Data/day29AO-fit.csv")

## OA Pupae vs Larvae
### Fixed effect: density + random intercept
stan.fit4 <- stan_glmer(cbind(Adults, Total - Adults) ~ Density + (1 | Week),
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
write(as.matrix(stan.fit4), file = "Data/day29OA-fit.csv")

# Use models to predict
## Obtain posterior draws of linear predictor removing the stock id random effect
prednd3 <- posterior_linpred(stan.fit3, re.form = ~0, transform = TRUE, newdata = data.frame(Density = c(0:150)))
prednd4 <- posterior_linpred(stan.fit4, re.form = ~0, transform = TRUE, newdata = data.frame(Density = c(0:150)))
# Save predictions
write(prednd3, file = "Data/day29AO-predictions.csv")
write(prednd4, file = "Data/day29OA-predictions.csv")
