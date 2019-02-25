# Load libraries
library(rstanarm)

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
stan.fit1 <- stan_glmer(Count ~ Density + (1 | Week),
                        data = subset(data, Type == "AO"),
                        family = neg_binomial_2,
                        prior = normal(0, scale = 2.5),
                        prior_intercept = normal(0, scale = 10),
                        seed = SEED,
                        iter = 5000,
                        chains = 4,
                        cores = 4,
                        control = list(adapt_delta = 0.99))
print(summary(stan.fit1), digits = 4)
# Save posterior samples
write(as.matrix(stan.fit1), file = "Data/eggAOfit.csv")

## OA egg count
# fixed + random intercept
stan.fit2 <- stan_glmer(Count ~ Density + (1 | Week),
                        data = subset(data, Type == "OA"),
                        family = neg_binomial_2,
                        prior = normal(0, scale = 2.5),
                        prior_intercept = normal(0, scale = 10),
                        seed = SEED,
                        iter = 5000,
                        chains = 4,
                        cores = 4)
print(summary(stan.fit2), digits = 4)
# Save posterior samples
write(as.matrix(stan.fit2), file = "Data/eggOAfit.csv")

# Use model to predict
## Create new dataset
nd2 <- expand.grid(Density = c(0:150), Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"))
## Obtain posterior draws of linear predictor
prednd2 <- posterior_linpred(stan.fit2, transform = TRUE, newdata = nd2)
write(prednd2, file = "Data/eggOApredictions.csv")
