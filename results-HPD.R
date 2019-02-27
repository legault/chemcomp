# Function for estimating highest posterior density interval
# Written by: Brett Melbourne
credint <- function ( samp, prob = 0.95 ) {
    vals <- sort(samp)
    nsamp <- length(vals)
    gap <- max(1, min(nsamp - 1, round(nsamp * prob)))
    init <- 1:(nsamp - gap)
    inds <- which.min(vals[init + gap,drop=FALSE] - vals[init, drop=FALSE])
    ans <- cbind(lower=vals[inds], upper=vals[inds + gap], mean = mean(samp))
    return(ans)
}

# Egg AO fit
eggAO <- scan("Data/eggAO-fit.csv")
eggAO <- matrix(eggAO, ncol = 10, byrow = F)
head(eggAO)
# Intercept
mean(eggAO[, 1])
sd(eggAO[, 1])
credint(eggAO[, 1])
# Density
mean(eggAO[, 2])
sd(eggAO[, 2])
credint(eggAO[, 2])
# Dispersion
mean(eggAO[, 9])
sd(eggAO[, 9])
credint(eggAO[, 9])
# Sigma[Week:(Intercept),(Intercept)]
mean(eggAO[, 10])
sd(eggAO[, 10])
credint(eggAO[, 10])
# Make table with random intercepts (added to mean intercept and untransformed)
rand.intcpt <- data.frame(Week = c("AO.2", "AO.3", "AO.4", "AO.5", "AO.6", "AO.7"),
                          Intercept = c(mean(eggAO[, 3]),
                                        mean(eggAO[, 4]),
                                        mean(eggAO[, 5]),
                                        mean(eggAO[, 6]),
                                        mean(eggAO[, 7]),
                                        mean(eggAO[, 8])))
# Save table
write.csv(rand.intcpt, file = "Data/eggAO-rand.csv", row.names = FALSE)

# Egg OA fit
eggOA <- scan("Data/eggOA-fit.csv")
eggOA <- matrix(eggOA, ncol = 10, byrow = F)
head(eggOA)
# Intercept
mean(eggOA[, 1])
sd(eggOA[, 1])
credint(eggOA[, 1])
# Density
mean(eggOA[, 2])
sd(eggOA[, 2])
credint(eggOA[, 2])
# Dispersion
mean(eggOA[, 9])
sd(eggOA[, 9])
credint(eggOA[, 9])
# Sigma[Week:(Intercept),(Intercept)]
mean(eggOA[, 10])
sd(eggOA[, 10])
credint(eggOA[, 10])
# Make table with random intercepts (added to mean intercept and untransformed)
rand.intcpt <- data.frame(Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"),
                          Intercept = c(mean(eggOA[, 3]),
                                        mean(eggOA[, 4]),
                                        mean(eggOA[, 5]),
                                        mean(eggOA[, 6]),
                                        mean(eggOA[, 7]),
                                        mean(eggOA[, 8])))
# Save table
write.csv(rand.intcpt, file = "Data/eggOA-rand.csv", row.names = FALSE)

# Day 22 AO fit
day22AO <- scan("Data/day22AO-fit.csv")
day22AO <- matrix(day22AO, ncol = 9, byrow = F)
head(day22AO)
# Intercept
mean(day22AO[, 1])
sd(day22AO[, 1])
credint(day22AO[, 1])
# Density
mean(day22AO[, 2])
sd(day22AO[, 2])
credint(day22AO[, 2])
# Sigma[Week:(Intercept),(Intercept)]
mean(day22AO[, 9])
sd(day22AO[, 9])
credint(day22AO[, 9])
# Make table with random intercepts (added to mean intercept and untransformed)
rand.intcpt <- data.frame(Week = c("AO.2", "AO.3", "AO.4", "AO.5", "AO.6", "AO.7"),
                          Intercept = c(mean(day22AO[, 3]),
                                        mean(day22AO[, 4]),
                                        mean(day22AO[, 5]),
                                        mean(day22AO[, 6]),
                                        mean(day22AO[, 7]),
                                        mean(day22AO[, 8])))
# Save table
write.csv(rand.intcpt, file = "Data/day22AO-rand.csv", row.names = FALSE)

# Day 22 OA fit
day22OA <- scan("Data/day22OA-fit.csv")
day22OA <- matrix(day22OA, ncol = 9, byrow = F)
head(day22OA)
# Intercept
mean(day22OA[, 1])
sd(day22OA[, 1])
credint(day22OA[, 1])
# Density
mean(day22OA[, 2])
sd(day22OA[, 2])
credint(day22OA[, 2])
# Sigma[Week:(Intercept),(Intercept)]
mean(day22OA[, 9])
sd(day22OA[, 9])
credint(day22OA[, 9])
# Make table with random intercepts (added to mean intercept and untransformed)
rand.intcpt <- data.frame(Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"),
                          Intercept = c(mean(day22OA[, 3]),
                                        mean(day22OA[, 4]),
                                        mean(day22OA[, 5]),
                                        mean(day22OA[, 6]),
                                        mean(day22OA[, 7]),
                                        mean(day22OA[, 8])))
# Save table
write.csv(rand.intcpt, file = "Data/day22OA-rand.csv", row.names = FALSE)

# Day 29 AO fit
day29AO <- scan("Data/day29AO-fit.csv")
day29AO <- matrix(day29AO, ncol = 9, byrow = F)
head(day29AO)
# Intercept
mean(day29AO[, 1])
sd(day29AO[, 1])
credint(day29AO[, 1])
# Density
mean(day29AO[, 2])
sd(day29AO[, 2])
credint(day29AO[, 2])
# Sigma[Week:(Intercept),(Intercept)]
mean(day29AO[, 9])
sd(day29AO[, 9])
credint(day29AO[, 9])
# Make table with random intercepts (added to mean intercept and untransformed)
rand.intcpt <- data.frame(Week = c("AO.2", "AO.3", "AO.4", "AO.5", "AO.6", "AO.7"),
                          Intercept = c(mean(day29AO[, 3]),
                                        mean(day29AO[, 4]),
                                        mean(day29AO[, 5]),
                                        mean(day29AO[, 6]),
                                        mean(day29AO[, 7]),
                                        mean(day29AO[, 8])))
# Save table
write.csv(rand.intcpt, file = "Data/day29AO-rand.csv", row.names = FALSE)

# Day 29 OA fit
day29OA <- scan("Data/day29OA-fit.csv")
day29OA <- matrix(day29OA, ncol = 9, byrow = F)
head(day29OA)
# Intercept
mean(day29OA[, 1])
sd(day29OA[, 1])
credint(day29OA[, 1])
# Density
mean(day29OA[, 2])
sd(day29OA[, 2])
credint(day29OA[, 2])
# Sigma[Week:(Intercept),(Intercept)]
mean(day29OA[, 9])
sd(day29OA[, 9])
credint(day29OA[, 9])
# Make table with random intercepts (added to mean intercept and untransformed)
rand.intcpt <- data.frame(Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"),
                          Intercept = c(mean(day29OA[, 3]),
                                        mean(day29OA[, 4]),
                                        mean(day29OA[, 5]),
                                        mean(day29OA[, 6]),
                                        mean(day29OA[, 7]),
                                        mean(day29OA[, 8])))
# Save table
write.csv(rand.intcpt, file = "Data/day29OA-rand.csv", row.names = FALSE)

# Credible interval for model predictions
## Eggs
### Get predictions (AO)
pred1 <- scan("Data/eggAO-predictions.csv")
pred1 <- matrix(pred1, ncol = 151, byrow = F)
### Calculate credible interval
cred1 <- apply(pred1, MARGIN = 2, FUN = credint)
## Save results
write.table(cred1, file = "Data/eggAO-credint.csv", row.names = FALSE, col.names = FALSE, sep = ",")
### Get predictions (OA)
pred2 <- scan("Data/eggOA-predictions.csv")
pred2 <- matrix(pred2, ncol = 151, byrow = F)
### Calculate credible interval
cred2 <- apply(pred2, MARGIN = 2, FUN = credint)
### Save results
write.table(cred2, file = "Data/eggOA-credint.csv", row.names = FALSE, col.names = FALSE, sep = ",")

## Destruction, Day 22
### Get predictions (AO)
pred3 <- scan("Data/day22AO-predictions.csv")
pred3 <- matrix(pred3, ncol = 151, byrow = F)
### Calculate credible interval
cred3 <- apply(pred3, MARGIN = 2, FUN = credint)
## Save results
write.table(cred3, file = "Data/day22AO-credint.csv", row.names = FALSE, col.names = FALSE, sep = ",")
### Get predictions (OA)
pred4 <- scan("Data/day22OA-predictions.csv")
pred4 <- matrix(pred4, ncol = 151, byrow = F)
### Calculate credible interval
cred4 <- apply(pred4, MARGIN = 2, FUN = credint)
## Save results
write.table(cred4, file = "Data/day22OA-credint.csv", row.names = FALSE, col.names = FALSE, sep = ",")

## Destruction, Day 29
### Get predictions (AO)
pred5 <- scan("Data/day29AO-predictions.csv")
pred5 <- matrix(pred5, ncol = 151, byrow = F)
### Calculate credible interval
cred5 <- apply(pred5, MARGIN = 2, FUN = credint)
## Save results
write.table(cred5, file = "Data/day29AO-credint.csv", row.names = FALSE, col.names = FALSE, sep = ",")
### Get predictions (OA)
pred6 <- scan("Data/day29OA-predictions.csv")
pred6 <- matrix(pred6, ncol = 151, byrow = F)
### Calculate credible interval
cred6 <- apply(pred6, MARGIN = 2, FUN = credint)
## Save results
write.table(cred6, file = "Data/day29OA-credint.csv", row.names = FALSE, col.names = FALSE, sep = ",")

