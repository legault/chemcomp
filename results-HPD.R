# Function for estimating highest posterior density interval
# Written by: Brett Melbourne
credint <- function ( samp, prob = 0.95 ) {
    vals <- sort(samp)
    nsamp <- length(vals)
    gap <- max(1, min(nsamp - 1, round(nsamp * prob)))
    init <- 1:(nsamp - gap)
    inds <- which.min(vals[init + gap,drop=FALSE] - vals[init, drop=FALSE])
    ans <- cbind(lower=vals[inds], upper=vals[inds + gap])
    return(ans)
}

# Egg AO fit
eggAO <- scan("Data/eggAOfit.csv")
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
## # Make table with random intercepts (added to mean intercept and untransformed)
## rand.intcpt <- data.frame(Week = c("AO.2", "AO.3", "AO.4", "AO.5", "AO.6", "AO.7"),
##                           Intercept = c(exp(mean(eggAO[, 1]) + mean(eggAO[, 3])),
##                                         exp(mean(eggAO[, 1]) + mean(eggAO[, 4])),
##                                         exp(mean(eggAO[, 1]) + mean(eggAO[, 5])),
##                                         exp(mean(eggAO[, 1]) + mean(eggAO[, 6])),
##                                         exp(mean(eggAO[, 1]) + mean(eggAO[, 7])),
##                                         exp(mean(eggAO[, 1]) + mean(eggAO[, 8]))))
## # Save table
## write.csv(rand.intcpt, file = "Data/eggAO-rand.csv", row.names = FALSE)

# Egg OA fit
eggOA <- scan("Data/eggOAfit.csv")
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
## # Make table with random intercepts (added to mean intercept and untransformed)
## rand.intcpt <- data.frame(Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"),
##                           Intercept = c(exp(mean(eggOA[, 1]) + mean(eggOA[, 3])),
##                                         exp(mean(eggOA[, 1]) + mean(eggOA[, 4])),
##                                         exp(mean(eggOA[, 1]) + mean(eggOA[, 5])),
##                                         exp(mean(eggOA[, 1]) + mean(eggOA[, 6])),
##                                         exp(mean(eggOA[, 1]) + mean(eggOA[, 7])),
##                                         exp(mean(eggOA[, 1]) + mean(eggOA[, 8]))))
## # Save table
## write.csv(rand.intcpt, file = "Data/eggOA-rand.csv", row.names = FALSE)

# Day 22 AO fit
day22AO <- scan("Data/day22AOfit.csv")
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
## # Make table with random intercepts (added to mean intercept and untransformed)
## rand.intcpt <- data.frame(Week = c("AO.2", "AO.3", "AO.4", "AO.5", "AO.6", "AO.7"),
##                           Intercept = c(exp(mean(day22AO[, 1]) + mean(day22AO[, 3])),
##                                         exp(mean(day22AO[, 1]) + mean(day22AO[, 4])),
##                                         exp(mean(day22AO[, 1]) + mean(day22AO[, 5])),
##                                         exp(mean(day22AO[, 1]) + mean(day22AO[, 6])),
##                                         exp(mean(day22AO[, 1]) + mean(day22AO[, 7])),
##                                         exp(mean(day22AO[, 1]) + mean(day22AO[, 8]))))
## # Save table
## write.csv(rand.intcpt, file = "Data/day22AO-rand.csv", row.names = FALSE)

# Day 22 OA fit
day22OA <- scan("Data/day22OAfit.csv")
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
## # Make table with random intercepts (added to mean intercept and untransformed)
## rand.intcpt <- data.frame(Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"),
##                           Intercept = c(exp(mean(day22OA[, 1]) + mean(day22OA[, 3])),
##                                         exp(mean(day22OA[, 1]) + mean(day22OA[, 4])),
##                                         exp(mean(day22OA[, 1]) + mean(day22OA[, 5])),
##                                         exp(mean(day22OA[, 1]) + mean(day22OA[, 6])),
##                                         exp(mean(day22OA[, 1]) + mean(day22OA[, 7])),
##                                         exp(mean(day22OA[, 1]) + mean(day22OA[, 8]))))
## # Save table
## write.csv(rand.intcpt, file = "Data/day22OA-rand.csv", row.names = FALSE)

# Day 29 AO fit
day29AO <- scan("Data/day29AOfit.csv")
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
## # Make table with random intercepts (added to mean intercept and untransformed)
## rand.intcpt <- data.frame(Week = c("AO.2", "AO.3", "AO.4", "AO.5", "AO.6", "AO.7"),
##                           Intercept = c(exp(mean(day29AO[, 1]) + mean(day29AO[, 3])),
##                                         exp(mean(day29AO[, 1]) + mean(day29AO[, 4])),
##                                         exp(mean(day29AO[, 1]) + mean(day29AO[, 5])),
##                                         exp(mean(day29AO[, 1]) + mean(day29AO[, 6])),
##                                         exp(mean(day29AO[, 1]) + mean(day29AO[, 7])),
##                                         exp(mean(day29AO[, 1]) + mean(day29AO[, 8]))))
## # Save table
## write.csv(rand.intcpt, file = "Data/day29AO-rand.csv", row.names = FALSE)

# Day 29 OA fit
day29OA <- scan("Data/day29OAfit.csv")
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
## # Make table with random intercepts (added to mean intercept and untransformed)
## rand.intcpt <- data.frame(Week = c("OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7"),
##                           Intercept = c(exp(mean(day29OA[, 1]) + mean(day29OA[, 3])),
##                                         exp(mean(day29OA[, 1]) + mean(day29OA[, 4])),
##                                         exp(mean(day29OA[, 1]) + mean(day29OA[, 5])),
##                                         exp(mean(day29OA[, 1]) + mean(day29OA[, 6])),
##                                         exp(mean(day29OA[, 1]) + mean(day29OA[, 7])),
##                                         exp(mean(day29OA[, 1]) + mean(day29OA[, 8]))))
## # Save table
## write.csv(rand.intcpt, file = "Data/day29OA-rand.csv", row.names = FALSE)


