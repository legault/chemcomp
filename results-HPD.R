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
## # b[(Intercept) Week:AO.2]
## credint(eggAO[, 3])
## # b[(Intercept) Week:AO.3]
## credint(eggAO[, 4])
## # b[(Intercept) Week:AO.4]
## credint(eggAO[, 5])
## # b[(Intercept) Week:AO.5]
## credint(eggAO[, 6])
## # b[(Intercept) Week:AO.6]
## credint(eggAO[, 7])
## # b[(Intercept) Week:AO.7]
## credint(eggAO[, 8])


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
## # b[(Intercept) Week:OA.2]
## credint(eggOA[, 3])
## # b[(Intercept) Week:OA.3]
## credint(eggOA[, 4])
## # b[(Intercept) Week:OA.4]
## credint(eggOA[, 5])
## # b[(Intercept) Week:OA.5]
## credint(eggOA[, 6])
## # b[(Intercept) Week:OA.6]
## credint(eggOA[, 7])
## # b[(Intercept) Week:OA.7]
## credint(eggOA[, 8])


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
## # b[(Intercept) Week:AO.2]
## credint(day22AO[, 3])
## # b[(Intercept) Week:AO.3]
## credint(day22AO[, 4])
## # b[(Intercept) Week:AO.4]
## credint(day22AO[, 5])
## # b[(Intercept) Week:AO.5]
## credint(day22AO[, 6])
## # b[(Intercept) Week:AO.6]
## credint(day22AO[, 7])
## # b[(Intercept) Week:AO.7]
## credint(day22AO[, 8])

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
## # b[(Intercept) Week:OA.2]
## credint(day22OA[, 3])
## # b[(Intercept) Week:OA.3]
## credint(day22OA[, 4])
## # b[(Intercept) Week:OA.4]
## credint(day22OA[, 5])
## # b[(Intercept) Week:OA.5]
## credint(day22OA[, 6])
## # b[(Intercept) Week:OA.6]
## credint(day22OA[, 7])
## # b[(Intercept) Week:OA.7]
## credint(day22OA[, 8])

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
## # b[(Intercept) Week:AO.2]
## credint(day29AO[, 3])
## # b[(Intercept) Week:AO.3]
## credint(day29AO[, 4])
## # b[(Intercept) Week:AO.4]
## credint(day29AO[, 5])
## # b[(Intercept) Week:AO.5]
## credint(day29AO[, 6])
## # b[(Intercept) Week:AO.6]
## credint(day29AO[, 7])
## # b[(Intercept) Week:AO.7]
## credint(day29AO[, 8])

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
## # b[(Intercept) Week:OA.2]
## credint(day29OA[, 3])
## # b[(Intercept) Week:OA.3]
## credint(day29OA[, 4])
## # b[(Intercept) Week:OA.4]
## credint(day29OA[, 5])
## # b[(Intercept) Week:OA.5]
## credint(day29OA[, 6])
## # b[(Intercept) Week:OA.6]
## credint(day29OA[, 7])
## # b[(Intercept) Week:OA.7]
## credint(day29OA[, 8])


