
# Create logit function
logit <- function(x){
    log(x / (1 - x))
}
# Create inverse logit function
logit.inv <- function(x){
    exp(x) / (exp(x) + 1)
}

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
data <- subset(data, Week %in% c("AO.2", "AO.3", "AO.4", "AO.5", "AO.6", "AO.7", "OA.2", "OA.3", "OA.4", "OA.5", "OA.6", "OA.7") & Total < 70)

# Read credible intervals
cred1 <- scan("Data/day22AO-credint.csv", sep = ",")
cred1 <- matrix(cred1, ncol = 151, byrow = TRUE)
cred2 <- scan("Data/day22OA-credint.csv", sep = ",")
cred2 <- matrix(cred2, ncol = 151, byrow = TRUE)
# Read random effects
rand1 <- read.csv("Data/day22AO-rand.csv", header = TRUE)
rand2 <- read.csv("Data/day22OA-rand.csv", header = TRUE)
## Combine
rand <- rbind(rand1, rand2)
## Create day22 subset and merge with rand
data22 <- merge(subset(data, Day == 22), rand)
# Read credible intervals
cred3 <- scan("Data/day29AO-credint.csv", sep = ",")
cred3 <- matrix(cred3, ncol = 151, byrow = TRUE)
cred4 <- scan("Data/day29OA-credint.csv", sep = ",")
cred4 <- matrix(cred4, ncol = 151, byrow = TRUE)
# Read random effects
rand3 <- read.csv("Data/day29AO-rand.csv", header = TRUE)
rand4 <- read.csv("Data/day29OA-rand.csv", header = TRUE)
## Combine
rand <- rbind(rand3, rand4)
## Create day29 subset and merge with rand
data29 <- merge(subset(data, Day == 29), rand)

## Create column for first proportion (Pupae / (Pupae + Larvae))
data22$prop <- data22$Pupae / data22$Total
## Create column for second proportion (Adults / (Adults + Pupae))
data29$prop <- data29$Adults / data29$Total
## Subtract random effect from y ("prop")
data22$prop.mod <- logit.inv(logit(data22$prop) - data22$Intercept)
### Adjust NaN values
data22[which(data22$prop.mod == "NaN"), ] <- 1
## Subtract random effect from y ("prop")
data29$prop.mod <- logit.inv(logit(data29$prop) - data29$Intercept)
data29[which(data29$prop.mod == "NaN"), ] <- 1

png(filename = "Figure2.png", width = 800, height = 800)
# Summary of results
par(mfrow = c(2, 2), mar = c(5, 5, 2, 2))
# Day 22 (AO)
plot(NA, xlim = c(0, 150), ylim = c(0, 1), xaxt = "n", yaxt = "n", bty = "n", xlab = "Conditioning intensity", ylab = "Proportion in older stage", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(0, 1, .2), tcl = -.5, las = 1)
mtext(side = 3, c("(a) Pupae on day 22 (AO)"), adj = 0, cex = 1.5)
points(prop.mod ~ jitter(Density, factor = 2), data = subset(data22, Type == "AO"), pch = 1, cex = 1.2)
densities <- c(0:150)
xvert <- c(densities, rev(densities))
yvert <- c(cred1[1, ], rev(cred1[2, ]))
polygon(xvert, yvert, col = rgb(1, 0, 0, alpha = .4), bty = "n", border = NA)
points(cred1[3, ] ~ densities, type = "l", col = "red", lwd = 2)
# Day 29 (A0)
plot(NA, xlim = c(0, 150), ylim = c(0, 1), xaxt = "n", yaxt = "n", bty = "n", xlab = "Conditioning intensity", ylab = "", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(0, 1, .2), tcl = -.5, las = 1)
mtext(side = 3, c("(b) Adults on day 29 (AO)"), adj = 0, cex = 1.5)
## text(x = 0, y = 0.03, expression(italic("Effect not significant")), adj = 0, cex = 1.2)
points(prop.mod ~ jitter(Density, factor = 2), data = subset(data29, Type == "AO"), pch = 1, cex = 1.2)
densities <- c(0:150)
xvert <- c(densities, rev(densities))
yvert <- c(cred3[1, ], rev(cred3[2, ]))
polygon(xvert, yvert, col = rgb(1, 0, 0, alpha = .4), bty = "n", border = NA)
points(cred3[3, ] ~ densities, type = "l", col = "red", lwd = 2)
# Day 22 (OA)
plot(NA, xlim = c(0, 150), ylim = c(0.5, 1), xaxt = "n", yaxt = "n", bty = "n", xlab = "Conditioning intensity", ylab = "Proportion in older stage", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(0.5, 1, .1), tcl = -.5, las = 1)
mtext(side = 3, c("(c) Pupae on day 22 (OA)"), adj = 0, cex = 1.5)
points(prop.mod ~ jitter(Density, factor = 2), data = subset(data22, Type == "OA"), pch = 1, cex = 1.2)
densities <- c(0:150)
xvert <- c(densities, rev(densities))
yvert <- c(cred2[1, ], rev(cred2[2, ]))
polygon(xvert, yvert, col = rgb(1, 0, 0, alpha = .4), bty = "n", border = NA)
points(cred2[3, ] ~ densities, type = "l", col = "red", lwd = 2)
# Day 29 (OA)
plot(NA, xlim = c(0, 150), ylim = c(0.5, 1), xaxt = "n", yaxt = "n", bty = "n", xlab = "Conditioning intensity", ylab = "", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(0.5, 1, .1), tcl = -.5, las = 1)
mtext(side = 3, c("(d) Adults on day 29 (OA)"), adj = 0, cex = 1.5)
points(prop.mod ~ jitter(Density, factor = 2), data = subset(data29, Type == "OA"), pch = 1, cex = 1.2)
densities <- c(0:150)
xvert <- c(densities, rev(densities))
yvert <- c(cred4[1, ], rev(cred4[2, ]))
polygon(xvert, yvert, col = rgb(1, 0, 0, alpha = .4), bty = "n", border = NA)
points(cred4[3, ] ~ densities, type = "l", col = "red", lwd = 2)
#
dev.off()





