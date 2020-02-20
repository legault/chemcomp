
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

# Read credible intervals
cred1 <- scan("Data/eggAO-credint.csv", sep = ",")
cred1 <- matrix(cred1, ncol = 151, byrow = TRUE)
cred2 <- scan("Data/eggOA-credint.csv", sep = ",")
cred2 <- matrix(cred2, ncol = 151, byrow = TRUE)
# Read random effects
rand1 <- read.csv("Data/eggAO-rand.csv", header = TRUE)
rand2 <- read.csv("Data/eggOA-rand.csv", header = TRUE)
## Combine
rand <- rbind(rand1, rand2)
## Merge with data
data2 <- merge(data, rand)
## Subtract random effect from y ("Count")
data2$Count.mod <- exp(log(data2$Count) - data2$Intercept)

# Save plot
png(filename = "Figure2-revised.png", width = 4000, height = 2000, res = 300)
# Summary of results
par(mfrow = c(1, 2), mar = c(5, 5, 2, 2), cex.axis = 1.2)
# AO
plot(NA, xlim = c(0, 150), ylim = c(75, 275), xaxt = "n", yaxt = "n", bty = "n", cex.lab = 1.3, xlab = expression(paste("Conditioning intensity (density of ", italic("T. castaneum"), ")", sep = "")), ylab = "Number of eggs", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(75, 275, 50), tcl = -.5, las = 1)
mtext(side = 3, expression(paste("(a) Eggs laid by ", italic("T. confusum"), " after 24 h", sep = "")), adj = 0, cex = 1.5)
## text(x = 75, y = 80, expression(italic("Effect not significant")), adj = 0, cex = 1.2)
points(Count.mod ~ jitter(Density, factor = 2), data = subset(data2, Type == "AO"), pch = 1, cex = 1.2)
densities <- c(0:150)
xvert <- c(densities, rev(densities))
yvert <- c(cred1[1, ], rev(cred1[2, ]))
polygon(xvert, yvert, col = rgb(1, 0, 0, alpha = .2), bty = "n", border = NA)
points(cred1[3, ] ~ densities, type = "l", col = "red", lwd = 2)
## legend(x = 115, y = 300, c("Stock1", "Stock2", "Stock3", "Stock4", "Stock5", "Stock6"), pch = 16, col = darkcols, inset = .08, cex = 1.1)
# OA
plot(NA, xlim = c(0, 150), ylim = c(75, 275), xaxt = "n", yaxt = "n", bty = "n", cex.lab = 1.3, xlab = expression(paste("Conditioning intensity (density of ", italic("T. confusum"), ")", sep = "")), ylab = "", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(75, 275, 50), tcl = -.5, las = 1)
mtext(side = 3, expression(paste("(b) Eggs laid by ", italic("T. castaneum"), " after 24 h", sep = "")), adj = 0, cex = 1.5)
points(Count.mod ~ jitter(Density, factor = 2), data = subset(data2, Type == "OA"), pch = 1, cex = 1.2)
densities <- c(0:150)
xvert <- c(densities, rev(densities))
yvert <- c(cred2[1, ], rev(cred2[2, ]))
polygon(xvert, yvert, col = rgb(1, 0, 0, alpha = .2), bty = "n", border = NA)
points(cred2[3, ] ~ densities, type = "l", col = "red", lwd = 2)
#
dev.off()
