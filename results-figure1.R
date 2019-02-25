library(RColorBrewer)
# colors
darkcols <- brewer.pal(6, "Dark2")

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

# Get predictions
pred1 <- scan("Data/eggOApredictions.csv")
pred1 <- matrix(pred1, ncol = 906, byrow = F)

# Save plot
png(filename = "Figure1.png", width = 1000)
# Summary of results
par(mfrow = c(1, 2), mar = c(5, 5, 2, 2))
# AO
plot(NA, xlim = c(0, 150), ylim = c(50, 350), xaxt = "n", yaxt = "n", bty = "n", cex.lab = 1.3, xlab = "Conditioning intensity", ylab = "Counts", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(50, 350, 50), tcl = -.5, las = 1)
points(Count ~ Density, data = subset(data, Week == "AO.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "AO.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "AO.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "AO.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "AO.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "AO.7"), col = darkcols[6], pch = 16, cex = 1.2)
mtext(side = 3, c("(a) Egg count (AO)"), adj = 0, cex = 1.5)
text(x = 0, y = 60, expression(italic("Effect not significant")), adj = 0, cex = 1.2)
legend(x = 115, y = 350, c("Stock1", "Stock2", "Stock3", "Stock4", "Stock5", "Stock6"), pch = 16, col = darkcols, inset = .08, cex = 1.1)
# OA
plot(NA, xlim = c(0, 150), ylim = c(50, 350), xaxt = "n", yaxt = "n", bty = "n", cex.lab = 1.3, xlab = "Conditioning intensity", ylab = "", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(50, 350, 50), tcl = -.5, las = 1)
for(i in 1:200){
    points(c(pred1[i, 1:151]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[1]))/ 255, alpha = .1))
    points(c(pred1[i, 152:302]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[2]))/ 255, alpha = .1))
    points(c(pred1[i, 303:453]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[3]))/ 255, alpha = .1))
    points(c(pred1[i, 454:604]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[4]))/ 255, alpha = .1))
    points(c(pred1[i, 605:755]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[5]))/ 255, alpha = .1))
    points(c(pred1[i, 756:906]) ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[6]))/ 255, alpha = .1))
}
points(Count ~ Density, data = subset(data, Week == "OA.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "OA.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "OA.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "OA.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "OA.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(Count ~ Density, data = subset(data, Week == "OA.7"), col = darkcols[6], pch = 16, cex = 1.2)
mtext(side = 3, c("(b) Egg count (OA)"), adj = 0, cex = 1.5)
#
dev.off()
