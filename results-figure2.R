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
# Day 22
day22 <- subset(data, Day == 22 & Total <= 70)
# Day 29
day29 <- subset(data, Day == 29 & Total <= 70)

# Get predictions
pred1 <- scan("Data/day22AOpredictions.csv")
pred1 <- matrix(pred1, ncol = 906, byrow = F)
pred2 <- scan("Data/day22OApredictions.csv")
pred2 <- matrix(pred2, ncol = 906, byrow = F)
pred3 <- scan("Data/day29OApredictions.csv")
pred3 <- matrix(pred3, ncol = 906, byrow = F)

png(filename = "Figure2.png", width = 800, height = 800)
# Summary of results
par(mfrow = c(2, 2), mar = c(5, 5, 2, 2))
# Day 22 (AO)
plot(NA, xlim = c(0, 150), ylim = c(0, 1), xaxt = "n", yaxt = "n", bty = "n", xlab = "Conditioning intensity", ylab = "Proportion", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(0, 1, .2), tcl = -.5, las = 1)
for(i in 1:1000){
    points(pred1[i, 1:151] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[1]))/ 255, alpha = .05))
    points(pred1[i, 152:302] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[2]))/ 255, alpha = .05))
    points(pred1[i, 303:453] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[3]))/ 255, alpha = .05))
    points(pred1[i, 454:604] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[4]))/ 255, alpha = .05))
    points(pred1[i, 605:755] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[5]))/ 255, alpha = .05))
    points(pred1[i, 756:906] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[6]))/ 255, alpha = .05))
}
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "AO.7"), col = darkcols[6], pch = 16, cex = 1.2)
mtext(side = 3, c("(a) Pupae on day 22 (AO)"), adj = 0, cex = 1.5)
# Day 22 (OA)
plot(NA, xlim = c(0, 150), ylim = c(0.5, 1), xaxt = "n", yaxt = "n", bty = "n", xlab = "Conditioning intensity", ylab = "Proportion", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(0.5, 1, .1), tcl = -.5, las = 1)
for(i in 1:1000){
    points(pred2[i, 1:151] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[1]))/ 255, alpha = .05))
    points(pred2[i, 152:302] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[2]))/ 255, alpha = .05))
    points(pred2[i, 303:453] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[3]))/ 255, alpha = .05))
    points(pred2[i, 454:604] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[4]))/ 255, alpha = .05))
    points(pred2[i, 605:755] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[5]))/ 255, alpha = .05))
    points(pred2[i, 756:906] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[6]))/ 255, alpha = .05))
}
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(c(Pupae / (Pupae + Larvae)) ~ Density, data = subset(day22, Week == "OA.7"), col = darkcols[6], pch = 16, cex = 1.2)
legend("bottomright", c("Stock1", "Stock2", "Stock3", "Stock4", "Stock5", "Stock6"), pch = 16, col = darkcols, inset = .05)
mtext(side = 3, c("(b) Pupae on day 22 (OA)"), adj = 0, cex = 1.5)
# Day 29 (A0)
plot(NA, xlim = c(0, 150), ylim = c(0, 1), xaxt = "n", yaxt = "n", bty = "n", xlab = "Conditioning intensity", ylab = "Proportion", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(0, 1, .2), tcl = -.5, las = 1)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "AO.7"), col = darkcols[6], pch = 16, cex = 1.2)
mtext(side = 3, c("(c) Adults on day 29 (AO)"), adj = 0, cex = 1.5)
text(x = 0, y = 0.03, expression(italic("Effect not significant")), adj = 0, cex = 1.2)
# Day 29 (OA)
plot(NA, xlim = c(0, 150), ylim = c(0.5, 1), xaxt = "n", yaxt = "n", bty = "n", xlab = "Conditioning intensity", ylab = "Proportion", cex.lab = 1.5)
axis(1, seq(0, 150, 30), tcl = -.5)
axis(2, seq(0.5, 1, .1), tcl = -.5, las = 1)
for(i in 1:1000){
    points(pred3[i, 1:151] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[1]))/ 255, alpha = .05))
    points(pred3[i, 152:302] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[2]))/ 255, alpha = .05))
    points(pred3[i, 303:453] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[3]))/ 255, alpha = .05))
    points(pred3[i, 454:604] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[4]))/ 255, alpha = .05))
    points(pred3[i, 605:755] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[5]))/ 255, alpha = .05))
    points(pred3[i, 756:906] ~ c(0:150), type = "l", col = rgb(t(col2rgb(darkcols[6]))/ 255, alpha = .05))
}
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.2"), col = darkcols[1], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.3"), col = darkcols[2], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.4"), col = darkcols[3], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.5"), col = darkcols[4], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.6"), col = darkcols[5], pch = 16, cex = 1.2)
points(c(Adults / (Adults + Pupae)) ~ Density, data = subset(day29, Week == "OA.7"), col = darkcols[6], pch = 16, cex = 1.2)
mtext(side = 3, c("(d) Adults on day 29 (OA)"), adj = 0, cex = 1.5)
#
dev.off()





