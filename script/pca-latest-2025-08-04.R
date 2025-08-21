
# =====================
# Vector Space Model and Principal Component Analysis (PCA) sections
# =====================
library(MASS)
dat4 = readRDS("data/Coll1R1R_3_withoutinflection.rds") # data in my iCloud Drive project folder
# noun vector was taken from fastext
NounVector = read.table("data/Indwords_output-2025.txt", header = FALSE) # data in my iCloud Drive project folder

# subsetting data for each classifier
dat5 = NounVector[NounVector$V1 %in% dat4$Word,]

# number of noun in fastext
nrow(dat5)
## [1] 2398

# creating matrix
m = as.matrix(dat5[,-1])
# rename the rowname
rownames(m) = dat5[,1]

# goal: to see if the distribution of nouns modified by classifiers are different
noun.prcomp = prcomp(m, center = TRUE, scale = TRUE)
noun.prcomp[1:6]
# subset the data to only in the first three dimension
noun.prcomp = as.data.frame(noun.prcomp$x[,1:3])
head(noun.prcomp)
# adding other information to the data
noun.prcomp$ClassifierOrang = ""
noun.prcomp[rownames(noun.prcomp) %in% dat4[dat4$Classifier=="orang",]$Word,]$ClassifierOrang = "orang"
noun.prcomp$ClassifierEkor = ""
noun.prcomp[rownames(noun.prcomp) %in% dat4[dat4$Classifier=="ekor",]$Word,]$ClassifierEkor = "ekor"
noun.prcomp$ClassifierBuah = ""
noun.prcomp[rownames(noun.prcomp) %in% dat4[dat4$Classifier=="buah",]$Word,]$ClassifierBuah = "buah"

noun.prcomp$Affixed = ""
noun.prcomp[rownames(noun.prcomp) %in% dat4[dat4$Affixed==TRUE,]$Word,]$Affixed = TRUE
noun.prcomp[rownames(noun.prcomp) %in% dat4[dat4$Affixed==FALSE,]$Word,]$Affixed = FALSE

# number of fastex for each classifier
nrow(noun.prcomp[noun.prcomp$ClassifierOrang=="orang",])
## [1] 899
nrow(noun.prcomp[noun.prcomp$ClassifierEkor=="ekor",])
## [1] 119
nrow(noun.prcomp[noun.prcomp$ClassifierBuah=="buah",])
## [1] 1380


# PCA plot =============
opar <- par()
png("C:/Users/GRajeg/Downloads/pca-dimension-comparison.png", height = 14, width = 20, units = "in", res = 600)
par(mfrow=c(2,2))
par(mar=c(5,5,5,2))

## COMBINED CLASSIFIERS
plot(noun.prcomp[noun.prcomp$ClassifierOrang != "",]$PC1, noun.prcomp[noun.prcomp$ClassifierOrang != "",]$PC2, 
     col = c("black"),
     xlab = "", ylab = "dimension 2",
     xlim = c(-13.5,13.5), ylim= c(-10,10),
     cex.lab = 2.8, cex.axis = 2.8, cex.main = 2.8,
     pch = 19, cex = 0.6, main = "Indonesian classifier")
points(noun.prcomp[noun.prcomp$ClassifierEkor != "",]$PC1, noun.prcomp[noun.prcomp$ClassifierEkor != "",]$PC2, 
       col = c("red"), pch = 19, cex = 0.6)
points(noun.prcomp[noun.prcomp$ClassifierBuah != "",]$PC1, noun.prcomp[noun.prcomp$ClassifierBuah != "",]$PC2, 
       col = c("blue"), pch = 19, cex = 0.6)
# segments(x0 = -1, x1 = 1, y0 = 0, y1 = 0, col = "lightsteelblue2", lwd = 3)
# segments(x0 = 0, x1 = 0, y0 = -1, y1 = 1, col = "lightsteelblue2", lwd = 3)
abline(h = 0, v = 0, col = "honeydew4", lwd = 2, lty = 3)
legend(x="topright", legend = c("buah", "orang", "ekor"), 
       col = c("blue", "black", "red"), 
       pch=19, cex = 2.8, bty = "n")

## BUAH
plot(noun.prcomp[noun.prcomp$ClassifierBuah != "",]$PC1, noun.prcomp[noun.prcomp$ClassifierBuah != "",]$PC2, 
     type = "n",
     xlab = "", ylab = "",
     xlim = c(-13.5,13.5), ylim= c(-10,10),
     cex.lab = 2.8, cex.axis = 2.8, cex.main = 2.8,
     main = "Buah classifier")
text(noun.prcomp[noun.prcomp$ClassifierBuah != "",]$PC1, noun.prcomp[noun.prcomp$ClassifierBuah != "",]$PC2, 
     row.names(noun.prcomp[noun.prcomp$ClassifierBuah != "",]),
     cex = 0.8, pos = 2, col = "blue")
# segments(x0 = -1, x1 = 1, y0 = 0, y1 = 0, col = "firebrick1", lwd = 3)
# segments(x0 = 0, x1 = 0, y0 = -1, y1 = 1, col = "firebrick1", lwd = 3)
abline(h = 0, v = 0, col = "honeydew4", lwd = 2, lty = 3)

## ORANG
plot(noun.prcomp[noun.prcomp$ClassifierOrang != "",]$PC1, noun.prcomp[noun.prcomp$ClassifierOrang != "",]$PC2, 
     type = "n",
     xlab = "dimension 1", ylab = "dimension 2",
     xlim = c(-13.5,13.5), ylim= c(-10,10),
     cex.lab = 2.8, cex.axis = 2.8, cex.main = 2.8,
     main = "Orang classifier")
text(noun.prcomp[noun.prcomp$ClassifierOrang != "",]$PC1, noun.prcomp[noun.prcomp$ClassifierOrang != "",]$PC2, 
     row.names(noun.prcomp[noun.prcomp$ClassifierOrang != "",]),
     cex = 0.8, pos = 2, col = "black")
# segments(x0 = -1, x1 = 1, y0 = 0, y1 = 0, col = "firebrick1", lwd = 3)
# segments(x0 = 0, x1 = 0, y0 = -1, y1 = 1, col = "firebrick1", lwd = 3)
abline(h = 0, v = 0, col = "honeydew4", lwd = 2, lty = 3)

## EKOR
plot(noun.prcomp[noun.prcomp$ClassifierEkor != "",]$PC1, noun.prcomp[noun.prcomp$ClassifierEkor != "",]$PC2, 
     type = "n",
     xlab = "dimension 1", ylab = "",
     xlim = c(-13.5,13.5), ylim= c(-10,10),
     cex.lab = 2.8, cex.axis = 2.8, cex.main = 2.8,
     main = "Ekor classifier")
text(noun.prcomp[noun.prcomp$ClassifierEkor != "",]$PC1, noun.prcomp[noun.prcomp$ClassifierEkor != "",]$PC2, 
     row.names(noun.prcomp[noun.prcomp$ClassifierEkor != "",]),
     cex = 0.8, pos = 2, col = "red")
# segments(x0 = -1, x1 = 1, y0 = 0, y1 = 0, col = "royalblue3", lwd = 3)
# segments(x0 = 0, x1 = 0, y0 = -1, y1 = 1, col = "royalblue3", lwd = 3)
abline(h = 0, v = 0, col = "honeydew4", lwd = 2, lty = 3)

dev.off()

# UNUSED CODE BELOW ===============
## see "pca-dimension-comparison.R"
# PCA data point comparions - First Dimension ==============
## difference of number of nouns in the positive - negative space/values
# nrow(subset(noun.prcomp, ClassifierOrang != "" & PC1 > 0)) - nrow(subset(noun.prcomp, ClassifierOrang != "" & PC1 < 0))
# percentage
# nrow(subset(noun.prcomp, ClassifierOrang != "" & PC1 < 0))/sum(nrow(subset(noun.prcomp, ClassifierOrang != "" & PC1 < 0)), nrow(subset(noun.prcomp, ClassifierOrang != "" & PC1 > 0)))


# nrow(subset(noun.prcomp, ClassifierBuah != "" & PC1 > 0)) - nrow(subset(noun.prcomp, ClassifierBuah != "" & PC1 < 0))
# percentage
# nrow(subset(noun.prcomp, ClassifierBuah != "" & PC1 > 0))/sum(nrow(subset(noun.prcomp, ClassifierBuah != "" & PC1 < 0)), nrow(subset(noun.prcomp, ClassifierBuah != "" & PC1 > 0)))


# nrow(subset(noun.prcomp, ClassifierEkor != "" & PC1 > 0)) - nrow(subset(noun.prcomp, ClassifierEkor != "" & PC1 < 0))
# percentage
# nrow(subset(noun.prcomp, ClassifierEkor != "" & PC1 > 0))/sum(nrow(subset(noun.prcomp, ClassifierEkor != "" & PC1 < 0)), nrow(subset(noun.prcomp, ClassifierEkor != "" & PC1 > 0)))


# PCA data point comparions - Second Dimension ==============
## difference of number of nouns in the positive - negative space/values
# nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 > 0)) - nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 < 0))
# percentage
# nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 > 0))/sum(nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 < 0)), nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 > 0)))
# nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 < 0))/sum(nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 < 0)), nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 > 0)))


# nrow(subset(noun.prcomp, ClassifierBuah != "" & PC2 > 0)) - nrow(subset(noun.prcomp, ClassifierBuah != "" & PC2 < 0))
# percentage
# nrow(subset(noun.prcomp, ClassifierBuah != "" & PC2 < 0))/sum(nrow(subset(noun.prcomp, ClassifierBuah != "" & PC2 < 0)), nrow(subset(noun.prcomp, ClassifierBuah != "" & PC2 > 0)))


# nrow(subset(noun.prcomp, ClassifierEkor != "" & PC2 > 0)) - nrow(subset(noun.prcomp, ClassifierEkor != "" & PC2 < 0))
# percentage
# nrow(subset(noun.prcomp, ClassifierEkor != "" & PC2 > 0))/sum(nrow(subset(noun.prcomp, ClassifierEkor != "" & PC2 < 0)), nrow(subset(noun.prcomp, ClassifierEkor != "" & PC2 > 0)))
# UNUSED CODE ABOVE ===============

###############################
# affixed and non-affixed nouns
###############################
png("C:/Users/GRajeg/Downloads/pca-dimension-comparison-affixed.png", height = 10, width = 20, units = "in", res = 600)
par(mar=c(5,6,4,1)+.1, mfrow=c(1,2))
plot(noun.prcomp[noun.prcomp$ClassifierOrang != "" & noun.prcomp$Affixed==TRUE,]$PC1, 
     noun.prcomp[noun.prcomp$ClassifierOrang != ""& noun.prcomp$Affixed==TRUE,]$PC2, 
     col = c("black"),
     xlab = "dimension 1", ylab = "dimension 2",
     xlim = c(-10,10), ylim= c(-10,10),
     cex.lab = 2.7, cex.axis = 2.7, cex.main = 2.7,
     pch = 19, cex = 1, main = "Classifier Orang")
points(noun.prcomp[noun.prcomp$ClassifierOrang != "" & noun.prcomp$Affixed==FALSE,]$PC1, 
       noun.prcomp[noun.prcomp$ClassifierOrang != ""& noun.prcomp$Affixed==FALSE,]$PC2, 
       col = c("red"), pch = 19, cex = 0.8)
abline(h = 0, v = 0, col = "honeydew4", lwd = 2, lty = 3)
legend(x="topright", legend = c("affixed", "non-affixed"), 
       col = c("black", "red"), 
       pch=19, cex = 2.7, bty = "n")

plot(noun.prcomp[noun.prcomp$ClassifierBuah != "" & noun.prcomp$Affixed==TRUE,]$PC1, 
     noun.prcomp[noun.prcomp$ClassifierBuah != ""& noun.prcomp$Affixed==TRUE,]$PC2, 
     col = c("black"),
     xlab = "dimension 1", ylab = "",
     xlim = c(-10,10), ylim= c(-10,10),
     cex.lab = 2.7, cex.axis = 2.7, cex.main = 2.7,
     pch = 19, cex = 1, main = "Classifier Buah")
points(noun.prcomp[noun.prcomp$ClassifierBuah != "" & noun.prcomp$Affixed==FALSE,]$PC1, 
       noun.prcomp[noun.prcomp$ClassifierBuah != ""& noun.prcomp$Affixed==FALSE,]$PC2, 
       col = c("red"), pch = 19, cex = 0.8)
abline(h = 0, v = 0, col = "honeydew4", lwd = 2, lty = 3)
legend(x="topright", legend = c("affixed", "non-affixed"), 
       col = c("black", "red"), 
       pch=19, cex = 2.7, bty = "n")
dev.off()
par(opar)