library(tidyverse)
## code file "pca-latest-2025-08-04.R" needs to be run

# First Dimension

## difference of number of nouns in the positive - negative space/values
nrow(subset(noun.prcomp, ClassifierOrang != "" & PC1 > 0)) - nrow(subset(noun.prcomp, ClassifierOrang != "" & PC1 < 0))
# percentage
round(nrow(subset(noun.prcomp, ClassifierOrang != "" & PC1 > 0))/sum(nrow(subset(noun.prcomp, ClassifierOrang != "" & PC1 < 0)), nrow(subset(noun.prcomp, ClassifierOrang != "" & PC1 > 0))) * 100, 1)

nrow(subset(noun.prcomp, ClassifierEkor != "" & PC1 > 0)) - nrow(subset(noun.prcomp, ClassifierEkor != "" & PC1 < 0))
# percentage
round(nrow(subset(noun.prcomp, ClassifierEkor != "" & PC1 < 0))/sum(nrow(subset(noun.prcomp, ClassifierEkor != "" & PC1 < 0)), nrow(subset(noun.prcomp, ClassifierEkor != "" & PC1 > 0))) * 100, 1)

nrow(subset(noun.prcomp, ClassifierBuah != "" & PC1 > 0)) - nrow(subset(noun.prcomp, ClassifierBuah != "" & PC1 < 0))
# percentage
round(nrow(subset(noun.prcomp, ClassifierBuah != "" & PC1 < 0))/sum(nrow(subset(noun.prcomp, ClassifierBuah != "" & PC1 < 0)), nrow(subset(noun.prcomp, ClassifierBuah != "" & PC1 > 0))) * 100, 1)


# Second Dimension
## difference of number of nouns in the positive - negative space/values
nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 > 0)) - nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 < 0))
# percentage
round(nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 > 0))/sum(nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 < 0)), nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 > 0))) * 100, 1)
round(nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 < 0))/sum(nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 < 0)), nrow(subset(noun.prcomp, ClassifierOrang != "" & PC2 > 0))) * 100, 1)

nrow(subset(noun.prcomp, ClassifierBuah != "" & PC2 > 0)) - nrow(subset(noun.prcomp, ClassifierBuah != "" & PC2 < 0))
# percentage
round(nrow(subset(noun.prcomp, ClassifierBuah != "" & PC2 > 0))/sum(nrow(subset(noun.prcomp, ClassifierBuah != "" & PC2 < 0)), 
                                                                    nrow(subset(noun.prcomp, ClassifierBuah != "" & PC2 > 0))) * 100, 1)

nrow(subset(noun.prcomp, ClassifierEkor != "" & PC2 > 0)) - nrow(subset(noun.prcomp, ClassifierEkor != "" & PC2 < 0))
# percentage
round(nrow(subset(noun.prcomp, ClassifierEkor != "" & PC2 < 0))/sum(nrow(subset(noun.prcomp, ClassifierEkor != "" & PC2 < 0)), 
                                                                    nrow(subset(noun.prcomp, ClassifierEkor != "" & PC2 > 0))) * 100, 1)


# affixed non-affixed
## orang
### affixed positive space
round(nrow(subset(noun.prcomp, ClassifierOrang != "" & Affixed == TRUE & PC1 > 0))/
  nrow(subset(noun.prcomp, ClassifierOrang != "" & Affixed == TRUE)) * 100, 1)
### affixed negative space
round(nrow(subset(noun.prcomp, ClassifierOrang != "" & Affixed == TRUE & PC1 < 0))/
  nrow(subset(noun.prcomp, ClassifierOrang != "" & Affixed == TRUE)) * 100, 1)

### no affix positive space
round(nrow(subset(noun.prcomp, ClassifierOrang != "" & Affixed == FALSE & PC1 > 0))/
        nrow(subset(noun.prcomp, ClassifierOrang != "" & Affixed == FALSE)) * 100, 1)
### no affix negative space
round(nrow(subset(noun.prcomp, ClassifierOrang != "" & Affixed == FALSE & PC1 < 0))/
        nrow(subset(noun.prcomp, ClassifierOrang != "" & Affixed == FALSE)) * 100, 1)

## buah
### affixed positive space
round(nrow(subset(noun.prcomp, ClassifierBuah != "" & Affixed == TRUE & PC1 > 0))/
        nrow(subset(noun.prcomp, ClassifierBuah != "" & Affixed == TRUE)) * 100, 1)
### affixed negative space
round(nrow(subset(noun.prcomp, ClassifierBuah != "" & Affixed == TRUE & PC1 < 0))/
        nrow(subset(noun.prcomp, ClassifierBuah != "" & Affixed == TRUE)) * 100, 1)

### non-affixed positive space
round(nrow(subset(noun.prcomp, ClassifierBuah != "" & Affixed == FALSE & PC1 > 0))/
        nrow(subset(noun.prcomp, ClassifierBuah != "" & Affixed == FALSE)) * 100, 1)
### non-affixed negative space
round(nrow(subset(noun.prcomp, ClassifierBuah != "" & Affixed == FALSE & PC1 < 0))/
        nrow(subset(noun.prcomp, ClassifierBuah != "" & Affixed == FALSE)) * 100, 1)














# UNUSED CODE BELOW ===============


# shared spaces across two dimensions for nouns collocated with "buah" and "orang"
# head(subset(noun.prcomp, PC2 > -5 & PC2 < -1 & PC1 > -5 & PC1 < 0 & ClassifierBuah != ""), n = 10)
# head(subset(noun.prcomp, PC2 > -5 & PC2 < -1 & PC1 > -5 & PC1 < 0 & ClassifierOrang != ""), n = 10)

# orang_pc <- subset(noun.prcomp, ClassifierOrang != "", c(1:3))
# orang_pc$clf <- "orang"
# buah_pc <- subset(noun.prcomp, ClassifierBuah != "", c(1:3))
# buah_pc$clf <- "buah"
# ob <- tibble::tibble(tibble::rownames_to_column(rbind(orang_pc, buah_pc), var = "noun"))

# dplyr::arrange(head(subset(orang_pc, PC1 > -3 & PC1 < -2)), PC1)
#           PC1         PC2
# anggota   -2.940365 -0.22136794
# pengguna  -2.642154 -0.57743458
# perempuan -2.348247  1.17168928
# penduduk  -2.340173  1.61738300
# wisatawan -2.131413  1.05761896
# siswa     -2.105170  0.05119756
# buah_pc <- subset(noun.prcomp, ClassifierBuah != "", c(1:2))
# dplyr::arrange(head(subset(buah_pc, PC1 > -3 & PC1 < -2)), PC1)
#               PC1        PC2
# organisasi  -2.519122 -1.6413367
# pengetahuan -2.399253 -1.4436908
# penelitian  -2.107050 -1.7380674
# kelompok    -2.079004 -0.4680750
# keluarga    -2.031683  0.4678005
# pertanyaan  -2.001997 -1.5905345

# orang_pc2 <- subset(noun.prcomp, ClassifierOrang != "", c(1:2))
# head(subset(orang_pc2, PC2 > -3 & PC2 < -2 & PC1 > -3 & PC1 < -2))
#           PC1       PC2
# publik   -2.212178 -2.349530
# operator -2.013041 -2.270671
# panitia  -2.687415 -2.200752
# direksi  -2.662848 -2.378790
# humas    -2.155932 -2.953932
# perumus  -2.347955 -2.324887
# buah_pc2 <- subset(noun.prcomp, ClassifierBuah != "", c(1:2))
# head(subset(buah_pc2, PC2 > -3 & PC2 < -2 & PC1 > -3 & PC1 < -2))
#               PC1       PC2
# kebijakan   -2.069586 -2.925172
# keputusan   -2.180467 -2.383726
# pemahaman   -2.776210 -2.306376
# pernyataan  -2.203862 -2.308985
# pendekatan  -2.462486 -2.526082
# pembicaraan -2.150117 -2.181839

# ob |> filter(PC1 > -2 & PC1 < -1, PC2 < -1 & PC2 > -2) |> arrange(clf, PC1) |> group_by(clf) |> slice_max(order_by = -PC1, n = 10)
