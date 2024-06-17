# install.packages("devtools")
# devtools::install_github("bmschmidt/wordVectors")

# This code assumes that your working directory is in the root directory of this OSF Indonesian classifier repository

# Load packages =====
library(wordVectors)
library(devtools)
library(tidyverse)
# library(PMCMRplus)
library(ggtext)
library(ggpubr)

# noun vector was taken from fastext
NounVector <- read.table("data/Indwords_output.txt", header = FALSE)
colloc <- readRDS("data/Coll1R1R.rds")
colloc2 <- colloc |> 
  filter(!is.na(Freq))

# Cosine Similarity Analysis ======
## retrieve the data (BUAH) =====
### noun with BUAH all forms =====
noun_with_buah_all <- colloc2 |> 
  filter(Classifier == "buah") |> 
  as_tibble()
vsm_with_buah_all_df <- NounVector |> 
  filter(V1 %in% noun_with_buah_all$Word) |> 
  as_tibble()
vsm_with_buah_all_mtx <- vsm_with_buah_all_df |> 
  select(-V1) |> 
  as.matrix() |> 
  wordVectors::as.VectorSpaceModel()
rownames(vsm_with_buah_all_mtx) <- vsm_with_buah_all_df$V1

### noun with BUAH affixed forms =====
noun_with_buah_affixed <- colloc2 |> 
  filter(Classifier == "buah", Affixed == "TRUE") |> 
  as_tibble()
vsm_with_buah_affixed_df <- NounVector |> 
  filter(V1 %in% noun_with_buah_affixed$Word) |> 
  as_tibble()
vsm_with_buah_affixed_mtx <- vsm_with_buah_affixed_df |> 
  select(-V1) |> 
  as.matrix() |> 
  wordVectors::as.VectorSpaceModel()
rownames(vsm_with_buah_affixed_mtx) <- vsm_with_buah_affixed_df$V1

### noun with BUAH NON-affixed forms =====
noun_with_buah_noaffix <- colloc2 |> 
  filter(Classifier == "buah", Affixed == "FALSE") |> 
  as_tibble()
vsm_with_buah_noaffix_df <- NounVector |> 
  filter(V1 %in% noun_with_buah_noaffix$Word) |> 
  as_tibble()
vsm_with_buah_noaffix_mtx <- vsm_with_buah_noaffix_df |> 
  select(-V1) |> 
  as.matrix() |> 
  wordVectors::as.VectorSpaceModel()
rownames(vsm_with_buah_noaffix_mtx) <- vsm_with_buah_noaffix_df$V1


## retrieve the data (EKOR) ====
### noun with EKOR all forms =====
noun_with_ekor_all <- colloc2 |> 
  filter(Classifier == "ekor") |> 
  as_tibble()
vsm_with_ekor_all_df <- NounVector |> 
  filter(V1 %in% noun_with_ekor_all$Word) |> 
  as_tibble()
vsm_with_ekor_all_mtx <- vsm_with_ekor_all_df |> 
  select(-V1) |> 
  as.matrix() |> 
  wordVectors::as.VectorSpaceModel()
rownames(vsm_with_ekor_all_mtx) <- vsm_with_ekor_all_df$V1

### noun with EKOR affixed forms =====
noun_with_ekor_affixed <- colloc2 |> 
  filter(Classifier == "ekor", Affixed == "TRUE") |> 
  as_tibble()
vsm_with_ekor_affixed_df <- NounVector |> 
  filter(V1 %in% noun_with_ekor_affixed$Word) |> 
  as_tibble()
vsm_with_ekor_affixed_mtx <- vsm_with_ekor_affixed_df |> 
  select(-V1) |> 
  as.matrix() |> 
  wordVectors::as.VectorSpaceModel()
rownames(vsm_with_ekor_affixed_mtx) <- vsm_with_ekor_affixed_df$V1

### noun with EKOR NON-affixed forms =====
noun_with_ekor_noaffix <- colloc2 |> 
  filter(Classifier == "ekor", Affixed == "FALSE") |> 
  as_tibble()
vsm_with_ekor_noaffix_df <- NounVector |> 
  filter(V1 %in% noun_with_ekor_noaffix$Word) |> 
  as_tibble()
vsm_with_ekor_noaffix_mtx <- vsm_with_ekor_noaffix_df |> 
  select(-V1) |> 
  as.matrix() |> 
  wordVectors::as.VectorSpaceModel()
rownames(vsm_with_ekor_noaffix_mtx) <- vsm_with_ekor_noaffix_df$V1


## retrieve the data (ORANG) ====
### noun with ORANG all forms =====
noun_with_orang_all <- colloc2 |> 
  filter(Classifier == "orang") |> 
  as_tibble()
vsm_with_orang_all_df <- NounVector |> 
  filter(V1 %in% noun_with_orang_all$Word) |> 
  as_tibble()
vsm_with_orang_all_mtx <- vsm_with_orang_all_df |> 
  select(-V1) |> 
  as.matrix() |> 
  wordVectors::as.VectorSpaceModel()
rownames(vsm_with_orang_all_mtx) <- vsm_with_orang_all_df$V1

### noun with ORANG affixed forms =====
noun_with_orang_affixed <- colloc2 |> 
  filter(Classifier == "orang", Affixed == "TRUE") |> 
  as_tibble()
vsm_with_orang_affixed_df <- NounVector |> 
  filter(V1 %in% noun_with_orang_affixed$Word) |> 
  as_tibble()
vsm_with_orang_affixed_mtx <- vsm_with_orang_affixed_df |> 
  select(-V1) |> 
  as.matrix() |> 
  wordVectors::as.VectorSpaceModel()
rownames(vsm_with_orang_affixed_mtx) <- vsm_with_orang_affixed_df$V1

### noun with ORANG NON-affixed forms =====
noun_with_orang_noaffix <- colloc2 |> 
  filter(Classifier == "orang", Affixed == "FALSE") |> 
  as_tibble()
vsm_with_orang_noaffix_df <- NounVector |> 
  filter(V1 %in% noun_with_orang_noaffix$Word) |> 
  as_tibble()
vsm_with_orang_noaffix_mtx <- vsm_with_orang_noaffix_df |> 
  select(-V1) |> 
  as.matrix() |> 
  wordVectors::as.VectorSpaceModel()
rownames(vsm_with_orang_noaffix_mtx) <- vsm_with_orang_noaffix_df$V1


## Computation of the Cosine Similarity =====
#  cosfunc <- function(m1, m2, n1 = NULL, n2 = NULL) {
#   mtx_temp <- wordVectors::cosineSimilarity(m1, m2)
#   cos_df <- mtx_temp |> 
#     data.frame(row.names = rownames(mtx_temp)) |> 
#     rownames_to_column(var = "n1") |> 
#     pivot_longer(cols = -n1,
#                  names_to = "n2",
#                  values_to = "cossim")
#   return(cos_df)
# }

### 1. Cosine similarities between nouns modified by "ekor" & "buah" ALL forms =====
cosim_ekor_buah <- wordVectors::cosineSimilarity(vsm_with_ekor_all_mtx, vsm_with_buah_all_mtx)
cosim_ekor_buah_df <- cosim_ekor_buah |> 
  data.frame(row.names = rownames(cosim_ekor_buah)) |> 
  rownames_to_column(var = "noun_w_ekor") |> 
  pivot_longer(cols = -noun_w_ekor, 
               names_to = "noun_w_buah", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsAllForms with 'ekor' and 'buah'") |> 
  filter(noun_w_ekor != noun_w_buah) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_ekor, noun_w_buah)), .keep_all = TRUE)

cosim_ekor_buah_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/09-cossim-full-ekor-buah.tsv")
  
#### 1.1 Cosine similarities between nouns modified by "ekor" & "buah" AFFIXED forms =====
cosim_ekor_buah_affixed <- wordVectors::cosineSimilarity(vsm_with_ekor_affixed_mtx, vsm_with_buah_affixed_mtx)
cosim_ekor_buah_affixed_df <- cosim_ekor_buah_affixed |> 
  data.frame(row.names = rownames(cosim_ekor_buah_affixed)) |> 
  rownames_to_column(var = "noun_w_ekor") |> 
  pivot_longer(cols = -noun_w_ekor, 
               names_to = "noun_w_buah", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsAffixedForms with 'ekor' and 'buah'") |> 
  filter(noun_w_ekor != noun_w_buah) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_ekor, noun_w_buah)), .keep_all = TRUE)

cosim_ekor_buah_affixed_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/09-cossim-full-ekor-buah-affixed.tsv")
  
#### 1.2 Cosine similarities between nouns modified by "ekor" & "buah" NON-AFFIXED forms =====
cosim_ekor_buah_noaffix <- wordVectors::cosineSimilarity(vsm_with_ekor_noaffix_mtx, vsm_with_buah_noaffix_mtx)
cosim_ekor_buah_noaffix_df <- cosim_ekor_buah_noaffix |> 
  data.frame(row.names = rownames(cosim_ekor_buah_noaffix)) |> 
  rownames_to_column(var = "noun_w_ekor") |> 
  pivot_longer(cols = -noun_w_ekor, 
               names_to = "noun_w_buah", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsNoAffixForms with 'ekor' and 'buah'") |> 
  filter(noun_w_ekor != noun_w_buah) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_ekor, noun_w_buah)), .keep_all = TRUE)

cosim_ekor_buah_noaffix_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/09-cossim-full-ekor-buah-noaffixed.tsv")
  
### 2. Cosine similarities between nouns modified by "ekor" & "orang" ALL forms =====
cosim_ekor_orang <- wordVectors::cosineSimilarity(vsm_with_ekor_all_mtx, vsm_with_orang_all_mtx)
cosim_ekor_orang_df <- cosim_ekor_orang |> 
  data.frame(row.names = rownames(cosim_ekor_orang)) |> 
  rownames_to_column(var = "noun_w_ekor") |> 
  pivot_longer(cols = -noun_w_ekor, 
               names_to = "noun_w_orang", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsAllForms with 'ekor' and 'orang'") |> 
  filter(noun_w_ekor != noun_w_orang) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_ekor, noun_w_orang)), .keep_all = TRUE)

cosim_ekor_orang_df |> 
  # arrange(desc(cossim)) |> 
  slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |> 
  rename(`Nouns modified by EKOR` = noun_w_ekor,
         `Nouns modified by ORANG` = noun_w_orang,
         `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |> 
  write_tsv("stats_output/03-sample-cossime-database.tsv")

cosim_ekor_orang_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/09-cossim-full-ekor-orang.tsv")

#### 2.1 Cosine similarities between nouns modified by "ekor" & "orang" AFFIXED forms =====
cosim_ekor_orang_affixed <- wordVectors::cosineSimilarity(vsm_with_ekor_affixed_mtx, vsm_with_orang_affixed_mtx)
cosim_ekor_orang_affixed_df <- cosim_ekor_orang_affixed |> 
  data.frame(row.names = rownames(cosim_ekor_orang_affixed)) |> 
  rownames_to_column(var = "noun_w_ekor") |> 
  pivot_longer(cols = -noun_w_ekor, 
               names_to = "noun_w_orang", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsAffixedForms with 'ekor' and 'orang'") |> 
  filter(noun_w_ekor != noun_w_orang) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_ekor, noun_w_orang)), .keep_all = TRUE)

cosim_ekor_orang_affixed_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/09-cossim-full-ekor-orang-affixed.tsv")

#### 2.2 Cosine similarities between nouns modified by "ekor" & "orang" NON-AFFIXED forms =====
cosim_ekor_orang_noaffix <- wordVectors::cosineSimilarity(vsm_with_ekor_noaffix_mtx, vsm_with_orang_noaffix_mtx)
cosim_ekor_orang_noaffix_df <- cosim_ekor_orang_noaffix |> 
  data.frame(row.names = rownames(cosim_ekor_orang_noaffix)) |> 
  rownames_to_column(var = "noun_w_ekor") |> 
  pivot_longer(cols = -noun_w_ekor, 
               names_to = "noun_w_orang", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsNoAffixForms with 'ekor' and 'orang'") |> 
  filter(noun_w_ekor != noun_w_orang) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_ekor, noun_w_orang)), .keep_all = TRUE)

cosim_ekor_orang_noaffix_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/09-cossim-full-ekor-orang-noaffixed.tsv")

### 3. Cosine similarities between nouns modified by "buah" & "orang" ALL forms =====
cosim_buah_orang <- wordVectors::cosineSimilarity(vsm_with_buah_all_mtx, vsm_with_orang_all_mtx)
cosim_buah_orang_df <- cosim_buah_orang |> 
  data.frame(row.names = rownames(cosim_buah_orang)) |> 
  rownames_to_column(var = "noun_w_buah") |> 
  pivot_longer(cols = -noun_w_buah, 
               names_to = "noun_w_orang", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsAllForms with 'buah' and 'orang'") |> 
  filter(noun_w_orang != noun_w_buah) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_buah, noun_w_orang)), .keep_all = TRUE)

cosim_buah_orang_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/09-cossim-full-buah-orang.tsv")

#### 3.1 Cosine similarities between nouns modified by "buah" & "orang" AFFIXED forms =====
cosim_buah_orang_affixed <- wordVectors::cosineSimilarity(vsm_with_buah_affixed_mtx, vsm_with_orang_affixed_mtx)
cosim_buah_orang_affixed_df <- cosim_buah_orang_affixed |> 
  data.frame(row.names = rownames(cosim_buah_orang_affixed)) |> 
  rownames_to_column(var = "noun_w_buah") |> 
  pivot_longer(cols = -noun_w_buah, 
               names_to = "noun_w_orang", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsAffixedForms with 'buah' and 'orang'") |> 
  filter(noun_w_orang != noun_w_buah) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_buah, noun_w_orang)), .keep_all = TRUE)

cosim_buah_orang_affixed_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/09-cossim-full-buah-orang-affixed.tsv")

#### 3.2 Cosine similarities between nouns modified by "buah" & "orang" NON-AFFIXED forms =====
cosim_buah_orang_noaffix <- wordVectors::cosineSimilarity(vsm_with_buah_noaffix_mtx, vsm_with_orang_noaffix_mtx)
cosim_buah_orang_noaffix_df <- cosim_buah_orang_noaffix |> 
  data.frame(row.names = rownames(cosim_buah_orang_noaffix)) |> 
  rownames_to_column(var = "noun_w_buah") |> 
  pivot_longer(cols = -noun_w_buah, 
               names_to = "noun_w_orang", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsNoAffixForms with 'buah' and 'orang'") |> 
  filter(noun_w_orang != noun_w_buah) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_buah, noun_w_orang)), .keep_all = TRUE)

cosim_buah_orang_noaffix_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/09-cossim-full-buah-orang-noaffixed.tsv")

### 4. Cosine similarities between nouns modified by "ekor" ALL forms =====
cosim_ekor_only <- wordVectors::cosineSimilarity(vsm_with_ekor_all_mtx, vsm_with_ekor_all_mtx)
cosim_ekor_only_df <- cosim_ekor_only |> 
  data.frame(row.names = rownames(cosim_ekor_only)) |> 
  rownames_to_column(var = "noun_w_ekor") |> 
  pivot_longer(cols = -noun_w_ekor, 
               names_to = "noun_w_ekor_only", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsAllForms with 'ekor' and 'ekor'") |> 
  filter(noun_w_ekor != noun_w_ekor_only) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_ekor, noun_w_ekor_only)), .keep_all = TRUE)

cosim_ekor_only_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-ekor-all-forms.tsv")

#### 4.1 Cosine similarities between nouns modified by "ekor" NON-AFFIXED vs. AFFIXED forms =====
cosim_ekor_noaffix_vs_affix <- wordVectors::cosineSimilarity(vsm_with_ekor_noaffix_mtx, vsm_with_ekor_affixed_mtx)
cosim_ekor_noaffix_vs_affix_df <- cosim_ekor_noaffix_vs_affix |> 
  data.frame(row.names = rownames(cosim_ekor_noaffix_vs_affix)) |> 
  rownames_to_column(var = "noun_w_ekor_noaffix") |> 
  pivot_longer(cols = -noun_w_ekor_noaffix, 
               names_to = "noun_w_ekor_affixed", 
               values_to = "cossim") |> 
  mutate(noun_compared = "Nouns with 'ekor' No-Affix vs. Affix") |> 
  filter(noun_w_ekor_noaffix != noun_w_ekor_affixed) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_ekor_noaffix, noun_w_ekor_affixed)), .keep_all = TRUE)

cosim_ekor_noaffix_vs_affix_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-ekor-noaffix-affix.tsv")

#### 4.2 Cosine similarities between nouns modified by "ekor" NON-AFFIXED =====
cosim_ekor_noaffix <- wordVectors::cosineSimilarity(vsm_with_ekor_noaffix_mtx, vsm_with_ekor_noaffix_mtx)
cosim_ekor_noaffix_df <- cosim_ekor_noaffix |> 
  data.frame(row.names = rownames(cosim_ekor_noaffix)) |> 
  rownames_to_column(var = "noun_w_ekor_noaffix1") |> 
  pivot_longer(cols = -noun_w_ekor_noaffix1, 
               names_to = "noun_w_ekor_noaffix2", 
               values_to = "cossim") |> 
  mutate(noun_compared = "Nouns with 'ekor' No-Affix") |> 
  filter(noun_w_ekor_noaffix1 != noun_w_ekor_noaffix2) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_ekor_noaffix1, noun_w_ekor_noaffix2)), .keep_all = TRUE)

cosim_ekor_noaffix_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-ekor-noaffix-noaffix.tsv")
  
#### 4.3 Cosine similarities between nouns modified by "ekor" AFFIXED =====
cosim_ekor_affixed <- wordVectors::cosineSimilarity(vsm_with_ekor_affixed_mtx, vsm_with_ekor_affixed_mtx)
cosim_ekor_affixed_df <- cosim_ekor_affixed |> 
  data.frame(row.names = rownames(cosim_ekor_affixed)) |> 
  rownames_to_column(var = "noun_w_ekor_affixed1") |> 
  pivot_longer(cols = -noun_w_ekor_affixed1, 
               names_to = "noun_w_ekor_affixed2", 
               values_to = "cossim") |> 
  mutate(noun_compared = "Nouns with 'ekor' Affixed") |> 
  filter(cossim != 1, noun_w_ekor_affixed1 != noun_w_ekor_affixed2) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_ekor_affixed1, noun_w_ekor_affixed2)), .keep_all = TRUE)

cosim_ekor_affixed_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-ekor-affix-affix.tsv")

### 5. Cosine similarities between nouns modified by "buah" ALL forms =====
cosim_buah_only <- wordVectors::cosineSimilarity(vsm_with_buah_all_mtx, vsm_with_buah_all_mtx)
cosim_buah_only_df <- cosim_buah_only |> 
  data.frame(row.names = rownames(cosim_buah_only)) |> 
  rownames_to_column(var = "noun_w_buah") |> 
  pivot_longer(cols = -noun_w_buah, 
               names_to = "noun_w_buah_only", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsAllForms with 'buah' and 'buah'") |> 
  filter(noun_w_buah != noun_w_buah_only) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_buah, noun_w_buah_only)), .keep_all = TRUE)

cosim_buah_only_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-buah-all-forms.tsv")

#### 5.1 Cosine similarities between nouns modified by "buah" NON-AFFIXED vs. AFFIXED forms =====
cosim_buah_noaffix_vs_affix <- wordVectors::cosineSimilarity(vsm_with_buah_noaffix_mtx, vsm_with_buah_affixed_mtx)
cosim_buah_noaffix_vs_affix_df <- cosim_buah_noaffix_vs_affix |> 
  data.frame(row.names = rownames(cosim_buah_noaffix_vs_affix)) |> 
  rownames_to_column(var = "noun_w_buah_noaffix") |> 
  pivot_longer(cols = -noun_w_buah_noaffix, 
               names_to = "noun_w_buah_affixed", 
               values_to = "cossim") |> 
  mutate(noun_compared = "Nouns with 'buah' No-Affix vs. Affix") |> 
  filter(noun_w_buah_noaffix != noun_w_buah_affixed) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_buah_noaffix, noun_w_buah_affixed)), .keep_all = TRUE)

cosim_buah_noaffix_vs_affix_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-buah-noaffix-affix.tsv")

#### 5.2 Cosine similarities between nouns modified by "buah" NON-AFFIXED =====
cosim_buah_noaffix <- wordVectors::cosineSimilarity(vsm_with_buah_noaffix_mtx, vsm_with_buah_noaffix_mtx)
cosim_buah_noaffix_df <- cosim_buah_noaffix |> 
  data.frame(row.names = rownames(cosim_buah_noaffix)) |> 
  rownames_to_column(var = "noun_w_buah_noaffix1") |> 
  pivot_longer(cols = -noun_w_buah_noaffix1, 
               names_to = "noun_w_buah_noaffix2", 
               values_to = "cossim") |> 
  mutate(noun_compared = "Nouns with 'buah' No-Affix") |> 
  filter(noun_w_buah_noaffix1 != noun_w_buah_noaffix2) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_buah_noaffix1, noun_w_buah_noaffix2)), .keep_all = TRUE)

cosim_buah_noaffix_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-buah-noaffix-noaffix.tsv")

#### 5.3 Cosine similarities between nouns modified by "buah" AFFIXED =====
cosim_buah_affixed <- wordVectors::cosineSimilarity(vsm_with_buah_affixed_mtx, vsm_with_buah_affixed_mtx)
cosim_buah_affixed_df <- cosim_buah_affixed |> 
  data.frame(row.names = rownames(cosim_buah_affixed)) |> 
  rownames_to_column(var = "noun_w_buah_affixed1") |> 
  pivot_longer(cols = -noun_w_buah_affixed1, 
               names_to = "noun_w_buah_affixed2", 
               values_to = "cossim") |> 
  mutate(noun_compared = "Nouns with 'buah' Affixed") |> 
  filter(cossim != 1, noun_w_buah_affixed1 != noun_w_buah_affixed2) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_buah_affixed1, noun_w_buah_affixed2)), .keep_all = TRUE)

cosim_buah_affixed_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-buah-affix-affix.tsv")

### 6. Cosine similarities between nouns modified by "orang" ALL forms =====
cosim_orang_only <- wordVectors::cosineSimilarity(vsm_with_orang_all_mtx, vsm_with_orang_all_mtx)
cosim_orang_only_df <- cosim_orang_only |> 
  data.frame(row.names = rownames(cosim_orang_only)) |> 
  rownames_to_column(var = "noun_w_orang") |> 
  pivot_longer(cols = -noun_w_orang, 
               names_to = "noun_w_orang_only", 
               values_to = "cossim") |> 
  mutate(noun_compared = "NounsAllForms with 'orang' and 'orang'") |> 
  filter(noun_w_orang != noun_w_orang_only) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_orang, noun_w_orang_only)), .keep_all = TRUE)

cosim_orang_only_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-orang-all-forms.tsv")

cosim_orang_only_df |> 
  # arrange(desc(cossim)) |> 
  slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |> 
  rename(`Nouns modified by ORANG (set 1)` = noun_w_orang,
         `Nouns modified by ORANG (set 2)` = noun_w_orang_only,
         `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |> 
  write_tsv("stats_output/04-sample-cossime-database-within-classifier-orang.tsv")

#### 6.1 Cosine similarities between nouns modified by "orang" NON-AFFIXED vs. AFFIXED forms =====
cosim_orang_noaffix_vs_affix <- wordVectors::cosineSimilarity(vsm_with_orang_noaffix_mtx, vsm_with_orang_affixed_mtx)
cosim_orang_noaffix_vs_affix_df <- cosim_orang_noaffix_vs_affix |> 
  data.frame(row.names = rownames(cosim_orang_noaffix_vs_affix)) |> 
  rownames_to_column(var = "noun_w_orang_noaffix") |> 
  pivot_longer(cols = -noun_w_orang_noaffix, 
               names_to = "noun_w_orang_affixed", 
               values_to = "cossim") |> 
  mutate(noun_compared = "Nouns with 'orang' No-Affix vs. Affix") |> 
  filter(noun_w_orang_noaffix != noun_w_orang_affixed) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_orang_noaffix, noun_w_orang_affixed)), .keep_all = TRUE)

cosim_orang_noaffix_vs_affix_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-orang-noaffix-affix.tsv")

#### 6.2 Cosine similarities between nouns modified by "orang" NON-AFFIXED =====
cosim_orang_noaffix <- wordVectors::cosineSimilarity(vsm_with_orang_noaffix_mtx, vsm_with_orang_noaffix_mtx)
cosim_orang_noaffix_df <- cosim_orang_noaffix |> 
  data.frame(row.names = rownames(cosim_orang_noaffix)) |> 
  rownames_to_column(var = "noun_w_orang_noaffix1") |> 
  pivot_longer(cols = -noun_w_orang_noaffix1, 
               names_to = "noun_w_orang_noaffix2", 
               values_to = "cossim") |> 
  mutate(noun_compared = "Nouns with 'orang' No-Affix") |> 
  filter(noun_w_orang_noaffix1 != noun_w_orang_noaffix2) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_orang_noaffix1, noun_w_orang_noaffix2)), .keep_all = TRUE)

cosim_orang_noaffix_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-orang-noaffix-noaffix.tsv")

#### 6.3 Cosine similarities between nouns modified by "orang" AFFIXED =====
cosim_orang_affixed <- wordVectors::cosineSimilarity(vsm_with_orang_affixed_mtx, vsm_with_orang_affixed_mtx)
cosim_orang_affixed_df <- cosim_orang_affixed |> 
  data.frame(row.names = rownames(cosim_orang_affixed)) |> 
  rownames_to_column(var = "noun_w_orang_affixed1") |> 
  pivot_longer(cols = -noun_w_orang_affixed1, 
               names_to = "noun_w_orang_affixed2", 
               values_to = "cossim") |> 
  mutate(noun_compared = "Nouns with 'orang' Affixed") |> 
  filter(noun_w_orang_affixed1 != noun_w_orang_affixed2) |>  # filter out identical word for it has cosine score of 1 (perfect similarity)
  distinct(across(-c(noun_w_orang_affixed1, noun_w_orang_affixed2)), .keep_all = TRUE)

cosim_orang_affixed_df |> 
  # arrange(desc(cossim)) |> 
  # slice_max(order_by = cossim, n = 10) |> 
  mutate(cossim = round(cossim, 2)) |>
  # rename(`Nouns modified by EKOR` = noun_w_ekor,
  #        `Nouns modified by ORANG` = noun_w_orang,
  #        `Cosine Similarity` = cossim) |> 
  select(-noun_compared) |>
  write_tsv("stats_output/10-cossim-full-only-orang-affix-affix.tsv")

## Total cosim database ====

dfs <- dir("stats_output", pattern = "^(10|09)", full.names = TRUE)
dfs_df <- map(dfs, read_tsv)
dfs_df_nrow <- map_int(dfs_df, nrow)
comparison_database <- data.frame(dbase = str_replace_all(basename(dfs), "\\-", "_"), n_items = dfs_df_nrow)
write_tsv(comparison_database, "stats_output/11-cossim-full-number-of-comparison-items.tsv")


## Comparing the means of the Cosine Similarity ====

### 1. Kruskal Test for "buah", "ekor", "orang" ALL NOUN FORMS =====
cosim_compare_orang_buah_ekor_nounALLFORMS <- cosim_orang_only_df |> 
  rename(n1 = noun_w_orang,
         n2 = noun_w_orang_only) |> 
  mutate(noun_compared = "Nouns all forms with *ORANG* only") |> 
  bind_rows(cosim_buah_only_df |> 
              rename(n1 = noun_w_buah,
                     n2 = noun_w_buah_only) |> 
              mutate(noun_compared = "Nouns all forms with *BUAH* only")) |> 
  bind_rows(cosim_ekor_only_df |> 
              rename(n1 = noun_w_ekor,
                     n2 = noun_w_ekor_only) |> 
              mutate(noun_compared = "Nouns all forms with *EKOR* only")) |> 
  bind_rows(cosim_buah_orang_df |> 
              rename(n1 = noun_w_buah,
                     n2 = noun_w_orang) |> 
              mutate(noun_compared = "Nouns all forms with *BUAH* and *ORANG*")) |> 
  bind_rows(cosim_ekor_buah_df |> 
              rename(n1 = noun_w_ekor,
                     n2 = noun_w_buah) |> 
              mutate(noun_compared = "Nouns all forms with *EKOR* and *BUAH*")) |> 
  bind_rows(cosim_ekor_orang_df |> 
              rename(n1 = noun_w_ekor,
                     n2 = noun_w_orang) |> 
              mutate(noun_compared = "Nouns all forms with *EKOR* and *ORANG*")) |> 
  mutate(condition_group = if_else(str_detect(noun_compared, "\\sand\\s"), "Between classifiers", "Within one classifier")) |> 
  mutate(condition_group = factor(condition_group, levels = c("Within one classifier", "Between classifiers"))) |> 
  mutate(noun_compared = factor(noun_compared, levels = c("Nouns all forms with *EKOR* only",
                                                          "Nouns all forms with *ORANG* only",
                                                          "Nouns all forms with *BUAH* only",
                                                          "Nouns all forms with *EKOR* and *ORANG*",
                                                          "Nouns all forms with *BUAH* and *ORANG*",
                                                          "Nouns all forms with *EKOR* and *BUAH*")))
# cosim_compare_orang_buah_ekor_nounALLFORMS_res <- PMCMRplus::kruskalTest(cossim ~ noun_compared, data = cosim_compare_orang_buah_ekor_nounALLFORMS)
# cosim_compare_orang_buah_ekor_nounALLFORMS_res
# 
# cosim_compare_orang_buah_ekor_nounALLFORMS_posthoc <- PMCMRplus::kwAllPairsNemenyiTest(cossim ~ noun_compared, 
#                                                                                        data = cosim_compare_orang_buah_ekor_nounALLFORMS,
#                                                                                        dist = "Chisquare")
# summary(cosim_compare_orang_buah_ekor_nounALLFORMS_posthoc)

### 

## Reference: https://www.r-bloggers.com/2017/06/add-p-values-and-significance-levels-to-ggplots/amp/ (last access 1 May 2024)

#### a. Kruskal Test for ALL groups =====
res_1_a <- ggpubr::compare_means(cossim ~ noun_compared, data = cosim_compare_orang_buah_ekor_nounALLFORMS, method = "kruskal.test", p.adjust.method = "holm")
res_1_a
res_1_a |> 
  write_tsv("stats_output/01-c-nouns-all-classifiers-GLOBAL-Kruskal-comparison-ALL.tsv")
res_1_a |> 
  write_rds("stats_output/01-c-nouns-all-classifiers-GLOBAL-Kruskal-comparison-ALL.rds")
#### b. Kruskal Test for groups for 'within a classifier' and 'between classifiers' =====
res_1_b <- ggpubr::compare_means(cossim ~ noun_compared, data = cosim_compare_orang_buah_ekor_nounALLFORMS, group.by = "condition_group", method = "kruskal.test", p.adjust.method = "holm")
res_1_b
res_1_b |> 
  write_tsv("stats_output/01-b-nouns-all-classifiers-GLOBAL-Kruskal-comparison-within-and-between-classifiers.tsv")
res_1_b |> 
  write_rds("stats_output/01-b-nouns-all-classifiers-GLOBAL-Kruskal-comparison-within-and-between-classifiers.rds")
#### c. Pairwise comparison ALL groups ====
res_1_c <- ggpubr::compare_means(cossim ~ noun_compared, data = cosim_compare_orang_buah_ekor_nounALLFORMS, method = "wilcox.test", p.adjust.method = "holm")
res_1_c
res_1_c |> 
  write_tsv("stats_output/01-a-nouns-all-classifiers-comparison.tsv")
res_1_c |> 
  write_rds("stats_output/01-a-nouns-all-classifiers-comparison.rds")
res_1_c |> 
  filter(str_detect(group1, "EKOR.+only")) |> 
  # as.data.frame() |> 
  mutate(p = prettyNum(p, digits = 3),
         p.adj = prettyNum(p.adj, digits = 3)) |> 
  write_tsv("stats_output/05-snippet-comparison-EKOR-vs-others.tsv")

res_1_c |> 
  filter(str_detect(group1, "ORANG.+only")) |> 
  # as.data.frame() |> 
  mutate(p = prettyNum(p, digits = 3),
         p.adj = prettyNum(p.adj, digits = 3)) |> 
  write_tsv("stats_output/06-snippet-comparison-ORANG-vs-others.tsv")

res_1_c <- res_1_c |> 
  mutate(orang_buah = if_else(str_detect(group1, "(\\*BUAH\\*|\\*ORANG\\*)\\sonly") & str_detect(group2, "\\*EKOR\\*", TRUE), TRUE, FALSE),
         orang_ekor = if_else(str_detect(group1, "(\\*EKOR\\*|\\*ORANG\\*)\\sonly") & str_detect(group2, "\\*BUAH\\*", TRUE), TRUE, FALSE),
         ekor_buah = if_else(str_detect(group1, "(\\*BUAH\\*|\\*EKOR\\*)\\sonly") & str_detect(group2, "\\*ORANG\\*", TRUE), TRUE, FALSE))
#### d. Pairwise comparison for groups 'within a classifier' and 'between classifiers' =====
res_1_d <- ggpubr::compare_means(cossim ~ noun_compared, data = cosim_compare_orang_buah_ekor_nounALLFORMS, group.by = "condition_group", method = "wilcox.test", p.adjust.method = "holm")
res_1_d
res_1_d |> 
  write_tsv("stats_output/01-d-nouns-all-classifiers-comparison-within-and-between-classifiers.tsv")
res_1_d |> 
  write_rds("stats_output/01-d-nouns-all-classifiers-comparison-within-and-between-classifiers.rds")


##### 1.1 boxplot =====
# 
# mycomparison_orang_buah <- list(c("Nouns all forms with *BUAH* only", "Nouns all forms with *ORANG* only"),
#                                 c("Nouns all forms with *BUAH* only", "Nouns all forms with *BUAH* and *ORANG*"),
#                                 c("Nouns all forms with *ORANG* only", "Nouns all forms with *BUAH* and *ORANG*"))
# mycomparison_orang_ekor <- list(c("Nouns all forms with *EKOR* only", "Nouns all forms with *ORANG* only"),
#                                 c("Nouns all forms with *EKOR* only", "Nouns all forms with *EKOR* and *ORANG*"),
#                                 c("Nouns all forms with *ORANG* only", "Nouns all forms with *EKOR* and *ORANG*"))
# mycomparison_ekor_buah <- list(c("Nouns all forms with *BUAH* only", "Nouns all forms with *EKOR* only"),
#                                 c("Nouns all forms with *BUAH* only", "Nouns all forms with *EKOR* and *BUAH*"),
#                                 c("Nouns all forms with *EKOR* only", "Nouns all forms with *EKOR* and *BUAH*"))
# 
# ###### 1.1.1 ORANG-BUAH
# cosim_compare_orang_buah_ekor_nounALLFORMS |> 
#   # filter(str_detect(noun_compared, "\\*EKOR\\*", negate = TRUE)) |> 
#   # mutate(noun_compared = fct_rev(noun_compared)) |> 
#   ggpubr::ggboxplot(x = "noun_compared",
#                     y = "cossim",
#                     color = "noun_compared") +
#   stat_compare_means(comparisons = mycomparison_orang_buah) +
#   stat_compare_means(label.y = 1.4) +
#   # theme(axis.text.x.bottom = element_markdown(angle = 45,
#   #                                             vjust = 0.5),
#   #       legend.text = element_markdown()) +
#   theme(axis.text.x.bottom = element_blank(),
#         legend.text = element_markdown(),
#         legend.position = "right")


cosim_compare_orang_buah_ekor_nounALLFORMS |>
  mutate(noun_compared = fct_rev(noun_compared)) |>
  ggplot(aes(x = noun_compared, y = cossim)) +
  geom_boxplot(notch = TRUE) +
  coord_flip() +
  labs(y = "Cosine Similarity",
       x = "Classifier types") +
  theme_bw() +
  theme(axis.text.y.left = ggtext::element_markdown(size = 12),
        axis.title.y.left = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        axis.text.x.bottom = element_text(size = 12))
ggsave("Figures/01-boxplot-classifiers-nouns.png",
       width = 8.5,
       height = 6,
       units = "in",
       dpi = 600)

##### 1.2 descriptive statistics =====
res_1_descriptivestats <- cosim_compare_orang_buah_ekor_nounALLFORMS |> 
  group_by(noun_compared) |> 
  summarise(avg = round(mean(cossim), 4), 
            sd = round(sd(cossim), 4), 
            meds = round(median(cossim), 4), 
            iqr = round(IQR(cossim), 4)) |> 
  as.data.frame() |> 
  arrange(desc(avg))
res_1_descriptivestats |> 
  write_tsv("stats_output/07-descriptive-stats-nouns-all.tsv")

### 2. Kruskal Test for "orang" & "buah" AFFIXED vs NO AFFIX ========
cosim_compare_orang_noun_AFFIXED_NOAFFIX <-  cosim_orang_affixed_df |>
  rename(n1 = noun_w_orang_affixed1,
         n2 = noun_w_orang_affixed2) |> 
  mutate(noun_compared = "'Affixed' nouns with *ORANG*") |>
  bind_rows(cosim_orang_noaffix_df |>
              rename(n1 = noun_w_orang_noaffix1,
                     n2 = noun_w_orang_noaffix2) |> 
              mutate(noun_compared = "'Non-Affixed' nouns with *ORANG*")) |> 
  bind_rows(cosim_orang_only_df |> 
              rename(n1 = noun_w_orang,
                     n2 = noun_w_orang_only) |> 
              mutate(noun_compared = "Nouns all forms with *ORANG*")) |>
  mutate(condition_group = "orang",
         noun_compared = factor(noun_compared, levels = c("Nouns all forms with *ORANG*", 
                                                          "'Affixed' nouns with *ORANG*",
                                                          "'Non-Affixed' nouns with *ORANG*")))

cosim_compare_buah_noun_AFFIXED_NOAFFIX <-  cosim_buah_affixed_df |>
  rename(n1 = noun_w_buah_affixed1,
         n2 = noun_w_buah_affixed2) |> 
  mutate(noun_compared = "'Affixed' nouns with *BUAH*") |>
  bind_rows(cosim_buah_noaffix_df |>
              rename(n1 = noun_w_buah_noaffix1,
                     n2 = noun_w_buah_noaffix2) |> 
              mutate(noun_compared = "'Non-Affixed' nouns with *BUAH*")) |> 
  bind_rows(cosim_buah_only_df |> 
              rename(n1 = noun_w_buah,
                     n2 = noun_w_buah_only) |> 
              mutate(noun_compared = "Nouns all forms with *BUAH*")) |>
  mutate(condition_group = "buah",
         noun_compared = factor(noun_compared, levels = c("Nouns all forms with *BUAH*", 
                                                          "'Affixed' nouns with *BUAH*",
                                                          "'Non-Affixed' nouns with *BUAH*")))

cosim_affix_compare <- bind_rows(cosim_compare_buah_noun_AFFIXED_NOAFFIX,
                                 cosim_compare_orang_noun_AFFIXED_NOAFFIX) |> 
  mutate(condition_group = factor(condition_group, levels = c("buah", "orang")))

## Reference: https://www.r-bloggers.com/2017/06/add-p-values-and-significance-levels-to-ggplots/amp/ (last access 1 May 2024)

#### b. Kruskal Test for per groups (i.e., by "buah" and "orang") ====
res_affix_b <- compare_means(cossim ~ noun_compared, data = cosim_affix_compare, group.by = "condition_group", method = "kruskal.test", p.adjust.method = "holm")
res_affix_b
##### b.1 for "buah" using kruskal.test from the stats package for comparison with compare_means()
kruskal.test(cossim ~ noun_compared, data = cosim_compare_buah_noun_AFFIXED_NOAFFIX)
# Kruskal-Wallis rank sum test
# 
# data:  cossim by noun_compared
# Kruskal-Wallis chi-squared = 6493, df = 2, p-value < 2.2e-16

# kruskal.test(cossim ~ noun_compared, data = cosim_compare_buah_noun_AFFIXED_NOAFFIX)$statistic
# kruskal.test(cossim ~ noun_compared, data = cosim_compare_buah_noun_AFFIXED_NOAFFIX)$p.value
# kruskal.test(cossim ~ noun_compared, data = cosim_compare_buah_noun_AFFIXED_NOAFFIX)$parameter
##### b.2 for "orang" using kruskal.test from the stats package for comparison with compare_means()
kruskal.test(cossim ~ noun_compared, data = cosim_compare_orang_noun_AFFIXED_NOAFFIX)
# Kruskal-Wallis rank sum test
# 
# data:  cossim by noun_compared
# Kruskal-Wallis chi-squared = 1566.9, df = 2, p-value < 2.2e-16

# kruskal.test(cossim ~ noun_compared, data = cosim_compare_orang_noun_AFFIXED_NOAFFIX)$statistic
# kruskal.test(cossim ~ noun_compared, data = cosim_compare_orang_noun_AFFIXED_NOAFFIX)$p.value
# kruskal.test(cossim ~ noun_compared, data = cosim_compare_orang_noun_AFFIXED_NOAFFIX)$parameter

#### c. Pairwise comparison for per groups (i.e., by "buah" and "orang") ====
res_affix_c <- compare_means(cossim ~ noun_compared, data = cosim_affix_compare, group.by = "condition_group", method = "wilcox.test", p.adjust.method = "holm")
res_affix_c
res_affix_c |> 
  write_tsv("stats_output/02-affixed-nonaffixed-pairwise-comparison.tsv")
res_affix_c |> 
  write_rds("stats_output/02-affixed-nonaffixed-pairwise-comparison.rds")

#### d. Pairwise comparison for ALL groups ====
res_affix_d <- compare_means(cossim ~ noun_compared, data = cosim_affix_compare, method = "wilcox.test", p.adjust.method = "holm")
res_affix_d

#### e. Finding the Descriptive Stats for Cosine Similarity comparison for data in Figure "02-boxplot-affixed-nonaffixed-buah-orang.png"
cosim_affix_compare |> 
  group_by(condition_group, noun_compared) |> 
  summarise(mean_cosim = mean(cossim),
            sd_cosim = sd(cossim),
            median_cosim = median(cossim),
            IQR_cosim = IQR(cossim)) |> 
  mutate(across(matches("^(mean|sd|median|IQR)_"), ~round(., digits = 4))) |> 
  as.data.frame() |> 
  write_tsv("stats_output/08-descriptive-stats-nouns-BUAH-ORANG.tsv")

##### 2.1 boxplot =====
cosim_affix_compare |> 
  rename(classifier = condition_group) |> 
  mutate(classifier = fct_recode(classifier,
                                 `*BUAH*` = "buah",
                                 `*ORANG*` = "orang")) |> 
  mutate(noun_compared = fct_recode(noun_compared,
                                    `Affixed nouns` = "'Affixed' nouns with *BUAH*",
                                    `Affixed nouns` = "'Affixed' nouns with *ORANG*",
                                    `Non-Affixed nouns` = "'Non-Affixed' nouns with *ORANG*",
                                    `Non-Affixed nouns` = "'Non-Affixed' nouns with *BUAH*",
                                    `Nouns all forms` = "Nouns all forms with *ORANG*",
                                    `Nouns all forms` = "Nouns all forms with *BUAH*")) |> 
  # mutate(noun_compared = fct_rev(noun_compared)) |>
  ggplot(aes(x = noun_compared, y = cossim)) +
  geom_boxplot(notch = TRUE) +
  facet_wrap(~classifier, scales = "free_x", labeller = "label_both") +
  # coord_flip() +
  labs(y = "Cosine Similarity",
       x = "Noun Types") +
  theme_bw() +
  theme(axis.text.x = ggtext::element_markdown(size = 12),
        axis.title = element_text(size = 14),
        strip.text.x.top = element_markdown(size = 16),
        axis.text.y.left = element_text(size = 12))
ggsave("Figures/02-boxplot-affixed-nonaffixed-buah-orang.png",
       width = 9,
       height = 6,
       units = "in",
       dpi = 600)








# 
# ### 2. Kruskal Test for "orang" AFFIXED vs NO AFFIX 
# cosim_compare_orang_noun_AFFIXED_NOAFFIX <-  cosim_orang_affixed_df |> 
#   rename(n1 = noun_w_orang_affixed1,
#          n2 = noun_w_orang_affixed2) |> 
#   bind_rows(cosim_orang_noaffix_df |> 
#               rename(n1 = noun_w_orang_noaffix1,
#                      n2 = noun_w_orang_noaffix2)) |> 
#   mutate(noun_compared = factor(noun_compared, levels = c("Nouns with 'orang' Affixed", "Nouns with 'orang' No-Affix")))
# cosim_compare_orang_noun_AFFIXED_NOAFFIX_res <- PMCMRplus::kruskalTest(cossim ~ noun_compared, data = cosim_compare_orang_noun_AFFIXED_NOAFFIX)
# cosim_compare_orang_noun_AFFIXED_NOAFFIX_res 
# 
# cosim_compare_orang_noun_AFFIXED_NOAFFIX_posthoc <- PMCMRplus::kwAllPairsNemenyiTest(cossim ~ noun_compared, 
#                                                                                        data = cosim_compare_orang_noun_AFFIXED_NOAFFIX,
#                                                                                        dist = "Chisquare")
# summary(cosim_compare_orang_noun_AFFIXED_NOAFFIX_posthoc)
# 
# #### 2.1 Boxplot 
# cosim_compare_orang_noun_AFFIXED_NOAFFIX |> 
#   mutate(noun_compared = fct_rev(noun_compared)) |>  
#   ggplot(aes(x = noun_compared, y = cossim)) + 
#   geom_boxplot(notch = TRUE) +
#   coord_flip() +
#   labs(y = "Cosine Similarity",
#        x = "Noun types modified by *ORANG*") +
#   theme(axis.title.y.left = ggtext::element_markdown())
# 
# ### 3. Kruskal Test for "buah" AFFIXED vs NO AFFIX 
# cosim_compare_buah_noun_AFFIXED_NOAFFIX <-  cosim_buah_affixed_df |> 
#   rename(n1 = noun_w_buah_affixed1,
#          n2 = noun_w_buah_affixed2) |> 
#   bind_rows(cosim_buah_noaffix_df |> 
#               rename(n1 = noun_w_buah_noaffix1,
#                      n2 = noun_w_buah_noaffix2)) |> 
#   mutate(noun_compared = factor(noun_compared, levels = c("Nouns with 'buah' Affixed", "Nouns with 'buah' No-Affix")))
# cosim_compare_buah_noun_AFFIXED_NOAFFIX_res <- PMCMRplus::kruskalTest(cossim ~ noun_compared, data = cosim_compare_buah_noun_AFFIXED_NOAFFIX)
# cosim_compare_buah_noun_AFFIXED_NOAFFIX_res 
# 
# cosim_compare_buah_noun_AFFIXED_NOAFFIX_posthoc <- PMCMRplus::kwAllPairsNemenyiTest(cossim ~ noun_compared, 
#                                                                                      data = cosim_compare_buah_noun_AFFIXED_NOAFFIX,
#                                                                                      dist = "Chisquare")
# summary(cosim_compare_buah_noun_AFFIXED_NOAFFIX_posthoc)
# 
# #### 3.1 Boxplot 
# cosim_compare_buah_noun_AFFIXED_NOAFFIX |> 
#   mutate(noun_compared = fct_rev(noun_compared)) |>  
#   ggplot(aes(x = noun_compared, y = cossim)) + 
#   geom_boxplot(notch = TRUE) +
#   coord_flip() +
#   labs(y = "Cosine Similarity",
#        x = "Noun types modified by *BUAH*") +
#   theme(axis.title.y.left = ggtext::element_markdown())
