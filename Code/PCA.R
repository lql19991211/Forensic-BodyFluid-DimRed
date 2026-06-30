# 1. Load required packages
library(readxl)
library(dplyr)
library(ggplot2)

# 2. Read data and separate metadata from gene expression matrix
raw_data <- read_excel("data/PB.xlsx", .name_repair = "unique")

metadata <- raw_data[, 1:2] %>% 
  rename(tissue = 1, sample = 2) %>%
  mutate(sample = as.character(sample))

gene_matrix <- raw_data[, -(1:2)] %>% 
  as.matrix() %>%
  `rownames<-`(metadata$sample)

if (!all(sapply(gene_matrix, is.numeric))) stop("Non-numeric data found in gene columns. Please check the input file!")

# 3. Data preprocessing: Log2 transformation and removal of all-zero genes
log_transformed <- log2(gene_matrix + 1)
non_zero_cols <- colSums(log_transformed) > 0
log_filtered <- log_transformed[, non_zero_cols]
cat("Number of all-zero genes removed:", sum(!non_zero_cols), "\n")

# 4. Perform PCA and calculate variance explained
pca_result <- prcomp(log_filtered, center = TRUE, scale. = FALSE)
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# 5. Build plot data frame and generate PCA scatter plot with confidence ellipses
plot_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Tissue = metadata$tissue,
  SampleID = metadata$sample
)

ggplot(plot_data, aes(x = PC1, y = PC2, color = Tissue)) +
  geom_point(size = 4, alpha = 0.8) +
  theme_bw() +
  labs(
    x = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)"),
    title = "PCA of Log2-Transformed Gene Expression"
  ) +
  stat_ellipse(level = 0.95) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )