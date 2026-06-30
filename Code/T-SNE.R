# 1. Load required packages
rm(list = ls())
library(ggplot2)
library(dplyr)
library(Rtsne)
library(openxlsx)

# 2. Import data and verify dimensions
df <- read.csv("data/PB.xlsx", check.names = FALSE)
cat("Data dimensions (Rows, Columns):", dim(df), "\n")

# 3. Data preprocessing: Separate metadata and gene data, verify numeric format
metadata <- df[, 1:2]
gene_data <- df[, -c(1:2)]
if (!all(sapply(gene_data, is.numeric))) stop("Non-numeric data found in gene columns. Please check the input file!")

# 4. Log transformation: log2(x+1) and handle zero-variance columns/invalid values
log_data <- log2(gene_data + 1)
zero_var_cols <- which(apply(log_data, 2, sd) == 0)
if (length(zero_var_cols) > 0) {
  warning("Zero-variance columns detected and removed: ", length(zero_var_cols))
  log_data <- log_data[, -zero_var_cols]
}
if (any(is.na(log_data)) || any(is.infinite(as.matrix(log_data)))) stop("Invalid values detected after log transformation!")

# 5. Perform t-SNE dimensionality reduction
set.seed(123)
tsne_result <- Rtsne(
  X = as.matrix(log_data),
  dims = 2,
  perplexity = 35,
  max_iter = 1000,
  pca = FALSE,
  verbose = TRUE,
  check_duplicates = FALSE
)

# 6. Build and export machine learning dataset
ml_dataset <- data.frame(
  SampleID = metadata[, 2],
  Tissue = metadata[, 1],
  tSNE1 = tsne_result$Y[, 1],
  tSNE2 = tsne_result$Y[, 2]
)
output_path <- "E:/Desktop/赵/t-sne_Results.xlsx"
openxlsx::write.xlsx(ml_dataset, file = output_path, colNames = TRUE, rowNames = FALSE)
cat("Dimensionality reduction data saved to:", output_path, "\n")

# 7. Visualization: Generate t-SNE scatter plot
plot_data <- data.frame(
  tSNE1 = tsne_result$Y[, 1],
  tSNE2 = tsne_result$Y[, 2],
  Tissue = metadata[, 1],
  SampleID = metadata[, 2]
)

ggplot(plot_data, aes(x = tSNE1, y = tSNE2, color = Tissue)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw() +
  labs(
    x = "t-SNE Dimension 1",
    y = "t-SNE Dimension 2"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )