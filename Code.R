
# Load required libraries
library(factoextra)
library(FactoMineR)
library(dplyr)
library(gt)
library(ggrepel)

# Read data from CSV file
data <- read.csv("Morph.csv")

# Extract Genotype names
genotype_names <- data[, "Gen"]  # Assuming "Gen" is the actual column name

# Convert genotype column to factor
data$Gen <- as.factor(data$Gen)

# Perform PCA
pca_res <- PCA(data[, -c(1, 2)], graph = FALSE)

# Generate biplot and manually add labels with different colors
biplot <- fviz_pca_biplot(pca_res, geom = "point", pointshape = 21, pointsize = 2,
                          palette = "RdYlBu",  # Using a different color palette
                          addEllipses = TRUE,
                          alpha.var = "contrib", col.var = "black",
                          gradient.cols = "RdYlBu",
                          legend.title = list(fill = colnames(data)[2], color = "Contrib",
                                              alpha = "Contrib")) +
    geom_text_repel(aes(label = genotype_names, color = data$Gen), 
                    size = 3, max.overlaps = Inf) +
    scale_color_manual(values = rainbow(length(levels(data$Gen))),
                       name = "Genotype", labels = paste("Genotype", 1:length(levels(data$Gen))))  # Set legend title and labels

# Save biplot
ggsave(plot = biplot, filename = "biplot_with_genotype_colors.pdf", width = 10, height = 10)


# Save biplot as JPEG
ggsave(plot = biplot, filename = "biplot_with_genotype_colors.jpg", width = 10, height = 10)

