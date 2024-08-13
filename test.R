# Step 1: Load the necessary libraries
library(tidyverse)
library(ggplot2)
library(ggfortify)  # Useful for creating biplots
library(readr)

# Step 2: Load the data
url <- "https://raw.githubusercontent.com/aeiwz/urine_metabolite_example/main/data/human_cachexia.csv"
data <- read_csv(url)

# Step 3: Rename the column to remove space (if needed)
data <- data %>% rename(Muscle_loss = `Muscle loss`)

# Step 4: Prepare the data for PCA
pca_data <- data[,3:ncol(data)]
# Ensure 'Muscle_loss' is a factor
data$Muscle_loss <- as.factor(data$Muscle_loss)

# Step 5: Perform PCA
pca_result <- prcomp(pca_data, scale. = TRUE)

# Step 6: Extract PCA scores
pca_scores <- as.data.frame(pca_result$x)

# Combine PCA scores with the original metadata
pca_scores <- cbind(pca_scores, Muscle_loss = data$Muscle_loss)

# Step 7: Plot PCA scores (PC1 vs PC2)
score_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = Muscle_loss)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "PCA Score Plot",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  scale_color_manual(values = c("blue", "red"))  # Customize colors as needed

# Display the PCA score plot
print(score_plot)

# Step 8: Create a biplot
biplot(pca_result, scale = 0, col = c("blue", "red"))

# Optional: Custom biplot using ggplot2 and ggbiplot package
# Uncomment the following if you prefer a custom biplot (requires ggbiplot package)
# library(ggbiplot)
# ggbiplot(pca_result, obs.scale = 1, var.scale = 1,
#          groups = data$Muscle_loss, ellipse = TRUE, circle = TRUE) +
#  scale_color_manual(values = c("blue", "red")) +
#  theme_minimal() +
#  labs(title = "PCA Biplot", x = "PC1", y = "PC2")



#T-test all feature






















