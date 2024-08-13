# load packages
library(metabom8)



#Load data from github
data <- read.csv("https://raw.githubusercontent.com/aeiwz/urine_metabolite_example/main/data/human_cachexia.csv")

#Data manipulation
rownames(data) <- data$Patient.ID


# Declare variables
X<-data[, 3:ncol(data)]
dim(X)

# get column names of X to list
metabolite<-colnames(X) 
length(metabolite)


meta<-data[,1:2]  # spectrometer metadata
dim(meta)


an<-meta      # sample annotation
dim(an)


# list all environment varaibales
ls()

# Perform PCA
pca_model=pca(X=X, pc=2, scale='UV', center=TRUE)

# Plot PCA results: scores of the first two components
plotscores(obj=pca_model, pc=c(1,2), an=list(Intervention=meta$Muscle.loss), title='PCA - Scores plot')

# define scores that should be labelled
idx<-which(pca_model@t[,1]>10)

# construct label vector with mouse IDs
outliers<-rep('', nrow(an))
outliers[idx]<-an$Patient.ID[idx]

# Plot PCA scores, colour according to class, point shape according to time of sample collection and label outliers
plotscores(obj=pca_model, pc=c(1,2), an=list(
  Class=an$Muscle.loss,         # point colour
  ID=outliers),           # point label
  title='PCA - Scores plot')

# Plot PCA scores, colour according to class, point shape according to time of sample collection and label outliers
plotscores(obj=pca_model, pc=c(1,2), an=list(
  Class=an$Muscle.loss,         # point colour
  Timepoint=an$Muscle.loss, # point shape
  ID=outliers),           # point label
  title='PCA - Scores plot')


# Plot PCA loadings
plotloadings(obj=pca_model, pc=c(1,2), metabolite=metabolite, title='PCA - Loadings plot')





# Load necessary libraries
library(readr)
library(ggplot2)
library(ggfortify)  # For visualizing PCA
library(dplyr)

# Read the CSV file directly from the URL
url <- "https://raw.githubusercontent.com/aeiwz/urine_metabolite_example/main/data/human_cachexia.csv"
data <- read_csv(url)

# Check the structure of the data
str(data)

# Optional: Remove non-numeric columns (e.g., sample IDs or labels) if present
# Assuming the first column is non-numeric (e.g., sample names), you can remove it like this:
data_numeric <- data %>%
  select_if(is.numeric)

# Perform PCA analysis
pca_result <- prcomp(data_numeric, scale. = TRUE)

# Summary of PCA to check variance explained by each principal component
summary(pca_result)

# Visualize PCA
autoplot(pca_result, data = data, colour = 'group') +
  theme_minimal() +
  labs(title = "PCA of Human Cachexia Data")

# You can replace 'group' with the actual column name that contains the grouping variable



