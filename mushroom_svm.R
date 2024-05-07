# title: Mushroom Classification in SVM using R
# author: Joriz Villanueva
# date: 21.04.2024

# Environment Preparation
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(kernlab))
library(ggplot2)
library(plotly)
library(visdat)
library(lattice)
library(caret)
library(e1071)
library(kernlab)

# Reading Dataset
mushrooms.df <- read.csv("/home/jrzvnn/Documents/Projects/mushroom-svm/mushrooms.csv")

###############################################
#          Exploratory Data Analysis          #
###############################################

# Class Distribution
labels <- c('Edible', 'Poisonous')
freq <- sum(mushrooms.df$class == "e")
count <- length(mushrooms.df$class)
values <- c(freq, count - freq)
colors <- c('#018100', '#fe0001')

# Plotting class distribution
fig <- plot_ly(labels = labels, values = values, type = 'pie', opacity = 1) %>%
  layout(title = list(text = 'Distribution of the Mushrooms by their Classes', x = 0.479, 
                      font = list(size = 18, family = "Arial", color = "black")), 
         showlegend = TRUE) %>%
  add_trace(marker = list(line = list(color = '#000000', width = 2), colors = colors),
            textinfo = 'percent+label')
fig

# Missing Values
vis_miss(mushrooms.df)

# Dataset Dimensions
dim(mushrooms.df)

# Class Distributions for every Features

# Define function to convert ggplot to plotly
ggplot_to_plotly <- function(ggplot_obj){
  p <- ggplotly(ggplot_obj)
  p <- layout(p, title = list(text = ggplot_obj$labels$title, x = 0.02),
              showlegend = TRUE)
  return(p)
}

#Taking the backup the data set Mushroom & checking the distinct value of it
mush <- mushrooms.df %>% distinct()

# Convert ggplot histograms to plotly histograms with customizations
cap_shape_hist <- ggplot(mush, aes(x = cap.shape, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) + 
  ggtitle("CAP SHAPE") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
cap_shape_plotly <- ggplot_to_plotly(cap_shape_hist)
cap_shape_plotly

# Bar Graph for plotting of few features - cap surface, cap color, bruises, odor
cap_surface_hist <- ggplot(mush, aes(x = cap.surface, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("CAP SURFACE") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
cap_surface_plotly <- ggplot_to_plotly(cap_surface_hist)
cap_surface_plotly

cap_color_hist <- ggplot(mush, aes(x = cap.color, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("CAP COLOR") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
cap_color_plotly <- ggplot_to_plotly(cap_color_hist)
cap_color_plotly

bruises_hist <- ggplot(mush, aes(x = bruises, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("BRUISES") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
bruises_plotly <- ggplot_to_plotly(bruises_hist)
bruises_plotly

odor_hist <- ggplot(mush, aes(x = odor, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("ODOR") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
odor_plotly <- ggplot_to_plotly(odor_hist)
odor_plotly


# Bar Graph for plotting of few features - Gill Attachment, Gill Spacing, Gill Size, Gill Color
gill_attachment <- ggplot(mush, aes(x = gill.attachment, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("GILL ATTACHMENT") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
gill_spacing <- ggplot(mush, aes(x = gill.spacing, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("GILL SPACING") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
gill_size <- ggplot(mush, aes(x = gill.size, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("GILL SIZE") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
gill_color <- ggplot(mush, aes(x = gill.color, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("GILL COLOR") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))

gill_attachment_plotly <- ggplot_to_plotly(gill_attachment)
gill_spacing_plotly <- ggplot_to_plotly(gill_spacing)
gill_size_plotly <- ggplot_to_plotly(gill_size)
gill_color_plotly <- ggplot_to_plotly(gill_color)

gill_attachment_plotly
gill_spacing_plotly
gill_size_plotly
gill_color_plotly


# Bar Graph for plotting of few features - Stalk Shape & Root
stalk_shape <- ggplot(mush, aes(x = stalk.shape, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("STALK SHAPE") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
stalk_root <- ggplot(mush, aes(x = stalk.root, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("STALK ROOT") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
stalk_shape_plotly <- ggplot_to_plotly(stalk_shape)
stalk_root_plotly <- ggplot_to_plotly(stalk_root)
stalk_shape_plotly
stalk_root_plotly


# Bar Graph for plotting of few features - Stalk Surface above & below ring
stalk_surface_above <- ggplot(mush, aes(x = stalk.surface.above.ring, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("STALK SURFACE ABOVE RING") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
stalk_surface_below <- ggplot(mush, aes(x = stalk.surface.below.ring, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("STALK SURFACE BELOW RING") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
stalk_surface_above_plotly <- ggplot_to_plotly(stalk_surface_above)
stalk_surface_below_plotly <- ggplot_to_plotly(stalk_surface_below)
stalk_surface_above_plotly
stalk_surface_below_plotly



# Bar Graph for plotting of few features - Stalk Color above & below ring
stalk_color_above <- ggplot(mush, aes(x = stalk.color.above.ring, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("STALK COLOR ABOVE RING") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
stalk_color_below <- ggplot(mush, aes(x = stalk.color.below.ring, fill = class)) + 
  geom_bar(alpha = 1, colour = "black", linewidth = 0.5) +  
  ggtitle("STALK COLOR BELOW RING") + 
  theme(plot.title = element_text(hjust = 0)) + 
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))
stalk_color_above_plotly <- ggplot_to_plotly(stalk_color_above)
stalk_color_below_plotly <- ggplot_to_plotly(stalk_color_below)
stalk_color_above_plotly
stalk_color_below_plotly


# Bar Graph for plotting of few features - Veil Type & Veil Color
veil_type <- ggplot(mush, aes(x = veil.type, fill = class)) + 
  geom_bar(colour = "black", linewidth = 0.5) +  
  ggtitle("VEIL TYPE") + 
  theme(plot.title = element_text(hjust = 0)) +
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))

veil_color <- ggplot(mush, aes(x = veil.color, fill = class)) + 
  geom_bar(colour = "black", linewidth = 0.5) +  
  ggtitle("VEIL COLOR") + 
  theme(plot.title = element_text(hjust = 0)) +
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))

veil_type_plotly <- ggplot_to_plotly(veil_type)
veil_color_plotly <- ggplot_to_plotly(veil_color)

veil_type_plotly
veil_color_plotly


# Bar Graph for plotting of few features - Ring Number & Type
ring_number <- ggplot(mush, aes(x = ring.number, fill = class)) + 
  geom_bar(colour = "black", linewidth = 0.5) +  
  ggtitle("RING NUMBER") + 
  theme(plot.title = element_text(hjust = 0)) +
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))

ring_type <- ggplot(mush, aes(x = ring.type, fill = class)) + 
  geom_bar(colour = "black", linewidth = 0.5) +  
  ggtitle("RING TYPE") + 
  theme(plot.title = element_text(hjust = 0)) +
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))

ring_number_plotly <- ggplot_to_plotly(ring_number)
ring_type_plotly <- ggplot_to_plotly(ring_type)

ring_number_plotly
ring_type_plotly


# Bar Graph for plotting of few features - Spore Print Color, Population & Habitat
spore_print_color <- ggplot(mush, aes(x = spore.print.color, fill = class)) + 
  geom_bar(colour = "black", linewidth = 0.5) +  
  ggtitle("SPORE PRINT COLOR") + 
  theme(plot.title = element_text(hjust = 0)) +
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))

population <- ggplot(mush, aes(x = population, fill = class)) + 
  geom_bar(colour = "black", linewidth = 0.5) +  
  ggtitle("POPULATION") + 
  theme(plot.title = element_text(hjust = 0)) +
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))

habitat <- ggplot(mush, aes(x = habitat, fill = class)) + 
  geom_bar(colour = "black", linewidth = 0.5) +  
  ggtitle("HABITAT") + 
  theme(plot.title = element_text(hjust = 0)) +
  scale_fill_manual(values = c("e" = "#018100", "p" = "#fe0001"))

spore_print_color_plotly <- ggplot_to_plotly(spore_print_color)
population_plotly <- ggplot_to_plotly(population)
habitat_plotly <- ggplot_to_plotly(habitat)

spore_print_color_plotly
population_plotly
habitat_plotly

# Set the theme settings
my_theme <- theme(
  plot.title = element_text(size = 20, hjust = 0.5),  # Center the plot title and increase its size
  legend.title = element_blank(),                      # Remove legend title
  legend.text = element_text(size = 14),               # Increase legend text size
  axis.title = element_text(size = 16),                # Increase axis title size
  axis.text = element_text(size = 14)                  # Increase axis text size
)

# visualization of instances' distributions
ggplot(mush, aes(x = cap.color, y = bruises, col = class)) + 
  geom_jitter() + ggtitle("Relation between Cap color, Bruises \n and their classification") +
  scale_color_manual(values = c("#018100", "#fe0001"), 
                     breaks = c("e", "p"), 
                     labels = c("edible", "poisonous")) +
  my_theme +
  theme(plot.margin = margin(20, 20, 20, 20))  # Add margin to center the plot and increase its size

ggplot(mush, aes(x = gill.color, y = spore.print.color, col = class)) + 
  geom_jitter() + ggtitle("Relation between Gill color, Spore print color \n and their classification") +
  scale_color_manual(values = c("#018100", "#fe0001"), 
                     breaks = c("e", "p"), 
                     labels = c("edible", "poisonous")) +
  my_theme +
  theme(plot.margin = margin(20, 20, 20, 20))

ggplot(mush, aes(x = population, y = odor, col = class)) + 
  geom_jitter() + ggtitle("Relation between Population, Odor \n and their classification") +
  scale_color_manual(values = c("#018100", "#fe0001"), 
                     breaks = c("e", "p"), 
                     labels = c("edible", "poisonous")) +
  my_theme +
  theme(plot.margin = margin(20, 20, 20, 20))

ggplot(mush, aes(x = class, y = gill.color, col = class)) + 
  geom_jitter(width = 0.25) + ggtitle("Classification of instances, based on Gill color ") +
  scale_color_manual(values = c("#018100", "#fe0001"), 
                     breaks = c("e", "p"), 
                     labels = c("edible", "poisonous")) +
  my_theme +
  theme(plot.margin = margin(20, 20, 20, 20))

ggplot(mush, aes(x = class, y = odor, col = class)) + 
  geom_jitter(width = 0.25) + ggtitle("Classification of instances, based on Odor") +
  scale_color_manual(values = c("#018100", "#fe0001"), 
                     breaks = c("e", "p"), 
                     labels = c("edible", "poisonous")) +
  my_theme +
  theme(plot.margin = margin(20, 20, 20, 20))

# This will visualize the distribution of 'stalk.root' indicating its limited relevance in classifying the mushrooms.
ggplot(mushrooms.df, aes(x = class, y = stalk.root, col = class)) + 
  geom_jitter() + ggtitle("Classification of instances, based on Stalk root") +
  scale_color_manual(values = c("#018100", "#fe0001"), 
                     breaks = c("e", "p"), 
                     labels = c("edible", "poisonous")) +
  my_theme +
  theme(plot.margin = margin(20, 20, 20, 20))


###############################################
#             Data Preprocessing              #
###############################################

# Removing veil.type & stalk.root
mushrooms.df$veil.type <- NULL 
mushrooms.df$stalk.root <- NULL

# Data Splitting 70% | 30%
split.data <- function(data, p, s = 1){
  set.seed(s)
  index <- sample(1:dim(data)[1])
  train <- data[index[1:floor(dim(data)[1] * p)], ]
  test <- data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)[1]], ]
  return(list(train = train, test = test)) 
}

set.seed(42)
allset <- split.data(mushrooms.df, p = 0.7, s = 42)
mushrooms.training <- allset$train
mushrooms.test <- allset$test

# Check Train/Test Data Ratio
total_rows <- nrow(mushrooms.df) # Check the number of rows in the original dataset
cat("Total number of rows in the original dataset:", total_rows, "\n")

# Check the number of rows in the train and test datasets
train_rows <- nrow(mushrooms.training)
test_rows <- nrow(mushrooms.test)
cat("Number of rows in the train dataset:", train_rows, "\n")
cat("Number of rows in the test dataset:", test_rows, "\n")
cat("Train/Total Ratio:", nrow(mushrooms.training) / total_rows, "\n")
cat("Test/Total Ratio:", nrow(mushrooms.test) / total_rows, "\n")

# training set is still balanced
class.table <- table(mushrooms.training$class)
bp <- barplot(class.table, xlab="Classifications", ylab="Amount", 
        main="Distribution of classifications",
        col = c("#018100", "#fe0001") , border = "white", ylim = c(0, 3200))
text(bp, class.table, paste(class.table), pos = 3, cex = 1) 

# Extract the test data
test_data <- mushrooms.test

# Write the test data to a CSV file
# write.csv(test_data, file = "/home/jrzvnn/Documents/Projects/mushroom-svm/mushrooms_test.csv", row.names = FALSE)

str(mushrooms.df)

###############################################
#               Model Training                #
###############################################

### SVM Training

# Convert class variables to factors with the same levels
mushrooms.training$class <- as.factor(mushrooms.training$class)

svm.model <- svm(class ~ ., data = mushrooms.training, kernel = 'linear')
print(svm.model)

# Save the SVM model to a file
saveRDS(svm.model, file = "/home/jrzvnn/Documents/Projects/mushroom-svm/svm_model.rds")

###############################################
#           Performance Evaluation            #
###############################################

### Confusion Matrix

# Predict using the SVM model on the test set
svm.pred <- predict(svm.model, mushrooms.test)

# Convert class variables to factors with the same levels
mushrooms.test$class <- factor(mushrooms.test$class, levels = levels(svm.pred))

### Precision, Recall and F-Measure

# Compute confusion matrix with precision-recall mode
confusionMatrix(mushrooms.test$class, svm.pred, mode = "prec_recall")

