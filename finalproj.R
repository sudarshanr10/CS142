library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(dplyr)
library(ggplot2)


#Load and Update Dataset
setwd("C:\Users\Plasm\OneDrive\Documents\labels.csv")
data <- read.csv('labels.csv')

#Data Preprocessing and Feature Engineering From Animal Categories
data <- data %>%
  filter(category != "Other") %>%
  droplevels() %>%
  mutate(
    is_aquatic = case_when(
      category == "Underwater Specialists" ~ 1,
      TRUE ~ 0
    ),
    has_wings = case_when(
      category %in% c("Flight Masters", "Stealth & Shadows") ~ 1,
      TRUE ~ 0
    ),
    is_armored = case_when(
      category %in% c("Tough Defenders", "Tiny Survivors") ~ 1,
      TRUE ~ 0
    ),
    group_behavior = case_when(
      category %in% c("Pack Hunters & Social Strategists", "Apex Predators") ~ 1,
      TRUE ~ 0
    ),
    intelligence_level = case_when(
      category == "Survival Geniuses" ~ "High",
      category %in% c("Tiny Survivors", "Tough Defenders") ~ "Low",
      category %in% c("Pack Hunters & Social Strategists", "Apex Predators", "Stealth & Shadows") ~ "Medium",
      TRUE ~ "Medium"
    ),
    # Mobility type (Land / Air / Water)
    mobility_type = case_when(
      category %in% c("Flight Masters", "Stealth & Shadows") ~ "Air",
      category == "Underwater Specialists" ~ "Water",
      TRUE ~ "Land"
    ),
    climate_type = case_when(
      category == "Cold-Climate Survivors" ~ "Cold",
      TRUE ~ "Varied"
    ),
    diet_type = case_when(
      category %in% c("Apex Predators", "Pack Hunters & Social Strategists") ~ "Carnivore",
      category %in% c("Tiny Survivors", "Flight Masters") ~ "Omnivore",
      TRUE ~ "Unknown"
    ),
    is_nocturnal = case_when(
      category == "Stealth & Shadows" ~ 1,
      TRUE ~ 0
    ),
    mobility_speed = case_when(
      category == "Speed Demons" ~ "Fast",
      category == "Tough Defenders" ~ "Slow",
      TRUE ~ "Average"
    ),
    
    # Classification target
    Class = as.factor(category)
  ) %>%
  mutate(
    is_aquatic = as.factor(is_aquatic),
    has_wings = as.factor(has_wings),
    is_armored = as.factor(is_armored),
    group_behavior = as.factor(group_behavior),
    intelligence_level = factor(intelligence_level, levels = c("Low", "Medium", "High")),
    mobility_type = as.factor(mobility_type),
    climate_type = as.factor(climate_type),
    diet_type = as.factor(diet_type),
    is_nocturnal = as.factor(is_nocturnal),
    mobility_speed = as.factor(mobility_speed)
  )

data <- data %>% filter(category != "Other")
data <- droplevels(data)
head(data)


#Visualize Class Distribution
ggplot(data, aes(x = Class)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Animal Categories", x = "Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Train/Test Split
set.seed(42)
index <- createDataPartition(data$Class, p = 0.7, list = FALSE)
train <- data[index, ]
test <- data[-index, ]

#Decision Tree
tree_model <- rpart(Class ~ is_aquatic + has_wings + is_armored + group_behavior + intelligence_level + mobility_type + climate_type + diet_type + is_nocturnal + mobility_speed, data = train, method = "class")
rpart.plot(tree_model, main = "Decision Tree", type = 2, extra = 104, box.palette = "Blues", fallen.leaves = TRUE, shadow.col = 'gray')
tree_pred <- predict(tree_model, test, type = "class")
conf_matrix_tree <- confusionMatrix(tree_pred, test$Class)
cat("Decision Tree Confusion Matrix:\n")
print(conf_matrix_tree)

#Naive Bayes
nb_model <- naiveBayes(Class ~ is_aquatic + has_wings + is_armored + group_behavior + intelligence_level + mobility_type + climate_type + diet_type + is_nocturnal + mobility_speed, data = train)
nb_pred <- predict(nb_model, test)
conf_matrix_nb <- confusionMatrix(nb_pred, test$Class)
cat("\nNaive Bayes Confusion Matrix:\n")
print(conf_matrix_nb)

#Evaluation
cat("\nModel Accuracy Comparison:\n")
cat("Decision Tree Accuracy:", round(conf_matrix_tree$overall['Accuracy'], 4), "\n")
cat("Naive Bayes Accuracy:", round(conf_matrix_nb$overall['Accuracy'], 4), "\n")





















