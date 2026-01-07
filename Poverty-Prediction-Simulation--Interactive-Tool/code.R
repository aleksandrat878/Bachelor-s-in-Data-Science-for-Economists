library(readxl)
library(ggplot2)
library(dplyr)
poverty <- read_excel("train_values.xlsx")
View(poverty)

#removed the first coloumn
poverty <- poverty[, -1]

#removed the following columns as there were no data values
library(dplyr)
poverty <- poverty %>% 
  select(-bank_interest_rate, -mm_interest_rate, -mfi_interest_rate,
         -other_fsp_interest_rate)

library(janitor)
poverty <- clean_names(poverty)
poverty

poverty <- remove_empty(poverty, which = c("rows", "cols"), quiet = FALSE)
poverty

poverty <- distinct(poverty)
poverty

#turning true/false (logical) responses to 0/1 (numerical)
poverty[] <- lapply(poverty, function(x) if(is.logical(x)) as.numeric(x) else x)

summary(poverty)

#using the information from summary (column age), we can separate the age groups into 3 groups
poverty$age_group <- cut(poverty$age, 
                         breaks = c(15, 25, 45, 115), 
                         labels = c(1, 2, 3), 
                         right = TRUE)

#category 1: young (15–25 years) → Covers the Min to 1st Quartile (Q1), 
#category 2: middle-age (26–45 years) → Covers the Q1 to 3rd Quartile (Q3),
#category 3: older (46+ years) → Covers the Q3 to Max

colnames(poverty)

library(ISLR)
library(glmnet)
library(tidyverse)
library(caret)

poverty1 <- poverty %>% filter(!is.na(poverty_probability))
nrow(poverty1)

# seed() function is used to create random numbers that can be reproduced; it helps creating the same output when a function that contains randomness is called. 
set.seed(5462)

# define the training partition 
train_index <- createDataPartition(poverty1$poverty_probability, p = .5, 
                                   list = FALSE, 
                                   times = 1)

# split the data using the training partition to obtain training data
poverty1_train <- poverty1[train_index,]

# remainder of the split is the validation and test data (still) combined 
poverty1_val_and_test <- poverty1[-train_index,]

# split the remaining 50% of the data in a validation and test set
val_index <- createDataPartition(poverty1_val_and_test$poverty_probability, p = .6, 
                                 list = FALSE, 
                                 times = 1)

poverty1_valid <- poverty1_val_and_test[val_index,]
poverty1_test  <- poverty1_val_and_test[-val_index,]

# Outcome of this section is that the data (100%) is split into:
# training (~50%)
# validation (~30%)
# test (~20%)

ggplot() + 
  geom_histogram(data = poverty1_train, mapping = aes(x = poverty_probability), alpha = 0.3, colour = "Blue") + 
  geom_histogram(data = poverty1_valid, mapping = aes(x = poverty_probability), alpha = 0.3, colour = "Red") + 
  geom_histogram(data = poverty1_test, mapping = aes(x = poverty_probability), alpha = 0.3, colour = "Orange")  


ggplot() + 
  geom_density(data = poverty1_train, mapping = aes(x = poverty_probability), alpha = 0.3, colour = "Blue") + 
  geom_density(data = poverty1_valid, mapping = aes(x = poverty_probability), alpha = 0.3, colour = "Red") + 
  geom_density(data = poverty1_test, mapping = aes(x = poverty_probability), alpha = 0.3, colour = "Orange")  

lm_mse <- function(formula, train_data, valid_data) {
  y_name <- as.character(formula)[2]
  y_true <- valid_data[[y_name]]
  
  lm_fit <- lm(formula, train_data)
  y_pred <- predict(lm_fit, newdata = valid_data)
  
  mean((y_true - y_pred)^2)
}

source("generate_formulas.R")

x_vars <- colnames(poverty1)
x_vars <- x_vars[x_vars != "poverty_probability"]
formulas <- generate_formulas(p = 3, x_vars = x_vars, y_var = "poverty_probability")
length(formulas)
#[1] 26235

mses <- rep(0, 26235 )
for (i in 1:26235 ) {
  mses[i] <- lm_mse(as.formula(formulas[i]), poverty1_train, poverty1_valid)
}

best_3_preds <- formulas[which.min(mses)]

# Generate formulas

formulas_1 <- generate_formulas(p = 1, x_vars = x_vars, y_var = "poverty_probability")
formulas_2 <- generate_formulas(p = 2, x_vars = x_vars, y_var = "poverty_probability")
formulas_4 <- generate_formulas(p = 4, x_vars = x_vars, y_var = "poverty_probability")

# Initialise a vector we will fill with MSE values

mses_1 <- rep(0, length(formulas_1))
mses_2 <- rep(0, length(formulas_2))
mses_4 <- rep(0, length(formulas_4))

# loop over all the formulas

for (i in 1:length(formulas_1)) {
  mses_1[i] <- lm_mse(as.formula(formulas_1[i]), poverty1_train, poverty1_valid)
}

for (i in 1:length(formulas_2)) {
  mses_2[i] <- lm_mse(as.formula(formulas_2[i]), poverty1_train, poverty1_valid)
}

for (i in 1:length(formulas_4)) {
  mses_4[i] <- lm_mse(as.formula(formulas_4[i]), poverty1_train, poverty1_valid)
}

# Compare mses

min(mses_1)
min(mses_2)
min(mses)
min(mses_4)

# min(mses_4) is lowest of them all!
# So let's see which model that is

formulas_4[which.min(mses_4)]

# Estimate model and calculate mse
lm_best <- lm(poverty_probability ~ country + income_public_sector_last_year + borrowed_for_daily_expenses_last_year + can_make_transaction, 
              poverty1_val_and_test)
mse <- function(y_true, y_pred) mean((y_true - y_pred)^2)
mse(poverty1_test$poverty_probability, predict(lm_best, newdata = poverty1_test))
#0.06849676

# create a plot
tibble(
  y_true = poverty1_test$poverty_probability,
  y_pred = predict(lm_best, newdata = poverty1_test)
) %>% 
  ggplot(aes(x = y_pred, y = y_true)) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_point() +
  theme_minimal()


x_train <- model.matrix(poverty_probability ~ ., data = poverty1_train)
head(x_train)

nrow(x_train)  # Number of rows in x_train
length(poverty1_train$poverty_probability)  # Number of elements in y

poverty1_train <- poverty1_train[rownames(x_train), ]
nrow(x_train)  # Should be 5948
length(poverty1_train$poverty_probability)  # Should also be 5948


result <- glmnet(x      = x_train[, -1],          # X matrix without intercept
                 y      = poverty1_train$poverty_probability,  # Salary as response
                 family = "gaussian",             # Normally distributed errors
                 alpha  = 1,                      # LASSO penalty
                 lambda = 15)                     # Penalty value
rownames(coef(result))[which(coef(result) != 0)]

x_valid <- model.matrix(poverty_probability ~ ., data = poverty1_valid)[, -1]
y_pred <- as.numeric(predict(result, newx = x_valid))

# Subset poverty1_valid to match the number of predictions
poverty1_valid <- poverty1_valid[1:length(y_pred), ]

# Create the tibble for plotting
tibble(Predicted = y_pred, Observed = poverty1_valid$poverty_probability) %>% 
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  theme_minimal() +
  labs(title = "Predicted versus observed salary")

result_nolambda <- glmnet(x = x_train[, -1], y = poverty1_train$poverty_probability, 
                          family = "gaussian", alpha  = 1)

# This object contains sets of coefficients for different values of lambda,
# i.e., different models ranging from an intercept-only model (very high 
# lambda) to almost no shrinkage (very low lambda).

plot(result_nolambda)


x_cv <- model.matrix(poverty_probability ~ ., bind_rows(poverty1_train, poverty1_valid))[, -1]

nrow(x_cv)  # Number of rows in the predictor matrix
length(c(poverty1_train$poverty_probability, poverty1_valid$poverty_probability))  # Length of the response variable

nrow(poverty1_train)  # Check number of rows in training set
nrow(poverty1_valid)  # Check number of rows in validation set
sum(is.na(poverty1_train))  # Check for NA values in training set
sum(is.na(poverty1_valid))  # Check for NA values in validation set
poverty1_valid <- poverty1_valid[complete.cases(poverty1_valid), ]


result_cv <- cv.glmnet(x = x_cv, y = c(poverty1_train$poverty_probability, 
                                       poverty1_valid$poverty_probability), nfolds = 15)
best_lambda <- result_cv$lambda.min
best_lambda
#[1] 0.0004594818
plot(result_cv)

---
   
# Summary Statistics for Numerical Variables
    selected_vars <- poverty %>% 
    select(
      age, 
      female, 
      employment_type_last_year, 
      married, 
      is_urban, 
      country, 
      income_public_sector_last_year, 
      borrowed_for_daily_expenses_last_year, 
      can_make_transaction
    )
  
  
## Generating summary statistics for numerical variables only
  summary_stats <- selected_vars %>% 
    select(where(is.numeric)) %>% 
    summary()
  
##  Print the summary statistics
  print(summary_stats)
  
---

 # Selecting new dataset with chosen variables and poverty probability
  correlation_vars <- poverty %>%
    select(poverty_probability, female, married, is_urban, country,
           income_public_sector_last_year, borrowed_for_daily_expenses_last_year, 
           can_make_transaction)
  
  ## Defining dummy variables
  dummy_vars <- c("female", 
                  "married", 
                  "is_urban", 
                  "income_public_sector_last_year", 
                  "borrowed_for_daily_expenses_last_year", 
                  "can_make_transaction")
  
  ## Creating violin plots for each dummy variable with proper dataset
  for (var in dummy_vars) {
    violin_plot <- ggplot(correlation_vars, aes(x = factor(.data[[var]], labels = c("No", "Yes")), 
                                                y = poverty_probability, 
                                                fill = factor(.data[[var]]))) +
      geom_violin(trim = FALSE, alpha = 0.6) +
      geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
      labs(title = paste("Poverty Probability by", var),
           x = var,
           y = "Poverty Probability") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
    print(violin_plot)
  }
  
---
    
 #Creating boxplot for poverty probability by country
    
  country_boxplot <- ggplot(correlation_vars, aes(x = reorder(country, poverty_probability, median),
                                                  y = poverty_probability,
                                                  fill = factor(country))) +
    geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1.5) +
    labs(title = "Poverty Probability by Country",
         x = "Country",
         y = "Poverty Probability",
         fill = "Country") +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none")  

  print(country_boxplot)
  
  
---

#Creating a histogram with a density curve for age distribution
  ggplot(selected_vars, aes(x = age)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "#69b3a2", color = "black", alpha = 0.7) +  
    geom_density(color = "#1f78b4", size = 1.2, linetype = "dashed") + 
    labs(title = "Age Distribution",
         x = "Age",
         y = "Density") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
---  


  # Calculating means for selected dummy variables, handling NAs
  dummy_means <- selected_vars %>%
    select(female, 
           married, 
           is_urban, 
           income_public_sector_last_year, 
           borrowed_for_daily_expenses_last_year, 
           can_make_transaction) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
    filter(!is.na(Mean)) 
  
  #Creating the bar plot with cleaned data
  ggplot(dummy_means, aes(x = reorder(Variable, Mean), y = Mean, fill = Variable)) +
    geom_bar(stat = "identity", width = 0.7, color = "black") +
    geom_text(aes(label = paste0(round(Mean * 100, 1), "%")), vjust = -0.5, size = 4, na.rm = TRUE) +  
    scale_fill_brewer(palette = "Set3") +
    labs(title = "Mean Values of Dummy Variables",
         x = "Variable",
         y = "Mean (Proportion)") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  
    
---   
# Frequency Tables for Categorical Variables
employment_freq <- poverty %>% count(employment_type_last_year)
education_freq <- poverty %>% count(education_level)
head_freq <- poverty %>%
  count(relationship_to_hh_head) %>%
  mutate(percentage = round((n / sum(n)) * 100, 2))

print(employment_freq)
print(education_freq)
print(head_freq)

##Visualization: Employment Type Frequency
ggplot(employment_freq, aes(x = reorder(employment_type_last_year, -n), y = n, fill = employment_type_last_year)) +
  geom_bar(stat = "identity") +
  labs(title = "Employment Type Frequency",
       x = "Employment Type", y = "Count") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

## Visualization: Education Level Frequency

###Converting variables to factors
education_freq <- education_freq %>%
  mutate(education_level = as.factor(education_level))

###Removing NA values and calculate percentages
education_freq_clean <- education_freq %>%
  filter(!is.na(education_level)) %>%  # Remove NAs
  mutate(percentage = round((n / sum(n)) * 100, 2),  
         education_level = as.factor(education_level))  

## Visualization: Education Level Frequency with Percentages
ggplot(education_freq_clean, aes(x = reorder(education_level, -n), y = n, fill = education_level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 4) +  
  labs(title = "Education Level Frequency",
       x = "Education Level", y = "Count") +
  scale_fill_brewer(palette = "Pastel1") +  
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  

## Visualization: Relationship to Household Head Frequency (with percentages)
ggplot(head_freq, aes(x = reorder(relationship_to_hh_head, -n), y = n, fill = relationship_to_hh_head)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 4) +  # Add percentage labels
  labs(title = "Relationship to Household Head",
       x = "Relationship", y = "Count") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

---
  

# Poverty Probability Across Age Groups
poverty_by_age_group <- poverty %>%
  group_by(age_group) %>%
  summarise(avg_poverty_probability = mean(poverty_probability, na.rm = TRUE))

## Removing NA values in age_group
poverty_filtered <- poverty %>% filter(!is.na(age_group))

## Violin plot with improved aesthetics
ggplot(poverty_filtered, aes(x = age_group, y = poverty_probability, fill = age_group)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +  # Violin with black outline
  geom_boxplot(width = 0.1, fill = "white", outlier.color = "red") +  
  scale_fill_brewer(palette = "Set3") +  # Nicer color palette
  labs(title = "Distribution of Poverty Probability by Age Group",
       x = "Age Group", y = "Poverty Probability") +
  theme_minimal(base_size = 14) +  # Cleaner theme with larger text
  theme(legend.position = "none",  # Remove legend (not needed for x-axis categories)
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(face = "bold"), axis.text = element_text(color = "black", size = 12)) 

---
  
library(knitr)
library(kableExtra)

# Selecting the 4 most predictive variables
lasso_vars <- poverty %>%
  select(income_public_sector_last_year, borrowed_for_daily_expenses_last_year, can_make_transaction)

# Computing the correlation matrix
cor_matrix <- round(cor(lasso_vars, use = "complete.obs"), 2)  

# Displaing as a neat table
kable(cor_matrix, caption = "Correlation Matrix of Top Predictors") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3") 

---
  
#  Poverty Probability by Employment Status
ggplot(poverty, aes(x = employment_type_last_year, y = poverty_probability, fill = employment_type_last_year)) +
  geom_boxplot() +
  labs(title = "Poverty Probability by Employment Status",
       x = "Employment Status", y = "Poverty Probability") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

---
# Employment Status Distribution Across Age Groups
  # Removing NA values from employment_type_last_year and age_group
  poverty_clean <- poverty %>%
  filter(!is.na(employment_type_last_year), !is.na(age_group))

ggplot(poverty_clean, aes(x = age_group, fill = employment_type_last_year)) +
  geom_bar(position = "fill") +
  labs(title = "Employment Status Distribution Across Age Groups",
       x = "Age Group", y = "Proportion") +
  scale_fill_brewer(palette = "Set2") +  # Nice color palette
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


