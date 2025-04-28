# Install required packages if not already installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, ggplot2, caret, corrplot, car, MASS)

# Load the data
ipl_au <- read_csv("/Users/mintuchowdary/ipl.csv")

# View structure of data
str(ipl_au)

# View column names
colnames(ipl_au)

# Define features
x_features <- c('AGE', 'COUNTRY', 'PLAYING ROLE', 'T-RUNS', 'T-WKTS', 
                'ODI-RUNS-S', 'ODI-SR-B', 'ODI-WKTS', 'ODI-SR-BL', 
                'CAPTAINCY EXP', 'RUNS-S', 'HS', 'AVE', 'SR-B', 
                'SIXERS', 'RUNS-C', 'WKTS', 'AVE-BL', 'ECON', 'SR-BL')

# Define categorical features
cate_features <- c('AGE', 'COUNTRY', 'PLAYING ROLE', 'CAPTAINCY EXP')

# Create dummy variables for categorical features
dummy_model <- dummyVars(~ `AGE` + `COUNTRY` + `PLAYING ROLE` + `CAPTAINCY EXP`, 
                         data = ipl_au, fullRank = TRUE)
dummy_data <- data.frame(predict(dummy_model, newdata = ipl_au))

# Numerical features (all others except categorical)
numerical_features <- setdiff(x_features, cate_features)

# Combine numerical + dummy variables
ipl_au_final <- cbind(ipl_au[, numerical_features], dummy_data)

# Add Target Variable
ipl_au_final$SOLD_PRICE <- ipl_au$`SOLD PRICE`

# View final dataset
str(ipl_au_final)

# --------------------------------------
# Data Visualization

# 1. Distribution of SOLD_PRICE
ggplot(ipl_au, aes(x = `SOLD PRICE`)) +
  geom_histogram(fill = "skyblue", bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Sold Price", x = "Sold Price", y = "Frequency")

# 2. Age vs Sold Price
ggplot(ipl_au, aes(x = AGE, y = `SOLD PRICE`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(title = "Age vs Sold Price", x = "Age", y = "Sold Price")

# 3. Playing Role vs Sold Price
ggplot(ipl_au, aes(x = `PLAYING ROLE`, y = `SOLD PRICE`)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Playing Role vs Sold Price", x = "Playing Role", y = "Sold Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Country vs Sold Price
ggplot(ipl_au, aes(x = COUNTRY, y = `SOLD PRICE`)) +
  geom_boxplot(fill = "lightcoral") +
  theme_minimal() +
  labs(title = "Country vs Sold Price", x = "Country", y = "Sold Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Correlation matrix (Numerical Variables Only)
corr_data <- ipl_au_final %>%
  select(where(is.numeric)) %>%
  select(-SOLD_PRICE)  # Exclude target variable for correlation

cor_matrix <- cor(corr_data, use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.7, tl.col = "black", addCoef.col = "black")

# 6. Multicollinearity check - VIF
model_lm <- lm(SOLD_PRICE ~ ., data = ipl_au_final)
vif_values <- vif(model_lm)
print(vif_values)

# 7. Stepwise regression for feature selection
step_model <- stepAIC(model_lm, direction = "both", trace = FALSE)
summary(step_model)

# 8. Final model after stepwise selection
final_model <- lm(formula(step_model), data = ipl_au_final)
summary(final_model)

# --------------------------------------

# Save cleaned data (optional)
write.csv(ipl_au_final, "/Users/mintuchowdary/ipl_cleaned.csv", row.names = FALSE)