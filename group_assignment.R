# Load necessary libraries
library(dplyr)
library(lubridate)

# Load the dataset
file_path <- "/Users/sandipmahata/Documents/data\ mining/group_assignment/Online_Retail.csv"
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Feature Engineering
# Convert 'InvoiceDate' to datetime format and extract Year, Month, Day, and Hour
data$InvoiceDate <- mdy_hm(data$InvoiceDate)  # Using 'mdy_hm' based on format '12/1/10 8:26'
data$Year <- year(data$InvoiceDate)
data$Month <- month(data$InvoiceDate)
data$Day <- day(data$InvoiceDate)
data$Hour <- hour(data$InvoiceDate)

# Data Cleaning
# Check for missing values
missing_values <- colSums(is.na(data))

# Handle duplicates by removing them
data <- data %>% distinct()

# Fill missing 'Description' with placeholder and handle missing 'CustomerID' by removing those rows
data$Description[is.na(data$Description)] <- "No Description"
data <- data %>% filter(!is.na(CustomerID))

# Remove rows with non-positive Quantity or UnitPrice
data <- data %>% filter(Quantity > 0, UnitPrice > 0)

# Convert Quantity to integer and CustomerID to integer
data$Quantity <- as.integer(data$Quantity)
data$CustomerID <- as.integer(data$CustomerID)

# Drop row with 'Unspecified' country and change 'EIRE' to 'Ireland'
data <- data %>%
  filter(Country != "Unspecified") %>%
  mutate(Country = ifelse(Country == "EIRE", "Ireland", Country))

# Display missing values count, if any
print("Missing values after preprocessing:")
print(colSums(is.na(data)))

# Display the first few rows to verify the changes
head(data)


  






#EDA
# Load necessary libraries
library(ggplot2)

library(scales)

# EDA Insights and Visualizations

# . Total Sales Calculation
data <- data %>% mutate(TotalSales = Quantity * UnitPrice)

# . Total sales by month
ggplot(data, aes(x = factor(Month), y = TotalSales)) +
  geom_bar(stat = "summary", fun = "sum", fill = "steelblue") +
  labs(title = "Total Sales by Month", x = "Month", y = "Total Sales") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal()

# . Total sales by country
ggplot(data, aes(x = reorder(Country, TotalSales), y = TotalSales)) +
  geom_bar(stat = "summary", fun = "sum", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Total Sales by Country", x = "Country", y = "Total Sales") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal()

# . Sales distribution by hour of the day
ggplot(data, aes(x = factor(Hour))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Sales Distribution by Hour", x = "Hour of Day", y = "Number of Transactions") +
  theme_minimal()

# . Top-selling products
top_products <- data %>%
  group_by(Description) %>%
  summarise(TotalQuantity = sum(Quantity)) %>%
  arrange(desc(TotalQuantity)) %>%
  head(10)

ggplot(top_products, aes(x = reorder(Description, TotalQuantity), y = TotalQuantity)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Top 10 Best-Selling Products", x = "Product Description", y = "Total Quantity Sold") +
  theme_minimal()


# Sales Trend Over Time (By Day)
daily_sales <- data %>%
  group_by(InvoiceDate = as.Date(InvoiceDate)) %>%
  summarise(TotalSales = sum(TotalSales))

#Sales Trend Over Time (By Day)
ggplot(daily_sales, aes(x = InvoiceDate, y = TotalSales)) +
  geom_line(color = "blue") +
  labs(title = "Sales Trend Over Time (By Day)", x = "Date", y = "Total Sales") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Sales Performance by Weekday
data$Weekday <- wday(data$InvoiceDate, label = TRUE)

weekday_sales <- data %>%
  group_by(Weekday) %>%
  summarise(TotalSales = sum(TotalSales))

ggplot(weekday_sales, aes(x = Weekday, y = TotalSales, fill = Weekday)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Weekday", x = "Weekday", y = "Total Sales") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal()




#Repeat Customers vs New Customers
repeat_customers <- data %>%
  group_by(CustomerID) %>%
  summarise(RepeatPurchase = n_distinct(InvoiceNo)) %>%
  mutate(CustomerType = ifelse(RepeatPurchase > 1, "Repeat", "New"))

customer_sales <- data %>%
  group_by(CustomerID) %>%
  summarise(TotalSales = sum(TotalSales)) %>%
  left_join(repeat_customers, by = "CustomerID")

ggplot(customer_sales, aes(x = CustomerType, y = TotalSales, fill = CustomerType)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales by Customer Type (Repeat vs New)", x = "Customer Type", y = "Total Sales") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal()











# Load necessary libraries
library(leaflet)
library(tidygeocoder)
library(dplyr)

# Assuming you have a 'country_sales_summary' dataset with 'Country', 'TotalSales', and 'OrderID'
# You may need to adjust column names if needed.

# Aggregate sales data by Country
country_sales_summary <- data %>%
  group_by(Country) %>%
  summarise(
    Total_Sales = sum(TotalSales, na.rm = TRUE),
    Order_Count = n_distinct(InvoiceNo),  # Count unique orders (InvoiceNo)
    .groups = 'drop'
  )

# Geocode the Country names to get latitude and longitude
country_sales_summary <- country_sales_summary %>%
  geocode(Country, method = 'osm')

# Check the updated data frame
str(country_sales_summary)  # Verify the structure to ensure lat/lon exist

# Create the leaflet map
leaflet(country_sales_summary) %>%
  addTiles() %>%
  addMarkers(
    lng = ~long, lat = ~lat,
    icon = makeIcon(
      iconUrl = "https://img.icons8.com/ios-filled/50/000000/star.png",  # Star icon
      iconWidth = 30, iconHeight = 30  # Adjust the size of the icon
    ),
    popup = ~paste("Country:", Country,
                   "<br>Total Sales: $", format(Total_Sales, big.mark = ","), 
                   "<br>Number of Orders:", Order_Count)
  )
















# Outlier Detection and Removal in Quantity and UnitPrice
# Detecting outliers using the IQR method

# Define a function to remove outliers based on IQR
remove_outliers <- function(x, na.rm = TRUE) {
  Q1 <- quantile(x, 0.25, na.rm = na.rm)
  Q3 <- quantile(x, 0.75, na.rm = na.rm)
  IQR <- Q3 - Q1
  x > (Q1 - 1.5 * IQR) & x < (Q3 + 1.5 * IQR)
}

# Applying outlier removal on Quantity and UnitPrice columns
data <- data %>%
  filter(remove_outliers(Quantity)) %>%
  filter(remove_outliers(UnitPrice))

# Visualize Quantity and UnitPrice distributions after outlier removal
ggplot(data, aes(x = Quantity)) +
  geom_histogram(bins = 50, fill = "blue", color = "black") +
  labs(title = "Quantity Distribution After Outlier Removal", x = "Quantity", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = UnitPrice)) +
  geom_histogram(bins = 50, fill = "red", color = "black") +
  labs(title = "Unit Price Distribution After Outlier Removal", x = "Unit Price", y = "Frequency") +
  theme_minimal()


#Feature Engineering 
library(tidyr)

# Create the new dataset
new_data <- data %>%
  group_by(InvoiceNo, Country, CustomerID, Year, Month, Day, Hour) %>%
  summarise(
    Description = list(unique(Description)),  # Combine descriptions into an array
    TotalSales = sum(TotalSales, na.rm = TRUE)  # Sum total sales for each invoice
  ) %>%
  ungroup()  # Remove grouping

# Rename columns to match desired format
colnames(new_data) <- c("InvoiceNo", "Country", "CustomerID", "Year", "Month", "Day", "Hour", "Description", "TotalSales")

# Display the new dataset
head(new_data)



# Load necessary libraries
library(dplyr)

# Create a new dataset with aggregated information by CustomerID
customer_summary <- data %>%
  group_by(CustomerID) %>%
  summarise(
    TotalSales = sum(TotalSales, na.rm = TRUE),           # Sum of all sales for each customer
    TotalQuantity = sum(Quantity, na.rm = TRUE),          # Sum of all quantities for each customer
    UniqueInvoiceDates = n_distinct(as.Date(InvoiceDate)) # Count of unique dates (converted to Date format)
  ) %>%
  ungroup() # Remove grouping

# Display the first few rows of the new dataset
head(customer_summary)




#Machine Learning
# Libraries for modeling and visualization
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(class)
library(arules)
library(arulesViz)


                              #Regression
#1 Linear Regression Model
linear_model <- lm(TotalSales ~ Quantity + UnitPrice + Country + Year + Month + Hour, data = data)
summary(linear_model)

# Predict using Linear Model
linear_predictions <- predict(linear_model, data)

# Calculate MSE for Linear Model
linear_mse <- mean((data$TotalSales - linear_predictions)^2)

# Calculate RMSE for Linear Model
linear_rmse <- sqrt(linear_mse)

# Calculate R-squared for Linear Model
linear_r2 <- summary(linear_model)$r.squared

# Print results
print(paste("Linear Regression MSE:", linear_mse))
print(paste("Linear Regression RMSE:", linear_rmse))
print(paste("Linear Regression R-squared:", linear_r2))



# Decision Tree Regression Model
tree_model <- rpart(TotalSales ~ Quantity + UnitPrice + Country + Year + Month + Hour, data = data, method = "anova")
rpart.plot(tree_model)

# Predict using Decision Tree Model
tree_predictions <- predict(tree_model, data)

# Calculate MSE for Decision Tree Model
tree_mse <- mean((data$TotalSales - tree_predictions)^2)

# Calculate RMSE for Decision Tree Model
tree_rmse <- sqrt(tree_mse)

# Calculate R-squared for Decision Tree Model
tree_r2 <- 1 - sum((data$TotalSales - tree_predictions)^2) / sum((data$TotalSales - mean(data$TotalSales))^2)

# Print results
print(paste("Decision Tree Regression MSE:", tree_mse))
print(paste("Decision Tree Regression RMSE:", tree_rmse))
print(paste("Decision Tree Regression R-squared:", tree_r2))






# Random Forest Regression Model
set.seed(123)
rf_model <- randomForest(TotalSales ~ Quantity + UnitPrice + Country + Year + Month + Hour, data = data, ntree = 100)
print(rf_model)

# Predict using Random Forest Model
rf_predictions <- predict(rf_model, data)

# Calculate MSE for Random Forest Model
rf_mse <- mean((data$TotalSales - rf_predictions)^2)

# Calculate RMSE for Random Forest Model
rf_rmse <- sqrt(rf_mse)

# Calculate R-squared for Random Forest Model
rf_r2 <- 1 - sum((data$TotalSales - rf_predictions)^2) / sum((data$TotalSales - mean(data$TotalSales))^2)

# Print results
print(paste("Random Forest Regression MSE:", rf_mse))
print(paste("Random Forest Regression RMSE:", rf_rmse))
print(paste("Random Forest Regression R-squared:", rf_r2))




# Libraries for visualization
library(ggplot2)
library(dplyr)

# 1. Compile model metrics into a data frame for easy plotting
model_metrics <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest"),
  MSE = c(linear_mse, tree_mse, rf_mse),
  RMSE = c(linear_rmse, tree_rmse, rf_rmse),
  R_squared = c(linear_r2, tree_r2, rf_r2)
)

# Plot MSE, RMSE, and R-squared side by side for comparison
metrics_plot <- model_metrics %>%
  pivot_longer(cols = c("MSE", "RMSE", "R_squared"), names_to = "Metric", values_to = "Value")

ggplot(metrics_plot, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performance Metrics", y = "Metric Value") +
  theme_minimal()




# 3. Feature Importance Plot for Random Forest
# Extract and plot feature importance for Random Forest
rf_importance <- data.frame(Feature = rownames(importance(rf_model)), Importance = importance(rf_model)[, "IncNodePurity"])
ggplot(rf_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Random Forest Feature Importance", x = "Feature", y = "Importance") +
  coord_flip() +
  theme_minimal()




                                          #classification
# Load necessary libraries
library(dplyr)
library(caret)  # For model training and evaluation
library(class)  # For KNN model
library(e1071)  # For Logistic Regression and SVM
library(nnet)   # For Multinomial Logistic Regression

# Assuming `customer_summary` is your dataset

# Step 1: Create classification labels based on TotalSales
customer_summary <- customer_summary %>%
  mutate(CustomerType = case_when(
    TotalSales >= 1000 ~ "Good Customer",
    TotalSales >= 500 & TotalSales < 1000 ~ "Moderate Customer",
    TRUE ~ "Bad Customer"
  ))

# Ensure that CustomerType is a factor (categorical)
customer_summary$CustomerType <- factor(customer_summary$CustomerType, 
                                        levels = c("Good Customer", "Moderate Customer", "Bad Customer"))

# Step 2: Split the data into training and testing sets (80/20 split)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(customer_summary$CustomerType, p = 0.8, list = FALSE)
train_data <- customer_summary[train_index, ]
test_data <- customer_summary[-train_index, ]

# Step 3: Train and evaluate Multinomial Logistic Regression model
multinom_model <- multinom(CustomerType ~ TotalSales + TotalQuantity + UniqueInvoiceDates, 
                           data = train_data)

# Predict using the Multinomial Logistic Regression model
multinom_preds <- predict(multinom_model, newdata = test_data)

# Confusion Matrix for Multinomial Logistic Regression
multinom_cm <- confusionMatrix(multinom_preds, factor(test_data$CustomerType, 
                                                      levels = c("Good Customer", "Moderate Customer", "Bad Customer")))
print("Multinomial Logistic Regression Classification Report:")
print(multinom_cm)





# KNN model
knn_model <- knn(train = train_data[, c("TotalSales", "TotalQuantity", "UniqueInvoiceDates")], 
                 test = test_data[, c("TotalSales", "TotalQuantity", "UniqueInvoiceDates")], 
                 cl = train_data$CustomerType, k = 3)

# Confusion Matrix for KNN
knn_cm <- confusionMatrix(knn_model, factor(test_data$CustomerType, levels = c("Good Customer", "Moderate Customer", "Bad Customer")))
print("KNN Classification Report:")
print(knn_cm)





# SVM model
svm_model <- svm(CustomerType ~ TotalSales + TotalQuantity + UniqueInvoiceDates, 
                 data = train_data, 
                 kernel = "linear")

# Predict using the SVM model
svm_preds <- predict(svm_model, newdata = test_data)

# Confusion Matrix for SVM
svm_cm <- confusionMatrix(svm_preds, factor(test_data$CustomerType, 
                                            levels = c("Good Customer", "Moderate Customer", "Bad Customer")))
print("SVM Classification Report:")
print(svm_cm)



# Load necessary library for visualization
library(ggplot2)
library(caret)
library(reshape2)

# Function to plot confusion matrix as a heatmap
plot_confusion_matrix <- function(cm, title) {
  cm_df <- as.data.frame(as.table(cm))
  colnames(cm_df) <- c("Predicted", "Actual", "Frequency")
  
  ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Frequency)) +
    geom_tile() +
    geom_text(aes(label = Frequency), color = "white", size = 5) +
    scale_fill_gradient(low = "white", high = "red") +
    labs(title = title, x = "Predicted", y = "Actual") +
    theme_minimal()
}

# Confusion Matrix Plots
plot_confusion_matrix(multinom_cm, "Multinomial Logistic Regression")
plot_confusion_matrix(knn_cm, "KNN Model")
plot_confusion_matrix(svm_cm, "SVM Model")



library(corrplot)

# Correlation matrix
corr_matrix <- cor(train_data[, c("TotalSales", "TotalQuantity", "UniqueInvoiceDates")])

# Plot the correlation matrix
corrplot(corr_matrix, method = "color", type = "upper", order = "hclust", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45)








# Create a data frame to compare model accuracies
model_comparison <- data.frame(
  Model = c("Multinomial Logistic Regression", "KNN", "SVM"),
  Accuracy = c(multinom_cm$overall["Accuracy"], knn_cm$overall["Accuracy"], svm_cm$overall["Accuracy"])
)

ggplot(model_comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Model Accuracy Comparison", x = "Model", y = "Accuracy") +
  theme_minimal()











                          #Clustering Models
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggrepel)
library(reshape2)

# Sample data (replace this with your actual 'data' DataFrame)
# Assuming you have a dataset with columns 'Country', 'CustomerID', 'Quantity', 'UnitPrice'
# Uncomment the following line if you have a CSV file
# data <- read.csv("your_data.csv")

# Step 1: Create a new dataset with aggregated features
clustering_data <- data %>%
  mutate(TotalSales = Quantity * UnitPrice) %>%
  group_by(Country, CustomerID) %>%
  summarise(
    TotalSales = sum(TotalSales),
    TotalQuantity = sum(Quantity),
    AverageUnitPrice = mean(UnitPrice),
    PurchaseFrequency = n()
  ) %>%
  ungroup()

# Step 2: Aggregate by Country for clustering purposes
country_clustering_data <- clustering_data %>%
  group_by(Country) %>%
  summarise(
    AvgTotalSales = mean(TotalSales),
    AvgTotalQuantity = mean(TotalQuantity),
    AvgUnitPrice = mean(AverageUnitPrice),
    AvgPurchaseFrequency = mean(PurchaseFrequency)
  ) %>%
  ungroup()

# Step 3: Scale the data
scaled_clustering_data <- scale(country_clustering_data %>% select(-Country))

# Step 4: Elbow Method to find the optimal number of clusters
wss <- sapply(1:10, function(k){
  kmeans(scaled_clustering_data, centers = k, nstart = 20)$tot.withinss
})

# Plotting the Elbow Method results
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares",
     main = "Elbow Method for Optimal K")

# Step 5: Apply K-Means Clustering with the chosen number of clusters (e.g., 3)
set.seed(123)  # Set seed for reproducibility
kmeans_model <- kmeans(scaled_clustering_data, centers = 3)
country_clustering_data$Cluster <- kmeans_model$cluster

# Step 6: Visualize the Clusters
ggplot(country_clustering_data, aes(x = AvgTotalSales, y = AvgTotalQuantity, color = as.factor(Cluster))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("blue", "red", "green")) +
  labs(title = "Country Clustering by Sales and Quantity",
       x = "Average Total Sales",
       y = "Average Total Quantity",
       color = "Cluster") +
  theme_minimal()





# Step 5: Hierarchical Clustering
distances <- dist(scaled_clustering_data, method = "euclidean")
hclust_model <- hclust(distances, method = "ward.D2")

# Step 6: Visualizations
# Modify the labels of the dendrogram to show country names
country_labels <- country_clustering_data$Country
hclust_model$labels <- country_labels

# Plot the hierarchical clustering dendrogram with country names
plot(hclust_model, main = "Hierarchical Clustering Dendrogram",
     xlab = "Countries", ylab = "Distance", sub = "")
# You can cut the tree to form clusters if needed by adding a line to the dendrogram
abline(h = 30, col = "red", lwd = 2)  # Adjust height to form clusters, here h = 30 as an example























                           #Association Models
# Load necessary libraries
library(arules)
library(arulesViz)
library(ggplot2)
library(reshape2)

# Create a transaction dataset (already done in your code)
transaction_data <- as(split(data$Description, data$InvoiceNo), "transactions")

# Apply the Apriori Algorithm to generate association rules
apriori_rules <- apriori(transaction_data, 
                         parameter = list(supp = 0.01, conf = 0.5))

# Inspect top 10 rules based on confidence
top_rules <- inspect(sort(apriori_rules, by = "confidence")[1:10])
print(top_rules)

# Identify highly correlated products from the association rules
# We will filter the rules with high lift (which indicates strong correlations)
high_lift_rules <- subset(apriori_rules, lift > 1.5)  # Adjust threshold as needed
inspect(high_lift_rules)


#  Matrix-based Visualization 
plot(high_lift_rules, method = "matrix", measure = "lift", shading = "confidence")



# Scatter Plot of Lift vs. Confidence
# Customize a scatter plot with ggplot for better readability
rules_df <- as(high_lift_rules, "data.frame")
ggplot(rules_df, aes(x = confidence, y = lift, size = support, color = support)) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(2, 10)) +
  labs(title = "Scatter Plot of Association Rules",
       x = "Confidence", y = "Lift") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Load necessary libraries
library(arules)
library(arulesViz)

# Generate the transaction data
transaction_data <- as(split(data$Description, data$InvoiceNo), "transactions")

# Apply the Apriori Algorithm to generate association rules
apriori_rules <- apriori(transaction_data, 
                         parameter = list(supp = 0.01, conf = 0.5))

# Sort and select the top 10 rules based on confidence
top_10_rules <- sort(apriori_rules, by = "confidence")[1:10]
