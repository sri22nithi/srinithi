library(randomForest)

data <- read.csv("C:/Users/srini/Downloads/survey lung cancer.csv", stringsAsFactors = TRUE)

if ("X" %in% colnames(data)) {
  data$X <- NULL
}

data$LUNG_CANCER <- as.factor(data$LUNG_CANCER)

set.seed(123)
train_index <- sample(1:nrow(data), 0.7 * nrow(data))

train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

rf_model <- randomForest(LUNG_CANCER ~ ., 
                         data = train_data, 
                         ntree = 200, 
                         importance = TRUE)

predictions <- predict(rf_model, test_data)

table(predictions, test_data$LUNG_CANCER)

accuracy <- sum(predictions == test_data$LUNG_CANCER) / nrow(test_data)
accuracy

varImpPlot(rf_model)