1.1 # Load libraries ####
library(readr)

library(caret)

library(kernlab)

library(corrplot)

library(e1071) # SVM #

library(xgboost)

1.2 # Loading data ####
Raw.Products <- read.csv(file= "existingproductattributes2017.2.csv")

View(Raw.Products)

# Create Dummy variables
existing.products <- dummyVars("~.", 
                  data = Raw.Products)

readyData <- data.frame(predict(existing.products, 
            newdata = Raw.Products))

str(Raw.Products)

summary(Raw.Products)

# Delete Attribute with NA's
Raw.Products$BestSellersRank <- NULL

# Remove Attribute columns
readyData <- readyData[ -c(1:14, 20:28) ]

# Build Correlation Matrix
corrData <- cor(readyData)

corrplot(corrData)

# Set the seed
set.seed(333)

# Create Training and Testing Sets
inTrain <- createDataPartition(y= readyData$Volume, 
                               p= 0.75, list = FALSE)

# Train set
train.existing <- readyData[inTrain, ]

# Test set
test.existing <- readyData[-inTrain, ]

plot(train.existing)

head(train.existing)

1.3 # Linear Regression Model ####
Linear.Model <- train(Volume ~., data = train.existing, 
                 method = "lm", tuneLangth = 1)

Linear.Model

Linear.predict <- predict(Linear.Model, test.existing)

Linear.predict

summary(Linear.predict)

plot(Linear.predict)

qplot(Volume, color = ProfitMargin, data = train.existing, geom = "density")

1.4 # SVM Model ####
model.svm <- svm(Volume ~., data = train.existing, 
                 method = "svmRadial", tuneLength = 1)
                  
model.svm

pred.svm <- predict(model.svm, newdata = test.existing)

pred.svm

model.svm2 <- svm(Volume ~., data = train.existing, 
              kernel = "linear", gamma = 0.2, cost =100, 
              metric = "RMSE")

model.svm2

1.5 # Random Forest Model ####
rf.model <- train(Volume ~., data = train.existing, method = "rf",
                  tuneLength = 1)

rf.model

rf.predict <- predict(rf.model, newdata = test.existing)

rf.predict

plot(rf.predict)

table(test.existing$Volume, rf.predict)

model.list <- list(lm = Linear.Model, rf = rf.model)

resamples = resamples(model.list)

summary(resamples)

bwplot(resamples, metric = "RMSE")

1.6 # XGBoost Model ####
tune_grid <- expand.grid(nrounds =200, max_depth = 5,
                         eta = 0.05, gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)
# (Comment: https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/) #

xgbmTree <- train(Volume ~., 
              data = train.existing, 
              tuneLength = 1, method = "xgbTree",
              tuneGrid = tune_grid)

xgbmTree

xgbm.predict <- predict(xgbmTree, newdata = test.existing)

xgbm.predict

1.7 # Loading & Testing ####
New.Products <- read.csv(file= "newproductattributes2017.2.csv")

View(New.Products)

# Create Dummy variables with Test data
new.products <- dummyVars("~.", 
                               data = New.Products)

New.readyData <- data.frame(predict(new.products, 
                                newdata = New.Products))

summary(New.readyData)

# Delete Attribute with NA's
New.readyData$BestSellersRank <- NULL

# Remove Attribute columns
New.readyData <- New.readyData[ -c(1:14, 20:27) ]

# Predictions
predictions <- predict(xgbmTree, newdata = New.readyData)

predictions

plot(predictions)

New.Products$Volume <- predictions

# Visualize Data
raw.products <- raw.products[!(raw.products$Product_type ==
                                 "Tablet"),]

raw.products$total_ratings <- raw.products$X5Stars +
  raw.products$X4Stars +
  raw.products$X3Stars + raw.products$X2Stars +
  raw.products$X1Stars

raw.products$average_rating <-
  ((raw.products$X5Stars * 5) + (raw.products$X4Stars * 4)
   + (raw.products$X3Stars * 3)
   + (raw.products$X2Stars * 2)
   + (raw.products$X1Stars * 1))/raw.products$total_ratings

raw.products$average_rating[is.na(raw.products$average_rating)] <- 0

product.plot <- ggplot(data=raw.products, aes(x=average_rating,
                                              y=Volume, col = Product_type)) +  geom_point() +
  geom_smooth(method="loess",
              col="blue", alpha=0.1,
              size=1) + ggtitle("Volume- Average Rating") + theme_bw()

product.plot

# Create CSV file
write.csv(New.Products, file = 
            "Course 3 Task 3 Predicted Products.csv",
          row.names = TRUE)

qplot(predict(gbm.model, test.existing), Volume, data = test.existing)

