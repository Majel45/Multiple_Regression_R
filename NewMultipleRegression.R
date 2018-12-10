1.1 # Load libraries ####
library(readr)

library(caret)

library(kernlab)

library(corrplot)

library(e1071) # SVM #

library(xgboost)

1.2 # Loading data ####
raw.products <- read.csv(file = "existing.relativeEnvirondurability.csv", 
                         sep = ";", dec = ",")

View(raw.products)

# Factor trap: Factor - Character, Character -Numeric
raw.products$Depth <- as.character(raw.products$Depth)

raw.products$Depth <- as.numeric(raw.products$Depth)

# Remove Rows
raw.products <- raw.products [-c(32:41), ]

View(raw.products)

# Visualize Data
raw_products_types <- subset(raw.products, 
      raw.products$Product_type == "Laptop" |
      raw.products$Product_type ==  "PC" | 
      raw.products$Product_type =="Netbook" | 
      raw.products$Product_type =="Smartphone"
    )

View(raw_products_types)

# Scatter plot
qq <- qplot(Positive_service_review, Volume, col= Product_type,
            data = raw_products_types)

qq 

# Bar graph
ggplot(data=raw_products_types_agg, aes(x=Positive_service_review, 
      y=Volume, fill=Product_type)) +
  geom_bar(stat="identity", width = 1, position=position_dodge())


# Aggregate Service Reviews 
#libs: dplr and magrittr 
# %>% is a pipe, it joins a function together LHS %>% RHS
raw_products_types_agg <- raw_products_types %>% 
  select(Product_type, X1Stars, X2Stars, 
         X3Stars, X4Stars, X5Stars,
         Positive_service_review, Negative_service_review, Volume
         ) %>%
  group_by(Product_type) %>%
  summarise(X1Stars = mean(X1Stars),
            X2Stars = mean(X2Stars),
            X3Stars = mean(X3Stars),
            X4Stars = mean(X4Stars),
            X5Stars = mean(X5Stars),
            Positive_service_review = mean(Positive_service_review),
            Negative_service_review = mean(Negative_service_review),
            Volume = mean(Volume))


plot(raw_products_types$PositiveServiceReview, 
     raw_products_types$Volume, col= raw_products_types)
  

product.plot <- ggplot(data=raw_products_types_agg, aes(x=PositiveServiceReview,
                                              y=Volume, group=ProductType))+  geom_bar() 
product.plot
  #geom_smooth(method='loess',
   #           col='blue', alpha=0.1,
    #          size=1) + ggtitle("Volume- Average Rating") + theme_bw()

product.plot

# Create Dummy variables
exist.products <- dummyVars("~.", 
                               data = raw.products)

readyData <- data.frame(predict(exist.products, 
                                newdata = raw.products))

str(raw.products)

summary(raw.products)

# Delete Attribute with NA's
raw.products$Best_seller_rank <- NULL

na.omit(raw.products$Width)

View(readyData)

# Remove Attribute columns
readyData <- readyData[ -c(1:15, 21:29, 31:33) ]

# Build Correlation Matrix
corrData <- cor(readyData)

corrplot(corrData)

# GGplot graph
ggplot(data=readyData, aes(x=X5Stars,
                          y=Volume)) + 
  geom_point() + geom_smooth(method='loess',
                     col='green', alpha=0.3,
  size=1) + ggtitle("Volume- X5Stars+X3Stars") + theme_bw()

# Scatter plot
raw_products_types <- raw_products_types[!(raw_products_types$Product_type ==
                                 "Tablet"),]

raw_products_types$total_ratings <- raw_products_types$X5Stars + 
  raw_products_types$X4Stars + 
  raw_products_types$X3Stars + raw_products_types$X2Stars + 
  raw_products_types$X1Stars

raw_products_types$average_rating <-
        ((raw_products_types$X5Stars * 5) + (raw_products_types$X4Stars * 4) 
      + (raw_products_types$X3Stars * 3)
      + (raw_products_types$X2Stars * 2)
      + (raw_products_types$X1Stars * 1))/raw_products_types$total_ratings

raw_products_types$average_rating[is.na(raw_products_types$average_rating)] <- 0
# Remove Rows
raw_products_types_remove_rows <- subset(raw_products_types, 
                                         raw_products_types$average_rating > 0)

product.plot <- ggplot(data=raw_products_types_remove_rows, aes(x=average_rating,
                y=Volume, col = Product_type)) +  geom_point() + 
  geom_smooth(method='loess',
  col='blue', alpha=0.1,
  size=1)  + theme_bw()

product.plot

View(raw_products_types$average_rating)

# Remove 1% and 99% Quantile for Outliers
newdata <- subset(readyData,!(readyData$X5Stars > quantile
          (readyData$X5Stars, probs=c(.01, .99))[2] | 
            readyData$X5Stars < quantile(readyData$X5Stars, 
            probs=c(.01, .99))[1]) )


# Check Outliers
outliers <- boxplot(newdata$X5Stars, plot = FALSE)$out

print(outliers)

outliers <- hist(newdata$Volume)

summary(newdata$X5Stars)

# Remove Outliers
readyData <- readyData[!readyData$Volume > 6000, ]

readyData <- readyData [!readyData$X1Stars > 1000, ]

newdata[which(newdata$Volume %in% outliers),]

boxplot(newdata$Volume)

# Set the seed
set.seed(333)

# Create Training and Testing Sets
inTrain <- createDataPartition(y= readyData$Volume, 
                               p= 0.75, list = FALSE)

# Train set
train.existing <- readyData[inTrain, ]

# Test set
test.existing <- readyData[-inTrain, ]

fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, repeats = 10)

plot(train.existing)

tail(train.existing)

1.3 # Linear Regression Model ####
Linear.Model <- train(Volume ~ X5Stars + X3Stars, 
                      data = train.existing, 
                      method = "lm", tuneLangth = 1)

Linear.Model

Linear.predict <- predict(Linear.Model, test.existing)

Linear.predict

1.4 # SVM Model ####
model.svm <- svm(Volume ~ X5Stars + X3Stars, 
                 data = train.existing, 
                 method = "svmRadial", tuneLength = 1)

model.svm

pred.svm <- predict(model.svm, newdata = test.existing)

pred.svm

RMSE(pred.svm, test.existing$Volume, na.rm = FALSE)

cor(test.existing$Volume, pred.svm)^2

1.5 # Random Forest Model ####
rf.model <- train(Volume ~ X5Stars + X3Stars, 
                  data = train.existing, method = "rf",
                  tuneLength = 1)

rf.model

rf.predict <- predict(rf.model, newdata = test.existing)

rf.predict

1.6 # XGBoost Model ####
tune_grid <- expand.grid(nrounds =200, max_depth = 5,
                         eta = 0.05, gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)
# (Comment: https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/) #

xgbmTree <- train(Volume ~ X5Stars + X3Stars, 
                  data = train.existing, 
                  tuneLength = 1, method = "xgbTree",
                  tuneGrid = tune_grid)

xgbmTree

xgbm.predict <- predict(xgbmTree, newdata = test.existing)

xgbm.predict

1.7 # Loading & Testing ####
New.products <- read.csv(file= "new.relativeEnvirondurability.csv",
                         sep = ";", dec = ",")

View(New.products)

# Factor trap: Factor - Character, Character -Numeric
New.products$Depth <- as.character(New.products$Depth)

New.products$Depth <- as.numeric(New.products$Depth)

# Remove Rows
raw.products <- raw.products [-c(32:41), ]

# Create Dummy variables with Test data
new.products <- dummyVars("~.", 
                          data = New.products)

new.readyData <- data.frame(predict(new.products, 
                                    newdata = New.products))

summary(new.readyData)


# Remove Attribute columns
new.readyData <- new.readyData[ -c(1:15, 21:29, 31:33) ]


# Predictions
predictions <- predict(Linear.Model, newdata = new.readyData)

predictions

plot(predictions)

New.products$Volume <- predictions

# Create CSV file
write.csv(New.products, file = 
            "Course 3 Task 3 Predicted Products.csv",
          row.names = TRUE)

