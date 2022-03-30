library("reshape2")
library("ggplot2")
library("ie2misc")
library(Metrics)
library(e1071)
library(dplyr)
library(plyr)
library(stringr)

#Dataset URL: https://www.kaggle.com/datasets/austinreese/craigslist-carstrucks-data
data = read.csv("vehicles.csv")
df <- data[-c(22)]
cars <- na.omit(df) 
cars <- cars[!duplicated(cars$VIN), ]
cars[c("id","url", "region_url", "region", "state", "title_status", "VIN", "size", "image_url", "description", "lat", "long", "posting_date", "model")] <- list(NULL)
cars <- cars[!apply(cars, 1, function(x) any(x=="")),] 
cars$cylinders<-gsub(" cylinders","",as.character(cars$cylinders)) 
#The values of the Cylinders field, for instance, change from "8 cylinders" to 8.

cars$cylinders <- with(cars, ifelse(cylinders == "other", 0, cylinders))
#Here, some cars are removed (cars with a mileage of 1 or 999999 whose data look suspicious)
cars<-cars[!(cars$odometer>400000),]
cars<-cars[!(cars$odometer<30),]
cars<-cars[!(cars$price<250),]

#Here, manufacturers are ordered by the average price of their cars.
#This function is also applied to the drive, the transmission, the fuel, etc, to substitute the values accordingly.
res.plyr <- ddply( cars_backup, .(manufacturer), function(x) mean(x$price) )
res.plyr

cars_backup <- cars #Build a backup dataset before applying numerical substitution.

cars$drive <- with(cars, ifelse(drive == "fwd", 1, ifelse(drive == "rwd", 2, 3)))
cars$transmission <- with(cars, ifelse(transmission == "manual", 1, ifelse(transmission == "automatic", 2, 3)))
cars$fuel <- with(cars, ifelse(fuel == "diesel", 1, 
ifelse(fuel == "gas", 2, 
ifelse(fuel == "hybrid", 3,
ifelse(fuel == "electric", 4, 5)))))

cars$condition <- with(cars, ifelse(condition == "salvage", 1, 
ifelse(condition == "fair", 2, 
ifelse(condition == "good", 3,
ifelse(condition == "excellent", 4,
ifelse(condition == "like new", 5, 6))))))


cars$paint_color <- with(cars, ifelse(paint_color == "green", 1, 
ifelse(paint_color == "purple", 2, 
ifelse(paint_color == "brown", 2,
ifelse(paint_color == "silver", 2,
ifelse(paint_color == "custom", 2,
ifelse(paint_color == "blue", 2,
ifelse(paint_color == "grey", 2,
ifelse(paint_color == "red", 8,
ifelse(paint_color == "yellow", 9,
ifelse(paint_color == "black", 10,
ifelse(paint_color == "orange", 11, 12))))))))))))

cars$condition <- as.integer(cars$condition)
cars$cylinders <- as.integer(cars$cylinders)
cars$fuel <- as.integer(cars$fuel)
cars$transmission <- as.integer(cars$transmission)
cars$drive <- as.integer(cars$drive)
cars$paint_color <- as.integer(cars$paint_color)
cars$price <- as.integer(cars$price)
cars$year <- as.integer(cars$year)


cars$manufacturer <- with(cars, 
ifelse(manufacturer == "land rover", 1,
ifelse(manufacturer == "saturn", 2,
ifelse(manufacturer == "mercury", 3,
ifelse(manufacturer == "fiat", 4,
ifelse(manufacturer == "pontiac", 5,
ifelse(manufacturer == "mazda", 6,
ifelse(manufacturer == "mini", 7,
ifelse(manufacturer == "chrysler", 8,
ifelse(manufacturer == "hyundai", 9,
ifelse(manufacturer == "mitsubishi", 10, 
ifelse(manufacturer == "honda", 11, 
ifelse(manufacturer == "kia", 12, 
ifelse(manufacturer == "volkswagen", 13,
ifelse(manufacturer == "volvo", 14,
ifelse(manufacturer == "buick", 15,
ifelse(manufacturer == "subaru", 16,
ifelse(manufacturer == "nissan", 17,
ifelse(manufacturer == "toyota", 18,
ifelse(manufacturer == "acura", 19,
ifelse(manufacturer == "dodge", 20,
ifelse(manufacturer == "lincoln", 21,
ifelse(manufacturer == "jaguar", 22, 
ifelse(manufacturer == "bmw", 23, 
ifelse(manufacturer == "chevrolet", 24, 
ifelse(manufacturer == "lexus", 25,
ifelse(manufacturer == "audi", 26,
ifelse(manufacturer == "jeep", 27,
ifelse(manufacturer == "cadillac", 28,
ifelse(manufacturer == "ford", 29,
ifelse(manufacturer == "mercedes-benz", 30,
ifelse(manufacturer == "datsun", 31,
ifelse(manufacturer == "gmc", 32, 
ifelse(manufacturer == "alfa-romeo", 33, 
ifelse(manufacturer == "infiniti", 34,
ifelse(manufacturer == "harley-davidson", 35,
ifelse(manufacturer == "ram", 36, 
ifelse(manufacturer == "rover", 37, 
ifelse(manufacturer == "porsche", 38,
ifelse(manufacturer == "tesla", 39,
ifelse(manufacturer == "aston-martin", 40, 41)))))))))))))))))))))))))))))))))))))))))
cars$manufacturer <- as.integer(cars$manufacturer)



cars$type <- with(cars, ifelse(type == "hatchback", 1, 
                               ifelse(type == "mini-van", 2, 
                               ifelse(type == "sedan", 3,
                               ifelse(type == "wagon", 4,
                               ifelse(type == "bus", 5,
                               ifelse(type == "SUV", 6,
                               ifelse(type == "convertible", 7,
                               ifelse(type == "van", 8,
                               ifelse(type == "coupe", 9,
                               ifelse(type == "offroad", 10,
                               ifelse(type == "truck", 11,
                               ifelse(type == "pickup", 12, 13)))))))))))))

cars$type <- as.integer(cars$type)

#Splitting the data into training and testing (80%-20%)
set.seed(123)
size <- floor(0.8 * nrow(cars))

train_ind <- sample(seq_len(nrow(cars)), size = size)
train_labels <- cars[train_ind]
test_labels <- row_labels[-train_ind]

data_train <- cars[train_ind,]
data_test <- cars[-train_ind,]

#Developing the linear regression model
model1 = lm((price)~., data=data_train)
summary(model1)

#Compare the distributions after applying logarithmic transformation and squared transformation.
par(mfrow=c(3,1))
ggplot(data_train, aes(price)) + geom_density(fill="blue") 
ggplot(data_train, aes(log(price))) + geom_density(fill="blue") 
ggplot(data_train, aes(sqrt(price))) + geom_density(fill="blue")

#Create a new model choosing the best transformation
model2 = lm((log(price))~., data=data_train)
summary(model2)

#Remove the irrelevant features identified in the summary of Model2.
data_train[c("transmission", "paint_color")] <- list(NULL)
data_test[c("transmission", "paint_color")] <- list(NULL)

model3 = lm((log(price))~., data=data_train)
summary(model3)

#Correlation matrix between variables
cormat <- round(cor(data_test,method="pearson"),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)

#Do predictions and calculate the MSE and the MAE
dataLR <- data.frame(pred = predict(model3), actual = log(data_test$price))
MSE_lr <- mean((dataLR$actual - dataLR$pred)^2) 
MAE_lr <- mae(log(data_test$price), predict(model3)) 

#Support vector regression
modelSVM <- svm(log(price)~., data=data_train)
svr.pred = predict(modelSVM, data_test)

#Tuning hyper-parameters in SVR
tune.model = tune(svm,
                  log(price)~.,
                  data=data_train,
                  range=list(cost=2^(2:9), epsilon = seq(0,1,0.1))
)

#Build a second model based on the tuned SVR model.
tunedModelSVM <- svm(log(price) ~ . , data_train,cost = tune.model$best.parameters$cost,epsilon = tune.model$best.parameters$epsilon)

#Build a dataframe comparing the three different predictions with the current values.
dataSVM <- data.frame(predSVM = predict(modelSVM,data_test), actual = log(data_test$price), tuned = predict(tunedModelSVM, data_test), LR = predict(model3, data_test))

#Create a random sample of 70 cars.
dataSVM_plot <- sample_n(dataSVM, 70)

#Order the sample by their value predicted with SVR.
dataSVM_plot <- dataSVM[order(dataSVM$predSVM), ]

#Plot the difference between algorithms.
plot(dataSVM_plot$actual, ylab="log(price)")
lines(dataSVM_plot$predSVR, pch = 18, col = "blue", type = "b", lty = 2)
lines(dataSVM_plot$tuned, pch = 18, col = "red", type = "b", lty = 2)
lines(dataSVM_plot$LR, pch = 18, col = "azure3", type = "b", lty = 2)
legend("topleft", legend=c("Predicted with tuned SVR", "Predicted with SVR", "Predicted with LR"), col=c("red", "blue", "azure3"), lty = 1:2, cex=0.8)

#Get the MAE and the MSE with SVR and tuned SVR
MAE_SVM <- mae(log(data_test$price), predict(modelSVM, data_test)) 
MAE_TunedSVM <- mae(log(data_test$price), predict(tunedModelSVM, data_test))

MSE_SVM <- mean((log(data_test$price) - predict(modelSVM, data_test))^2)
MSE_TunedSVM <- mean((log(data_test$price) - predict(tunedModelSVM, data_test))^2)