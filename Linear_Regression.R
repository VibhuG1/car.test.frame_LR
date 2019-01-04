rm(list=ls(all=TRUE))

library(rpart)
library(DMwR)
library(ggplot2)
library(dplyr)

library(lmtest)

?car.test.frame
summary(car.test.frame)
dim(car.test.frame)
str(car.test.frame)

car.test.frame
class(car.test.frame)
colnames(car.test.frame)
nrow(car.test.frame)

plot(y=car.test.frame$price,x=car.test.frame$Weight)
plot(y=car.test.frame$price,x=car.test.frame$Reliability)

#removed as categorical 
car.test.frame <-  subset(car.test.frame, select= -c(Country))

#removed due to non linearity
car.test.frame <-  subset(car.test.frame, select= -c(Reliability))

#removed due to high vif
car.test.frame <-  subset(car.test.frame, select= -c(Type))



library(caTools)
spl <- sample.split ( Y = car.test.frame$Price, SplitRatio  = 0.7)
trainDF <- car.test.frame[spl,]
testDF <- car.test.frame[!spl,]

nrow(trainDF)
nrow(testDF)


cor(trainDF$Price,trainDF$Mileage)
 

library(corrgram)
corrgram(trainDF)

model3 = lm(Price~. , data=trainDF)

library(car)


vif(model3)
summary(model3)

predicted_traindf <- as.data.frame(predict(model3, newdata = trainDF, interval = "prediction"))

predicted_train <- cbind(predicted_traindf,trainDF)


predicted_testdf <- as.data.frame(predict(model3, newdata = testDF, interval = "prediction"))
colnames(predicted_testdf)

predicted_test <- cbind(predicted_testdf,testDF)

library(dplyr)
predicted_train <- predicted_train %>% mutate(squared_err = (Price - fit)^2 , err = Price - fit   )

predicted_test <- predicted_test %>% mutate(squared_err = (Price - fit)^2 , err = Price - fit)

Mean_Squared_Error_train <- (colSums(predicted_train, na.rm = TRUE)[9])/nrow(predicted_train)
Mean_Squared_Error_test <- (colSums(predicted_test, na.rm = TRUE)[9])/nrow(predicted_test)
Mean_Squared_Error_train
Mean_Squared_Error_test

# does errors have constant variance?

plot(y=predicted_train$fit,x=predicted_train$err)

plot(y=predicted_test$fit,x=predicted_test$err)

lmtest::bptest(model3)  #  P value should be greater than 0.05
                        # p-value = 0.2753

#are errors are normally distributed?
hist(predicted_train$err)
mean(predicted_train$err)
median(predicted_train$err)


hist(predicted_test$err)


#are errors auto-correlation assumption
plot(x=predicted_train$err)
plot(x=predicted_test$err)
dwtest(model3)

par(mfrow=c(2,2))
plot(model3)
pred_train <- predict(model1,car.test.frame)
regr.eval(car.test.frame$Price, pred_train)

plot(car.test.frame$Weight,car.test.frame$Mileage)
cor(car.test.frame$Price,car.test.frame$Mileage)
























