dat<-read.csv("airline.csv")
library(class)
# training and test data
trainI <- sample(1:1000, 700)
traind <- dat[trainI,]
testd <- dat[-trainI,]


#辨認忠誠度＃
#利用svm來預測
library(e1071)
s <- svm(is_loyal ~ ., data = traind, probability = TRUE)
results <- predict(s, testd, probability = TRUE)
table(Real = testd$is_loyal, Predict = results)#觀看結果

# Draw Data and Decision Boundary
library(RColorBrewer)
display.brewer.all()
rcols <- palette(brewer.pal(n = 3, name = "Set3"))
plot(s, dat, register_rate ~ meal_rate,
      slice = list(depart_on_time = 1, arrive_on_time = 1),col = rcols)

#Tuning SVM to find the best cost and gamma 
svm_tune <- tune(svm, is_loyal ~ .,data=traind, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
svm_tune$best.model
plot(svm_tune)
after_tune <- svm(is_loyal ~ ., data=dat, kernel="radial", cost=10, gamma=0.5)
summary(after_tune)
pred <- predict(after_tune,testd)
table(Real = testd$is_loyal, Predict = pred)#調整後準確率明顯提高

#找尋重要變數
library(rminer)
M <- fit(is_loyal~., data=traind, model="svm", kpar=list(sigma=0.10), C=2)
svm.imp <- Importance(M, data=traind)
View(svm.imp)#觀看結果
svm.imp[2]#利用imp找尋較重要之變數像是dm_message,seat_rate,depart_on_time
#航空公司可以改善座椅舒適度、提高準時起飛的比率，並以簡訊廣告為主來降低
#其他廣告費用，藉此來降低成本及提高顧客忠誠度
#
