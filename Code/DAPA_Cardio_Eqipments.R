setwd("C:/Users/MAYURESH/OneDrive - National College of Ireland/NCI/SEMESTER 2/DAPA/Project/IMPLEMENATATION DOCUMENTATION/CODE/dataset")

CardioEquipments <- read.csv("CardioEquipments.csv",header=T, na.strings=c(""), stringsAsFactors = T)


ts_CardioEquipmentsts <- ts(CardioEquipments,start = c(2015,1),frequency = 12)

is.ts(campinghikingts)



plot.ts(ts_CardioEquipmentsts, main= "Sales for CardioEquipments")

start(ts_CardioEquipmentsts)

end(ts_CardioEquipmentsts)


autoplot(ts_CardioEquipmentsts)
monthplot(ts_CardioEquipmentsts)
seasonplot(ts_CardioEquipmentsts)

dec.ts_CardioEquipmentsts<- decompose(ts_CardioEquipmentsts, type="additive")
dec.ts_CardioEquipmentsts
plot(dec.ts_CardioEquipmentsts)
summary(dec.ts_CardioEquipmentsts)

adf.test(ts_CardioEquipmentsts)

ndiffs(ts_CardioEquipmentsts)
nsdiffs(ts_CardioEquipmentsts)



acf(ts_CardioEquipmentsts)
pacf(ts_CardioEquipmentsts)

fitCardioEquipmentsts <-arima(ts_CardioEquipmentsts,order = c(0,1,3), seasonal= c(1,1,1))

fitCardioEquipmentsts <-arima(ts_CardioEquipmentsts,order = c(0,1,3), seasonal= c(1,0,1))
fitCardioEquipmentsts
summary(fitCardioEquipmentsts)



qqnorm(fitCardioEquipmentsts$residuals)
qqline(fitCardioEquipmentsts$residuals)
Box.test(fitCardioEquipmentsts$residuals, type = "Ljung-Box")
checkresiduals(fitCardioEquipmentsts)
forecast(fitCardioEquipmentsts,4)

plot(forecast(fitCardioEquipmentsts,8),Xlab="Month",ylab="Consumption")



plot(forecast(fitCardioEquipmentsts, 12)) #forecast and actual data
pred_CardioEquipmentsts <- fitted.values(fitCardioEquipmentsts)
lines(pred_CardioEquipmentsts, col="red") #add predicted values during training




predicted_CardioEquipments= as.data.frame(pred_CardioEquipmentsts)

write_xlsx(predicted_CardioEquipments, "predicted_CardioEquipments.xlsx")


fitCardioEquipmentsts_pred <- forecast(fitCardioEquipmentsts, h =36)
fr_RoadBikes[["mean"]]