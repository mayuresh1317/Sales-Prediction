setwd("C:/Users/MAYURESH/OneDrive - National College of Ireland/NCI/SEMESTER 2/DAPA/Project/IMPLEMENATATION DOCUMENTATION/CODE/dataset")

Fishing <- read.csv("Fishing.csv",header=T, na.strings=c(""), stringsAsFactors = T)


ts_Fishing <- ts(Fishing,start = c(2015,1),frequency = 12)

is.ts(campinghikingts)



plot.ts(ts_Fishing, main= "Sales for CardioEquipments")

start(ts_Fishing)

end(ts_Fishing)


autoplot(ts_Fishing)
monthplot(ts_Fishing)
seasonplot(ts_Fishing)

dec.ts_Fishing<- decompose(ts_Fishing, type="additive")
dec.ts_Fishing
plot(dec.ts_Fishing)
summary(dec.ts_Fishing)

adf.test(ts_Fishing)

ndiffs(ts_Fishing)
nsdiffs(ts_Fishing)



acf(ts_Fishing)
pacf(ts_Fishing)


fit_Fishing <-arima(ts_Fishing,order = c(1,0,3), seasonal= c(2,0,1))
fit_Fishing
summary(fit_Fishing)



qqnorm(fit_Fishing$residuals)
qqline(fit_Fishing$residuals)
Box.test(fit_Fishing$residuals, type = "Ljung-Box")
checkresiduals(fit_Fishing)
forecast(fit_Fishing,4)

plot(forecast(fit_Fishing,8),Xlab="Month",ylab="Consumption")



plot(forecast(fit_Fishing, 12)) #forecast and actual data
pred_Fishing <- fitted.values(fit_Fishing)
lines(pred_Cleats, col="red") #add predicted values during training




predicted_Fishing= as.data.frame(pred_Fishing)

write_xlsx(predicted_Fishing, "predicted_Fishing.xlsx")

rm(CardioEquipmentsts)