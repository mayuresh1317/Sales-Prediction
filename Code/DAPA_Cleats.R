setwd("C:/Users/MAYURESH/OneDrive - National College of Ireland/NCI/SEMESTER 2/DAPA/Project/IMPLEMENATATION DOCUMENTATION/CODE/dataset")

Cleats <- read.csv("Cleats.csv",header=T, na.strings=c(""), stringsAsFactors = T)


ts_Cleats <- ts(Cleats,start = c(2015,1),frequency = 12)

is.ts(campinghikingts)



plot.ts(ts_Cleats, main= "Sales for CardioEquipments")

start(ts_Cleats)

end(ts_Cleats)


autoplot(ts_Cleats)
monthplot(ts_Cleats)
seasonplot(ts_Cleats)

dec.ts_Cleats<- decompose(ts_Cleats, type="additive")
dec.ts_Cleats
plot(dec.ts_Cleats)
summary(dec.ts_Cleats)

adf.test(ts_Cleats)

ndiffs(ts_Cleats)
nsdiffs(ts_Cleats)



acf(ts_Cleats)
pacf(ts_Cleats)


fitts_Cleats <-arima(ts_Cleats,order = c(2,1,3), seasonal= c(1,1,1))
fitts_Cleats
summary(fitts_Cleats)



qqnorm(fitts_Cleats$residuals)
qqline(fitts_Cleats$residuals)
Box.test(fitts_Cleats$residuals, type = "Ljung-Box")
checkresiduals(fitts_Cleats)
forecast(fitts_Cleats,4)

plot(forecast(fitts_Cleats,8),Xlab="Month",ylab="Consumption")



plot(forecast(fitts_Cleats, 12)) #forecast and actual data
pred_Cleats <- fitted.values(fitts_Cleats)
lines(pred_Cleats, col="red") #add predicted values during training




predicted_Cleats= as.data.frame(pred_Cleats)

write_xlsx(predicted_Cleats, "predicted_Cleats.xlsx")

rm(CardioEquipmentsts)