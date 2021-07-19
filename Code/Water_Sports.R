setwd("C:/Users/MAYURESH/OneDrive - National College of Ireland/NCI/SEMESTER 2/DAPA/Project/IMPLEMENATATION DOCUMENTATION/CODE/dataset")

Water_Sports <- read.csv("Water_Sports.csv",header=T, na.strings=c(""), stringsAsFactors = T)


ts_Water_Sports <- ts(Water_Sports,start = c(2015,1),frequency = 12)

is.ts(ts_Water_Sports)



plot.ts(ts_Water_Sports, main= "Sales for Water_Sports")

start(ts_Water_Sports)

end(ts_Water_Sports)


autoplot(ts_Water_Sports)
monthplot(ts_Water_Sports)
seasonplot(ts_Water_Sports)

dec.ts_Water_Sports<- decompose(ts_Water_Sports, type="additive")
dec.ts_Water_Sports
plot(dec.ts_Water_Sports)
summary(dec.ts_Water_Sports)

adf.test(ts_Water_Sports)

ndiffs(ts_Water_Sports)
nsdiffs(ts_Water_Sports)



acf(ts_Water_Sports)
pacf(ts_Water_Sports)


fit_Water_Sports <-arima(ts_Water_Sports,order = c(4,0,4), seasonal= c(2,0,2))

fit_Water_Sports
summary(fit_Water_Sports)



qqnorm(fit_Water_Sports$residuals)
qqline(fit_Water_Sports$residuals)
Box.test(fit_Water_Sports$residuals, type = "Ljung-Box")
checkresiduals(fit_Water_Sports)
forecast(fit_Water_Sports,4)

plot(forecast(fit_Water_Sports,8),Xlab="Month",ylab="Consumption")



plot(forecast(fit_Water_Sports, 12)) #forecast and actual data
pred_Water_Sports <- fitted.values(fit_Water_Sports)
lines(pred_Water_Sports, col="red") #add predicted values during training




predicted_Water_Sports= as.data.frame(pred_Water_Sports)

write_xlsx(predicted_Water_Sports, "predicted_Water_Sports.xlsx")

