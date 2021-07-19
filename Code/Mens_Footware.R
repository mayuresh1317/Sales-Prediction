setwd("C:/Users/MAYURESH/OneDrive - National College of Ireland/NCI/SEMESTER 2/DAPA/Project/IMPLEMENATATION DOCUMENTATION/CODE/dataset")

Mens_Footware <- read.csv("Mens_Footware.csv",header=T, na.strings=c(""), stringsAsFactors = T)


ts_Mens_Footware <- ts(Mens_Footware,start = c(2015,1),frequency = 12)

is.ts(ts_Mens_Footware)



plot.ts(ts_Mens_Footware, main= "Sales for Mens_Footware")

start(ts_Mens_Footware)

end(ts_Mens_Footware)


autoplot(ts_Mens_Footware)
monthplot(ts_Mens_Footware)
seasonplot(ts_Mens_Footware)

dec.ts_Mens_Footware<- decompose(ts_Mens_Footware, type="additive")
dec.ts_Mens_Footware
plot(dec.ts_Mens_Footware)
summary(dec.ts_Mens_Footware)

adf.test(ts_Mens_Footware)

ndiffs(ts_Mens_Footware)
nsdiffs(ts_Mens_Footware)



acf(ts_Mens_Footware)
pacf(ts_Mens_Footware)


fit_Mens_Footware <-arima(ts_Mens_Footware,order = c(1,1,1), seasonal= c(1,1,1))

fit_Mens_Footware
summary(fit_Mens_Footware)



qqnorm(fit_Mens_Footware$residuals)
qqline(fit_Mens_Footware$residuals)
Box.test(fit_Mens_Footware$residuals, type = "Ljung-Box")
checkresiduals(fit_Mens_Footware)
forecast(fit_Mens_Footware,4)

plot(forecast(fit_Mens_Footware,8),Xlab="Month",ylab="Consumption")



plot(forecast(fit_Mens_Footware, 12)) #forecast and actual data
pred_Mens_Footware <- fitted.values(fit_Mens_Footware)
lines(pred_Mens_Footware, col="red") #add predicted values during training




predicted_Mens_Footware= as.data.frame(pred_Mens_Footware)

write_xlsx(predicted_Mens_Footware, "predicted_Mens_Footware.xlsx")

