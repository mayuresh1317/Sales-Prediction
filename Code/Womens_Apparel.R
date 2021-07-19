setwd("C:/Users/MAYURESH/OneDrive - National College of Ireland/NCI/SEMESTER 2/DAPA/Project/IMPLEMENATATION DOCUMENTATION/CODE/dataset")

Womens_Apparel <- read.csv("Womens_Apparel.csv",header=T, na.strings=c(""), stringsAsFactors = T)


ts_Womens_Apparel <- ts(Womens_Apparel,start = c(2015,1),frequency = 12)

is.ts(ts_Womens_Apparel)



plot.ts(ts_Womens_Apparel, main= "Sales for Womens_Apparel")

start(ts_Womens_Apparel)

end(ts_Womens_Apparel)


autoplot(ts_Womens_Apparel)
monthplot(ts_Womens_Apparel)
seasonplot(ts_Womens_Apparel)

dec.ts_Womens_Apparel<- decompose(ts_Womens_Apparel, type="additive")
dec.ts_Womens_Apparel
plot(dec.ts_Womens_Apparel)
summary(dec.ts_Womens_Apparel)

adf.test(ts_Womens_Apparel)

ndiffs(ts_Womens_Apparel)
nsdiffs(ts_Womens_Apparel)



acf(ts_Womens_Apparel)
pacf(ts_Womens_Apparel)


fit_Womens_Apparel <-arima(ts_Womens_Apparel,order = c(2,0,3), seasonal= c(2,0,3))

fit_Womens_Apparel
summary(fit_Womens_Apparel)



qqnorm(fit_Womens_Apparel$residuals)
qqline(fit_Womens_Apparel$residuals)
Box.test(fit_Womens_Apparel$residuals, type = "Ljung-Box")
checkresiduals(fit_Womens_Apparel)
forecast(fit_Womens_Apparel,4)

plot(forecast(fit_Womens_Apparel,8),Xlab="Month",ylab="Consumption")



plot(forecast(fit_Womens_Apparel, 12)) #forecast and actual data
pred_Womens_Apparel <- fitted.values(fit_Womens_Apparel)
lines(pred_Womens_Apparel, col="red") #add predicted values during training




predicted_Womens_Apparel= as.data.frame(pred_Womens_Apparel)

write_xlsx(predicted_Womens_Apparel, "predicted_Womens_Apparel.xlsx")

