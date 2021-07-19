setwd("C:/Users/MAYURESH/OneDrive - National College of Ireland/NCI/SEMESTER 2/DAPA/Project/IMPLEMENATATION DOCUMENTATION/CODE/dataset")

IndoorOutdoorGame <- read.csv("IndoorOutdoorGame.csv",header=T, na.strings=c(""), stringsAsFactors = T)


ts_IndoorOutdoorGame <- ts(IndoorOutdoorGame,start = c(2015,1),frequency = 12)

is.ts(ts_IndoorOutdoorGame)



plot.ts(ts_IndoorOutdoorGame, main= "Sales for IndoorOutdoorGame")

start(ts_IndoorOutdoorGame)

end(ts_IndoorOutdoorGame)


autoplot(ts_IndoorOutdoorGame)
monthplot(ts_IndoorOutdoorGame)
seasonplot(ts_IndoorOutdoorGame)

dec.ts_IndoorOutdoorGame<- decompose(ts_IndoorOutdoorGame, type="additive")
dec.ts_IndoorOutdoorGame
plot(dec.ts_IndoorOutdoorGame)
summary(dec.ts_IndoorOutdoorGame)

adf.test(ts_IndoorOutdoorGame)

ndiffs(ts_IndoorOutdoorGame)
nsdiffs(ts_IndoorOutdoorGame)



acf(ts_IndoorOutdoorGame)
pacf(ts_IndoorOutdoorGame)


fit_IndoorOutdoorGame <-arima(ts_IndoorOutdoorGame,order = c(4,0,4), seasonal= c(2,0,2))

fit_IndoorOutdoorGame
summary(fit_IndoorOutdoorGame)



qqnorm(fit_IndoorOutdoorGame$residuals)
qqline(fit_IndoorOutdoorGame$residuals)
Box.test(fit_IndoorOutdoorGame$residuals, type = "Ljung-Box")
checkresiduals(fit_IndoorOutdoorGame)
forecast(fit_IndoorOutdoorGame,4)

plot(forecast(fit_IndoorOutdoorGame,8),Xlab="Month",ylab="Consumption")



plot(forecast(fit_IndoorOutdoorGame, 12)) #forecast and actual data
pred_IndoorOutdoorGame <- fitted.values(fit_IndoorOutdoorGame)
lines(pred_IndoorOutdoorGame, col="red") #add predicted values during training




predicted_IndoorOutdoorGame= as.data.frame(pred_IndoorOutdoorGame)

write_xlsx(predicted_IndoorOutdoorGame, "predicted_IndoorOutdoorGame.xlsx")

