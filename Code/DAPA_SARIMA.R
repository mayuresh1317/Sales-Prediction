#rm(list=ls())

setwd("C:/Users/MAYURESH/OneDrive - National College of Ireland/NCI/SEMESTER 2/DAPA/Project/IMPLEMENATATION DOCUMENTATION/CODE/dataset")

campinghiking <- read.csv("campinghiking.csv", header=T, na.strings=c(""), stringsAsFactors = T)

getwd()

install.packages("fpp2")
install.packages("carTool")
install.packages("xlsx")

install.packages(c("readxl","writexl")) 

library(xlsx)
library(carTool)
library(writexl)

campinghikingts <- ts(campinghiking,start = c(2015,1) ,frequency = 12)

CardioEquipmentsts <- ts(CardioEquipments,start = c(2015,1),frequency = 12)

is.ts(campinghikingts)

plot.ts(campinghikingts, main= "Sales for Camping and Hiking")

plot.ts(CardioEquipments, main= "Sales for CardioEquipments")

start(CardioEquipments)

end(CardioEquipments)

autoplot(campinghikingts)
monthplot(campinghikingts)
seasonplot(campinghikingts)


dec.camphike<- decompose(campinghikingts, type="additive")
dec.camphike
plot(dec.camphike)
summary(dec.camphike)


#Arima

#Test
adf.test(campinghikingts)

ndiffs(campinghikingts)
nsdiffs(campinghikingts)

dcampinghikingts= diff(campinghikingts)

adf.test(dcampinghikingts)


ndiffs(dcampinghikingts)


acf(campinghikingts)
pacf(campinghikingts)

#Arima Model

autofitcamphike<-auto.arima(campinghikingts)

qqnorm(autofitcamphike$residuals)
qqline(autofitcamphike$residuals)
Box.test(autofitcamphike$residuals, type = "Ljung-Box")
checkresiduals(autofitcamphike)
forecast(autofitcamphike,4)




fitcamphike <-arima(campinghikingts,order = c(1,1,1), seasonal= c(1,1,1))
fitcamphike
summary(fitcamphike)



qqnorm(fitcamphike$residuals)
qqline(fitcamphike$residuals)
Box.test(fitcamphike$residuals, type = "Ljung-Box")
checkresiduals(fitcamphike)
forecast(fitcamphike,4)

plot(forecast(fitcamphike,8),Xlab="Month",ylab="Consumption")



plot(forecast(fitcamphike, 12)) #forecast and actual data
pred_camphike <- fitted.values(fitcamphike)
lines(pred_camphike, col="red") #add predicted values during training


campinghiking


predicted_camphike= as.data.frame(predicted)

write_xlsx(predicted_camphike, "predicted.xlsx")

rm(fitcamphikecamphike)