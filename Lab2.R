library(dplyr)
library(stringr)
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(zoo)

consumo <-read.csv("C:/Users/Diego/Documents/Universidad/8vo Semestre/Data Science/Laboratorio-2---Data-Science/DatosConsumoCombustibles.csv", stringsAsFactors = FALSE)
importaciones <-read.csv("C:/Users/Diego/Documents/Universidad/8vo Semestre/Data Science/Laboratorio-2---Data-Science/DatosImportacionCombustibles.csv", stringsAsFactors = FALSE)

consumo <-(consumo[,c("Anio","GasolinaSuper","GasolinaRegular","Diesel","DieselLS")])
consumo[is.na(consumo)]<-0
consumo[consumo$Anio>="2018","Diesel"]<-consumo[consumo$Anio>="2018","DieselLS"]


importaciones <-(consumo[,c("Anio","GasolinaSuper","GasolinaRegular","Diesel","DieselLS")])
importaciones[is.na(importaciones)]<-0
importaciones[importaciones$Anio>="2018","Diesel"]<-importaciones[importaciones$Anio>="2018","DieselLS"]

consumo$DieselLS <- NULL
importaciones$DieselLS <- NULL



## consumo
dieselTimeSeriesC <-ts(consumo$Diesel,start = 2001, end = 2021,frequency = 12)
superTimeSeriesC <-ts(consumo$GasolinaSuper,start = 2001,end = 2021,frequency = 12)
regularTimeSeriesC<-ts(consumo$GasolinaRegular,start = 2001,end = 2021,frequency = 12)
View(dieselTimeSeriesC)
View(regularTimeSeriesC)
View(superTimeSeriesC)
### graficas
plot(dieselTimeSeriesC, col = "magenta")
plot(superTimeSeriesC, col = "magenta")
plot(regularTimeSeriesC, col = "magenta")




## importaciones
dieselTimeSeriesI<-ts(importaciones$Diesel,start = 2001, end = 2020,frequency = 12)
superTimeSeriesI<-ts(importaciones$GasolinaSuper,start = 2001,end = 2020,frequency = 12)
regularTimeSeriesI<-ts(importaciones$GasolinaRegular,start = 2001,end = 2020,frequency = 12)
View(dieselTimeSeriesI)
View(regularTimeSeriesI)
View(superTimeSeriesI)
### graficas
plot(dieselTimeSeriesI, col = "blue")
plot(superTimeSeriesI, col = "blue")
plot(regularTimeSeriesI, col = "blue")

##decompose 

plot(decompose(dieselTimeSeriesI))
plot(decompose(superTimeSeriesI))
plot(decompose(regularTimeSeriesI))

plot(decompose(dieselTimeSeriesC))
plot(decompose(superTimeSeriesC))
plot(decompose(regularTimeSeriesC))

#Analisis de estacionariedad en varianza

#Estacionariedad 
##Consumo
###Diesel
plot(aggregate(dieselTimeSeriesC,FUN=mean))
dec.Diesel<-decompose(na.StructTS(dieselTimeSeriesC))
plot(dec.Diesel)
plot(dec.Diesel$seasonal)
###Super
plot(aggregate(superTimeSeriesC,FUN=mean))
dec.Super<-decompose(na.StructTS(superTimeSeriesC))
plot(dec.Super)
plot(dec.Super$seasonal)
###Regular
plot(aggregate(regularTimeSeriesC,FUN=mean))
dec.Regular<-decompose(na.StructTS(regularTimeSeriesC))
plot(dec.Regular)
plot(dec.Regular$seasonal)

##Importaciones
###Diesel
plot(aggregate(dieselTimeSeriesI,FUN=mean))
dec.Diesel<-decompose(na.StructTS(dieselTimeSeriesI))
plot(dec.Diesel)
plot(dec.Diesel$seasonal)
###Super
plot(aggregate(superTimeSeriesI,FUN=mean))
dec.Super<-decompose(na.StructTS(superTimeSeriesI))
plot(dec.Super)
plot(dec.Super$seasonal)
###Regular
plot(aggregate(regularTimeSeriesI,FUN=mean))
dec.Regular<-decompose(na.StructTS(regularTimeSeriesI))
plot(dec.Regular)
plot(dec.Regular$seasonal)

#Analisis de estacionariedad en varianza
##Importaciones
varDieselI <-cbind(dieselTimeSeriesI)
plot.ts(varDieselI)

varRegularI <-cbind(regularTimeSeriesI)
plot.ts(varRegularI)

varsuperI <-cbind(superTimeSeriesI)
plot.ts(varsuperI)

lambda <- BoxCox.lambda(dieselTimeSeriesI)
print(lambda)
plot(BoxCox(dieselTimeSeriesI, lambda = -0.05377146))

lambda <- BoxCox.lambda(regularTimeSeriesI)
print(lambda)
plot(BoxCox(regularTimeSeriesI, lambda = -0.2309985))

lambda <- BoxCox.lambda(superTimeSeriesI)
print(lambda)
plot(BoxCox(superTimeSeriesI, lambda = -0.4456302))

##Consumo
varDieselC <-cbind(dieselTimeSeriesC)
plot.ts(varDieselC)

varRegularC <-cbind(regularTimeSeriesC)
plot.ts(varRegularC)

varsuperC <-cbind(superTimeSeriesC)
plot.ts(varsuperC)

lambda <- BoxCox.lambda(dieselTimeSeriesC)
print(lambda)
plot(BoxCox(dieselTimeSeriesC, lambda = -0.2047953))

lambda <- BoxCox.lambda(regularTimeSeriesC)
print(lambda)
plot(BoxCox(regularTimeSeriesC, lambda = -0.3088451))

lambda <- BoxCox.lambda(superTimeSeriesC)
print(lambda)
plot(BoxCox(superTimeSeriesC, lambda = -0.5363523))

#logaritmo

logDieselI <- log(dieselTimeSeriesI)
logRegularI <- log(regularTimeSeriesI)
logSuperI <- log(superTimeSeriesI)

logDieselC <- log(dieselTimeSeriesC)
logRegularC <- log(regularTimeSeriesC)
logSuperC <- log(superTimeSeriesC)


#Determinar si hay raices

adf.test(logDieselI)
adf.test(logRegularI)
adf.test(logSuperI)

adf.test(logDieselC)
adf.test(logRegularC)
adf.test(logSuperC)

#Determinacion de valores Q y D 
acf(logDieselI, 50)
acf(logRegularI, 50)
acf(logSuperI, 50)

pacf(diff(logDieselI), 50)
pacf(diff(logRegularI), 50)
pacf(diff(logSuperI),50)

acf(logDieselC, 50)
acf(logRegularC, 50)
acf(logSuperC, 50)

pacf(diff(logDieselC), 50)
pacf(diff(logRegularC), 50)
pacf(diff(logSuperC), 50)

#Modelo

#Consumo
arimaDieselC<- auto.arima(dieselTimeSeriesC)

fit <- arima(log(dieselTimeSeriesC), c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
pred<- predict(fit, n.ahead = 10*12)
ts.plot(dieselTimeSeriesC,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(dieselTimeSeriesC), c(2, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))

forecastAPC <- forecast(fit2, level = c(95), h = 50)
autoplot(forecastAPC)

#importaciones

arimaDieselI<- auto.arima(dieselTimeSeriesI)

fit <- arima(log(dieselTimeSeriesI), c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
pred<- predict(fit, n.ahead = 10*12)
ts.plot(dieselTimeSeriesI,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(dieselTimeSeriesI), c(2, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))

forecastAPI <- forecast(fit2, level = c(95), h = 50)
autoplot(forecastAPI)



#Modelo para Regular
#Consumo
arimaRegularC<- auto.arima(regularTimeSeriesC)

fit <- arima(log(regularTimeSeriesC), c(1,1,2), seasonal = list(order = c(1,1,1), period = 12))
pred<- predict(fit, n.ahead = 10*12)
ts.plot(regularTimeSeriesC,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(regularTimeSeriesC), c(1, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))

forecastAP <- forecast(fit2, level = c(95), h = 50)
autoplot(forecastAP)

#Importaciones
arimaRegularI<- auto.arima(regularTimeSeriesI)

fit <- arima(log(regularTimeSeriesI), c(1,1,2), seasonal = list(order = c(1,1,1), period = 12))
pred<- predict(fit, n.ahead = 10*12)
ts.plot(regularTimeSeriesI,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(regularTimeSeriesI), c(1, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))

forecastAP <- forecast(fit2, level = c(95), h = 50)
autoplot(forecastAP)


#Modelo para Super
#Consumo
arimaSuper<- auto.arima(superTimeSeriesC)

fit <- arima(log(superTimeSeriesC), c(1,1,2), seasonal = list(order = c(1,1,1), period = 12))
pred<- predict(fit, n.ahead = 10*12)
ts.plot(superTimeSeriesC,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(superTimeSeriesC), c(1, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))

forecastAP <- forecast(fit2, level = c(95), h = 50)
autoplot(forecastAP)

#Importaciones

arimaSuper<- auto.arima(superTimeSeriesI)

fit <- arima(log(superTimeSeriesI), c(1,1,2), seasonal = list(order = c(1,1,1), period = 12))
pred<- predict(fit, n.ahead = 10*12)
ts.plot(superTimeSeriesI,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(superTimeSeriesI), c(1, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))

forecastAP <- forecast(fit2, level = c(95), h = 50)
autoplot(forecastAP)



# Separacion de conjunto de datos para entrenamiento 
dieselSTEntrenamientoC<-ts(consumo$Diesel,start = 2001, end = 2015,frequency = 12)
superSTEntrenamientoC<-ts(consumo$GasolinaSuper,start = 2001,end = 2015,frequency = 12)
regularSTEntrenamientoC<-ts(consumo$GasolinaRegular,start = 2001,end = 2015,frequency = 12)

dieselSTEntrenamientoI<-ts(importaciones$Diesel,start = 2001, end = 2015,frequency = 12)
superSTEntrenamientoI<-ts(importaciones$GasolinaSuper,start = 2001,end = 2015,frequency = 12)
regularSTEntrenamientoI<-ts(importaciones$GasolinaRegular,start = 2001,end = 2015,frequency = 12)

# Separacion de conjunto de datos para test
dieselSTTestC<-ts(consumo$Diesel,start = 2016, end = 2021,frequency = 12)
superSTTesC<-ts(consumo$GasolinaSuper,start = 2016,end = 2021,frequency = 12)
regularSTTesC<-ts(consumo$GasolinaRegular,start = 2016,end = 2021,frequency = 12)

dieselSTTestI<-ts(importaciones$Diesel,start = 2016, end = 2021,frequency = 12)
superSTTesI<-ts(importaciones$GasolinaSuper,start = 2016,end = 2021,frequency = 12)
regularSTTesI<-ts(importaciones$GasolinaRegular,start = 2016,end = 2021,frequency = 12)


# Prophet Modelo 
install.packagies('Rcpp')
library(Rcpp)
install.packages('rlang')
library(rlang)
install.packages('prophet')
library(prophet)
library(zoo)
install.packages('rstan')
library(rstan)

# Gasolina diesel 
#Consumo
prophetDieseldfC <- data.frame(ds=as.Date(yearmon(time(dieselSTEntrenamientoC))),y = as.matrix(dieselSTEntrenamientoC))
prophetDieseltestC <- data.frame(ds=as.Date(yearmon(time(dieselSTTestC))),y = as.matrix(dieselSTTestC))

head(prophetDieseldfC)

fitProphet<-prophet(prophetDieseldfC,yearly.seasonality = T,weekly.seasonality = T)
future <- make_future_dataframe(fitProphet,periods = 49,freq = "month", include_history = T)
p <- predict(fitProphet,future)
p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]
plot(fitProphet,p)


pred<-tail(p,61)
pred$y<-prophetDieseltestC$y


ggplot(pred, aes(x=ds, y=yhat)) +
  geom_line(size=1, alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=pred, aes(x=ds, y=y),color="red")

#Importaciones

prophetDieseldfI <- data.frame(ds=as.Date(yearmon(time(dieselSTEntrenamientoI))),y = as.matrix(dieselSTEntrenamientoI))
prophetDieseltestI <- data.frame(ds=as.Date(yearmon(time(dieselSTTestI))),y = as.matrix(dieselSTTestI))

head(prophetDieseldfI)

fitProphet<-prophet(prophetDieseldfI,yearly.seasonality = T,weekly.seasonality = T)
future <- make_future_dataframe(fitProphet,periods = 49,freq = "month", include_history = T)
p <- predict(fitProphet,future)
p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]
plot(fitProphet,p)


pred<-tail(p,61)
pred$y<-prophetDieseltestI$y


ggplot(pred, aes(x=ds, y=yhat)) +
  geom_line(size=1, alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=pred, aes(x=ds, y=y),color="red")


# Gasolina super

#Consumo
prophetSuperdfC <- data.frame(ds=as.Date(yearmon(time(superSTEntrenamientoC))),y = as.matrix(superSTEntrenamientoC))
prophetSupertestC <- data.frame(ds=as.Date(yearmon(time(superSTTesC))),y = as.matrix(superSTTesC))

head(prophetDieseldfC)

fitProphet<-prophet(prophetSuperdfC,yearly.seasonality = T,weekly.seasonality = T)
future <- make_future_dataframe(fitProphet,periods = 49,freq = "month", include_history = T)

p <- predict(fitProphet,future)
p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]

plot(fitProphet,p)


pred<-tail(p,61)
pred$y<-prophetSupertestC$y


ggplot(pred, aes(x=ds, y=yhat)) +
  geom_line(size=1, alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=pred, aes(x=ds, y=y),color="red")

#Importaciones

prophetSuperdfI <- data.frame(ds=as.Date(yearmon(time(superSTEntrenamientoI))),y = as.matrix(superSTEntrenamientoI))
prophetSupertestI <- data.frame(ds=as.Date(yearmon(time(superSTTesI))),y = as.matrix(superSTTesI))

head(prophetDieseldfI)

fitProphet<-prophet(prophetSuperdfI,yearly.seasonality = T,weekly.seasonality = T)
future <- make_future_dataframe(fitProphet,periods = 49,freq = "month", include_history = T)
p <- predict(fitProphet,future)

p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]
plot(fitProphet,p)


pred<-tail(p,61)
pred$y<-prophetSupertestI$y


ggplot(pred, aes(x=ds, y=yhat)) +
  geom_line(size=1, alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=pred, aes(x=ds, y=y),color="red")



# Gasolina Regular

#Consumo
prophetRegulardfC <- data.frame(ds=as.Date(yearmon(time(regularSTEntrenamientoC))),y = as.matrix(regularSTEntrenamientoC))
prophetRegulartestC <- data.frame(ds=as.Date(yearmon(time(regularSTTesC))),y = as.matrix(regularSTTesC))

head(prophetRegulardfC)

fitProphet<-prophet(prophetRegulardfC,yearly.seasonality = T,weekly.seasonality = T)
future <- make_future_dataframe(fitProphet,periods = 49,freq = "month", include_history = T)
p <- predict(fitProphet,future)


p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]
plot(fitProphet,p)


pred<-tail(p,61)
pred$y<-prophetRegulartestC$y


ggplot(pred, aes(x=ds, y=yhat)) +
  geom_line(size=1, alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=pred, aes(x=ds, y=y),color="red")


#Importaciones

prophetRegulardfI <- data.frame(ds=as.Date(yearmon(time(regularSTEntrenamientoI))),y = as.matrix(regularSTEntrenamientoI))
prophetRegulartestI <- data.frame(ds=as.Date(yearmon(time(regularSTTesI))),y = as.matrix(regularSTTesI))

head(prophetRegulardfI)

fitProphet<-prophet(prophetRegulardfI,yearly.seasonality = T,weekly.seasonality = T)
future <- make_future_dataframe(fitProphet,periods = 49,freq = "month", include_history = T)
p <- predict(fitProphet,future)

p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]
plot(fitProphet,p)


pred<-tail(p,61)
pred$y<-prophetRegulartestI$y


ggplot(pred, aes(x=ds, y=yhat)) +
  geom_line(size=1, alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=pred, aes(x=ds, y=y),color="red")


