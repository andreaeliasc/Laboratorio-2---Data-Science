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
