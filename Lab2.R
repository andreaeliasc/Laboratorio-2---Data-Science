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

