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
