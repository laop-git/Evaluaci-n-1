rm(list = ls())
setwd(dir = "C:/Users/USUARIO/Documents/Cursos Profesionales/Especialización en R para Data Science/Clase 4_R4DS")
getwd()
dir()

library(lubridate)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(stringi)

#Data Enero de 2018

OsinergEnero2k18 <- read.table("201801_TABLA04_SICLI.txt", header = TRUE,sep = "\t",
                               col.names = c("CodEmpresa","Suministro", "PuntoSuministro","Fecha","RegistroActiva","RegistroPasiva","Periodo"),
                               colClasses = c("factor","factor","factor","character","numeric", "numeric", "character"))

str(OsinergEnero2k18)
dim(OsinergEnero2k18)
OsinergEnero2k18<-na.omit(OsinergEnero2k18)

#Confirmación data frame fecha y funciones anidadas
OsinergEnero2k18$Fechadate<-ymd_hm(OsinergEnero2k18$Fecha)
class(OsinergEnero2k18$FechaDate)

OsinergEnero2k18$año<-year(OsinergEnero2k18$FechaDate)
OsinergEnero2k18$mes<-month(OsinergEnero2k18$FechaDate)
OsinergEnero2k18$dia<-day(OsinergEnero2k18$FechaDate)
OsinergEnero2k18$Nhora<-hour(OsinergEnero2k18$FechaDate)

#Diagrama de dispersión del data frame

head(OsinergEnero2k18, n=1)$Fecha
tail(OsinergEnero2k18, n=1)$Fecha

#Data Enero de 2019

OsinergEnero2k19<-read.table("201901_TABLA4.txt", header=TRUE, dec=",", sep="\t",
                             col.names = c("CodEmpresa","Suministro","PuntoSuministro","Fecha",
                                           "RegistroActiva","RegistroPasiva","Periodo"),
                             colClasses = c("factor","factor","factor","character","character",
                                            "character","character"))
str(OsinergEnero2k19)
dim(OsinergEnero2k19)
OsinergEnero2k19<-na.omit(OsinergEnero2k19)

OsinergEnero2k19$RegistroActiva<- as.numeric(sub(",", ".", OsinergEnero2k19$RegistroActiva, fixed = TRUE))
OsinergEnero2k19$RegistroPasiva<- as.numeric(sub(",", ".", OsinergEnero2k19$RegistroPasiva, fixed = TRUE))

#Confirmación data frame fecha y funciones anidadas

OsinergEnero2k19$FechaDate<-dmy_hms(OsinergEnero2k19$Fecha)
class(OsinergEnero2k19$FechaDate)

OsinergEnero2k19$año<-year(OsinergEnero2k19$FechaDate)
OsinergEnero2k19$mes<-month(OsinergEnero2k19$FechaDate)
OsinergEnero2k19$dia<-day(OsinergEnero2k19$FechaDate)
OsinergEnero2k19$Nhora<-hour(OsinergEnero2k19$FechaDate)

#Diagrama de dispersión del data frame

head(OsinergEnero2k19, n=1)$Fecha
tail(OsinergEnero2k19, n=1)$Fecha

# Resumen estadistico Activa Enero 2018-2019
summary(OsinergEnero2k18$RegistroActiva)
summary(OsinergEnero2k19$RegistroActiva)

#Empresa con valor mínimo RegistroActiva Enero 2018 y 2019

minimoRA18<-min(OsinergEnero2k18$RegistroActiva)
minimoRA19<-min(OsinergEnero2k19$RegistroActiva)

# Resumen estadístico y Correlación entre valores maximos de Julio 2018 y Julio 2019 correspondientes al Suministro = "CL0036" DE Registro Activa
Suministro.max2018<-CodEmpresa.max2018[CodEmpresa.max2018$Suministro=="CL0036",]
summary(Suministro.max2018$RegistroActiva)
Suministro.max2019<-CodEmpresa.max2019[CodEmpresa.max2019$Suministro=="CL0036",]
summary(Suministro.max2019$RegistroActiva)

plot(Suministro.max2018$RegistroActiva,Suministro.max2019$RegistroActiva)
cor(Suministro.max2018$RegistroActiva,Suministro.max2019$RegistroActiva)


#Gráficas de Energía Pasiva por CodEmpresa="CEEP" en base al Suministro 2018 y 2019

CodEmpresa.max2018<-OsinergEnero2k18[OsinergEnero2k18$CodEmpresa=="CEEP",]
Plt1<-ggplot(CodEmpresa.max2018, aes(x=FechaDate, y=RegistroPasiva, group = Suministro, colour = Suministro )) + 
  geom_line()  + 
  theme_minimal()

CodEmpresa.max2019<-OsinergEnero2k19[OsinergEnero2k19$CodEmpresa=="CEEP ",]
Plt2<-ggplot(CodEmpresa.max2019, aes(x=FechaDate, y=RegistroPasiva, group = Suministro, colour = Suministro )) + 
  geom_line()  + 
  theme_minimal()

grid.arrange(Plt1,Plt2,nrow=2)