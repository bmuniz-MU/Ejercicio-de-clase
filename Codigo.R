setwd('C:/Users/bmuniz/Desktop/Ejercicio repaso para examen Mayo')
library(dplyr)
library(visdat)
library(naniar)
library(VIM)
library(BBmisc)
library(discretization)
library(smoothmest)

WHR <- read.csv("world-happiness-report.csv")
names(WHR)[1]<-'Country_Name'
WHR_2021 <- read.csv("world-happiness-report-2021.csv")
names(WHR_2021)[1]<-'Country_Name'

str(WHR)
str(WHR_2021)

estad_HWR <- as.data.frame(summary(WHR[,-c(1)]))

distinct(WHR_2021[,c(1,2)])
Paises<- as.vector(distinct(as.data.frame(WHR_2021[,c(1)])))
Regiones <- as.vector(distinct(as.data.frame(WHR_2021[,c(2)])))
estad_WHR_2021 <- as.data.frame(summary(WHR_2021[,-c(1,2)]))

##Trabajamos las ausencias
#WHR
miss_case_summary(WHR)
miss_var_summary(WHR)

gg_miss_var(WHR, facet=year)

sum_ausencias_pais <- WHR %>%
  mutate(var=apply(is.na(WHR)[,-c(1,2)], 1, sum)) %>%
  group_by (Country_Name) %>%
  summarize(sum(var))

sum_ausencias_año <- WHR %>%
  mutate(var=apply(is.na(WHR)[,-c(1,2)], 1, sum)) %>%
  group_by (year) %>%
  summarize(sum(var))

vis_miss(WHR)

#las ausencias son de tipo MCAR, por lo que imputar es la mejor opción
#en este caso, un sistema de imputación kNN utilizando como variables para
#medir la distancia el pais y el año encaja bien

for(i in (3:11)) {WHR <- kNN(WHR, colnames(WHR)[i], dist_var=c("year", "Country_Name"))}
vis_miss(WHR)

#WHR_2021
miss_case_summary(WHR_2021)
miss_var_summary(WHR_2021)

#En este caso solo hay una ausencia. Miramos donde
WHR_2021 %>%
  filter(is.na(Explained.by..Social.support)==TRUE)
#imputamos el valor ausente considerando otros paises que sean cercanos en los 
#tasas de explicación que ofrecen el resto de valores
WHR_2021 <- kNN(WHR_2021, "Explained.by..Social.support", k=10, dist_var=c("Explained.by..Log.GDP.per.capita", "Explained.by..Healthy.life.expectancy", "Explained.by..Freedom.to.make.life.choices", "Explained.by..Generosity" , "Explained.by..Perceptions.of.corruption", "Dystopia...residual"))
miss_var_summary(names(WHR_2021))

#Empezamos con los outliers
boxplot(as.data.frame(WHR_2021[,-c(1,2)]))
boxplot(as.data.frame(WHR_2021[,-c(1,2,3)]))
boxplot(as.data.frame(WHR_2021[,-c(1,2,9)]))
boxplot(as.data.frame(WHR_2021[,c(9)]))

boxplot(as.data.frame(WHR[,-c(1,2)]))
boxplot(as.data.frame(WHR[,-c(1,2,6)]))
boxplot(as.data.frame(WHR[,c(6)]))
#la esperanza de vida en Haití ha sido y sigue siendo un drama. Me niego a 
#corregir un dato proporciona tanta información
#sí corrijo el Ladder.Score, xque tiene pinta que se han movido los decimales
WHR_2021[1,3]<-7.842
boxplot(as.data.frame(WHR_2021[,-c(1,2)]))

#normalizo y discretizo
WHR_2021 <- cbind(WHR_2021[,1:6], normalize(WHR_2021[,c(7)], method='standardize'), WHR_2021[,8:20])
WHR <- cbind(WHR[,1:3], normalize(WHR[,c(4)], method='standardize'), WHR[,5:11])

CAIM <- disc.Topdown(WHR[,c(4,3)], method=1)
discretizacion_CAIM <- as.data.frame(CAIM$Disc.data)
cortes_CAIM <- as.data.frame(CAIM$cutp)

#anonimizo consulta
Generosity_media <- WHR %>%
  group_by(year, Country_Name) %>%
  summarize(Generosity_media=mean(Generosity))

epsilon=1.5
a<-max(WHR$Generosity)
b<-min(WHR$Generosity)
n<-nrow(WHR)
gs <- (b-a)/n

Generosity_media_anon <- cbind(as.data.frame(Generosity_media[,c(1,2)]), round(rdoublex(1949, Generosity_media[,3], gs/epsilon),2))


