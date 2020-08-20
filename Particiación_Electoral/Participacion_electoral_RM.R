#######Participación electoral RM Chile####
###Cargamos nuestras librerias 
library(tidyverse)
###### elecciones 2013 (primera vuelta presidencia y elecciones al congreso)####
#Cargamos nuestro dataset de las elecciones de 2013 (1 vuelta, incluye parlamentarias)
participacion_2013 <- read_delim("E:/Documentos/Trabajo/electoral/participacion/e_2013/Vuelta_1_2013.csv", 
                                  ";", escape_double = FALSE, trim_ws = TRUE)

#Las variables categoricas/cualitativas que consideramos relevante para el analisis las convertiremos en factor
participacion_2013$SEXO<-as.factor(participacion_2013$SEXO)
participacion_2013$SUFRAGIO<-as.factor(participacion_2013$SUFRAGIO)
participacion_2013$COMUNA<-as.factor(participacion_2013$COMUNA)
#Filtraremos nuestra base de datos, queremos las comunas de la RM y la gente que participo de ella (un resumen)
participacion<-participacion_2013 %>%
  group_by(COMUNA) %>% 
  filter(REGION=="Metropolitana De Santiago")%>%
  count(SUFRAGIO)

## creamos columnas independientes con la gente que voto y la que se abstuvo
voto<- participacion %>%
  filter(SUFRAGIO=="sufragó")

no_voto<-participacion %>%
  filter(SUFRAGIO=="no sufragó")
## Unimos ambas columnas a un nuevo DF creado, llamado votación
votacion<- voto%>%
  left_join(no_voto, by= "COMUNA")
##Calulamos el total del padron (suma de votantes como no votantes)
votacion<- votacion %>%
  mutate(padron= sum(n.x + n.y))
#Calculamos la participacion electoral a nivel comunal en la RM
votacion<- votacion %>%
  mutate(parti= n.x*100/ padron)
#Pasamos nuestro valor de participación a un numero con dos decimanles.
##### Elecciones municipales 2012 ####
#Cargamos la base de datos a ocupar
Municipales_2012 <- read_delim("E:/Documentos/Trabajo/electoral/participacion/e_2012/VW_VOTARON_2012.csv", 
                               ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                               trim_ws = TRUE) #Este archivo tenía una codificación distinta

Municipales_2012$SEXO<-as.factor(Municipales_2012$SEXO)
Municipales_2012$SUFRAGIO<-as.factor(Municipales_2012$SUFRAGIO)
Municipales_2012$COMUNA<-as.factor(Municipales_2012$COMUNA)
#Filtraremos nuestra base de datos, queremos las comunas de la RM y la gente que participo de ella (un resumen)
participacion<-Municipales_2012 %>%
  group_by(COMUNA) %>% 
  filter(REGION=="Metropolitana De Santiago")%>%
  count(SUFRAGIO)

## creamos columnas independientes con la gente que voto y la que se abstuvo
voto<- participacion %>%
  filter(SUFRAGIO=="sufragó")

no_voto<-participacion %>%
  filter(SUFRAGIO=="no sufragó")
## Unimos ambas columnas a un nuevo DF creado, llamado votación
votacion<- voto%>%
  left_join(no_voto, by= "COMUNA")
##Calulamos el total del padron (suma de votantes como no votantes)
votacion<- votacion %>%
  mutate(padron= sum(n.x + n.y))
#Calculamos la participacion electoral a nivel comunal en la RM
votacion<- votacion %>%
  mutate(parti= n.x*100/ padron)

##### Elecciones 2013 segunda vuelta ####
#Carfamos el dataset
