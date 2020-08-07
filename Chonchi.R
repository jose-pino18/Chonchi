##Mapeo escolar de Chonchi

#Cargamos librerias necesarias
#####
library(tidyverse)
library(mapview)
library(sf)
library(ggmap)
library(readxl)
library(rgdal)
library(sp)
library(chilemapas)
####################     PARTE 1 TRABAJO CON DATOS   #############################

#Cargamos los datasets y el shape

geo_chonchi <- read_excel("E:/Documentos/Trabajo/Rodrigo/Chonchi/Chonchi_geocoding.xlsx", #datos de ni�os a geocoding
                          skip = 1)
colegios_Chonchi <- read_csv("E:/Documentos/Trabajo/Rodrigo/Chonchi/colegios_Chonchi.csv")

Censo2017_Manzanas <- read_delim("E:/Documentos/Trabajo/Rodrigo/Chonchi/Censo2017_16R_ManzanaEntidad_CSV/Censo2017_Manzanas.csv", 
                                 ";", escape_double = FALSE, trim_ws = TRUE) #Censo 2017 a nivel nacional en manzanas

#Cargamos el Shape (en formato tabla/dataframe) esto se hace para hacer un posterior join
municipios <- st_read("E:/Documentos/Trabajo/Rodrigo/Chonchi/shape_chonchi/chonchi_shape.shp")#cargamos el shape como un dataframe
chonchi<-readOGR("E:/Documentos/Trabajo/Rodrigo/Chonchi/shape_chonchi/chonchi_shape.shp")#Cargamos el Shape en formato shape
##Preparamos los datos/limpieza

#Limpieza de NA
geo_chonchi <- geo_chonchi[!is.na(geo_chonchi$G1),]#Eliminamos los valores NA pq no nos permiten hacer el geocoding
colegios_Chonchi<- colegios_Chonchi[!is.na(colegios_Chonchi$lat),]#Eliminamos los valores NA pq no nos permiten hacer el geocoding
Censo2017_Manzanas[is.na(Censo2017_Manzanas)] <- 0 #Todos los NA que tenga la base censal, pasan a ser 0
#Convertir datos a numeric
Censo2017_Manzanas$EDAD_6A14<-as.numeric(Censo2017_Manzanas$EDAD_6A14) #Para graficar atributos cuantitativos necesitamos que sean reconocidos de esa manera por R
Censo2017_Manzanas$EDAD_0A5<-as.numeric(Censo2017_Manzanas$EDAD_0A5)
#cole_chonchi$lat<-as.numeric(cole_chonchi$lat)#Nuestra variable lat y lon deben ser numericas por OBLIGACION

#Filtramos nuestro dataset censal para solamente tener los datos de Chonchi y creamos una variable con los estudiantes de la zona

Censo_chonchi<-Censo2017_Manzanas%>%
  filter(COMUNA==10203)%>%
  mutate(escolar= EDAD_0A5 + EDAD_6A14)

#Unimos la informaci�n que tenemos del shape con el csv censal
Chonchi<-left_join(municipios,Censo_chonchi, by=c("MANZENT_I"="ID_MANZENT"))

####################     PARTE 2 MAPEO DE DATOS Censales    #############################
# Mapeamos la comuna de Chonchi en primer lugar
comunas<-mapa_comunas# Obtenemos desde el paquete Chilemapas la geometria de las comunas chilenas (en caso de necesitarlo)

#Grficamos Chonchi con chilemaps
comunas%>% 
  filter(codigo_comuna==10203)%>%
  ggplot() +
  geom_sf(aes(geometry = geometry))

#Graficamos nuestro mapa con la informaci�n ya unida por tanto, lo haremos bajo los atributos que nosotros queremos,
# en este caso corresonden a la variable creada llamada estudiantes. Esto se har� con GGPLOT2

#Definimos una paleta de colores a utilizar
paleta <- c("#DCA761", "#CFB567", "#BFBC71", "#9EA887", "#819897") #paleta de colores tierra

#Mapeamos con ggplot2 y nuestra data creada con datos censales
ggplot(Chonchi)+
  geom_sf(aes(fill= escolar, geometry = geometry))+
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nEstudiantil")+
  labs(title = "Poblacion en edad escolar en Chonchi")+
theme(line = element_blank(),  # Quitamos el fono (tema) del mapa
      axis.text=element_blank(),
      axis.title=element_blank(),
      panel.background = element_blank())



###################      PARTE 3 AGREGAMOS GEOCODING  #####################
# Trabajo con datos
distritos_chonchi <- read_delim("E:/Documentos/Trabajo/Rodrigo/Chonchi/Censo2017_16R_ManzanaEntidad_CSV/distritos_chonchi.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)

D_chonchi<- st_read("E:/Documentos/Trabajo/Rodrigo/Chonchi/shape_chonchi/Distrito_chonchi.shp")#cargamos el shape como un dataframe

Chonchi_Distrital<-left_join(D_chonchi,distritos_chonchi, by= c("COD_DISTRI"="Distrito"))

#Mapeamos
ggplot(Chonchi_Distrital)+
  geom_sf(aes(fill= Estudiantes, geometry = geometry))+
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nEstudiantil")+
  labs(title = "Colegios en distritos censales con estudiantes potenciales")+
  geom_point(aes(x = lon, y = lat), data = colegios_Chonchi, size = 2)+
  theme(line = element_blank(),  # Quitamos el fono (tema) del mapa
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())


#Mapeamos a los estudiantes georreferenciados
ggplot(Chonchi_Distrital)+
  geom_sf(aes(fill= Estudiantes, geometry = geometry))+
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nEstudiantil")+
  labs(title = "Distribuci�n Escolar 2020")+
  geom_point(aes(x = lon, y = lat, color= `COLEGIO 2020`), data = geo_chonchi, size = 2)+
  theme(line = element_blank(),  # Quitamos el fono (tema) del mapa
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())


jose<-ggplot(Chonchi_Distrital)+
  geom_sf(aes(fill= Estudiantes, geometry = geometry))+
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nEstudiantil")+
  labs(title = "Poblacion escolar Chonchi")+
  theme(line = element_blank(),  # Quitamos el fono (tema) del mapa
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())

#Distribucion escolar por cada colegio de chonchi
jose +
  geom_point(aes(x = lon, y = lat, color= `COLEGIO 2020`), data = geo_chonchi, size = 2) + 
  facet_wrap(~`COLEGIO 2020`)

#HTML          
coordinates(colegios_Chonchi) <- ~lon + lat
proj4string(colegios_Chonchi) <- "+init=epsg:4326"
mapview(colegios_Chonchi) ## nos crea el html



###Referencias########
#https://www.datanalytics.com/libro_r/ejemplos-1.html
#https://rpubs.com/HAVB/geoinfo
#https://rpubs.com/daniballari/srs_eh
#https://rpubs.com/rubenfbc/mapa_coordenadas
#https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf
#https://www.r-bloggers.com/cheesecake-diagrams-pie-charts-with-a-different-flavour/
