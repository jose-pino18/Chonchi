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
library(leaflet)
library(leafpop)
####################     PARTE 1 TRABAJO CON DATOS   #############################

#Cargamos los datasets y el shape

geo_chonchi <- read_excel("E:/Documentos/Trabajo/Rodrigo/Chonchi/Chonchi_geocoding.xlsx", #datos de niños a geocoding
                          skip = 1)
colegios_Chonchi <- read_csv("E:/Documentos/Trabajo/Rodrigo/Chonchi/Colegios_Chonchi.csv")

Censo2017_Manzanas <- read_delim("E:/Documentos/Trabajo/Rodrigo/Chonchi/Censo2017_16R_ManzanaEntidad_CSV/Censo2017_Manzanas.csv", 
                                 ";", escape_double = FALSE, trim_ws = TRUE) #Censo 2017 a nivel nacional en manzanas
Jardines_Chonchi <- read_csv("E:/Documentos/Trabajo/Rodrigo/Chonchi/Jardines_Chonchi.csv")

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
#Filtramos para selecionar solamente las variables que nos importan
colegios_Chonchi<-colegios_Chonchi%>%
  select(RBD,NOM_RBD.x,lat,lon,MAT_TOTAL)
#cole_chonchi$lat<-as.numeric(cole_chonchi$lat)#Nuestra variable lat y lon deben ser numericas por OBLIGACION

#Filtramos nuestro dataset censal para solamente tener los datos de Chonchi y creamos una variable con los estudiantes de la zona

Censo_chonchi<-Censo2017_Manzanas%>%
  filter(COMUNA==10203)%>%
  mutate(escolar= EDAD_0A5 + EDAD_6A14)

#Unimos la información que tenemos del shape con el csv censal
Chonchi<-left_join(municipios,Censo_chonchi, by=c("MANZENT_I"="ID_MANZENT"))

####################     PARTE 2 MAPEO DE DATOS Censales    #############################
# Mapeamos la comuna de Chonchi en primer lugar
comunas<-mapa_comunas# Obtenemos desde el paquete Chilemapas la geometria de las comunas chilenas (en caso de necesitarlo)

#Grficamos Chonchi con chilemaps
comunas%>% 
  filter(codigo_comuna==10203)%>%
  ggplot() +
  geom_sf(aes(geometry = geometry))

#Graficamos nuestro mapa con la información ya unida por tanto, lo haremos bajo los atributos que nosotros queremos,
# en este caso corresonden a la variable creada llamada estudiantes. Esto se hará con GGPLOT2

#Definimos una paleta de colores a utilizar
paleta <- c("#DCA761", "#CFB567", "#BFBC71", "#9EA887", "#819897")

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
  labs(title = "Distribución Escolar 2020")+
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

#Comenzamos con el trabajo a nivel de HTML 
#HTML          
coordinates(colegios_Chonchi) <- ~lon + lat
proj4string(colegios_Chonchi) <- "+init=epsg:4326"
mapview(colegios_Chonchi, cex= 10, zcol= "RBD",map.types= "OpenStreetMap")

#Haremos el mapa solamente con las variables que nos importan de la base de datos de estudiantes
estudiantes<-geo_chonchi%>%
  select("NOMBRE","DOMICILIO","COLEGIO 2020","lon","lat")

coordinates(estudiantes) <- ~lon + lat
proj4string(estudiantes) <- "+init=epsg:4326"
mapview(estudiantes, popup = popupTable(geo_chonchi,
                                        zcol = c("NOMBRE",
                                                 "DOMICILIO",
                                                 "COLEGIO 2020"
                                        ))) ## nos crea el html

#COLEGIO 2020 (prueba)
mapview(estudiantes,zcol="COLEGIO 2020",alpha = 0, legend=FALSE, burst= TRUE) #colegios como capas
mapview(estudiantes,zcol="COLEGIO 2020",alpha = 0) # estudiantes con leyenda de colegio

#Mapa final
mapview(estudiantes, alpha = 0, legend=FALSE, burst= TRUE, zcol="COLEGIO 2020",map.types= "OpenStreetMap")+  mapview(colegios_Chonchi, cex= 8,map.types= "OpenStreetMap")



###################      Parte 4 Jaridines infantiles #############
#Trabajo de datos... Recordar que ya tenemos la base de datos cargadas y este paso es posterior al mapeo de los colegios,
# ya que utilizamos datos que son resultados como imput para este proceso de mapear los jardines
Censo_chonchi[is.na(Censo_chonchi)] <- 0 #Pasamos los NA a 0 para poder mapearlos como atributos

#Agrupamos la información para obtener un consolidado según unidad vecinal
jose<-group_by(Censo_chonchi, DC)%>%
  summarise(sum(EDAD_0A5))

#Unimos nuestras base de datos
Jardines<-distritos_chonchi%>%
  left_join(jose, by= c("Distrito"="DC"))

Jardines_Censo<-Chonchi_Distrital%>%
  left_join(Jardines, by=c("COD_DISTRI"="Distrito"))

##Mapeamos los jardines infantiles 
ggplot(Jardines_Censo)+
  geom_sf(aes(fill= `sum(EDAD_0A5)`, geometry = geometry))+
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nInfante")+
  labs(title = "Distribución de menores de 0 a 5 años y ubicación de jardines")+
  geom_point(aes(x = lon, y = lat), data = Jardines_Chonchi, size = 2)+
  theme(line = element_blank(),  # Quitamos el fono (tema) del mapa
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank())

###Referencias########
#https://www.datanalytics.com/libro_r/ejemplos-1.html
#https://rpubs.com/HAVB/geoinfo
#https://rpubs.com/daniballari/srs_eh
#https://rpubs.com/rubenfbc/mapa_coordenadas
#https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf
#https://www.r-bloggers.com/cheesecake-diagrams-pie-charts-with-a-different-flavour/

############             Parte Bonus Muestra Chicho############
#La idea de este trabajo es poder mapear a los estudiantes con su respectivo colegio para ver la distribucion en dos colegios
#especificos, además de contar con un mapa que define de mejor manera las calles y carrerteras)
#Cargamos archivos
CHANQUIN <- read_csv("E:/Documentos/Trabajo/Rodrigo/Chonchi/Chicho/CHANQUIN.csv")
San_Carlos <- read_csv("E:/Documentos/Trabajo/Rodrigo/Chonchi/Chicho/San_Carlos.csv")

#Convertimos csv de Chanquin
coordinates(CHANQUIN) <- ~lon + lat
proj4string(CHANQUIN) <- "+init=epsg:4326"
mapview(CHANQUIN, zcol= "tipo")

## Convertimos CSV de la escuela San Carlos
coordinates(San_Carlos) <- ~lon + lat
proj4string(San_Carlos) <- "+init=epsg:4326"
mapview(San_Carlos, zcol= "tipo", map.types= "OpenTopoMap")
#Mapa unido
mapview(San_Carlos, zcol= "tipo", map.types= "OpenTopoMap") + mapview(CHANQUIN, zcol= "tipo", map.types= "OpenTopoMap")
