###############################
#Aspirantes A PREGRADO 2017-I##
###############################

########***** Admmisión especial por lugar de residencia

#Se requieren las siguientes librerías#

library(rgdal)
library(leaflet)
library(htmlwidgets)
library(tidyverse)
library(rjson)
library(readxl)
library(extrafont)


#Lectura de base de datos DIVIPOLA#

divipola.R <- read.table("DIVIPOLA_20160930.csv", sep=";", header=T)
municipios <-divipola.R[,5]
departamentos <- divipola.R[,4]
latitud <- divipola.R[,9]
longitud <- divipola.R[,8]
tipo_centro <- divipola.R[,7]
code_mun <- divipola.R[,2]
code_dept <- divipola.R[,1]
poblados <- data.frame(municipios, code_mun, code_dept, departamentos, longitud, latitud, tipo_centro)

# Base de datos con solo cabeceras municipales #

cabeceras <- poblados[tipo_centro == "CABECERA MUNICIPAL (CM)",]



# Lectura de datos de aspirantes por lugar de residencia#

procedencia.R <- read.table("Data\\2018.csv", sep=";", header=T) 
depart_asp <- procedencia.R[,10]
codept_asp<-procedencia.R[,11]
ciudad_asp <- procedencia.R[,12]
codecity_asp <-procedencia.R[,13]
codecity_asp <-as.integer(as.character(codecity_asp))
long_asp <- as.numeric(gsub(",", ".", as.character(procedencia.R[,14])))
lat_asp <- as.numeric(gsub(",", ".", as.character(procedencia.R[,15])))
mod_asp <- procedencia.R[,26] # si fue inscripción regular o irregular
tipo_asp <- procedencia.R[,27] # si es paes, peama o regular la inscripción
tipopeama_asp <- procedencia.R[,29]  
sexo <- procedencia.R[,21]
aspirantes<-data.frame(depart_asp,codecity_asp,codept_asp, ciudad_asp,long_asp, lat_asp, sexo, mod_asp, tipo_asp, tipopeama_asp)


# Omitir aspirantes del extranjero

aspirantes <- aspirantes %>%filter(depart_asp!="DEPARTAMENTO EXTRANJERO")

cantesp_asp <-aspirantes %>% group_by(mod_asp, depart_asp, codept_asp)%>% summarise (n = n()) %>% filter(mod_asp=="Especial")

cantesp_asp <- data.frame(cantesp_asp)[order(data.frame(cantesp_asp)$codept_asp),]

cantesp_asp <- rbind(cantesp_asp[-28,],cantesp_asp[28,])

cantesp_asp_mun <-  aspirantes %>% group_by(mod_asp, ciudad_asp, codecity_asp)%>% summarise (n = n()) %>% filter(mod_asp=="Especial")


#Extraer lista de códigos de los municipios con la respectiva cantidad de aspirantes

json_file <- "mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS=as.integer(json_data$features[[i]]$properties$MPIOS)
}

codigos <- matrix(0, nrow=1122,ncol=2)

for(i in 1:1122){
  codigos[i,1]=json_data$features[[i]]$properties$MPIOS
}


for(i in cantesp_asp_mun$codecity_asp){
  codigos[codigos[,1]==i,2]= cantesp_asp_mun$n[cantesp_asp_mun$codecity_asp==i]
}

codigos


######### Json por jurisdicciones de municipios

cities_col <- rgdal::readOGR("mpio5.json",use_iconv = T, encoding="UTF-8")

cities_col@data <- cities_col@data[-c(1,2,3,4,5,7,9,10,11,12,13,14,15)]



#### Agregar información al Spatial Data Frame


cities_col@data$CODE_MPI=codigos[,1]

cities_col@data$CANT_ASP=codigos[,2]



#Lectura de JSON de Colombia por departamentos


colombia.R<- rgdal::readOGR("depto4.json", use_iconv = T, encoding = "UTF-8")

colombia.R@data<-colombia.R@data[-c(3,4,5)] #Se omite información complementaria

colombia.R@data$DPTO=as.integer(as.character(colombia.R@data$DPTO))

colombia.R@data$CANT_ASP=cantesp_asp$n


#### Buscar el centroide de cada departamento en la base


x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}


# Centroides de los departamentos

centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept" , "lon" , "lat")

# ESRI es un proveedor de bases de mapas con licencia pública

esri <- grep("^Esri" , providers , value = T)
esri<- esri[c(2,10,11)]
esri
names.esri <- c("Street" , "Satélite" , "Ligero")


# Leer en formato excel la base de datos de DIVIPOLA

divipola2.R <- read_excel("DIVIPOLA_20160930.xlsx")

cabeceras2 <- divipola2.R %>% filter(divipola2.R$`Tipo Centro Poblado` == "CABECERA MUNICIPAL (CM)")


#Sedes de Presencia Nacional

check.integer <- function(x) {
  x == round(x)
}

capitaless <- cabeceras2[check.integer((cabeceras2$`Código Municipio`-1)/1000)==T,]

#Suprimir Agua de Dios

capitaless <- capitaless %>% filter(`Código Municipio`!="25001")

#Filtrar sedes de la Universidad Nacional de Colombia

sedes <- rbind(cabeceras2[cabeceras2$`Código Municipio`== "5001",],
               cabeceras2[cabeceras2$`Código Municipio`== "11001",], 
               cabeceras2[cabeceras2$`Código Municipio`== "76520",], 
               cabeceras2[cabeceras2$`Código Municipio`== "52835",],
               cabeceras2[cabeceras2$`Código Municipio`== "17001",],
               cabeceras2[cabeceras2$`Código Municipio`== "88001",],
               cabeceras2[cabeceras2$`Código Municipio`== "81001",],
               cabeceras2[cabeceras2$`Código Municipio`== "91001",])

Sede <- c("Medellín", "Bogotá", "Palmira", "Tumaco", "Manizales",   "Caribe", "Orinoquia", "Amazonía" )

sedes <- cbind(sedes, Sede)
sedes


sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")
                           

label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

#Función para mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}



## Filtrar tipos de población especial

tipo <- aspirantes %>% group_by(depart_asp, codept_asp, tipo_asp)%>% summarise (n = n())
PEAMA <-  tipo %>% filter(tipo_asp=="PEAMA ")

tipopeama <- aspirantes %>% group_by(depart_asp, codept_asp, tipopeama_asp)%>% summarise (n = n())
orinoquia <- tipopeama %>%  filter(tipopeama_asp=="PEAMA - Orinoquía")


codigoscol <- data.frame(matrix(0, nrow=33,ncol=4))

for(i in 1:33){
  codigoscol[i,1]=colombia.R@data$DPTO[i]
}


for(i in cantesp_asp$codept_asp){
  codigoscol[codigoscol[,1]==i,2]= cantesp_asp$n[cantesp_asp$codept_asp==i]
}


for(i in PEAMA$codept_asp){
  codigoscol[codigoscol[,1]==i,3]= PEAMA$n[PEAMA$codept_asp==i]
}


  for(i in orinoquia$codept_asp){
    codigoscol[codigoscol[,1]==i,4]= orinoquia$n[orinoquia$codept_asp==i]
  }


colnames (codigoscol) = c("code_dept", "Total", "PEAMA", "orinoquia")

codigoscol

colombia.R$PEAMA = codigoscol$PEAMA

colombia.R$ORINOQUIA = codigoscol$orinoquia


# Localizaciones idividuos poblacion especial


tipopeama <- aspirantes %>% group_by(depart_asp, codept_asp, tipopeama_asp, lat_asp, long_asp, sexo)
orinoquia <- tipopeama %>%  filter(tipopeama_asp=="PEAMA - Orinoquía")



centroidecol <- centro_dept%>% filter(dept =="CUNDINAMARCA")
centroidecol


