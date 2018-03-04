library(tidyverse) # version 1.2.1
library(readxl) # version 1.0.0
library(DT) # version 0.4
library(highcharter) # version 0.5.0.9999
library(treemap) # version 2.4-2
source("admitidos-pregrado.R", encoding = 'UTF-8')
source("funciones.R", encoding = 'UTF-8')
col <-   c( "#8cc63f", # verde
            "#f15a24", # naranja
            "#0071bc", # azul vivo
            "#6d6666", # gris
            "#fbb03b", # amarillo
            "#93278f", # morado
            "#29abe2", # azul claro
            "#c1272d", # rojo
            "#8b7355", # cafe
            "#855b5b", # vinotinto
            "#ed1e79") # rosado
ano <- 2018
semestre <- 1 # 1 o 2 según corresponda
periodo_actual_titulo <- " 2018-I"

# Desagregaciones temáticas:


############### Edad: ###############

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b", # amarillo, sin información
            "#93278f", # morado
            "#29abe2", # azul claro
            "#c1272d", # rojo
            "#8b7355", # cafe
            "#855b5b", # vinotinto
            "#ed1e79") # rosado

################ 1. Tabla

CAT_EDAD_TABLA <- tabla(
  datos = Consolidado,
  categoria = "CAT_EDAD",
  variable = 'Rango de edad - en años - del admitido',
  mensaje = "Número de admitidos por grupos de edad",
  titulo = "Admitidos por grupos de edad"
);CAT_EDAD_TABLA

# saveWidget(CAT_EDAD_TABLA, 
#            file = file.path(getwd(),  "Resultados/Admitidos",
#                             "CAT_EDAD_TABLA.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 2. Serie

CAT_EDAD_SERIE <- series(
  datos = Consolidado,
  categoria = "CAT_EDAD",
  colores = col,
  titulo = "Número de admitidos por grupos de edad (en años)",
  eje = "Número de admitidos (k: miles)"
);CAT_EDAD_SERIE

# saveWidget(CAT_EDAD_SERIE, 
#            file = file.path(getwd(),  "Resultados/Admitidos",
#                             "CAT_EDAD_SERIE.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 3. Actual

CAT_EDAD_BARRA <- barra_vertical(
  datos = Consolidado,
  categoria = "CAT_EDAD",
  colores = col,
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo,
  titulo = "Admitidos por grupos de edad",
  eje = "Número de admitidos (k: miles)"
  ); CAT_EDAD_BARRA

# saveWidget(CAT_EDAD_BARRA, 
#            file = file.path(getwd(),  "Resultados/Admitidos",
#                             "CAT_EDAD_BARRA.html"),
#            selfcontained = F, libdir = "libraryjs")
           
############### Sexo: ###############

col <-   c( "#8cc63f", # verde, hombres
            "#f15a24", # naranja, mujeres
            "#0071bc", # azul vivo
            "#6d6666", # gris
            "#fbb03b", # amarillo
            "#93278f", # morado
            "#29abe2", # azul claro
            "#c1272d", # rojo
            "#8b7355", # cafe
            "#855b5b", # vinotinto
            "#ed1e79") # rosado

################ 1. Tabla

SEXO_TABLA <- tabla(
  datos = Consolidado,
  categoria = "SEXO",
  variable = 'Sexo del admitido',
  mensaje = "Número de admitidos por sexo",
  titulo = "Admitidos por sexo"
);SEXO_TABLA

# saveWidget(SEXO_TABLA, 
#            file = file.path(getwd(),  "Resultados/Admitidos",
#                             "SEXO_TABLA.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 2. Serie

SEXO_SERIE <- series(
  datos = Consolidado,
  categoria = "SEXO",
  colores = col,
  titulo = "Número de admitidos por sexo",
  eje = "Número de admitidos (k: miles)"
);SEXO_SERIE

# saveWidget(SEXO_SERIE, 
#            file = file.path(getwd(),  "Resultados/Admitidos",
#                             "SEXO_SERIE.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 3. Actual

SEXO_TORTA <- torta(
  datos = Consolidado,
  variable = "SEXO",
  colores = col,
  titulo = "Admitidos por sexo",
  etiqueta = "Número de admitidos",
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo
);SEXO_TORTA

# saveWidget(SEXO_TORTA, 
#            file = file.path(getwd(),  "Resultados/Admitidos",
#                             "SEXO_TORTA.html"),
#            selfcontained = F, libdir = "libraryjs")

############### Estrato socioeconómico: ###############

col <-   c( "#8cc63f", # verde, estrato 2 o menos
            "#f15a24", # naranja, estrato 3
            "#0071bc", # azul vivo, estrato 4 o más
            "#6d6666", # gris, ND/NE
            "#fbb03b", # amarillo
            "#93278f", # morado
            "#29abe2", # azul claro
            "#c1272d", # rojo
            "#8b7355", # cafe
            "#855b5b", # vinotinto
            "#ed1e79") # rosado

################ 1. Tabla

ESTRATO_TABLA <- tabla(
  datos = Consolidado,
  categoria = "ESTRATO",
  variable = 'Estrato socioeconómico del admitido',
  mensaje = "Número de admitidos según el estrato socioeconómico",
  titulo = "Admitidos según el estrato socioeconómico"
);ESTRATO_TABLA

# saveWidget(ESTRATO_TABLA, 
#            file = file.path(getwd(),  "Resultados/Admitidos",
#                             "ESTRATO_TABLA.html"),
#            selfcontained = F, libdir = "libraryjs")


################ 2. Serie

ESTRATO_SERIE <- series(
  datos = Consolidado,
  categoria = "ESTRATO",
  colores = col,
  titulo = "Número de admitidos por estrato socioeconómico",
  eje = "Número de admitidos (k: miles)"
);ESTRATO_SERIE

# saveWidget(ESTRATO_SERIE, 
#            file = file.path(getwd(),  "Resultados/Admitidos",
#                             "ESTRATO_SERIE.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 3. Actual

ESTRATO_TORTA <- torta(
  datos = Consolidado,
  variable = "ESTRATO",
  colores = col,
  titulo = "Admitidos por estrato socioeconómico",
  etiqueta = "Número de admitidos",
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo
);ESTRATO_TORTA

# saveWidget(ESTRATO_TORTA, 
#            file = file.path(getwd(),  "Resultados/Admitidos",
#                             "ESTRATO_TORTA.html"),
#            selfcontained = F, libdir = "libraryjs")


ESTRATO_BARRA <- barra_vertical(
  datos = Consolidado,
  categoria = "ESTRATO",
  colores = col,
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo,
  titulo = "Admitidos por estrato socioeconómico",
  eje = "Número de admitidos (k: miles)"
); ESTRATO_BARRA

# saveWidget(CAT_EDAD_BARRA, 
#            file = file.path(getwd(),  "Resultados/Admitidos",
#                             "ESTRATO_BARRA.html"),
#            selfcontained = F, libdir = "libraryjs")



############### Área de conocimiento SNIES: ###############

col <-   c( "#93278f", # morado, agronomia..
            "#29abe2", # azul claro, bellas artes
            "#fbb03b", # amarillo, ciencias de...
            "#f15a24", # naranja, ciencias sociales...
            "#0071bc", # azul vivo, economia...
            "#8cc63f", # verde, ingenieria...
            "#6d6666", # gris, matemáticas...
            "#c1272d", # rojo, sin informacion
            "#8b7355", # cafe
            "#855b5b", # vinotinto
            "#ed1e79") # rosado

################ 1. Tabla

AREAC_SNIES_TABLA <- tabla(
  datos = Consolidado,
  categoria = "AREAC_SNIES",
  variable = 'Modalidades de los admitidos por área de conocimiento (SNIES)',
  mensaje = "Número de admitidos por área de conocimiento (SNIES)",
  titulo = "Admitidos por área de conocimiento (SNIES)"
);AREAC_SNIES_TABLA

# saveWidget(AREAC_SNIES_TABLA, 
#            file = file.path(getwd(), "Resultados/Admitidos",
#                             "AREAC_SNIES_TABLA.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 2. Serie

AREAC_SNIES_SERIE <- series(
  datos = Consolidado,
  categoria = "AREAC_SNIES",
  colores = col,
  titulo = "Número de admitidos por área de conocimiento (SNIES)",
  eje = "Número de admitidos"
);AREAC_SNIES_SERIE
# 
# saveWidget(AREAC_SNIES_SERIE, 
#            file = file.path(getwd(), "Resultados/Admitidos",
#                             "AREAC_SNIES_SERIE.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 3. Actual

AREAC_SNIES_BARRA <- barra_horizontal(
  datos = Consolidado,
  categoria = "AREAC_SNIES",
  colores = col,
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo,
  titulo = "Admitidos por área de conocimiento (SNIES)",
  eje = "Número de admitidos"
); AREAC_SNIES_BARRA

# saveWidget(AREAC_SNIES_BARRA, 
#            file = file.path(getwd(), "Resultados/Admitidos",
#                             "AREAC_SNIES_BARRA.html"),
#            selfcontained = F, libdir = "libraryjs")

############### Área de conocimiento CINE: ###############

col <-   c( "#29abe2", # azul claro, Administración...
            "#f15a24", # naranja, Agricultura...
            "#fbb03b", # amarillo, Artes y humanidades
            "#0071bc", # azul vivo, Ciencias naturales...
            "#93278f", # morado, Ciencias sociales...
            "#8cc63f", # verde, ingenieria...
            "#6d6666", # gris, Salud y ...
            "#8b7355", # cafe, sin información
            "#c1272d", # rojo, TIC
            "#855b5b", # vinotinto
            "#ed1e79") # rosado

################ 1. Tabla

AREA_CINE_TABLA <- tabla(
  datos = Consolidado %>% 
    filter(is.na(Clase)==FALSE),
  categoria = "AREA_CINE",
  variable = 'Modalidades de los admitidos por área de conocimiento (CINE)',
  mensaje = "Número de admitidos por área de conocimiento (CINE)",
  titulo = "Admitidos por área de conocimiento (CINE)"
);AREA_CINE_TABLA

# saveWidget(AREA_CINE_TABLA, 
#            file = file.path(getwd(), "Resultados/Admitidos",
#                             "AREA_CINE_TABLA.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 2. Serie

AREA_CINE_SERIE <- series(
  datos = Consolidado %>% 
    filter(is.na(Clase)==FALSE),
  categoria = "AREA_CINE",
  colores = col,
  titulo = "Número de admitidos por área de conocimiento (CINE)",
  eje = "Número de admitidos"
);AREA_CINE_SERIE

# saveWidget(AREA_CINE_SERIE, 
#            file = file.path(getwd(), "Resultados/Admitidos",
#                             "AREA_CINE_SERIE.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 3. Actual

AREA_CINE_BARRA <- barra_horizontal(
  datos = Consolidado %>% 
    filter(is.na(Clase)==FALSE),
  categoria = "AREA_CINE",
  colores = col,
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo,
  titulo = "Admitidos por área de conocimiento (CINE)",
  eje = "Número de admitidos"
); AREA_CINE_BARRA

# saveWidget(AREA_CINE_BARRA, 
#            file = file.path(getwd(), "Resultados/Admitidos",
#                             "AREA_CINE_BARRA.html"),
#            selfcontained = F, libdir = "libraryjs")

############### Serie de: Evolución Histórica Total de Admitidos ###############

col <-   c( "#8cc63f", # verde, Total
            "#f15a24", # naranja
            "#0071bc", # azul vivo
            "#6d6666", # gris
            "#fbb03b", # amarillo
            "#93278f", # morado
            "#29abe2", # azul claro
            "#c1272d", # rojo
            "#8b7355", # cafe
            "#855b5b", # vinotinto
            "#ed1e79") # rosado
EVOLUCION_TABLA <- tablaall(
  datos = Consolidado,
  categoria = "TOTAL",
  mensaje = "Número de admitidos",
  titulo = "Admitidos"
);EVOLUCION_TABLA

# saveWidget(ADMITIDO_TABLA, 
#            file = file.path(getwd(), "Resultados/Admitidos",
#                             "EVOLUCION_TABLA.html"),
#            selfcontained = F, libdir = "libraryjs")
EVOLUCION_SERIE <- series(
  datos = Consolidado,
  categoria = "TOTAL",
  colores = col,
  titulo = "Evolución histórica del número total de admitidos a pregrado",
  eje = "Número de admitidos (k: miles)"
);EVOLUCION_SERIE

# saveWidget(EVOLUCION_SERIE, 
#            file = file.path(getwd(), "Resultados/Admitidos",
#                             "EVOLUCION_SERIE.html"),
#            selfcontained = F, libdir = "libraryjs")
