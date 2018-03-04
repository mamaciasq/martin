library(tidyverse) # version 1.2.1
library(readxl) # version 1.0.0
library(DT) # version 0.4
library(highcharter) # version 0.5.0.9999
source("aspirantes-pregrado.R", encoding = 'UTF-8')
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
  variable = 'Rango de edad - en años - del aspirante',
  mensaje = "Número de aspirantes por grupos de edad",
  titulo = "Aspirantes por grupos de edad"
);CAT_EDAD_TABLA

# saveWidget(CAT_EDAD_TABLA, 
#            file = file.path(getwd(),  "Resultados/Aspirantes",
#                             "CAT_EDAD_TABLA.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 2. Serie

CAT_EDAD_SERIE <- series(
  datos = Consolidado,
  categoria = "CAT_EDAD",
  colores = col,
  titulo = "Número de aspirantes por grupos de edad (en años)",
  eje = "Número de aspirantes  (k: miles)"
);CAT_EDAD_SERIE

# saveWidget(CAT_EDAD_SERIE, 
#            file = file.path(getwd(),  "Resultados/Aspirantes",
#                             "CAT_EDAD_SERIE.html"),
#            selfcontained = F, libdir = "libraryjs")
# 
################ 3. Actual

CAT_EDAD_BARRA <- barra_vertical(
  datos = Consolidado,
  categoria = "CAT_EDAD",
  colores = col,
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo,
  titulo = "Aspirantes por grupos de edad",
  eje = "Número de aspirantes (k: miles)"
); CAT_EDAD_BARRA

# saveWidget(CAT_EDAD_BARRA, 
#            file = file.path(getwd(),  "Resultados/Aspirantes",
#                             "CAT_EDAD_BARRA.html"),
#            selfcontained = F, libdir = "libraryjs")

############### Sexo: ###############

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f", # verde, mujeres
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
  variable = 'Sexo del aspirante',
  mensaje = "Número de aspirantes por sexo",
  titulo = "Aspirantes por sexo"
);SEXO_TABLA

# saveWidget(SEXO_TABLA, 
#            file = file.path(getwd(),  "Resultados/Aspirantes",
#                             "SEXO_TABLA.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 2. Serie

SEXO_SERIE <- series(
  datos = Consolidado,
  categoria = "SEXO",
  colores = col,
  titulo = "Número de aspirantes por sexo",
  eje = "Número de aspirantes (k: miles)"
);SEXO_SERIE

# saveWidget(SEXO_SERIE, 
#            file = file.path(getwd(),  "Resultados/Aspirantes",
#                             "SEXO_SERIE.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 3. Actual

SEXO_TORTA <- torta(
  datos = Consolidado,
  variable = "SEXO",
  colores = col,
  titulo = "Aspirantes por sexo",
  etiqueta = "Número de aspirantes",
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo
);SEXO_TORTA

# saveWidget(SEXO_TORTA, 
#            file = file.path(getwd(),  "Resultados/Aspirantes",
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
  variable = 'Estrato socioeconómico del aspirante',
  mensaje = "Número de aspirantes según el estrato socioeconómico",
  titulo = "Aspirantes según el estrato socioeconómico"
);ESTRATO_TABLA

# saveWidget(ESTRATO_TABLA, 
#            file = file.path(getwd(),  "Resultados/Aspirantes",
#                             "ESTRATO_TABLA.html"),
#            selfcontained = F, libdir = "libraryjs")
# 
# ################ 2. Serie

ESTRATO_SERIE <- series(
  datos = Consolidado,
  categoria = "ESTRATO",
  colores = col,
  titulo = "Número de aspirantes por estrato socioeconómico",
  eje = "Número de aspirantes (k: miles)"
);ESTRATO_SERIE

# saveWidget(ESTRATO_SERIE, 
#            file = file.path(getwd(),  "Resultados/Aspirantes",
#                             "ESTRATO_SERIE.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 3. Actual

ESTRATO_TORTA <- torta(
  datos = Consolidado,
  variable = "ESTRATO",
  colores = col,
  titulo = "Aspirantes por estrato socioeconómico",
  etiqueta = "Número de aspirantes",
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo
);ESTRATO_TORTA

# saveWidget(ESTRATO_TORTA, 
#            file = file.path(getwd(),  "Resultados/Aspirantes",
#                             "ESTRATO_TORTA.html"),
#            selfcontained = F, libdir = "libraryjs")
# 

ESTRATO_BARRA <- barra_vertical(
  datos = Consolidado,
  categoria = "ESTRATO",
  colores = col,
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo,
  titulo = "Aspirantes por estrato socioeconómico",
  eje = "Número de aspirantes (k: miles)"
); ESTRATO_BARRA

# saveWidget(CAT_EDAD_BARRA, 
#            file = file.path(getwd(),  "Resultados/Aspirantes",
#                             "ESTRATO_BARRA.html"),
#            selfcontained = F, libdir = "libraryjs")



############### ADMITIDO: ###############

col <-   c( "#8cc63f", # verde, No
            "#f15a24", # naranja, Sí
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

ADMITIDO_TABLA <- tabla(
  datos = Consolidado,
  categoria = "ADMITIDO",
  variable = 'Resultados del proceso de admisión de los aspirantes',
  mensaje = "Número de aspirantes según resultados del proceso de admisión",
  titulo = "Aspirantes según resultados del proceso de admisión"
);ADMITIDO_TABLA

# saveWidget(ADMITIDO_TABLA, 
#            file = file.path(getwd(), "Resultados/Aspirantes",
#                             "ADMITIDO_TABLA.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 2. Serie

ADMITIDO_SERIE <- series(
  datos = Consolidado %>% 
    filter(Clase != "Regular"),
  categoria = "ADMITIDO",
  colores = col,
  titulo = "Número de aspirantes según resultados proceso de admisión a pregrado",
  eje = "Número de aspirantes (k: miles)"
);ADMITIDO_SERIE

# saveWidget(ADMITIDO_SERIE, 
#            file = file.path(getwd(), "Resultados/Aspirantes",
#                             "ADMITIDO_SERIE.html"),
#            selfcontained = F, libdir = "libraryjs")

################ 3. Actual

ADMITIDO_TORTA <- torta(
  datos = Consolidado %>% 
    filter(Clase != "Regular"),
  variable = "ADMITIDO",
  colores = col,
  titulo = "Aspirantes según resultados del proceso de admisión",
  etiqueta = "Número de aspirantes",
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo
);ADMITIDO_TORTA

# saveWidget(ADMITIDO_TORTA, 
#            file = file.path(getwd(), "Resultados/Aspirantes",
#                             "ADMITIDO_TORTA.html"),
#            selfcontained = F, libdir = "libraryjs")
# 
############### Serie de: Evolución Histórica Total de Aspirantes ###############

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
  mensaje = "Número de aspirantes",
  titulo = "Aspirantes"
);EVOLUCION_TABLA

# saveWidget(ADMITIDO_TABLA, 
#            file = file.path(getwd(), "Resultados/Aspirantes",
#                             "EVOLUCION_TABLA.html"),
#            selfcontained = F, libdir = "libraryjs")

EVOLUCION_SERIE <- series(
  datos = Consolidado,
  categoria = "TOTAL",
  colores = col,
  titulo = "Evolución histórica del número total de aspirantes a pregrado",
  eje = "Número de aspirantes  (k: miles)"
);EVOLUCION_SERIE

# saveWidget(EVOLUCION_SERIE, 
#            file = file.path(getwd(), "Resultados/Aspirantes",
#                             "EVOLUCION_SERIE.html"),
#            selfcontained = F, libdir = "libraryjs")
# 
