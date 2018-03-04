library(tidyverse)
library(readxl)
library(DT)
library(ggvis)
library(DiagrammeR)
library(xts)
library(highcharter)
library(lubridate)
library(treemap)
library(RColorBrewer)

source("prueba.R", encoding = 'UTF-8')

tabla_historica <- datatable(
  historico_admitidos, 
  caption = 'Tabla 0: Archivo histórico admitidos',
  filter = 'top',
  extensions = c('Buttons', 'Responsive'), width="100%",
  options = list(columnDefs = list(list(className = 'dt-center', targets = 0:3)), order = list(list(0, 'desc')),searchHighlight = TRUE,
                 dom = 'Bfrtip',
                 buttons = list(list(extend='copy',text='Copiar'), 'csv', 'excel',  list( extend = 'pdf', pageSize = 'A4',orientation = 'landscape', filename = 'pdf' ), list(extend='print',text='Imprimir',pageSize = 'A4')),
                 language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
tabla_historica



# Desagregaciones temáticas:

## Sexo: ##


sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Periodo'),
      th(rowspan = 2, 'Total'),
      th(colspan = 2, 'Sexo del admitido')
    ),
    tr(
      lapply(c('Hombres','Mujeres'), th)
    )
  )
))
J1_G2_I2_SEXO_F17 <- datatable(
  historico_admitidos[,1:4], 
  filter = 'top',
  extensions = c('Buttons', 'Responsive'), width="100%",
  container = sketch,
  rownames = FALSE,
  options = list(columnDefs = list(list(className = 'dt-center', targets = 0:3)), order = list(list(0, 'desc')),searchHighlight = TRUE,
                 dom = 'Bfrtip',
                 buttons = list(list(extend='copy',text='Copiar'), 'csv', 'excel',  list( extend = 'pdf', pageSize = 'A4',orientation = 'landscape', filename = 'pdf' ), list(extend='print',text='Imprimir',pageSize = 'A4')),
                 language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
J1_G2_I2_SEXO_F17
 
fecha0 <- as.POSIXct( as.yearmon(c( "2008-01", "2008-07", "2009-01", 
                                    "2009-07", "2010-01", "2010-07",
                                    "2011-01", "2011-07", "2012-01",
                                    "2012-07", "2013-01", "2013-07",
                                    "2014-01", "2014-07", "2015-01",
                                    "2015-07", "2016-01", "2016-07",
                                    "2017-01")) )
relativo <- function(x){
  div <- function(m){m*100/sum(m)}
  x1 <- t(apply(x,1,div))
  x2 <- round(x1,1)
  return(x2)
}

sexo <- xts(historico_admitidos %>% select(Hombres, Mujeres)%>% relativo(), order.by = fecha0, frequency = 6)


J1_G2_I2_SEXO_F16 <- hchart(sexo[,0:1],
                            name="Hombres",
                            color="#8cc63f",
                            zoomType=list(enabled=FALSE),
                            resetZoomButton=TRUE) %>%
  hc_add_series(name = "Mujeres", 
                data = sexo[,2], 
                color="#f15a24")%>% 
  hc_rangeSelector(inputEnabled = FALSE,enabled=FALSE) %>% 
  hc_add_theme(hc_theme_elementary()) %>% # blanco
  #hc_add_theme(hc_theme_chalk()) %>% # tablero
  hc_chart(zoomType = "x") %>% 
  hc_plotOptions( line = list(
    dataLabels = list( enabled = FALSE, format = "{y}%"),
    marker = list( enabled = TRUE, symbol = "square") ))%>%
  hc_yAxis(min=0, max=100,
           title = list(style = list( fontWeight = "bold",fontSize = "15px"),text = "Porcentaje de admitidos (%)"),
           opposite = FALSE) %>% 
  hc_title(style = list( fontWeight = "bold",fontSize = "20px" ), text = "Porcentaje de admitidos por sexo") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE, align = "right", verticalAlign ="top",
            layout = "vertical", x = 0, y = 100) %>%
  hc_tooltip(valueSuffix=" %",crosshairs = TRUE, backgroundColor =  hex_to_rgba("#baaeae", 0.7), 
             borderColor = "#6d6666", shared = TRUE,
             borderWidth = 5, useHTML = TRUE) 

J1_G2_I2_SEXO_F16


sexo_actual <- historico_admitidos %>%
  filter(Periodo == 20171) %>%
  select(Hombres, Mujeres)  %>% 
  gather(key="sexo",value="cantidad",Hombres,Mujeres) 

J1_G2_I2_SEXO_F4 <- highchart()  %>%
  hc_title(style = list( fontWeight = "bold",fontSize = "20px" ), text = 'Admitidos por sexo, periodo 2017-I'
  ) %>% 
  hc_add_series(sexo_actual, "pie", hcaes(name = paste(sexo,round(cantidad*100/sum(cantidad),1),"%"), y = cantidad), name = "Número de admitidos", showInLegend = TRUE) %>%
  hc_plotOptions( pie = list(
    allowPointSelect = TRUE,
    colorByPoint = TRUE, colors=c("#8cc63f","#f15a24") ))%>%
  hc_exporting(enabled = TRUE,
               buttons=list(
                 contextButton=list(
                   menuItems=list( 
                     list( text = 'Exportar a PNG')
                   )
                 )
               ))

J1_G2_I2_SEXO_F4

## Edad: ##

sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Periodo'),
      th(rowspan = 2, 'Total'),
      th(colspan = 5, 'Rango de edad - en años - del admitido')
    ),
    tr(
      lapply(c('0-17','18-20','21-25','26 o más','Sin información'), th)
    )
  )
))
J1_G2_I2_CAT_EDAD_F17 <- datatable(
  historico_admitidos[,c(1:2,5:9)], 
  filter = 'top',
  extensions = c('Buttons', 'Responsive'), width="100%",
  container = sketch,
  rownames = FALSE,
  #colnames = c('Periodo', 'Total', 'Edad: 0-17', 'Edad: 18-20','Edad: 21-25','Edad: 26 o más','Edad: Sin información'),
  options = list(columnDefs = list(list(className = 'dt-center', targets = 0:6)), order = list(list(0, 'desc')),searchHighlight = TRUE,
                 dom = 'Bfrtip',
                 buttons = list(list(extend='copy',text='Copiar'), 'csv', 'excel',  list( extend = 'pdf', pageSize = 'A4',orientation = 'landscape', filename = 'pdf' ), list(extend='print',text='Imprimir',pageSize = 'A4')),
                 language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
J1_G2_I2_CAT_EDAD_F17


fecha0 <- as.POSIXct( as.yearmon(c( "2008-01", "2008-07", "2009-01", 
                                    "2009-07", "2010-01", "2010-07",
                                    "2011-01", "2011-07", "2012-01",
                                    "2012-07", "2013-01", "2013-07",
                                    "2014-01", "2014-07", "2015-01",
                                    "2015-07", "2016-01", "2016-07",
                                    "2017-01")) )
relativo <- function(x){
  div <- function(m){m*100/sum(m)}
  x1 <- t(apply(x,1,div))
  x2 <- round(x1,2)
  return(x2)
}

edad <- xts(historico_admitidos %>% select("Edad: 0-17"=`E0-17`,"Edad: 18-20"=`E18-20`,"Edad: 21-25"=`E21-25`,"Edad: 26 o más"=`E26-N`,"Edad: Sin información"=`E-SI`)%>% relativo(), order.by = fecha0, frequency = 6)


J1_G2_I2_CAT_EDAD_F16 <- hchart(edad[,0:1],name="Edad: 0-17", color="#29abe2") %>%
  hc_add_series(name = "Edad: 18-20", data = edad[,2], color="#00a703")%>% 
  hc_add_series(name = "Edad: 21-25", data = edad[,3], color="#6d6666")%>% 
  hc_add_series(name = "Edad: 26 o más", data = edad[,4], color="#c1272d")%>% 
  hc_add_series(name = "Edad: Sin información", data = edad[,5], color="#fbb03b")%>% 
  hc_rangeSelector(inputEnabled = FALSE,enabled=FALSE) %>% 
  #hc_add_theme(hc_theme_db()) %>% # oscuro
  hc_add_theme(hc_theme_elementary()) %>% # blanco
  #hc_add_theme(hc_theme_chalk()) %>% # tablero
  hc_chart(zoomType = "x") %>% 
  hc_plotOptions( line = list(
    dataLabels = list( enabled = FALSE, format = "{y} %"),
    marker = list( enabled = TRUE, symbol = "square") ))%>%  hc_yAxis(min=0, max=100,title = list(style = list( fontWeight = "bold",fontSize = "15px"),text = "Porcentaje de admitidos (%)"),opposite = FALSE) %>% 
  hc_title(style = list( fontWeight = "bold",fontSize = "20px" ), text = "Porcentaje de admitidos por grupos de edad (en años)") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE,  align = "right", verticalAlign ="top",
            layout = "vertical", x = 0, y = 100) %>%
  hc_tooltip(valueSuffix=" %",crosshairs = TRUE, backgroundColor =  hex_to_rgba("#baaeae", 0.7), borderColor = "#6d6666",
             shared = TRUE, borderWidth = 5, useHTML = TRUE)


J1_G2_I2_CAT_EDAD_F16

edad_actual <- historico_admitidos %>% filter(Periodo == 20171) %>% select("Edad: 0-17"=`E0-17`,"Edad: 18-20"=`E18-20`,"Edad: 21-25"=`E21-25`,"Edad: 26 o más"=`E26-N`,"Edad: Sin información"=`E-SI`)  %>% gather(key="edad",value="cantidad",`Edad: 0-17`,`Edad: 18-20`,`Edad: 21-25`,`Edad: 26 o más`,`Edad: Sin información`) %>%  arrange( desc(cantidad))

J1_G2_I2_CAT_EDAD_F5 <- highchart()  %>%
  hc_add_series(edad_actual, "column", hcaes(x = paste(edad,"-",round(cantidad*100/sum(cantidad),1),"%"),                                             y = cantidad), name = "Número de admitidos", showInLegend =FALSE,showValues=TRUE) %>%
  hc_title(style = list( fontWeight = "bold",fontSize = "20px" ), text = 'Admitidos por rangos de edad (en años), periodo 2017-I'
  ) %>%
  hc_plotOptions( column = list( dataLabels= list(enabled=TRUE),
                                 colorByPoint = TRUE, colors=c("#29abe2", "#00a703", "#6d6666", "#c1272d", "#fbb03b") ))%>%
  hc_yAxis(
    title = list(style = list( fontWeight = "bold",fontSize = "15px"),text = "Número de admitidos (k: miles)")
  ) %>% 
  hc_xAxis(categories = edad_actual$edad) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_exporting(enabled = TRUE)

J1_G2_I2_CAT_EDAD_F5


