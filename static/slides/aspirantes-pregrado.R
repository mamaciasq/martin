# Script para hacer manipulación de las bases de datos
library(tidyverse) # version 1.2.1
library(readxl) # version 1.0.0

tipovar <- c("numeric",	"numeric", "numeric", "text", "numeric", "text", "numeric",
             "numeric", "numeric", "text", "numeric", "text", "numeric", "numeric",
             "numeric", "text", "numeric", "text", "numeric", "text", "text", "numeric",
             "text", "text", "text", "text", "text", "text", "text", "numeric", "text",
             "numeric", "text", "text", "text", "text", "numeric", "text", "text", "numeric",
             "numeric", "text")

importar <- function(year, periodo){
  read_excel(paste0("Data/",year,".xlsx"), 
             sheet = periodo, guess_max = 100000,  col_types = tipovar)
}



# bases de aspirantes por año y periodo
P2008_1 <- importar("2008", "I") 
P2008_2 <- importar("2008", "II") 
P2009_1 <- importar("2009", "I") 
P2009_2 <- importar("2009", "II") 
P2010_1 <- importar("2010", "I") 
P2010_2 <- importar("2010", "II") 
P2011_1 <- importar("2011", "I") 
P2011_2 <- importar("2011", "II") 
P2012_1 <- importar("2012", "I") 
P2012_2 <- importar("2012", "II") 
P2013_1 <- importar("2013", "I") 
P2013_2 <- importar("2013", "II") 
P2014_1 <- importar("2014", "I") 
P2014_2 <- importar("2014", "II") 
P2015_1 <- importar("2015", "I") 
P2015_2 <- importar("2015", "II") 
P2016_1 <- importar("2016", "I") 
P2016_2 <- importar("2016", "II") 
P2017_1 <- importar("2017", "I")
P2017_2 <- importar("2017", "II") 
P2018_1 <- importar("2018", "I")


# pegar las bases de datos por abajo
Microdatos <- bind_rows(P2008_1, P2008_2, P2009_1, P2009_2, P2010_1, P2010_2, P2011_1, 
                        P2011_2, P2012_1, P2012_2, P2013_1, P2013_2, P2014_1, P2014_2, 
                        P2015_1, P2015_2, P2016_1, P2016_2, P2017_1, P2017_2, P2018_1)

Microdatos <- Microdatos %>% filter(PEAMA=="PEAMA - Orinoquía")


# Función para consolidar información agregada tipo gather

Clases <- function(varc){
  Microdatos %>% group_by_(.dots = list("YEAR", "SEMESTRE", varc)) %>% 
    summarise(Total = n()) %>% rename_(.dots=list("Clase"=varc)) %>% 
    mutate_(Variable = "varc") %>% select(Variable, YEAR, SEMESTRE, Clase, Total)
}



DT1 <- Clases("NACIONALIDAD")
DT2 <- Clases("SEXO")
DT3 <- Clases("CAT_EDAD")
DT4 <- Clases("ESTRATO")
DT5 <- Clases("DISCAPACIDAD")
DT6 <- Clases("TIPO_DISC") 
DT7 <- Clases("MOD_INS") 
DT8 <- Clases("TIPO_INS")
DT9 <- Clases("PAES")
DT10 <- Clases("PEAMA")
DT11 <- Clases("INS_SEDE_NOMBRE")
DT12 <- Clases("ADMITIDO")
DT13 <- Clases("ADM_ANDINA_PEAMA")


# Consulta valores globales

Total <- Microdatos %>% group_by(YEAR, SEMESTRE) %>%  summarise(Total = n()) %>% ungroup() %>% 
  mutate(Variable="TOTAL", YEAR=YEAR, SEMESTRE=SEMESTRE, Clase = "Total", Total=Total) %>% 
  select(Variable, YEAR, SEMESTRE, Clase, Total)

# consolidado de información estadística para visualización 

Consolidado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, DT13, Total)
memory.size()
rm(P2008_1, P2008_2, P2009_1, P2009_2, P2010_1, P2010_2, P2011_1, 
   P2011_2, P2012_1, P2012_2, P2013_1, P2013_2, P2014_1, P2014_2, 
   P2015_1, P2015_2, P2016_1, P2016_2, P2017_1, P2017_2, P2018_1,
   DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, DT11, DT12, DT13,
   Microdatos,Total,tipovar,importar,Clases);memory.size(); gc(); memory.size()




