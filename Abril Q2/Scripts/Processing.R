#Análisis Pulso Ciudadano

library("haven")
library("dplyr")
library("readxl")
library("tidyverse")
library("summarytools")

#cargamos base de datos

Pulso <- read_spss("Original Data/BBDD_Pulso Abril Q2_.sav")

#Selección de variables de interés

Pulso <-select(Pulso,SEXO,EDAD,RANGOEDAD,GSE_COD,ZONA,REGION,DISTRITO_ELECTORAL,P1:P69,P156:P162,P216:PONDERADOR)

#Recodificación de variables y nombres de variables

## Edad ##

Pulso$EDAD <-as.numeric(Pulso$EDAD)
class(Pulso$EDAD)
Pulso <- mutate(Pulso, Edadrec = car::recode(Pulso$EDAD, "17:21 = 1; 22:35 = 2;
                                                           36:50 = 3; 50:90 =4; else = NA"))
Pulso <- mutate(Pulso, Edadrec = recode(Pulso$Edadrec,"1" = "Centennials","2" = "Millennials",
                                        "3" = "Gen X", "4" = "Boomers"))


## Sexo ##

Pulso$SEXO <-as.numeric(Pulso$SEXO)

Pulso <-mutate(Pulso, SexoRecod = recode(Pulso$SEXO, "1" = "hombre", "2" = "mujer"))

## GSE ##

Pulso$GSE_COD <-as.numeric(Pulso$GSE_COD)

Pulso <-mutate(Pulso, GSERecod = recode(Pulso$GSE_COD, "1" = "C1", "2" = "C2", "3" = "C3",
                                        "4" = "D", "5" = "E"))

# Revisión de código para ver la adhesión a Jiles

presi <-table(Pulso$P11_COD)

round((prop.table(presi)*100),1)

freq(Pulso$P11_COD, weights = Pulso$PONDERADOR)

#Nueva base sólo con preferencias de jiles

Pulso$P11_COD <-as.numeric(Pulso$P11_COD)
class(Pulso$P12_COD)
jiles <-filter(Pulso, P12_COD==45)

saveRDS(jiles, file = "Original Data/Jiles.rds")

saveRDS(Pulso, file = "Original Data/Pulso_Ciudadano.rds")



## FIN ## 