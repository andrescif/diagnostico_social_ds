library(tidyverse)
#Cargar datos
Respuestas_finales <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/DATA-Table 1.csv")
DATA <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/DATA-Table 1.csv", header=FALSE)
#Inseratar el CUI correcto
DATA <- DATA[-1,]
CUI <- DATA$V1
Respuestas_finales$V_DPI_CUI_185 <- CUI
rm(CUI)
rm(DATA)
#Establecer factores
table(Respuestas_finales$N_ESTUDIO_MAXIMO)






