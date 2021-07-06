library(tidyverse)
library(stringi)
#Cargar datos
Pasos_problemas_csv <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/Paosos_problemas_csv.csv")
#Vincular y agregar programa social
Pasos_problemas_csv$N_PROGRAMA_SOCIAL <- 
Respuestas_finales$N_PROGRAMA_SOCIAL[match(Pasos_problemas_csv$N_COLABORADOR_ID,
      Respuestas_finales$N_COLABORADOR_ID)]
#Vincular y agregar problemas sociales 1-3
N_PROBLEMA_SOC_1 <- read_csv("Datos/N_PROBLEMA_SOC_1-Table 1.csv")
Pasos_problemas_csv$N_PROBLEMA_SOC <- N_PROBLEMA_SOC_1$DESCRIPCIÓN[match(Pasos_problemas_csv$N_PROBLEMA_COD,
      N_PROBLEMA_SOC_1$CODIGO)]
Pasos_problemas_csv$N_PROBLEMA_COD <- NULL
#Guardar 
write.csv(Pasos_problemas_csv,
          file = "Arbol del problema PSDS")
rm(N_PROBLEMA_SOC_1)
rm(Pasos_problemas_csv)






