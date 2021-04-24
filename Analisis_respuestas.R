library(tidyverse)
#------------------Analisis por Programa Social--------------------------------------
#Adulto mayor
Adulto_mayor_respuestas <- subset.data.frame(Respuestas_finales,
                                             Respuestas_finales$N_PROGRAMA_SOCIAL=="Adulto Mayor")
#Caracterizacion del personal
Tabla_adulto_mayor_personal<- select(Adulto_mayor_respuestas,
       c("N_PUESTO","EDAD_RESPONDENTE","N_ESTUDIO_MAXIMO","N_TIEMPO_PROGRAMA"))
write.csv(Tabla_adulto_mayor_personal,
          file = "Personal adulto mayor")
rm(Tabla_adulto_mayor_personal)

#Enfoque del programa
table(Adulto_mayor_respuestas$N_EDAD_DIRIGIDO_DISENIO)
table(Adulto_mayor_respuestas$N_EDAD_ATIENDE)

table(Adulto_mayor_respuestas$N_GENERO_DIRIGIDO_DISENIO)
table(Adulto_mayor_respuestas$N_GENERO_ATIENDE)

Problemas_soc_am <- Adulto_mayor_respuestas %>%
        select("N_PROBLEMA_SOC_1","N_PROBLEMA_SOC_2","N_PROBLEMA_SOC_3")%>%
        gather()
x<- as.data.frame(table(Problemas_soc_am$value))
y <- as.data.frame(prop.table(table(Problemas_soc_am$value)))
x$Prop <- y$Freq
write.csv(x,
          file = "Problemas sociales adulto mayor")

rm(x)
rm(y)
rm(Problemas_soc_am)




