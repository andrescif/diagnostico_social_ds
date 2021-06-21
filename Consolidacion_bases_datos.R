library(tidyverse)
library(eeptools)
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

#Prueba
names(Respuestas_finales)
PB_1 <- subset.data.frame(Respuestas_finales,
                          select = c("V_DPI_CUI_185","N_ESTUDIO_MAXIMO"))
class(PB_1$N_ESTUDIO_MAXIMO)
PB_1$N_ESTUDIO_MAXIMO<- N_ESTUDIO_MAXIMO$DESCRIPCIÓN[match(PB_1$N_ESTUDIO_MAXIMO,N_ESTUDIO_MAXIMO$CODIGO)]
PB_1$N_ESTUDIO_MAXIMO <- as.character(PB_1$N_ESTUDIO_MAXIMO)
PB_1$N_ESTUDIO_MAXIMO<- trimws(PB_1$N_ESTUDIO_MAXIMO,which = "both")
PB_1$N_ESTUDIO_MAXIMO<- as.factor(PB_1$N_ESTUDIO_MAXIMO)
table(PB_1$N_ESTUDIO_MAXIMO)
rm(PB_1)
#---------------CONSOLIDACIÓN DE TODAS LAS RESPUESTAS EN UNA BASE DE DATOS----------------#
var(Respuestas_finales)
#Variable: N_ESTUDIO_MAXIMO (Maximo nivel educativo)
N_ESTUDIO_MAXIMO <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/N_ESTUDIO_MAXIMO-Table 1.csv")
Respuestas_finales$N_ESTUDIO_MAXIMO<- N_ESTUDIO_MAXIMO$DESCRIPCIÓN[match(Respuestas_finales$N_ESTUDIO_MAXIMO,
                                                                         N_ESTUDIO_MAXIMO$CODIGO)]
Respuestas_finales$N_ESTUDIO_MAXIMO <- as.factor(Respuestas_finales$N_ESTUDIO_MAXIMO)
levels(Respuestas_finales$N_ESTUDIO_MAXIMO)
Respuestas_finales$N_ESTUDIO_MAXIMO <- factor(Respuestas_finales$N_ESTUDIO_MAXIMO,
                                              levels = c("Primaria","Básicos","Diversificado",
                                                         "Técnico","Licenciatura","Maestria",
                                                         "Postgrado"))
rm(N_ESTUDIO_MAXIMO)

#Variable: N_PROGRAMA_SOCIAL (Programa social al que pertenece el respondente)
N_PROGRAMA_SOCIAL <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/N_PROGRAMA_SOCIAL-Table 1.csv")
Respuestas_finales$N_PROGRAMA_SOCIAL <- N_PROGRAMA_SOCIAL$DESCRIPCIÓN[match(Respuestas_finales$N_PROGRAMA_SOCIAL,
                                                                            N_PROGRAMA_SOCIAL$CODIGO)]
Respuestas_finales$N_PROGRAMA_SOCIAL <- as.factor(Respuestas_finales$N_PROGRAMA_SOCIAL)
table(Respuestas_finales$N_PROGRAMA_SOCIAL)
rm(N_PROGRAMA_SOCIAL)

#Variable: N_TIEMPO_PROGRAMA
N_TIEMPO_PROGRAMA <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/N_TIEMPO_PROGRAMA-Table 1.csv")
Respuestas_finales$N_TIEMPO_PROGRAMA <- N_TIEMPO_PROGRAMA$DESCRIPCIÓN[match(Respuestas_finales$N_TIEMPO_PROGRAMA,
                                                                            N_TIEMPO_PROGRAMA$CODIGO)]
Respuestas_finales$N_TIEMPO_PROGRAMA <- as.factor(Respuestas_finales$N_TIEMPO_PROGRAMA)
Respuestas_finales$N_TIEMPO_PROGRAMA <- factor(Respuestas_finales$N_TIEMPO_PROGRAMA,
                                               levels = c("6 meses o menos","Entre 6 meses y dos años",
                                                          "Entre 2 y 4 años","Entre 4 y 10 años",
                                                          "Más de 10 años"))
table(Respuestas_finales$N_TIEMPO_PROGRAMA)
rm(N_TIEMPO_PROGRAMA)

#Variable: N_EDAD_DIRIGIDO_DISENIO
N_EDAD_DIRIGIDO_DISENIO <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/N_EDAD_DIRIGIDO_DISENIO-Table 1.csv")
Respuestas_finales$N_EDAD_DIRIGIDO_DISENIO <- N_EDAD_DIRIGIDO_DISENIO$DESCRIPCIÓN[match(Respuestas_finales$N_EDAD_DIRIGIDO_DISENIO,
                                                                                        N_EDAD_DIRIGIDO_DISENIO$CODIGO)]
Respuestas_finales$N_EDAD_DIRIGIDO_DISENIO <- as.factor(Respuestas_finales$N_EDAD_DIRIGIDO_DISENIO)
table(Respuestas_finales$N_EDAD_DIRIGIDO_DISENIO)
Respuestas_finales$N_EDAD_DIRIGIDO_DISENIO <- factor(Respuestas_finales$N_EDAD_DIRIGIDO_DISENIO,
                                                     levels = c("Niños y niñas (hasta los 14 años de edad)",
                                                                "Jóvenes (entre 15 y 30 años de edad)",
                                                                "Adultos (entre 31 y 60 años de edad)",
                                                                "Adultos mayores (mayores de 60 años de edad)",
                                                                "Todas las edades"))
rm(N_EDAD_DIRIGIDO_DISENIO)

#Variable: N_GENERO_DIRIGIDO_DISENIO
N_GENERO_DIRIGIDO_DISENIO <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/N_GENERO_DIRIGIDO_DISENIO-Table 1.csv")
Respuestas_finales$N_GENERO_DIRIGIDO_DISENIO <- N_GENERO_DIRIGIDO_DISENIO$DESCRIPCIÓN[match(Respuestas_finales$N_GENERO_DIRIGIDO_DISENIO,
                                                                                            N_GENERO_DIRIGIDO_DISENIO$CODIGO)]
Respuestas_finales$N_GENERO_DIRIGIDO_DISENIO <- as.factor(Respuestas_finales$N_GENERO_DIRIGIDO_DISENIO)
table(Respuestas_finales$N_GENERO_DIRIGIDO_DISENIO)
rm(N_GENERO_DIRIGIDO_DISENIO)

#Variable: N_EDAD_ATIENDE
N_EDAD_ATIENDE <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/N_EDAD_ATIENDE-Table 1.csv")
Respuestas_finales$N_EDAD_ATIENDE <- N_EDAD_ATIENDE$DESCRIPCIÓN[match(Respuestas_finales$N_EDAD_ATIENDE,
                                                                      N_EDAD_ATIENDE$CODIGO)]
Respuestas_finales$N_EDAD_ATIENDE <- as.factor(Respuestas_finales$N_EDAD_ATIENDE)
table(Respuestas_finales$N_EDAD_ATIENDE)
Respuestas_finales$N_EDAD_ATIENDE <- factor(Respuestas_finales$N_EDAD_ATIENDE,
                                                     levels = c("Niños y niñas (hasta los 14 años de edad)",
                                                                "Jóvenes (entre 15 y 30 años de edad)",
                                                                "Adultos (entre 31 y 60 años de edad)",
                                                                "Adultos mayores (mayores de 60 años de edad)",
                                                                "Todas las edades"))
rm(N_EDAD_ATIENDE)

#Variable: N_GENERO_ATIENDE
N_GENERO_ATIENDE <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/N_GENERO_ATIENDE-Table 1.csv")
Respuestas_finales$N_GENERO_ATIENDE <- N_GENERO_ATIENDE$DESCRIPCIÓN[match(Respuestas_finales$N_GENERO_ATIENDE,
                                                                          N_GENERO_ATIENDE$CODIGO)]
Respuestas_finales$N_GENERO_ATIENDE <- as.factor(Respuestas_finales$N_GENERO_ATIENDE)
table(Respuestas_finales$N_GENERO_ATIENDE)
rm(N_GENERO_ATIENDE)

#Variable: N_PROBLEMA_SOC (1,2 y 3)
N_PROBLEMA_SOC <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/N_PROBLEMA_SOC_1-Table 1.csv")
Respuestas_finales$N_PROBLEMA_SOC_1 <- N_PROBLEMA_SOC$DESCRIPCIÓN[match(Respuestas_finales$N_PROBLEMA_SOC_1,
                                                                        N_PROBLEMA_SOC$CODIGO)]
Respuestas_finales$N_PROBLEMA_SOC_2 <- N_PROBLEMA_SOC$DESCRIPCIÓN[match(Respuestas_finales$N_PROBLEMA_SOC_2,
                                                                        N_PROBLEMA_SOC$CODIGO)]
Respuestas_finales$N_PROBLEMA_SOC_3 <- N_PROBLEMA_SOC$DESCRIPCIÓN[match(Respuestas_finales$N_PROBLEMA_SOC_3,
                                                                        N_PROBLEMA_SOC$CODIGO)]
Respuestas_finales$N_PROBLEMA_SOC_1 <- as.factor(Respuestas_finales$N_PROBLEMA_SOC_1)
Respuestas_finales$N_PROBLEMA_SOC_2 <- as.factor(Respuestas_finales$N_PROBLEMA_SOC_2)
Respuestas_finales$N_PROBLEMA_SOC_3 <- as.factor(Respuestas_finales$N_PROBLEMA_SOC_3)
rm(N_PROBLEMA_SOC)

#Variable: N_RESPUESTA_BASA
N_RESPUESTA_BASA <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/N_RESPUESTA_BASA-Table 1.csv")
Respuestas_finales$N_RESPUESTA_BASA <- N_RESPUESTA_BASA$DESCRIPCIÓN[match(Respuestas_finales$N_RESPUESTA_BASA,
                                                                          N_RESPUESTA_BASA$CODIGO)]
table(Respuestas_finales$N_RESPUESTA_BASA)
Respuestas_finales$N_RESPUESTA_BASA <- as.factor(Respuestas_finales$N_RESPUESTA_BASA)
class(Respuestas_finales$N_RESPUESTA_BASA)
rm(N_RESPUESTA_BASA)

#Variable: fecha de nacimiento (edad)
Fechas_nacimiento <- 
        read.csv("~/R Projects/diagnostico_social_ds/Datos/Fechas_nacimiento.csv", header=FALSE)
#Limpieza de cuadro
Fechas_nacimiento <- Fechas_nacimiento[-1,]
Fechas_nacimiento$V3 <- NULL
Fechas_nacimiento$V4 <- NULL
Fechas_nacimiento$V5 <- NULL
Fechas_nacimiento$V6 <- NULL
Fechas_nacimiento$V7 <- NULL
#Renombrar variables y crear edad
Fechas_nacimiento <- rename(Fechas_nacimiento,
       CUI=V1)
Fechas_nacimiento <- rename(Fechas_nacimiento,
                            FechaNac=V2)
Fechas_nacimiento$Edad2021 <- NA
#Definir la variable para crear edad
class(Fechas_nacimiento$FechaNac)
Fechas_nacimiento$FechaNac <- as.Date(Fechas_nacimiento$FechaNac)
#Crear edad en años
Fechas_nacimiento <- subset.data.frame(Fechas_nacimiento,
                  !is.na(Fechas_nacimiento$FechaNac))
Fechas_nacimiento$Edad2021 <- age_calc(dob = Fechas_nacimiento$FechaNac,
         enddate = Sys.Date(),
         units = "years")
Fechas_nacimiento$Edad2021 <- round(Fechas_nacimiento$Edad2021,
      digits = 0)
#Integrar a la base de datos general
Respuestas_finales$EDAD_RESPONDENTE <- NA
Respuestas_finales$EDAD_RESPONDENTE<- Fechas_nacimiento$Edad2021[match(Respuestas_finales$V_DPI_CUI_185,Fechas_nacimiento$CUI)]
Respuestas_finales$EDAD_RESPONDENTE[Respuestas_finales$EDAD_RESPONDENTE==1] <- 59
rm(Fechas_nacimiento)

#Variable: N_PUESTO
N_PUESTO <- read.csv("~/R Projects/diagnostico_social_ds/Datos/N_PUESTO-Table 1.csv")
Respuestas_finales$N_PUESTO <- N_PUESTO$DESCRIPCIÓN[match(Respuestas_finales$N_PUESTO,
                                                          N_PUESTO$CODIGO)]
table(Respuestas_finales$N_PUESTO)
#Corregir los errores basicos
grep("¿",Respuestas_finales$N_PUESTO)
grep("MAESTRO DE COMPUTACI¿N",Respuestas_finales$N_PUESTO) 
Respuestas_finales$N_PUESTO[grep("MAESTRO DE COMPUTACI¿N",Respuestas_finales$N_PUESTO)] <- 
  "MAESTRO DE COMPUTACION"
Respuestas_finales$N_PUESTO[grep("INSTRUCTORA DE CORTE Y CONFECCI¿N",Respuestas_finales$N_PUESTO)] <- 
  "INSTRUCTORA DE CORTE Y CONFECCION"
Respuestas_finales$N_PUESTO[grep("MANTENIMIENTO Y LIMPIEZA EN CENTROS DE CAPACITACI¿N",Respuestas_finales$N_PUESTO)] <- 
  "MANTENIMIENTO Y LIMPIEZA EN CENTROS DE CAPACITACION"
Respuestas_finales$N_PUESTO[grep("PROFESORA DE COMPUTACI¿N/BIBLIOTECAS",Respuestas_finales$N_PUESTO)] <- 
  "PROFESORA DE COMPUTACION DE BIBLIOTECAS"
Respuestas_finales$N_PUESTO[grep("MONITOR DE LECTURA Y ACTIVIDADES L¿DICAS",Respuestas_finales$N_PUESTO)] <- 
  "MONITOR DE LECTURA Y ACTIVIDADES LUDICAS"
rm(N_PUESTO)

#Correccion de programa
Respuestas_finales$N_PROGRAMA_SOCIAL <- as.character(Respuestas_finales$N_PROGRAMA_SOCIAL)
Respuestas_finales$N_PROGRAMA_SOCIAL[grep("EMEFUT",Respuestas_finales$N_PUESTO)] <- "EMEFUT"
Respuestas_finales$N_PROGRAMA_SOCIAL[Respuestas_finales$N_PROGRAMA_SOCIAL=="Comunidad Juvenil"] <- "EMEFUT"
Respuestas_finales$N_PROGRAMA_SOCIAL <- as.factor(Respuestas_finales$N_PROGRAMA_SOCIAL)
class(Respuestas_finales$N_PROGRAMA_SOCIAL)
table(Respuestas_finales$N_PROGRAMA_SOCIAL)
