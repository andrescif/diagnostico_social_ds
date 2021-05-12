library(tidyverse)
library(caret)
library(quanteda)
library(tm)
library(SnowballC)
library(wordcloud)
library(janitor)
library(stringi)
#------------------ANANLISIS POR PROGRAMA SOIAL--------------------------------------------#

#-----------------------------------Adulto mayor-------------------------------------------#
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

#Principal objetivo del programa (ANALISIS DE TEXTO)
        #Tokenizar
token_objetivoprograma_am <- tokens(Adulto_mayor_respuestas$V_PRINCIPAL_OBJ_PROGRAMA,
                                    what = "word",remove_punct = TRUE,
                                    remove_symbols = TRUE)
        #Hacer minusculas todo el texto
token_objetivoprograma_am <- tokens_tolower(token_objetivoprograma_am)
        #Remover los articulos
token_objetivoprograma_am <- tokens_select(token_objetivoprograma_am,
                                           stopwords(language = "es"),selection ="remove")
        #Paso recomendado pero NO usado, STEM, dejar en la base las palabras
tokens_wordstem(token_objetivoprograma_am, language = "es")
        #Crear objeto que contenga frecuencias de palabras
token_objetivoprograma_am_dfm <- dfm(token_objetivoprograma_am, tolower = FALSE)
dim(token_objetivoprograma_am_dfm)
        #Nube de palabras del objetivo del programa
programa_objetivo_am <- as.data.frame(token_objetivoprograma_am_dfm)
        #Sumar las columnas
programa_objetivo_am <- adorn_totals(programa_objetivo_am,where = "row")
y <- programa_objetivo_am[9,-1]
        #Nube de palabras
wordcloud(variable.names(y),y,min.freq = 2)
rm(x)
#¿Cómo sabe el objetivo principal?
table(Adulto_mayor_respuestas$N_RESPUESTA_BASA)
#Acciones principales
        #Juntar todas las respuestas
accion_principal_am <- Adulto_mayor_respuestas %>%
        select("V_ACCION_PRINCIPAL_1","V_ACCION_PRINCIPAL_2","V_ACCION_PRINCIPAL_3")%>%
        gather()
        #Limpiar las resputas
accion_principal_am$key <- NULL
colnames(accion_principal_am)[1] <- "Respuestas"
                #Minusculas
accion_principal_am$Respuestas <- tolower(accion_principal_am$Respuestas)
                #Remover puntuacion
accion_principal_am$Respuestas <- str_replace_all(accion_principal_am$Respuestas,"[[:punct:]]","")
                #Remover acentos
accion_principal_am$Respuestas <- stri_trans_general(accion_principal_am$Respuestas,"Latin-ASCII")
                #Remover espacios
accion_principal_am$Respuestas <- trimws(accion_principal_am$Respuestas,which = "both")


table(accion_principal_am$Respuestas)



