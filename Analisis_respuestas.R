library(tidyverse)
library(caret)
library(quanteda)
library(SnowballC)
library(wordcloud)
library(janitor)
library(stringi)
#------------------ANANLISIS POR PROGRAMA SOIAL--------------------------------------------#

#-----------------------------------Adulto mayor-------------------------------------------
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
        #Limpiar las respuestas
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
                #Estandarizacion basica
accion_principal_am$Respuestas <- gsub("contribuir al bienestar emocional a traves de las charlas de crecimiento personal",
     "charlas de crecimiento personal",accion_principal_am$Respuestas)
accion_principal_am$Respuestas <- gsub("ritmica ritmica","ritmica",accion_principal_am$Respuestas)
x <- as.data.frame(table(accion_principal_am$Respuestas))
y <- as.data.frame(prop.table(table(accion_principal_am$Respuestas)))
x$prop <- y$Freq
write.csv(x,file = "Tabla de respuestas Adulto Mayor 3 principales acciones")
rm(x)
rm(y)
rm(accion_principal_am)
rm(programa_objetivo_am)
rm(token_objetivoprograma_am)
rm(token_objetivoprograma_am_dfm)
#Explica relacion entre acciones principales y problemas del adulto mayor
        #Tokenizar
token_relacion_accion_objetivo_am <- tokens(Adulto_mayor_respuestas$V_EXPLICA_RELACION,
                                            what = "word",remove_punct = TRUE,
                                            remove_symbols = TRUE)
        #Hacer minusculas
token_relacion_accion_objetivo_am <- tokens_tolower(token_relacion_accion_objetivo_am)
        #Remover los articulos
token_relacion_accion_objetivo_am <- tokens_select(token_relacion_accion_objetivo_am,
                                                   stopwords(language = "es"),selection ="remove")
        #Crear dfm
token_relacion_accion_objetivo_am_dfm <- dfm(token_relacion_accion_objetivo_am,tolower = FALSE)
        #Crear base de datos
relacion_accion_objetivo_am <- as.data.frame(token_relacion_accion_objetivo_am_dfm)
        #Sumar uso de palabras
relacion_accion_objetivo_am <- adorn_totals(relacion_accion_objetivo_am,where = "row")
        #Wordcloud
y <- relacion_accion_objetivo_am[9,-1]
wordcloud(variable.names(y),y,min.freq = 2)
rm(y)
rm(token_relacion_accion_objetivo_am)
rm(token_relacion_accion_objetivo_am_dfm)
rm(relacion_accion_objetivo_am)
#Calificación de la coordinadora
summary(Adulto_mayor_respuestas$N_CALIFICACION_CORDI)
#Tablas finales para análisis cualitativo
x<- table(Adulto_mayor_respuestas$V_PRINCIPAL_OBJ_PROGRAMA)
write.csv(x,
          file = "Tabla de respuesta a objetivo final adulto mayor")
rm(x)
x <- table(Adulto_mayor_respuestas$V_EXPLICA_RELACION)
write.csv(x,
          file = "Tabla de respuestas de relacion objetivo accion adulto mayor")
rm(x)
x <- table(Adulto_mayor_respuestas$V_CAMBIOS_PROGRAMA)
write.csv(x,
          file = "Tabla de respuestas de cambios sugeridos al programa adulto mayor")
rm(x)
rm(Adulto_mayor_respuestas)
#--------------------------------Bibliotecas Municipales----------------------------------
levels(Respuestas_finales$N_PROGRAMA_SOCIAL)
Bibliotecas_municipales_respuestas <- subset.data.frame(Respuestas_finales,
                                             Respuestas_finales$N_PROGRAMA_SOCIAL=="Bibliotecas Municipales")
#Caracterizacion del personal
Tabla_bibliotecas_municipales_personal<- select(Bibliotecas_municipales_respuestas,
                                     c("N_PUESTO","EDAD_RESPONDENTE","N_ESTUDIO_MAXIMO","N_TIEMPO_PROGRAMA"))
write.csv(Tabla_bibliotecas_municipales_personal,
          file = "Personal bibliotecas municipales")

summary(Tabla_bibliotecas_municipales_personal$EDAD_RESPONDENTE)
        #Graficos de relaciones entre variables
Tabla_bibliotecas_municipales_personal %>%
        ggplot(aes(N_ESTUDIO_MAXIMO,fill= N_TIEMPO_PROGRAMA))+
        theme_bw()+
        geom_bar()+
        guides(fill=guide_legend(title="Tiempo en el Programa"))+
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
        xlab("Máximo nivel educativo")+
        ylab("")+
        ggtitle("Nivel Educativo y Tiempo en el Programa de Bibliotecas")+
        scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16))

Tabla_bibliotecas_municipales_personal %>%
        ggplot(aes(N_ESTUDIO_MAXIMO,EDAD_RESPONDENTE))+
        theme_bw()+
        geom_boxplot()+
        ylim(c(0,75))

Tabla_bibliotecas_municipales_personal %>%
        ggplot(aes(N_TIEMPO_PROGRAMA,EDAD_RESPONDENTE))+
        theme_bw()+
        geom_boxplot()+
        ylim(c(0,75))

Tabla_bibliotecas_municipales_personal %>%
        ggplot(aes(N_PUESTO ,EDAD_RESPONDENTE))+
        theme_bw()+
        geom_boxplot()+
        ylim(c(0,75))+
        theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

Bibliotecas_municipales_respuestas %>%
        ggplot(aes(N_PROGRAMA_SOCIAL,EDAD_RESPONDENTE,color=N_PUESTO))+
        theme_bw()+
        geom_jitter(stroke=3)+
        guides(color=guide_legend(title="Puesto"))
rm(Tabla_adulto_mayor_personal)

#Enfoque del programa
        #Edad
table(Bibliotecas_municipales_respuestas$N_EDAD_DIRIGIDO_DISENIO)
table(Bibliotecas_municipales_respuestas$N_EDAD_ATIENDE)
prop.table(table(Bibliotecas_municipales_respuestas$N_EDAD_DIRIGIDO_DISENIO))
prop.table(table(Bibliotecas_municipales_respuestas$N_EDAD_ATIENDE))
                #Hacerlo tablas
x <- as.data.frame(table(Bibliotecas_municipales_respuestas$N_EDAD_DIRIGIDO_DISENIO))
colnames(x) <- c("Respuesta","Diseno")
x1 <- as.data.frame(prop.table(table(Bibliotecas_municipales_respuestas$N_EDAD_DIRIGIDO_DISENIO)))
y <- as.data.frame(table(Bibliotecas_municipales_respuestas$N_EDAD_ATIENDE))
y2 <- as.data.frame(prop.table(table(Bibliotecas_municipales_respuestas$N_EDAD_ATIENDE)))
x$Prop.diseno <- x1$Freq
x$Atencion <- y$Freq
x$Pop.atencion <- y2$Freq
write.csv(x,
          file = "Tabla de respuestas enfoque de edad bibliotecas municipales")
rm(x)   
rm(x1)
rm(y)
rm(y2)
        #Género
table(Bibliotecas_municipales_respuestas$N_GENERO_DIRIGIDO_DISENIO)
prop.table(table(Bibliotecas_municipales_respuestas$N_GENERO_DIRIGIDO_DISENIO))
table(Bibliotecas_municipales_respuestas$N_GENERO_ATIENDE)
prop.table(table(Bibliotecas_municipales_respuestas$N_GENERO_ATIENDE))
                #Hacerlo tablas
x <- as.data.frame(table(Bibliotecas_municipales_respuestas$N_GENERO_DIRIGIDO_DISENIO))
x1 <- as.data.frame(prop.table(table(Bibliotecas_municipales_respuestas$N_GENERO_DIRIGIDO_DISENIO)))
y <- as.data.frame(table(Bibliotecas_municipales_respuestas$N_GENERO_ATIENDE))
y1 <- as.data.frame(prop.table(table(Bibliotecas_municipales_respuestas$N_GENERO_ATIENDE)))
colnames(x) <- c("Respuesta","Diseno")
x$Prop.diseno <- x1$Freq
x$Atencion <- y$Freq
x$Prop.atencion <- y1$Freq
write.csv(x,
          file = "Tabla de respuestas enfoque de genero bibliotecas municipales")
rm(x)
rm(x1)
rm(y)
rm(y1)

Problemas_soc_bm <- Bibliotecas_municipales_respuestas %>%
        select("N_PROBLEMA_SOC_1","N_PROBLEMA_SOC_2","N_PROBLEMA_SOC_3")%>%
        gather()
x<- as.data.frame(table(Problemas_soc_bm$value))
y <- as.data.frame(prop.table(table(Problemas_soc_bm$value)))
x$Prop <- y$Freq
write.csv(x,
          file = "Tabla de problemas sociales usuarios de bibliotecas municipales")
x %>%
        ggplot(aes(Var1,Freq))+
        theme_bw()+
        geom_col()+
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
rm(x)
rm(y)
rm(Problemas_soc_bm)
rm(Tabla_bibliotecas_municipales_personal)

#Principal objetivo del programa (ANALISIS DE TEXTO)
        #Tokenizar
token_objetivoprograma_bm <- tokens(Bibliotecas_municipales_respuestas$V_PRINCIPAL_OBJ_PROGRAMA,
                                    what = "word",remove_punct = TRUE,
                                    remove_symbols = TRUE)
        #Hacer minusculas todo el texto
token_objetivoprograma_bm <- tokens_tolower(token_objetivoprograma_bm)
        #Remover los articulos
token_objetivoprograma_bm <- tokens_select(token_objetivoprograma_bm,
                                           stopwords(language = "es"),selection ="remove")
        #Paso recomendado pero NO usado, STEM, dejar en la base las palabras
tokens_wordstem(token_objetivoprograma_bm, language = "es")
        #Crear objeto que contenga frecuencias de palabras
token_objetivoprograma_bm_dfm <- dfm(token_objetivoprograma_bm, tolower = FALSE)
dim(token_objetivoprograma_bm_dfm)
        #Nube de palabras del objetivo del programa
programa_objetivo_bm <- as.data.frame(token_objetivoprograma_bm_dfm)
        #Sumar las columnas
programa_objetivo_bm <- adorn_totals(programa_objetivo_bm,where = "row")
y <- programa_objetivo_bm[26,-1]
        #Nube de palabras
wordcloud(variable.names(y),y,min.freq = 2)
rm(y)
        #¿Cómo sabe el objetivo principal?
x <- as.data.frame(table(Bibliotecas_municipales_respuestas$N_RESPUESTA_BASA))
y <- as.data.frame(prop.table(table(Bibliotecas_municipales_respuestas$N_RESPUESTA_BASA)))
x$Prop.freq <- y$Freq
write.csv(x,
          file = "Tabla de conocimiento del objetivo principal del programa 
          bibliotecas municipales")
rm(token_objetivoprograma_bm)
rm(token_objetivoprograma_bm_dfm)
rm(programa_objetivo_bm)
rm(x)
rm(y)
#Acciones principales
        #Juntar todas las respuestas
accion_principal_bm <- Bibliotecas_municipales_respuestas %>%
        select("V_ACCION_PRINCIPAL_1","V_ACCION_PRINCIPAL_2","V_ACCION_PRINCIPAL_3")%>%
        gather()
        #Limpiar las respuestas
accion_principal_bm$key <- NULL
colnames(accion_principal_bm)[1] <- "Respuestas"
        #Minusculas
accion_principal_bm$Respuestas <- tolower(accion_principal_bm$Respuestas)
        #Remover puntuacion
accion_principal_bm$Respuestas <- str_replace_all(accion_principal_bm$Respuestas,"[[:punct:]]","")
        #Remover acentos
accion_principal_bm$Respuestas <- stri_trans_general(accion_principal_bm$Respuestas,"Latin-ASCII")
        #Remover espacios
accion_principal_bm$Respuestas <- trimws(accion_principal_bm$Respuestas,which = "both")
        #Hacer una tabla
write.csv(accion_principal_bm,file = "Tabla de respuestas Bibliotecas Municipales 3 principales acciones")
rm(accion_principal_bm)
#Explica relacion entre acciones principales y problemas del adulto mayor
#Tokenizar
token_relacion_accion_objetivo_bm <- tokens(Bibliotecas_municipales_respuestas$V_EXPLICA_RELACION,
                                            what = "word",remove_punct = TRUE,
                                            remove_symbols = TRUE)
#Hacer minusculas
token_relacion_accion_objetivo_bm <- tokens_tolower(token_relacion_accion_objetivo_bm)
#Remover los articulos
token_relacion_accion_objetivo_bm <- tokens_select(token_relacion_accion_objetivo_bm,
                                                   stopwords(language = "es"),selection ="remove")
#Crear dfm
token_relacion_accion_objetivo_bm_dfm <- dfm(token_relacion_accion_objetivo_bm,tolower = FALSE)
#Crear base de datos
relacion_accion_objetivo_bm <- as.data.frame(token_relacion_accion_objetivo_bm_dfm)
#Sumar uso de palabras
relacion_accion_objetivo_bm <- adorn_totals(relacion_accion_objetivo_bm,where = "row")
#Wordcloud
y <- relacion_accion_objetivo_bm[26,-1]
wordcloud(variable.names(y),y,min.freq = 3)
rm(y)
rm(token_relacion_accion_objetivo_bm)
rm(token_relacion_accion_objetivo_bm_dfm)
rm(relacion_accion_objetivo_bm)
#Calificación de la coordinadora
summary(Bibliotecas_municipales_respuestas$N_CALIFICACION_CORDI)
#Tablas finales para análisis cualitativo
x<- table(Bibliotecas_municipales_respuestas$V_PRINCIPAL_OBJ_PROGRAMA)
write.csv(x,
          file = "Tabla de respuesta a objetivo final bibliotecas municipales")
rm(x)
x <- table(Bibliotecas_municipales_respuestas$V_EXPLICA_RELACION)
write.csv(x,
          file = "Tabla de respuestas de relacion objetivo accion bibliotecas municipales")
rm(x)
x <- table(Bibliotecas_municipales_respuestas$V_CAMBIOS_PROGRAMA)
write.csv(x,
          file = "Tabla de respuestas de cambios sugeridos al programa bibliotecas municipales")
rm(x)
rm(Bibliotecas_municipales_respuestas)

#-----------------------------------------EMEFUT------------------------------------------
levels(Respuestas_finales$N_PROGRAMA_SOCIAL)
EMEFUT_respuestas <- subset.data.frame(Respuestas_finales,
                                                        Respuestas_finales$N_PROGRAMA_SOCIAL=="EMEFUT")
#Caracterizacion del personal
Tabla_personal_EMEFUT <- select(EMEFUT_respuestas,
                                                c("N_PUESTO","EDAD_RESPONDENTE","N_ESTUDIO_MAXIMO","N_TIEMPO_PROGRAMA"))
write.csv(Tabla_personal_EMEFUT,
          file = "Personal EMEFUT")

summary(Tabla_personal_EMEFUT$EDAD_RESPONDENTE)
#Graficos de relaciones entre variables
Tabla_personal_EMEFUT %>%
        ggplot(aes(N_ESTUDIO_MAXIMO,fill= N_TIEMPO_PROGRAMA))+
        theme_bw()+
        geom_bar()+
        guides(fill=guide_legend(title="Tiempo en el Programa"))+
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
        xlab("Máximo nivel educativo")+
        ylab("Cantidad de profesores")+
        ggtitle("Nivel Educativo y Tiempo en el Programa EMEFUT")+
        scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30))

Tabla_personal_EMEFUT %>%
        ggplot(aes(N_ESTUDIO_MAXIMO,EDAD_RESPONDENTE))+
        theme_bw()+
        geom_boxplot()+
        ylim(c(0,75))

Tabla_personal_EMEFUT %>%
        ggplot(aes(N_TIEMPO_PROGRAMA,EDAD_RESPONDENTE))+
        theme_bw()+
        geom_boxplot()+
        ylim(c(0,75))+
        xlab("Tiempo del profesor en EMEFUT")+
        ylab("Edad del profesor")+
        ggtitle("Distribución de Edad de Profesores EMEFUT por Nivel Educativo")

Tabla_personal_EMEFUT %>%
        ggplot(aes(N_PUESTO ,EDAD_RESPONDENTE))+
        theme_bw()+
        geom_boxplot()+
        ylim(c(0,75))+
        theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

EMEFUT_respuestas %>%
        ggplot(aes(N_PROGRAMA_SOCIAL,EDAD_RESPONDENTE,color=N_PUESTO))+
        theme_bw()+
        geom_jitter(stroke=3)+
        guides(color=guide_legend(title="Puesto"))
rm(Tabla_adulto_mayor_personal)

#Enfoque del programa
#Edad
table(EMEFUT_respuestas$N_EDAD_DIRIGIDO_DISENIO)
table(EMEFUT_respuestas$N_EDAD_ATIENDE)
prop.table(table(EMEFUT_respuestas$N_EDAD_DIRIGIDO_DISENIO))
prop.table(table(EMEFUT_respuestas$N_EDAD_ATIENDE))
#Hacerlo tablas
x <- as.data.frame(table(EMEFUT_respuestas$N_EDAD_DIRIGIDO_DISENIO))
colnames(x) <- c("Respuesta","Diseno")
x1 <- as.data.frame(prop.table(table(EMEFUT_respuestas$N_EDAD_DIRIGIDO_DISENIO)))
y <- as.data.frame(table(EMEFUT_respuestas$N_EDAD_ATIENDE))
y2 <- as.data.frame(prop.table(table(EMEFUT_respuestas$N_EDAD_ATIENDE)))
x$Prop.diseno <- x1$Freq
x$Atencion <- y$Freq
x$Pop.atencion <- y2$Freq
write.csv(x,
          file = "Tabla de respuestas enfoque de edad EMEFUT")
rm(x)   
rm(x1)
rm(y)
rm(y2)
#Género
table(EMEFUT_respuestas$N_GENERO_DIRIGIDO_DISENIO)
prop.table(table(EMEFUT_respuestas$N_GENERO_DIRIGIDO_DISENIO))
table(EMEFUT_respuestas$N_GENERO_ATIENDE)
prop.table(table(EMEFUT_respuestas$N_GENERO_ATIENDE))
#Hacerlo tablas
x <- as.data.frame(table(EMEFUT_respuestas$N_GENERO_DIRIGIDO_DISENIO))
x1 <- as.data.frame(prop.table(table(EMEFUT_respuestas$N_GENERO_DIRIGIDO_DISENIO)))
y <- as.data.frame(table(EMEFUT_respuestas$N_GENERO_ATIENDE))
y1 <- as.data.frame(prop.table(table(EMEFUT_respuestas$N_GENERO_ATIENDE)))
colnames(x) <- c("Respuesta","Diseno")
x$Prop.diseno <- x1$Freq
x$Atencion <- y$Freq
x$Prop.atencion <- y1$Freq
write.csv(x,
          file = "Tabla de respuestas enfoque de genero EMEFUT")
rm(x)
rm(x1)
rm(y)
rm(y1)
#Problema social principal
Problemas_soc_EMEFUT <- EMEFUT_respuestas %>%
        select("N_PROBLEMA_SOC_1","N_PROBLEMA_SOC_2","N_PROBLEMA_SOC_3")%>%
        gather()
x<- as.data.frame(table(Problemas_soc_EMEFUT$value))
y <- as.data.frame(prop.table(table(Problemas_soc_EMEFUT$value)))
x$Prop <- y$Freq
write.csv(x,
          file = "Tabla de problemas sociales usuarios de EMEFUT")
x %>%
        ggplot(aes(Var1,Freq))+
        theme_bw()+
        geom_col()+
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
rm(x)
rm(y)
rm(Problemas_soc_EMEFUT)
rm(Tabla_personal_EMEFUT)

#Principal objetivo del programa (ANALISIS DE TEXTO)
#Tokenizar
token_objetivoprograma_emefut <- tokens(EMEFUT_respuestas$V_PRINCIPAL_OBJ_PROGRAMA,
                                    what = "word",remove_punct = TRUE,
                                    remove_symbols = TRUE)
#Hacer minusculas todo el texto
token_objetivoprograma_emefut <- tokens_tolower(token_objetivoprograma_emefut)
#Remover los articulos
token_objetivoprograma_emefut <- tokens_select(token_objetivoprograma_emefut,
                                           stopwords(language = "es"),selection ="remove")
#Paso recomendado pero NO usado, STEM, dejar en la base las palabras
tokens_wordstem(token_objetivoprograma_emefut, language = "es")
#Crear objeto que contenga frecuencias de palabras
token_objetivoprograma_emefut_dfm <- dfm(token_objetivoprograma_emefut, tolower = FALSE)
dim(token_objetivoprograma_emefut_dfm)
#Nube de palabras del objetivo del programa
programa_objetivo_emefut <- convert(token_objetivoprograma_emefut_dfm,to="data.frame")
#Sumar las columnas
programa_objetivo_emefut <- adorn_totals(programa_objetivo_emefut,where = "row")
y <- programa_objetivo_emefut[57,-1]
#Nube de palabras
wordcloud(variable.names(y),y,min.freq = 3,fixed.asp = TRUE)
rm(y)
#¿Cómo sabe el objetivo principal?
x <- as.data.frame(table(EMEFUT_respuestas$N_RESPUESTA_BASA))
y <- as.data.frame(prop.table(table(EMEFUT_respuestas$N_RESPUESTA_BASA)))
x$Prop.freq <- y$Freq
write.csv(x,
          file = "Tabla de conocimiento del objetivo principal del programa EMEFUT")
rm(token_objetivoprograma_emefut)
rm(token_objetivoprograma_emefut_dfm)
rm(programa_objetivo_bm)
rm(x)
rm(y)
#Acciones principales
#Juntar todas las respuestas
accion_principal_emefut <- EMEFUT_respuestas %>%
        select("V_ACCION_PRINCIPAL_1","V_ACCION_PRINCIPAL_2","V_ACCION_PRINCIPAL_3")%>%
        gather()
#Limpiar las respuestas
accion_principal_emefut$key <- NULL
colnames(accion_principal_emefut)[1] <- "Respuestas"
#Minusculas
accion_principal_emefut$Respuestas <- tolower(accion_principal_emefut$Respuestas)
#Remover puntuacion
accion_principal_emefut$Respuestas <- str_replace_all(accion_principal_emefut$Respuestas,"[[:punct:]]","")
#Remover acentos
accion_principal_emefut$Respuestas <- stri_trans_general(accion_principal_emefut$Respuestas,"Latin-ASCII")
#Remover espacios
accion_principal_emefut$Respuestas <- trimws(accion_principal_emefut$Respuestas,which = "both")
#Hacer una tabla
write.csv(accion_principal_emefut,file = "Tabla de respuestas EMEFUT 3 principales acciones")
rm(accion_principal_emefut)
#Explica relacion entre acciones principales y problemas del adulto mayor
#Tokenizar
token_relacion_accion_objetivo_emefut <- tokens(EMEFUT_respuestas$V_EXPLICA_RELACION,
                                            what = "word",remove_punct = TRUE,
                                            remove_symbols = TRUE)
#Hacer minusculas
token_relacion_accion_objetivo_emefut <- tokens_tolower(token_relacion_accion_objetivo_emefut)
#Remover los articulos
token_relacion_accion_objetivo_emefut <- tokens_select(token_relacion_accion_objetivo_emefut,
                                                   stopwords(language = "es"),selection ="remove")
#Crear dfm
token_relacion_accion_objetivo_emefut_dfm <- dfm(token_relacion_accion_objetivo_emefut,tolower = FALSE)
#Crear base de datos
relacion_accion_objetivo_emefut <- as.data.frame(token_relacion_accion_objetivo_emefut_dfm)
#Sumar uso de palabras
relacion_accion_objetivo_emefut <- adorn_totals(relacion_accion_objetivo_emefut,where = "row")
#Wordcloud
y <- relacion_accion_objetivo_emefut[57,-1]
wordcloud(variable.names(y),y,min.freq = 3)
rm(y)
rm(token_relacion_accion_objetivo_emefut)
rm(token_relacion_accion_objetivo_emefut_dfm)
rm(relacion_accion_objetivo_emefut)
#Calificación de la coordinadora
summary(EMEFUT_respuestas$N_CALIFICACION_CORDI)
#Tablas finales para análisis cualitativo
x<- table(EMEFUT_respuestas$V_PRINCIPAL_OBJ_PROGRAMA)
write.csv(x,
          file = "Tabla de respuesta a objetivo final EMEFUT")
rm(x)
x <- table(EMEFUT_respuestas$V_EXPLICA_RELACION)
write.csv(x,
          file = "Tabla de respuestas de relacion objetivo accion EMEFUT")
rm(x)
x <- table(EMEFUT_respuestas$V_CAMBIOS_PROGRAMA)
write.csv(x,
          file = "Tabla de respuestas de cambios sugeridos a EMEFUT")
rm(x)
rm(EMEFUT_respuestas)

#---------------------------------TECNICO PRODUCTIVO---------------------------------------






