
#Versión de R
version
R.version


#Instalando librerias
install.packages("openxlsx")
install.packages("stringr")
install.packages("sqldf")
install.packages('reshape')
install.packages('rtools')

#Invocar librerías
library(stringr)
library(openxlsx)
library (sqldf)
library(stringi)
library(reshape)
library(dplyr)
library(tm)


#Funcion de limpieza de signos de puntuacion
clean.text = function(x)
{ x = gsub("@\\w+", "", x)
x = gsub("[[:punct:]]", "", x)
x = gsub("^ ", "", x)
x = gsub(" $", "", x)}



#---Estableciendo el directorio de trabajo
setwd("C:/Users/merly_000/Desktop/tesis_gian")

#---Importando base de datos
tablon<-read.xlsx("DATOS_COMERCIO.xlsx")

#---Importando fuentes externas
matriz_reniec<-read.xlsx("reniec.xlsx")
matriz_migraciones<-read.xlsx("migra.xlsx")
matriz_sbs<-read.xlsx("sbs.xlsx")


#---Verificando el Tipo de datos del tablón general
head(tablon)
str(tablon)


#---------------------------------------------------------------------#
#                       TRATAMIENTO DE DATOS                          #
#---------------------------------------------------------------------#

#Añadiendo la variable del dia de semana 
tablon$DIA_LLAMADA<- weekdays(as.Date(as.factor(tablon$SEGSTART), format = '%d/%m/%y'))

#Extrayendo hora de inicio de llamada para conocer el turno
tablon$HORA_LLAMADA<- substr(tablon$SEGSTART, 12,13)
tablon$TURNO<-ifelse(as.integer(substr(tablon$HORA_LLAMADA, 1,2))<6,"Madrugada",
                    ifelse(as.integer(substr(tablon$HORA_LLAMADA, 1,2))<12,"Manana",
                           ifelse(as.integer(substr(tablon$HORA_LLAMADA, 1,2))<18,"Tarde", "Noche")))


#Modificando el tipo de dato a integer
tablon$PREGUNTA1<-suppressWarnings(as.integer(tablon$PREGUNTA1))
tablon$IND_DOC<-as.data.frame(grepl("[A-Za-z]", tablon$CEDULA_IDENTIDADE,perl=T))

#Eliminando signos de puntuación 
tablon$CEDULA_IDENTIDADE=clean.text(tablon$CEDULA_IDENTIDADE)


#---Agregando el tipo de dato acorde al número de caracteres
tablon$LARGO<-sapply(sapply(strsplit(as.character(tablon$CEDULA_IDENTIDADE),' '),nchar),sum) 
tablon$TIPO_DOC<-ifelse(tablon$LARGO==11,"RUC",ifelse(tablon$LARGO==8 & tablon$IND_DOC=="FALSE","DNI","SIN DOC"))
tablon$PREGUNTA1<-ifelse(trimws(tablon$PREGUNTA1)=="NINM",0,tablon$PREGUNTA1)
tablon$PREGUNTA1[is.na(tablon$PREGUNTA1)] <- 0
head(tablon)


#pivotendo respuestas por cada mes
df=select ((subset(tablon,tablon$TIPO_DOC=='DNI')),CEDULA_IDENTIDADE,MES,PREGUNTA1)
temp2<-reshape(aggregate(PREGUNTA1~CEDULA_IDENTIDADE+MES,df,min),direction = "wide", idvar = c("CEDULA_IDENTIDADE"), timevar = "MES")

# renombrando nombres de columnas
colnames(temp2)[colnames(temp2)=="PREGUNTA1.ENERO"] <- "ENE"
colnames(temp2)[colnames(temp2)=="PREGUNTA1.FEBRERO"] <- "FEB"
colnames(temp2)[colnames(temp2)=="PREGUNTA1.MARZO"] <- "MAR"
colnames(temp2)[colnames(temp2)=="PREGUNTA1.ABRIL"] <- "ABR"
colnames(temp2)[colnames(temp2)=="PREGUNTA1.MAYO"] <- "MAY"
colnames(temp2)[colnames(temp2)=="PREGUNTA1.JUNIO"] <- "JUN"
colnames(temp2)[colnames(temp2)=="PREGUNTA1.JULIO"] <- "JUL"
colnames(temp2)[colnames(temp2)=="PREGUNTA1.AGOSTO"] <- "AGO"
colnames(temp2)[colnames(temp2)=="PREGUNTA1.SETIEMBRE"] <- "SET"
colnames(temp2)[colnames(temp2)=="PREGUNTA1.OCTUBRE"] <- "OCT"
colnames(temp2)[colnames(temp2)=="PREGUNTA1.NOVIEMBRE"] <- "NOV"
colnames(temp2)[colnames(temp2)=="PREGUNTA1.DICIEMBRE"] <- "DIC"

#Reorganizando los meses
temp3 = subset (temp2, select=c(1,5,6,9,2,10,8,7,3,13,12,11,4))
head(temp3)

# verificando valores de documentos únicos
length(temp3$CEDULA_IDENTIDADE) #22643
length(unique(temp3$CEDULA_IDENTIDADE)) #22643

#Agregando el número de encuestas,máximo,mínimo,ultima realizadas al cliente
temp3$n_encuestas <- 12-rowSums(is.na(temp3))
temp3$max_encuesta <-sapply(apply(temp3[, 2:13],1,na.exclude),max)
temp3$min_encuesta <-sapply(apply(temp3[, 2:13],1,na.exclude),min)
temp3$ultima_encuesta <-apply(temp3[, 2:13], 1, function(x)tail(x[!is.na(x)], 1))


#Agregando el mes recienye encuesta
temp3$mes_ultimaencuesta<-apply(temp3[, 2:13], 1, function(x) names(tail(x[!is.na(x)], 1)))
head(temp3)



#Agregando la duración de tiempo de la llamada
df2=select ((subset(tablon,tablon$TIPO_DOC=='DNI')),CEDULA_IDENTIDADE,MES,DURATION)
duracion<-reshape(aggregate(DURATION~CEDULA_IDENTIDADE+MES,df2,max),direction = "wide", idvar = c("CEDULA_IDENTIDADE"), timevar = "MES")
colnames(duracion)[colnames(duracion)=="DURATION.ENERO"] <- "ENE-DUR"
colnames(duracion)[colnames(duracion)=="DURATION.FEBRERO"] <- "FEBR-DUR"
colnames(duracion)[colnames(duracion)=="DURATION.MARZO"] <- "MARZ-DUR"
colnames(duracion)[colnames(duracion)=="DURATION.ABRIL"] <- "ABR-DUR"
colnames(duracion)[colnames(duracion)=="DURATION.MAYO"] <- "MAY-DUR"
colnames(duracion)[colnames(duracion)=="DURATION.JUNIO"] <- "JUN-DUR"
colnames(duracion)[colnames(duracion)=="DURATION.JULIO"] <- "JUL-DUR"
colnames(duracion)[colnames(duracion)=="DURATION.AGOSTO"] <- "AGOS-DUR"
colnames(duracion)[colnames(duracion)=="DURATION.SETIEMBRE"] <- "SET-DUR"
colnames(duracion)[colnames(duracion)=="DURATION.OCTUBRE"] <- "OCT-DUR"
colnames(duracion)[colnames(duracion)=="DURATION.NOVIEMBRE"] <- "NOV-DUR"
colnames(duracion)[colnames(duracion)=="DURATION.DICIEMBRE"] <- "DIC-DUR"
duracion= cbind(subset (duracion, select=c(1,5,6,9,2,10,8,7,3,13,12,11,4)))

#este codigo nos servirá para seleccionar los demas datos asociados a la encuesta
duracion$code = paste(temp3$mes_ultimaencuesta,'-DUR')
duracion$code=gsub(" ", "", duracion$code)


#Extrayendo la mas reciente duracion de llamada
duracion$duracion_ultima=sapply(seq_len(nrow(duracion)), function(i) {duracion[i, duracion$code[[i]], drop = TRUE ] })
head(duracion)


head(tablon)



#---------------------------------------------------------------------#
#                       MATRIZ FINAL DE DATOS                         #
#---------------------------------------------------------------------#


#-- Matriz de datos
temp4<-merge(temp3,duracion,by="CEDULA_IDENTIDADE")
#reeplazando valore na por espacio en blanco
#temp4[is.na(temp4)] <- ""
head(temp4)


#agrendo la variable edad 
matriz_reniec$edad<-suppressWarnings(round(as.integer(as.Date(Sys.time(), "%d/%m/%y")-as.Date(matriz_reniec$fecha_nac,"%d/%m/%Y"))/365,0))

#adicionando atributos sociodemograficos
matriz_final<-sqldf("select a.*,b.edad,b.estado_civil,b.genero,c.nro_viajes from temp4 a 
                    left join matriz_reniec b on a.CEDULA_IDENTIDADE=b.dni
                    left join matriz_migraciones c on a.CEDULA_IDENTIDADE=c.doc_dni")

length(matriz_final$CEDULA_IDENTIDADE) #22643
length(unique(matriz_final$CEDULA_IDENTIDADE)) #22643


head(matriz_final)

table(matriz_final$mesencuesta_min)



#---------------------------------------------------------------------#
#                       EXPLORATORIO DE DATOS                         #
#---------------------------------------------------------------------#



#---------------------------------------------------------------------#
#                       MODELO DE CLASIFICAION                        #
#---------------------------------------------------------------------#



#---------------------------------------------------------------------#
#                                FIN                                  #
#---------------------------------------------------------------------#
