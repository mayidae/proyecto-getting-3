#md

#Getting and Cleaning Data Course Project

#Los datos los descargué directamente desde el link y el zip lo guardé en una carpeta. Cree un proyecto en R y hice una copia en github


datosruta <- file.path(path = "C:/Users/Mayida El Souki/Documents/CIENCIADEDATOS/CURSODATASCIENCEUJH/Curso 3 Getting and Cleaning Data/proyecto GCD 3/proyecto getting 3", "UCI HAR Dataset")
archivos<-list.files(datosruta, recursive=TRUE)
archivos


#Los archivos son de "train" y "test", cada uno de ellos tiene asociado archivos de "sujeto" (Subject), "actividad" (Activity) y "caracteristicas" (Features). Por esta razón hay que descargar estos tres juegos de archivos. Por otro lado, caracteristicas es todo lo denotado con "X" y actividad es todas las variables denotadas con "Y"

#Ahora cargaré los archivos

#Archivos de Sujeto

datosSujetoTrain <- read.table(file.path(datosruta, "train", "subject_train.txt"),header = FALSE)
datosSujetoTest  <- read.table(file.path(datosruta, "test" , "subject_test.txt"),header = FALSE)


#Arhivos de actividad

datosactividadTest  <- read.table(file.path(datosruta, "test" , "Y_test.txt" ),header = FALSE)
datosactividadTrain <- read.table(file.path(datosruta, "train", "Y_train.txt"),header = FALSE)


#Archivos de Características


datoscaracTest  <- read.table(file.path(datosruta, "test" , "X_test.txt" ),header = FALSE)
datoscaracTrain <- read.table(file.path(datosruta, "train", "X_train.txt"),header = FALSE)



##1. Fusiona los conjuntos de entrenamiento y prueba para crear un conjunto de datos.

#Ahora vamos a fusionar los datos de "train" y "test" uniendo las tablas de datos por las filas


datossujeto <- rbind(datosSujetoTrain, datosSujetoTest)
datosactividad<- rbind(datosactividadTrain, datosactividadTest)
datoscarac<- rbind(datoscaracTrain, datoscaracTest)


#Darle nombre a las variables

names(datossujeto)<-c("subject")
names(datosactividad)<- c("activity")
datoscaracnombres <- read.table(file.path(datosruta, "features.txt"),head=FALSE)
names(datoscarac)<- datoscaracnombres$V2


#Ahora uniré todas las colunmas para crear el marco de datos, para los datos totales


datoscombinados <- cbind(datossujeto, datosactividad)
Datos <- cbind(datoscarac, datoscombinados)


## 2. Extrae solo las mediciones de la media y la desviación estándar de cada medición.

#Extraer la Media y Desviación estándard de cada variable

#Nombre de subconjunto de características (features) por medidas en la desviación estándar y media. Lo que se hará es seleccionar las variables que tienen media y desviación


subdatoscaracnombres<-datoscaracnombres$V2[grep("mean\\(\\)|std\\(\\)", datoscaracnombres$V2)]


#Extraerémos un Subconjunto los datos del marco de datos por nombres seleccionados de características (Features)


nombreseleccionados<-c(as.character(subdatoscaracnombres), "subject", "activity")
Datos<-subset(Datos,select=nombreseleccionados)
str(Datos)



##3. Utiliza nombres de actividades descriptivos para nombrar las actividades en el conjunto de datos


library(data.table)


etiquetasactividad <- read.table(file.path(datosruta, "activity_labels.txt"),header = FALSE)
setnames(etiquetasactividad, names(etiquetasactividad), c("activity","activityName"))


#Para setnames cargar data.table


Datos <- merge(etiquetasactividad, Datos, by="activity", all.x = TRUE)
Datos$activityName <- as.character(Datos$activityName)

head(Datos$activityName,30)



##4. Etiquete adecuadamente el conjunto de datos con nombres de variables descriptivos.

#Los nombres de las variables están abreviadas, por ejemplo, 
#t = time
#Acc = Accelerometer
#Gyro = Gyroscope
#f = frequency
#Mag = Magnitude
#BodyBody = Body
#Lo que se pide es reemplazarlas por los nombre completos en el marco de datos


names(Datos)<-gsub("^t", "time", names(Datos))
names(Datos)<-gsub("^f", "frequency", names(Datos))
names(Datos)<-gsub("Acc", "Accelerometer", names(Datos))
names(Datos)<-gsub("Gyro", "Gyroscope", names(Datos))
names(Datos)<-gsub("Mag", "Magnitude", names(Datos))
names(Datos)<-gsub("BodyBody", "Body", names(Datos))


names(Datos)


##5. A partir del conjunto de datos del paso 4, crea un segundo conjunto de datos ordenado e independiente con el promedio de cada variable para cada actividad y cada tema.

#Cargar el paquete dplyr

library(dplyr)

Datos2 <- Datos %>%
  group_by(subject, activityName) %>%
  summarise_all(funs(mean))
write.table(Datos2, "Datos2.txt", row.name=FALSE)
str(Datos2)

