Buenas

#Archivo para graficar las estad�sticas de cursos de verano III-2019
#hecho por Pablo Salvador Rodr�guez G�mez
#Ultima version: 16/09/19

#Bibliotecas a usar:
library(ggplot2) #important�simo para graficar
library(GGally) #extension de ggplot2
#library(forecast) #Para cargar todas las herramientas y funciones de pronostico
library(seasonal) #para descomposicion X11, cosas de X13
library(chron)  #para reconocer horas y fechas
#library(readxl) #Para leer excel
#library(viridis) #Para mejorar los gr�ficos (darle color por dia p.ej.)
library(data.table) #Se usa para ordenar los datos 
library(dplyr) #para manejar sumarios

#library(readxl) #se piensa usar csv para no meterse con excel, pero estas librer�as sirven a la perfeccion
#library("xlsx")

#install.packages("forecast")
#install.packages("ggplot2")  #este viene integrado a forecast
#install.packages("GGally")
#install.packages("seasonal")
#install.packages("chron")
#install.packages("readxl")
#install.packages("xlsx")

#Objetivo actual: Graficar los cursos con m�s peticion, obtener los que tiene m�s de la media.

file = 'C:/Users/pablo/Documents/UCR/Representacion_Estudiantil/Cursos_verano_2.csv'
#se puede usar setwd("~/Escritorio/") y file= 'Cursos_verano.csv'

encuesta <- read.csv(file = file, encoding = "UTF-8") #se a�ade UTF-8 para poder leer tildes.
names(encuesta) <- c("hora","carne","nombre","troncales","optativos","Servicio") # variable names
fix(encuesta) #para arreglar datos que est�n mal puestos por los estudiantes.


#esto que viene es para ver gente que llen� 2 veces.
dt_rep <- as.data.table(encuesta)
dt_rep = dt_rep[, replicate := 1:.N, by="nombre"]
#dt_rep = dt_rep[, replicate2 := 1:.N, by="carne"]
encuesta_sin_rep = subset(dt_rep, replicate !=2 )


#converto la estructura a matriz y luego a dataframe, por problemas en los tipos de dato:
encuesta_mat = as.matrix(encuesta_sin_rep)
#encuesta3 = as.data.frame(encuesta2)

#lo que viene es para separar los cursos que pidi� alguien.
#Se debe contar cu�ntas veces se pidi� 'X' curso, por lo que hay que separar en encuesta$troncales las palabras separadas por commas
###########################################################################
############### Cursos Troncales ##########################################
############################################################################
troncales = encuesta_mat[,4]
#View(troncales)
#troncales[1]

char_troncales= troncales[1]
for (i in 2:length(troncales)){
  char_troncales = paste0(char_troncales, ', ' , troncales[i])
}
troncales_separado = strsplit(char_troncales, ", ") #crea la lista con todos los cursos. Falta hacer un histograma/sumario
troncales_separado = data.frame(troncales_separado)
names(troncales_separado) <- ("cursos")
troncales_separado = table(troncales_separado$cursos)
troncales = as.data.frame(troncales_separado) 
troncales=troncales[-(1),]
troncales = troncales[order(troncales$Freq),]

#Graficacion del histograma:
#ggplot(troncales, aes(x = Freq)) + geom_histogram() #+ facet_wrap(~Var1)
ggplot(troncales, aes(Var1, weight = Freq)) +geom_bar() + coord_flip() + ylab("Cuenta") + xlab("Curso") #+ stat_count(width = 0.5)  #+geom_histogram()


#Estad�sticas:
write.csv(troncales, file = 'C:/Users/pablo/Documents/UCR/Representacion_Estudiantil/Resultados_troncales_verano_2.csv')

mean(troncales$Freq)

troncales[troncales$Freq >mean(troncales$Freq),] #obtengo los troncales por encima de la media

######################################################################################
################ Cursos Optativos #######################################################
######################################################################################


optativos = encuesta_mat[,5]
#View(optativos)
#optativos[1]

char_optativos= optativos[1]
for (i in 2:length(optativos)){
  char_optativos = paste0(char_optativos, ', ' , optativos[i])
}
optativos_separado = strsplit(char_optativos, ", ") #crea la lista con todos los cursos. Falta hacer un histograma/sumario
optativos_separado = data.frame(optativos_separado)
names(optativos_separado) <- ("cursos")
optativos_separado = table(optativos_separado$cursos)
optativos = as.data.frame(optativos_separado) 
#remover la fila de 0 personas, para no afectar a la estad�stica:
optativos = optativos[-(1),]
optativos = optativos[order(optativos$Freq),]



#Graficacion del histograma:
#ggplot(optativos, aes(x = Freq)) + geom_histogram() #+ facet_wrap(~Var1)
ggplot(optativos, aes(Var1, weight = Freq)) +geom_bar() + coord_flip() + ylab("Cuenta") + xlab("Curso") #+ stat_count(width = 0.5)  #+geom_histogram()


#Estad�sticas:
write.csv(optativos, file = 'C:/Users/pablo/Documents/UCR/Representacion_Estudiantil/Resultados_optativos_verano_2.csv')

mean(optativos$Freq)

optativos[optativos$Freq >mean(optativos$Freq),] #obtengo los optativos por encima de la media

##Esto mismo se puede hacer con los datos de cursos de servicio de la escuela.
#S�lamente correr todo de nuevo, pero usando:

servicio = encuesta_mat[,6]