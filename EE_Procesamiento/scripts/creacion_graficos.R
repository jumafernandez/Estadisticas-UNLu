# Se cargan las librerías
library(officer)
library(magrittr)
library(readxl)
library(ggplot2)
library(scales)
library(reshape)

carpeta_raiz="C:/Users/unlu/Desktop/EE_Procesamiento/Ingenieria_Industrial"
archivo_datos_materia<-paste(carpeta_raiz, "/data/", "Industrial-materias.xlsx", sep="")
archivo_datos_docente<-paste(carpeta_raiz, "/data/", "Industrial-docente.xlsx", sep="")

# Librería propia con definición de gráficos
source(file = paste(carpeta_raiz, "/scripts/funciones.R", sep = ""), encoding = "UTF-8")

# Importo el archivo para las materias
respuestas_materias <- read_excel(archivo_datos_materia, sheet = "Matriz Trabajada", col_types = c("numeric", "text", "text", "text", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text"))

# Importo el archivo para los graficos docentes
respuestas_docentes <- read_excel(archivo_datos_docente, sheet = "Docente", col_types = c("text", "numeric", "text", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
respuestas_docentes$Asignatura=paste(respuestas_docentes$Asignatura, " (",respuestas_docentes$Código,")", sep = "")
respuestas_docentes=respuestas_docentes[,-c(2)]

for (materia in unique(respuestas_materias$Asignatura)){
  # Creo la carpeta donde voy a guardar los graficos de cada materia
  carpeta_graficos=paste(carpeta_raiz,"/graficos/", materia, sep="")
  print(carpeta_graficos)
  dir.create(carpeta_graficos)
  setwd(carpeta_graficos)
  
  # Tomo el subconjunto de registros que corresponden a la materia nombre
  names(respuestas_materias)[5]="Comision"
  encuesta_materias<-subset(respuestas_materias, Asignatura %in% materia) 
  
  # Tomo el subconjunto de registros que corresponden al docente en la materia
  encuesta_docentes<-subset(respuestas_docentes, Asignatura %in% materia) 
  
  exportar_grafico_barras_horizontales_materia(materia, encuesta_materias)
  exportar_grafico_arania_materia(materia, encuesta_materias)
  
  for(docente_ingresado in unique(encuesta_docentes$Docente)){ # Por cada docente
    # Tomo el subconjunto de registros que corresponden al docente
    encuesta<-subset(encuesta_docentes, Docente %in% docente_ingresado) 
    exportar_graficos_barras_docentes(materia, docente_ingresado, encuesta)
  }
}

# Ahora hago los gráficos de la Carrera
carpeta_graficos=paste(carpeta_raiz,"/graficos/", "Carrera", sep="")
print(carpeta_graficos)
dir.create(carpeta_graficos)
setwd(carpeta_graficos)
exportar_grafico_arania_carrera(respuestas_materias)
exportar_grafico_barras_horizontales_carrera(respuestas_materias)
exportar_graficos_barras_docentes_Carrera(respuestas_docentes)
