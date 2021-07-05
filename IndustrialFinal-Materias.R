library(readxl)
library(ggplot2)
library(scales)
library(reshape)

# Seteo la materia y si tiene viaje curricular
nombre<-"SISTEMAS DE MANTENIMIENTO (A) (40806)"
viaje_curricular=TRUE #TRUE o FALSE

# Importo el archivo
Industrial <- read_excel("C:/Users/Usuario/Google Drive/PC-Juan/Carpeta Laboral/Industrial.xlsx", sheet = "Materia", col_types = c("numeric", "text", "text", "text", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "text"))
#View(Industrial)

# Tomo el subconjunto de registros que corresponden a la materia nombre
encuesta<-subset(Industrial, Asignatura %in% nombre) 
#View(encuesta)

# Tomo el subconjunto de columnas de los registros que utilizo para el grafico
encuesta.data<-encuesta[,c(5,7:25)]
#View(encuesta.data)

# Verifico si posee viaje curricular para ver si lo grafico
if (viaje_curricular) {
  preg_no_grafico <- c(1, 2, 3, 4, 5, 6, 16, 20)
  orden_preguntas <-c("Viajes Curriculares: Cumplimiento","Viajes Curriculares: Vinculación con contenidos","Viajes Curriculares: Aporte a la formación","Bibliografía: Claridad y Pertinencia","Bibliografía: Actualización","Evaluaciones: Claridad","Evaluaciones: Relación evaluado-clase","Trabajos Prácticos: Pertinencia","Trabajos Prácticos: Suficiencia","Trabajos Prácticos: Claridad", "Clases Teóricas: Profundidad", "Clases Teóricas: Claridad")
  print("Viajan")
} else {
  preg_no_grafico <- c(1, 2, 3, 4, 5, 6, 16:20)
  orden_preguntas <-c("Bibliografía: Claridad y Pertinencia","Bibliografía: Actualización","Evaluaciones: Claridad","Evaluaciones: Relación evaluado-clase","Trabajos Prácticos: Pertinencia","Trabajos Prácticos: Suficiencia","Trabajos Prácticos: Claridad", "Clases Teóricas: Profundidad", "Clases Teóricas: Claridad")
  print("NO Viajan")
}

# Calculo la media y el desvio
encuesta.media<-aggregate(encuesta.data[,-preg_no_grafico], by=list(encuesta.data$Comisión), FUN=mean, na.rm=TRUE)
names(encuesta.media)[1] = "Comision"
encuesta.desvio<-aggregate(encuesta.data[,-preg_no_grafico], by=list(encuesta.data$Comisión), FUN=sd, na.rm=TRUE)
names(encuesta.desvio)[1] = "Comision"
encuesta.media$Comision <- as.character(encuesta.media$Comision)
#View(encuesta.media)

# melt the data frame for plotting
data.m <- melt(encuesta.media, id.vars='Comision')
data.d <- melt(encuesta.desvio, id.vars='Comision')
#View(data.m)


# ORDENO: ahora las variables son Comision, variable -pregunta- y value -promedio-
data.m$variable = factor(data.m$variable, levels=orden_preguntas)
levels(data.m$variable)
data.d$variable = factor(data.d$variable, levels=orden_preguntas)
levels(data.d$variable)

# Pego el desvio en la tabla de datos
data.m$desvio = data.d$value


# plot everything
ggplot(data.m, aes(variable, value, fill=Comision)) +   
  geom_bar(width=0.8, position=position_dodge(), stat="identity") + 
  geom_errorbar(aes(ymin=value-(desvio/2), ymax=value+(desvio/2)), position=position_dodge(),
                colour="black", width=0.8) +
  coord_flip() +
  theme (text = element_text(size=8)) +
  #  ggtitle ("Evaluación de las actividades") + # Título del gráfico
  theme (plot.title = element_text(family="Helvetic",
                                   size=rel(2), #Tamaño relativo de la letra del título
                                   vjust=2, #Justificación vertical, para separarlo del gráfico
                                   face="plain")) + 
  labs(x = "",y = "") + # Etiquetas o títulos
  theme(legend.position = "bottom") +
  scale_fill_manual(values=c(rgb(79, 129, 189, max=255), rgb(155, 187, 89, max=255), rgb(75, 172, 198, max=255), rgb(44, 77, 117, max=255),rgb(95, 117, 48, max=255))) +
  scale_y_continuous(limits = c(0,5.5), breaks = c(1:6))