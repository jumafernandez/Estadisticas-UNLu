library(readxl)
library(ggplot2)
library(scales)
library(reshape)
setwd("C:/Users/Usuario/Desktop/graficos3")

# Importo el archivo
Industrial <- read_excel("C:/Users/Usuario/Google Drive/PC-Juan/Carpeta Laboral/Industrial.xlsx", sheet = "Docente", col_types = c("text", "numeric", "text", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
#View(Industrial)

for(codigo_ingresado in unique(Industrial$Código)){ # Por cada una de las materias

# Tomo el subconjunto de registros que corresponden al codigo de materia
encuesta_materia<-subset(Industrial, Código %in% codigo_ingresado) 
#View(encuesta)

for(docente_ingresado in unique(encuesta_materia$Docente)){ # Por cada docente

# Defino el nombre del archivo donde voy a guardar el gráfico
name_file=paste(codigo_ingresado,docente_ingresado,".png")
  
# Tomo el subconjunto de registros que corresponden al docente
encuesta<-subset(encuesta_materia, Docente %in% docente_ingresado) 

# Tomo el subconjunto de columnas de los registros que utilizo para el grafico
encuesta.data<-encuesta[,c(5:10)]
#View(encuesta.data)

names(encuesta.data)[2] = "Claridad Expositiva"
names(encuesta.data)[3] = "Disposición ante Preguntas"
names(encuesta.data)[4] = "Apertura a críticas"
names(encuesta.data)[5] = "Trato respetuoso"
names(encuesta.data)[6] = "Puntualidad"

# Calculo la media y el desvio
encuesta.media<-aggregate(encuesta.data[,-c(1)], by=list(encuesta.data$Docente), FUN=mean, na.rm=TRUE)
names(encuesta.media)[1] = "Docente"
encuesta.desvio<-aggregate(encuesta.data[,-c(1)], by=list(encuesta.data$Docente), FUN=sd, na.rm=TRUE)
names(encuesta.desvio)[1] = "Docente"

# melt the data frame for plotting
data.m <- melt(encuesta.media, id.vars='Docente')
data.d <- melt(encuesta.desvio, id.vars='Docente')
#View(data.m)

orden_preguntas <-c("Claridad Expositiva", "Disposición ante Preguntas", "Apertura a críticas", "Trato respetuoso", "Puntualidad")

# ORDENO: ahora las variables son Comision, variable -pregunta- y value -promedio-
data.m$variable = factor(data.m$variable, levels=orden_preguntas)
levels(data.m$variable)
data.d$variable = factor(data.d$variable, levels=orden_preguntas)
levels(data.d$variable)

# Pego el desvio en la tabla de datos
data.m$desvio = data.d$value

# plot everything
P<-ggplot(data.m, aes(variable, value, fill=Docente)) +   
  geom_bar(width=0.8, position=position_dodge(), stat="identity") + 
  geom_errorbar(aes(ymin=value-(desvio/2), ymax=value+(desvio/2)), position=position_dodge(),
                colour="black", width=0.8) +
#  coord_flip() +
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


ggsave(plot=P, name_file, device = "png")

}


}
