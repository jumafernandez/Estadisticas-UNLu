exportar_grafico_barras_horizontales_materia= function(materia, df) {

  # Se cargan las librerías
  library(readxl)
  library(ggplot2)
  library(scales)
  library(reshape)

  # Defino el nombre del archivo donde voy a guardar el grafico
  name_file=paste("barras_horizontales_",materia,".png",sep="")
  
  # Tomo el subconjunto de columnas de los registros que utilizo para el grafico
  encuesta.data<-df[,c(5,7:24)]
  viaje_curricular<-is.na(mean(encuesta.data$`Viajes Curriculares: Cumplimiento`, na.rm=TRUE))!=TRUE;
  
  # Verifico si posee viaje curricular para ver si lo grafico
  if (viaje_curricular){
    preg_no_grafico <- c(1, 2, 3, 4, 5, 6, 19)
    orden_preguntas <-c("Viajes Curriculares: Cumplimiento","Viajes Curriculares: Vinculación con contenidos","Viajes Curriculares: Aporte a la formación","Bibliografía: Claridad y Pertinencia","Bibliografía: Actualización","Evaluaciones: Claridad","Evaluaciones: Relación evaluado-clase","Trabajos Prácticos: Pertinencia","Trabajos Prácticos: Suficiencia","Trabajos Prácticos: Claridad", "Clases Teóricas: Profundidad", "Clases Teóricas: Claridad")
    print("Viajan")
  } else {
    preg_no_grafico <- c(1, 2, 3, 4, 5, 6, 16:19)
    orden_preguntas <-c("Bibliografía: Claridad y Pertinencia","Bibliografía: Actualización","Evaluaciones: Claridad","Evaluaciones: Relación evaluado-clase","Trabajos Prácticos: Pertinencia","Trabajos Prácticos: Suficiencia","Trabajos Prácticos: Claridad", "Clases Teóricas: Profundidad", "Clases Teóricas: Claridad")
    #  print("NO Viajan")
  }
  
  # Calculo la media y el desvio
  encuesta.media<-aggregate(encuesta.data[,-preg_no_grafico], by=list(encuesta.data$Comision), FUN=mean, na.rm=TRUE)
  names(encuesta.media)[1] = "Comision"
  encuesta.desvio<-aggregate(encuesta.data[,-preg_no_grafico], by=list(encuesta.data$Comision), FUN=sd, na.rm=TRUE)
  names(encuesta.desvio)[1] = "Comision"
  encuesta.media$Comision <- as.character(encuesta.media$Comision)

  # melt the data frame for plotting
  data.m <- melt(encuesta.media, id.vars='Comision')
  data.d <- melt(encuesta.desvio, id.vars='Comision')

  # ORDENO: ahora las variables son Comision, variable -pregunta- y value -promedio-
  data.m$variable = factor(data.m$variable, levels=orden_preguntas)
  levels(data.m$variable)
  data.d$variable = factor(data.d$variable, levels=orden_preguntas)
  levels(data.d$variable)
  
  # Pego el desvio en la tabla de datos
  data.m$desvio = data.d$value
  
  # plot everything
P=ggplot(data.m, aes(variable, value, fill=Comision)) +  
    #ggtitle(materia) +
    geom_bar(width=0.8, position=position_dodge(), stat="identity") + 
    geom_errorbar(aes(ymin=value-(desvio/2), ymax=value+(desvio/2)), position=position_dodge(),
                  colour="black", width=0.8) +
    coord_flip() +
    theme (text = element_text(size=8)) +
    #  ggtitle ("Evaluaci?n de las actividades") + # T?tulo del gr?fico
    theme (plot.title = element_text(family="Helvetic",
                                     size=rel(2), #Tama?o relativo de la letra del t?tulo
                                     vjust=2, #Justificaci?n vertical, para separarlo del gr?fico
                                     face="plain")) + 
    labs(x = "",y = "") + # Etiquetas o t?tulos
    theme(legend.position = "bottom") +
    scale_fill_manual(values=c(rgb(79, 129, 189, max=255), rgb(155, 187, 89, max=255), rgb(75, 172, 198, max=255), rgb(44, 77, 117, max=255),rgb(95, 117, 48, max=255), rgb(247, 150, 70, max=255))) +
    scale_y_continuous(limits = c(0,10), breaks = c(0:10))

  ggsave(plot=P, name_file, device = "png", height = 9, width = 16, units = c("cm"))
}


exportar_grafico_arania_materia= function(materia, df) {

  # Se cargan las librerías
  library(readxl)
  library(ggplot2)
  library(scales)
  library(reshape)
  
      librerias_instaladas<-rownames(installed.packages())
      if("fmsb" %in% librerias_instaladas == FALSE) {
      install.packages("fmsb", dependencies = TRUE)
    }
    library(fmsb)

    # Defino el nombre del archivo donde voy a guardar el grafico
    name_file=paste("arania_",materia,".png",sep="")
      
    encuesta.data<-df[,c(5,7:11)]
    names(encuesta.data)[2] = "Disponibilidad del\nPrograma"
    names(encuesta.data)[3] = "Cumplimiento de\nlos Contenidos"
    names(encuesta.data)[4] = "Disponibilidad del\nCronograma"
    names(encuesta.data)[5] = "Cumplimiento de\nla carga horaria"
    names(encuesta.data)[6] = "Disponibilidad de\nespacios de\nconsulta"
    
  
    encuesta.media<-aggregate(encuesta.data[,2:6], by=list(encuesta.data$Comision), FUN=mean, na.rm=TRUE)
    names(encuesta.media)[1]="Comision"

    encuesta.media[,2:6]=encuesta.media[,2:6]*100
    data=rbind(rep(100, 5) , rep(80,5) , encuesta.media)

    colors_border=c(rgb(79, 129, 189, max=255), rgb(155, 187, 89, max=255), rgb(75, 172, 198, max=255), rgb(44, 77, 117, max=255),rgb(95, 117, 48, max=255))

    png(name_file, width = 650, height = 650)
    radarchart( data[,-1]  , axistype=1, calcex = 0.8,
            #custom polygon
            pcol=colors_border, plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
    )
    legend(2, 1, legend = levels(as.factor(data[,1])), col = colors_border, seg.len = 2, border = "transparent", pch = 16, lty = 1)
    dev.off()
}

exportar_graficos_barras_docentes=function(materia, docente, df) {
    # Se cargan las librerias
    library(readxl)
    library(ggplot2)
    library(scales)
    library(reshape)
  
    # Defino el nombre del archivo donde voy a guardar el grafico
    name_file=paste("barras_docentes_",docente,".png",sep="")
  
    # Tomo el subconjunto de columnas de los registros que utilizo para el grafico
    encuesta.data<-df[,c(4:10)]
    #Elimino la variable 'Participación en clases' y renombro las variables
    encuesta.data=encuesta.data[,-c(2)]
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
    
    # ORDENO: 
    orden_preguntas <-c("Claridad Expositiva", "Disposición ante Preguntas", "Apertura a críticas", "Trato respetuoso", "Puntualidad")
        
    # ahora las variables son Comision, variable -pregunta- y value -promedio-
    data.m$variable = factor(data.m$variable, levels=orden_preguntas)
    levels(data.m$variable)
    data.d$variable = factor(data.d$variable, levels=orden_preguntas)
    levels(data.d$variable)
        
    # Pego el desvio en la tabla de datos
    data.m$desvio = data.d$value
        
    # plot everything
    P=ggplot(data.m, aes(variable, value, fill=Docente, width=.5)) +   
      geom_bar(width=0.5, position=position_dodge(), stat="identity") + 
      geom_errorbar(aes(ymin=value-(desvio/2), ymax=value+(desvio/2)), position=position_dodge(),
      colour="black", width=0.5) +
      #  coord_flip() +
      theme (text = element_text(size=9.5)) +
      #  ggtitle ("Evaluaci?n de las actividades") + # T?tulo del gr?fico
      theme ( plot.title = element_text(family="Helvetic",
              size=rel(2), #Tama?o relativo de la letra del t?tulo
              vjust=2, #Justificaci?n vertical, para separarlo del gr?fico
              face="plain")) + 
              labs(x = "",y = "") + # Etiquetas o t?tulos
      theme(legend.position = "bottom") +
      theme(axis.text.x = element_text(angle=10, hjust=1)) +
      scale_fill_manual(values=c(rgb(79, 129, 189, max=255), rgb(155, 187, 89, max=255), rgb(75, 172, 198, max=255), rgb(44, 77, 117, max=255),rgb(95, 117, 48, max=255))) +
      scale_y_continuous(limits = c(0,10.5), breaks = c(0:10))

    ggsave(plot=P, name_file, device = "png", height = 9, width = 16, units = c("cm"))
}

exportar_grafico_arania_carrera= function(df) {
  
  # Se cargan las librerías
  library(readxl)
  library(ggplot2)
  library(scales)
  library(reshape)
  
  librerias_instaladas<-rownames(installed.packages())
  if("fmsb" %in% librerias_instaladas == FALSE) {
    install.packages("fmsb", dependencies = TRUE)
  }
  library(fmsb)
  
  # Defino el nombre del archivo donde voy a guardar el grafico
  name_file=paste("arania_carrera",".png",sep="")
  
  encuesta.data<-df[,c(5,7:11)]
  names(encuesta.data)[2] = "Disponibilidad del\nPrograma"
  names(encuesta.data)[3] = "Cumplimiento de\nlos Contenidos"
  names(encuesta.data)[4] = "Disponibilidad del\nCronograma"
  names(encuesta.data)[5] = "Cumplimiento de\nla carga horaria"
  names(encuesta.data)[6] = "Disponibilidad de\nespacios de\nconsulta"
  
  # Hago esta chanchada para agrupar por todos los docentes de la Carrera
  encuesta.data$Comision <- "Ingeniería Industrial"
  
  encuesta.media<-aggregate(encuesta.data[,2:6], by=list(encuesta.data$Comision), FUN=mean, na.rm=TRUE)
  names(encuesta.media)[1]="Carrera"
  
  encuesta.media[,2:6]=encuesta.media[,2:6]*100
  data=rbind(rep(100, 5) , rep(80,5) , encuesta.media)
  
  colors_border=c(rgb(79, 129, 189, max=255), rgb(155, 187, 89, max=255), rgb(75, 172, 198, max=255), rgb(44, 77, 117, max=255),rgb(95, 117, 48, max=255))
  
  png(name_file, width = 650, height = 650)
  radarchart( data[,-1]  , axistype=1, calcex = 0.8,
              #custom polygon
              pcol=colors_border, plwd=4 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, 
              #custom labels
              vlcex=0.8 
  )
  legend(2, 1, legend = levels(as.factor(data[,1])), col = colors_border, seg.len = 2, border = "transparent", pch = 16, lty = 1)
  dev.off()
}

exportar_grafico_barras_horizontales_carrera= function(df) {
  
  # Se cargan las librerías
  library(readxl)
  library(ggplot2)
  library(scales)
  library(reshape)
  
  # Defino el nombre del archivo donde voy a guardar el grafico
  name_file=paste("barras_horizontales_","Carrera",".png",sep="")
  
  # Tomo el subconjunto de columnas de los registros que utilizo para el grafico
  encuesta.data<-df[,c(5,7:24)]
  viaje_curricular<-is.na(mean(encuesta.data$`Viajes Curriculares: Cumplimiento`, na.rm=TRUE))!=TRUE;
  
  # Verifico si posee viaje curricular para ver si lo grafico
  if (viaje_curricular){
    preg_no_grafico <- c(1, 2, 3, 4, 5, 6, 19, 20)
    orden_preguntas <-c("Viajes Curriculares: Cumplimiento","Viajes Curriculares: Vinculación con contenidos","Viajes Curriculares: Aporte a la formación","Bibliografía: Claridad y Pertinencia","Bibliografía: Actualización","Evaluaciones: Claridad","Evaluaciones: Relación evaluado-clase","Trabajos Prácticos: Pertinencia","Trabajos Prácticos: Suficiencia","Trabajos Prácticos: Claridad", "Clases Teóricas: Profundidad", "Clases Teóricas: Claridad")
    print("Viajan")
  } else {
    preg_no_grafico <- c(1, 2, 3, 4, 5, 6, 16:19, 20)
    orden_preguntas <-c("Bibliografía: Claridad y Pertinencia","Bibliografía: Actualización","Evaluaciones: Claridad","Evaluaciones: Relación evaluado-clase","Trabajos Prácticos: Pertinencia","Trabajos Prácticos: Suficiencia","Trabajos Prácticos: Claridad", "Clases Teóricas: Profundidad", "Clases Teóricas: Claridad")
    #  print("NO Viajan")
  }

  # Hago esta chanchada para agrupar por este campo
  encuesta.data$Asignatura <- "Ingeniería Industrial"
  
  # Calculo la media y el desvio
  encuesta.media<-aggregate(encuesta.data[,-preg_no_grafico], by=list(encuesta.data$Asignatura), FUN=mean, na.rm=TRUE)
  names(encuesta.media)[1] = "Carrera"
  
  encuesta.desvio<-aggregate(encuesta.data[,-preg_no_grafico], by=list(encuesta.data$Asignatura), FUN=sd, na.rm=TRUE)
  names(encuesta.desvio)[1] = "Carrera"
  encuesta.media$Carrera <- as.character(encuesta.media$Carrera)
  
  # melt the data frame for plotting
  data.m <- melt(encuesta.media, id.vars='Carrera')
  data.d <- melt(encuesta.desvio, id.vars='Carrera')
  
  # ORDENO: ahora las variables son Comision, variable -pregunta- y value -promedio-
  data.m$variable = factor(data.m$variable, levels=orden_preguntas)
  levels(data.m$variable)
  data.d$variable = factor(data.d$variable, levels=orden_preguntas)
  levels(data.d$variable)
  
  # Pego el desvio en la tabla de datos
  data.m$desvio = data.d$value
  
  # plot everything
  P=ggplot(data.m, aes(variable, value, fill=Carrera)) +  
    #ggtitle(materia) +
    geom_bar(width=0.8, position=position_dodge(), stat="identity") + 
    geom_errorbar(aes(ymin=value-(desvio/2), ymax=value+(desvio/2)), position=position_dodge(),
                  colour="black", width=0.8) +
    coord_flip() +
    theme (text = element_text(size=8)) +
    #  ggtitle ("Evaluaci?n de las actividades") + # T?tulo del gr?fico
    theme (plot.title = element_text(family="Helvetic",
                                     size=rel(2), #Tama?o relativo de la letra del t?tulo
                                     vjust=2, #Justificaci?n vertical, para separarlo del gr?fico
                                     face="plain")) + 
    labs(x = "",y = "") + # Etiquetas o t?tulos
    theme(legend.position = "bottom") +
    scale_fill_manual(values=c(rgb(79, 129, 189, max=255), rgb(155, 187, 89, max=255), rgb(75, 172, 198, max=255), rgb(44, 77, 117, max=255),rgb(95, 117, 48, max=255), rgb(247, 150, 70, max=255))) +
    scale_y_continuous(limits = c(0,10), breaks = c(0:10))
  
  ggsave(plot=P, name_file, device = "png", height = 9, width = 16, units = c("cm"))
}

exportar_graficos_barras_docentes_Carrera=function(df) {
  # Se cargan las librerias
  library(readxl)
  library(ggplot2)
  library(scales)
  library(reshape)
  
  # Defino el nombre del archivo donde voy a guardar el grafico
  name_file=paste("barras_docentes_","Carrera",".png",sep="")
  
  # Tomo el subconjunto de columnas de los registros que utilizo para el grafico
  encuesta.data<-df[,c(4:10)]
  #Elimino la variable 'Participación en clases' y renombro las variables
  encuesta.data=encuesta.data[,-c(2)]
  names(encuesta.data)[2] = "Claridad Expositiva"
  names(encuesta.data)[3] = "Disposición ante Preguntas"
  names(encuesta.data)[4] = "Apertura a críticas"
  names(encuesta.data)[5] = "Trato respetuoso"
  names(encuesta.data)[6] = "Puntualidad"

  # Hago esta chanchada para agrupar por todos los docentes de la Carrera
  encuesta.data$Docente <- "Ingeniería Industrial"
  
  # Calculo la media y el desvio
  encuesta.media<-aggregate(encuesta.data[,-c(1)], by=list(encuesta.data$Docente), FUN=mean, na.rm=TRUE)
  names(encuesta.media)[1] = "Carrera"
  encuesta.desvio<-aggregate(encuesta.data[,-c(1)], by=list(encuesta.data$Docente), FUN=sd, na.rm=TRUE)
  names(encuesta.desvio)[1] = "Carrera"
  
  # melt the data frame for plotting
  data.m <- melt(encuesta.media, id.vars='Carrera')
  data.d <- melt(encuesta.desvio, id.vars='Carrera')
  
  # ORDENO: 
  orden_preguntas <-c("Claridad Expositiva", "Disposición ante Preguntas", "Apertura a críticas", "Trato respetuoso", "Puntualidad")
  
  # ahora las variables son Comision, variable -pregunta- y value -promedio-
  data.m$variable = factor(data.m$variable, levels=orden_preguntas)
  levels(data.m$variable)
  data.d$variable = factor(data.d$variable, levels=orden_preguntas)
  levels(data.d$variable)
  
  # Pego el desvio en la tabla de datos
  data.m$desvio = data.d$value
  
  # plot everything
  P=ggplot(data.m, aes(variable, value, fill=Carrera, width=.5)) +   
    geom_bar(width=0.5, position=position_dodge(), stat="identity") + 
    geom_errorbar(aes(ymin=value-(desvio/2), ymax=value+(desvio/2)), position=position_dodge(),
                  colour="black", width=0.5) +
    #  coord_flip() +
    theme (text = element_text(size=9.5)) +
    #  ggtitle ("Evaluaci?n de las actividades") + # T?tulo del gr?fico
    theme ( plot.title = element_text(family="Helvetic",
                                      size=rel(2), #Tama?o relativo de la letra del t?tulo
                                      vjust=2, #Justificaci?n vertical, para separarlo del gr?fico
                                      face="plain")) + 
    labs(x = "",y = "") + # Etiquetas o t?tulos
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle=10, hjust=1)) +
    scale_fill_manual(values=c(rgb(79, 129, 189, max=255), rgb(155, 187, 89, max=255), rgb(75, 172, 198, max=255), rgb(44, 77, 117, max=255),rgb(95, 117, 48, max=255))) +
    scale_y_continuous(limits = c(0,10.5), breaks = c(0:10))
  
  ggsave(plot=P, name_file, device = "png", height = 9, width = 16, units = c("cm"))
}
