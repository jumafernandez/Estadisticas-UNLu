# Se cargan las librerías
library(officer)
library(magrittr)

carpeta_raiz="C:/Users/unlu/Desktop/EE_Procesamiento/Ingenieria_Industrial"
carpeta_graficos=paste(carpeta_raiz, "/graficos",sep="")

graficos_materias=dir(carpeta_graficos)

for(materia in graficos_materias){

  # Creo un nuevo documento para cada materia
  documento <- read_docx() 
  
  # Me paro en el directorio de la materia y pego los dos primeros graficos en el documento
  directorio_materia=paste(carpeta_graficos,"/",materia,sep="")
  arania_graph=paste(directorio_materia,"/","arania_",materia,".png", sep = "")
  barras_horizontales_graph=paste(directorio_materia,"/","barras_horizontales_",materia,".png", sep = "")
    
  documento <- documento %>% 
  body_add_par(materia, style = "Normal") %>% 
  body_add_par("", style = "Normal") %>% # blank paragraph
  body_add_img(src = arania_graph, width = 5, height = 6, style = "centered") %>% 
  body_add_img(src = barras_horizontales_graph, width = 5, height = 6, style = "centered")

  # Ahora recorro cada grafico docente y se lo pego al doc de la materia
  directorio=dir(directorio_materia)
  for(grafico in directorio){
    print(grafico)
    if(substr(grafico, 1, 16)=="barras_docentes_"){
      documento <- documento %>% 
        body_add_img(src = paste(directorio_materia,"/",grafico,sep=""), width = 5, height = 6, style = "centered")
      }
  }

  # Guardo todo en el documento
  destino_reporte=paste(carpeta_raiz, "/reportes/", materia, ".docx", sep="")
  print(documento, target = destino_reporte)
  
  }

# Creo un nuevo documento para la Carrera
documento <- read_docx() 
# Me paro en el directorio de la Carrera y pego los graficos en el documento
directorio=paste(carpeta_graficos,"/Carrera",sep="")
arania_graph=paste(directorio,"/","arania_carrera.png", sep = "")
barras_horizontales_graph=paste(directorio,"/","barras_horizontales_Carrera.png", sep = "")
barras_docentes_graph=paste(directorio,"/","barras_docentes_Carrera.png", sep = "")

# Agrego las gráficas al documento
documento <- documento %>% 
  body_add_par("Carrera", style = "Normal") %>% 
  body_add_par("", style = "Normal") %>% # blank paragraph
  body_add_img(src = arania_graph, width = 5, height = 6, style = "centered") %>% 
  body_add_img(src = barras_horizontales_graph, width = 5, height = 6, style = "centered") %>% 
  body_add_img(src = barras_docentes_graph, width = 5, height = 6, style = "centered")
# Guardo todo en el documento de la Carrera
destino_reporte=paste(carpeta_raiz, "/reportes/Carrera.docx", sep="")
print(documento, target = destino_reporte)
