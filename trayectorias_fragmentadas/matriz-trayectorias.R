library(sqldf)
library(readr)
egresados <- read_delim("Escritorio/CLABES_trayectorias/utiles/egresados_grado_161718.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Exploración de la duración real
hist(egresados$DURACION)
boxplot(egresados$DURACION)

# Cantidad de egresados por año
table(egresados$`AÑO EGRESO`)

# Me conecto a la db exportaciones_unlu
library(RPostgreSQL)
driver <- dbDriver("PostgreSQL")
conx <- dbConnect(driver, host="localhost", dbname="exportaciones_unlu", user="usupostgres", password="deztina327", port=5432)

# Limito a los primeros 10 egresados para acortar los tiempos
# egresados=egresados[1:10,]

#Creo el dataframe para guardar los datos
info = data.frame(matrix(vector(), 0, 4, dimnames=list(c(), c("legajo", "carrera", "anio","cantidad_cursadas"))))

# Recorro todos los egresados
for (egresado in unique(egresados$LEGAJO)) {
    # Tomo los datos de año de ingreso y egreso
    anio_inicio=egresados$`AÑO DE INGRESO`[egresados$LEGAJO==egresado]
    anio_fin=egresados$`AÑO EGRESO`[egresados$LEGAJO==egresado]
    nombre_carrera=egresados$CARRERA[egresados$LEGAJO==egresado]

    for (i in anio_inicio:anio_fin) {
      consulta<-paste("SELECT count(legajo) as cursadas_terminadas
                      FROM cursadas 
                      WHERE legajo=", egresado, " AND condicion<>'A' AND anio_cursada=", i)
      query <- dbSendQuery(conx, consulta)
      dato=dbFetch(query)

      # Guardo en el dataframe
      dato.nuevo=data.frame(legajo=egresado, carrera=nombre_carrera, anio=i, cantidad_cursadas=dato$cursadas_terminadas)
      info=rbind(info, dato.nuevo)
      }
}

library(reshape2)
info.decasteado <- dcast(info, legajo + carrera ~ anio)
getwd()

write.csv(info.decasteado,'/home/juan/Escritorio/exportacion-trayectorias.csv', row.names = TRUE)



