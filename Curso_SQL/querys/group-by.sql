select 	sede,
	carrera,
	count(legajo)
from estudiantes
where estado=''
group by sede, carrera;