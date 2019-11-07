select	legajo,
	apellido_nombre,
	(select denominacion from asignaturas where codigo=c.asignatura limit 1) as materia
from cursadas c
where asignatura=20038;	