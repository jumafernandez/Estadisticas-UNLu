select *
from estudiantes
where legajo IN (select distinct(legajo) from finales where calificacion>=4 and calificacion<=10 and fecha_examen>='01-01-2000' and fecha_examen<='31-12-2000');
