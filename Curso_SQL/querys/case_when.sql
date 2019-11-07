SELECT	legajo,
	apellido_nombre,
	calificacion,
	CASE WHEN calificacion>=4 and calificacion<=10  THEN 'APROBADO'
	     WHEN calificacion=99 THEN 'AUSENTE'
        ELSE 'DESAPROBADO' END as descripcion_condicion
FROM finales
WHERE asignatura = 11056 and fecha_examen>='01-01-2018' and fecha_examen<='31-12-2019';
