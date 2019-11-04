-- Archivo: analitico.txt
SELECT	'DNI',
	e.numero_documento,
	(select a.denominacion from asignaturas a where a.codigo=f.asignatura limit 1),
	f.fecha_examen,
	f.calificacion,
	CASE WHEN (f.calificacion>=1 and f.calificacion<=99) THEN f.libro ELSE 000 END,
	f.folio,
	CASE 	WHEN (f.condicion='P') THEN f.condicion 
		WHEN (f.condicion='R' or f.condicion='L') THEN 'E' 
		WHEN (f.condicion='E' or f.condicion='I') THEN 'Q'
	ELSE 'R' END,
	CASE	WHEN (f.calificacion>=4 and f.calificacion<=10) THEN 'A' 
		WHEN (f.condicion='X' or f.condicion='E' or f.condicion='I') THEN 'A' 
		ELSE 'D' END
FROM finales f
INNER JOIN estudiantes e ON e.legajo=f.legajo
WHERE e.legajo=119544 and f.calificacion<>99  --Modificar legajo
and f.asignatura in (select codigo from asignaturas where plan=e.plan_estudios);
