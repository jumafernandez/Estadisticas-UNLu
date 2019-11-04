-- Archivo: personales.txt
SELECT	'DNI',
	numero_documento,
	substr(apellido_nombre,1,strpos(apellido_nombre,',') -1),
	substr(apellido_nombre,strpos(apellido_nombre,',')+1),
	54,
	fecha_nacimiento,
	54,
	'',
	'',
	calle || ' ' || numero_direccion
FROM estudiantes e
WHERE legajo=119544;

-- Archivo: academicos.txt
SELECT	'DNI',
	numero_documento,
	2201,
	726,
	'',
	'S',
	'',
	'',
	'',
	(select min(fecha_examen) from finales where calificacion>=4 and calificacion<=10 and legajo=e.legajo),
	(select max(fecha_examen) from finales where calificacion>=4 and calificacion<=10 and legajo=e.legajo),
	'S',
	1,
	titulo_ingreso,
	titulo_ingreso,
	'',
	'',
	'',
	8.35,
	8.31,
	'N'
FROM estudiantes e
WHERE legajo=119544;

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
WHERE e.legajo=119544 and f.calificacion<>99
and f.asignatura in (select codigo from asignaturas where plan=e.plan_estudios);
