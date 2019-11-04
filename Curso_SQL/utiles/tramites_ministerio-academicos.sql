-- Archivo: academicos.txt
SELECT	'DNI',
	numero_documento,
	2201, --Modificar sede
	726,  --Modificar titulo Araucano
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
	8.35, --Modificar promedio sin aplazos
	8.31, --Modificar promedio con aplazos
	'N'
FROM estudiantes e
WHERE legajo=119544; --Modificar legajo
