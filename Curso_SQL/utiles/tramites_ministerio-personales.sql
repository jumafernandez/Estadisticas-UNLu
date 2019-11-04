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
WHERE legajo=119544; --Modificar legajo
