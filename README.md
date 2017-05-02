# ineeR
Funciones para analizar las bases de datos de las evaluaciones del aprendizaje *Excale* y *Planea*, del Instituto Nacional para la Evaluación de la Educación (*INEE*) de México.

En particular, *ineeR* ayuda en la estimación de estadísticos poblacionales, los cuales requieren de considerar el diseño muestral de estas evaluaciones para obtener los valores correctos.

Las funciones de *ineeR* son:

* **media_poblacion**.  Estima medias poblacionales, con error estándar e intervalos de confianza.
* **puntaje_plausible** Estima medias poblacionales para variables con puntuaciones plausibles, como es el caso de las puntuaciones de las asignaturas evaluadas en Excale y Planea, con error estándar e intervalos de confianza
* **estimacion_porcentaje**. Estima proporciones de variables, como porcentajes, co error estándar e intervalos de confianza
* **multiples_variables**. Estima medias poblaciones para más de una variable a la vez.
* **graf_resultado** y **graf_resultado_h**. Grafican los resultados de las funciones *media_poblacion* y *puntaje_plausible*.
* **graf_porcentaje** y **graf_porcentaje_h**. Grafican los resultados de la función *estimacion_porcentaje*.

Las funciones funcionan para todas las bases de datos publicadas por el *INEE* en los siguientes enlaces:
http://www.inee.edu.mx/
http://www.inee.edu.mx/index.php/planea/bases-de-datos-planea 
