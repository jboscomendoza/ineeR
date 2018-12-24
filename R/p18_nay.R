#' Resultados de Nayarit en Planea 2018.
#'
#' Datos con los resultados del estado de Nayarit en Planea 2018, aplicada a
#' estudiantes de sexto de primaria por el Instituto Nacional para la Evaluación
#' de la Educación. Contiene una selección mínima de variables así como los
#' pesos finales y replicados necesarios para estimar errores estándar.
#'
#' @encoding UTF-8
#'
#' @format Un data frame con 2755 renglones y 117 variables:
#' \describe{
#'   \item{NOM_ENT}{Nombre de la entidad}
#'   \item{SERV}{Tipo de servicio de la primaria}
#'   \item{MARGINC}{Nivel de marginación de acuerdo al Consejo Nacional de
#'     Población}
#'   \item{SEXO}{Sexo de los estudiantes}
#'   \item{EDAD}{Edad de los estudiantes. Los meses son expresados como
#'     fracciones decimales}
#'   \item{RFAB}{Variable construida "Recursos Familiares Asociados al
#'     Bienestar". Medidad del nivel socioeconómico de los estudiantes}
#'   \item{W_FSTUWT}{Peso muestral final}
#'   \item{PV1LYC a PV5LYC}{Resultados de logro de los estudiantes en Lenguaje y
#'     Comunicación}
#'   \item{PV1MAT a PV5MAT}{Resultados de logro de los estudiantes en
#'     Matemáticas}
#'   \item{W_FSTR1 a W_FSTR100}{Pesos muestrales replicados}
#' }
#' @source \url{https://www.inee.edu.mx/index.php/planea/bases-de-datos-planea/}
"p18_nay"
