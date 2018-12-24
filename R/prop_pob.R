#' Estimacion de proporción.
#'
#' Estima proporciónes de variables, expresado como porcentaje, y su error
#' estándar asociado con límites inferior y superior del intervalos de confianza
#' al 95\%.
#'
#' @encoding UTF-8
#'
#' @param x Data frame. Una tabla de datos de Excale o Planea.
#' @param variable Cadena de texto. Nombre de la variable para la que se
#'   estimará la proporción. La variable debe ser de tipo cadena de texto o
#'   factor.
#' @param w_final Cadena de texto. Nombre del peso muestral final.
#' @param w_rep Cadena de texto. Prefijo que identifica a los pesos muestrales
#'   replicados.
#' @param grupo Cadena de texto o vector de texto. Variable o variables de
#'   agrupación. Por defecto, ninguna.
#'
#' @return Un data frame con las siguientes columnas:
#' \itemize{
#'   \item Variable: Variable para la que se hizo la estimación.
#'   \item Grupo: Variable de agrupación. "Poblacion" si no se especificó
#'     ninguna.
#'   \item Porcentaje: proporción estimada.
#'   \item EE: Error estándar estimado.
#'   \item Lim_inf y Lim_sup: límites inferior y superior del intervalo de
#'     confianza estimado al 95\%.
#' }
#'
#' @details
#'   La variable para la que se desea estimar una proporción debe ser de tipo
#'   cadena de texto o factor. Si se intenta estimar una proporción con una
#'   variable numérica, se mostrara una advertencia.
#'
#'   El peso muestral final (w_final) es un valor único que siempre inicia con
#'   "W" y nunca es seguido por números.
#'
#'   Los pesos muestrales replicados (w_rep) siempre se encuentran seguidos por
#'   numeros, del 1 hasta el total de pesos. Por ejemplo, Planea 2018 se
#'   tuvieron cien pesos replicados, desde "W_FSTR1" hasta "W_FSTR100". El
#'   argumento w_rep requiere los primeros caracteres, sin los números.
#'
#'   El argumento grupo puede ser cualquier variable de tipo cadena de texto o
#'   factor para funcionar correctamente. Si se desean mas de dos niveles de
#'   agrupación, se debe proporciónar un vector de texto, por ejemplo
#'   c("SERV", "SEXO").
#'
#' @examples
#' # Data frame con resultados de Planea 2018 de Nayarit.
#' # (Las estimaciones pueden ser imprecisas por ser un fragmento de todos los
#' # datos, se presentan unicamente con fines demostrativos.)
#'
#' p18_nay
#'
#' # Proporción de estudiantes por Sexo.
#' prop_pob(x = p18_nay, variable = "SEXO", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR")
#'
#' # Proporción de estudiantes por Sexo y por Servicio.
#' prop_pob(x = p18_nay, variable = "SEXO", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR", grupo = "SERV")
#'
#' # Es posible obtener resultados para mas de un nivel de agrupación, pero se
#' # incrementa el error de estimación.
#' # Porporción de estudiantes por Sexo, por Servicio y Nivel de Marginación.
#' prop_pob(x = p18_nay, variable = "SEXO", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR", grupo = c("SERV", "MARGINC"))
#'
#' @author Juan Bosco Mendoza Vega
#'
#' @references OECD (2009) PISA Data Analysis Manual: SPSS and SAS, Second
#' Edition. OECD.

#' @export
prop_pob <- function(x, variable, w_final, w_rep, grupo = NULL) {

  if (is.numeric(x[[variable]])) {
    warning(paste0(variable, " es de tipo numerico. Verifica que esto sea correcto."))
  }

  x[[variable]] <- as.character(x[[variable]])
  x[variable][is.na(x[variable])] <- "Perdidos"

  if (is.null(grupo)) {
    tabla_final <- prop_calc(x, variable, w_final, w_rep)
  } else {
    x[grupo] <- lapply(x[grupo], as.character)
    x[grupo][is.na(x[grupo])] <- "Perdidos"

    tabla_final <- lapply(split(x, x[grupo]), prop_calc, variable,
                          w_final, w_rep)
    tabla_final <-  do.call(what = rbind, args = tabla_final)
  }

  tabla_final <- cbind(
    Variable = variable,
    Grupo = rownames(tabla_final),
    tabla_final
  )

  rownames(tabla_final) <- NULL

  tabla_final
}
