#' Estimación de media poblacional para variables con valores plausibles.
#'
#' Estima media poblacional para variables con valores plausibles y su error
#' estándar asociado con limites inferior y superior del intervalo de confianza
#' al 95\%.
#'
#' @encoding UTF-8
#'
#' @param x Data frame. Una tabla con datos de Excale o Planea.
#' @param variable Cadena de texto. Nombre de la variable para la que se
#'   estimará la media. La variable debe ser de tipo numérico.
#' @param w_final Cadena de texto. Nombre del peso muestral final.
#' @param w_rep Cadena de texto. Prefijo que identifica a los pesos muestrales
#'   replicados.
#' @param grupo Cadena de texto o vector de texto. Variable o variables de
#'   agrupacion. Por defecto, ninguna.
#'
#' @details
#'   Las medias poblacionales en Excale y Planea para ciertas variables,
#'   en particular los puntajes de logro, son estimadas a partir de valores
#'   plausibles.
#'
#'   Para cada individuo se estiman distintos valores plausibles,
#'   generalmente cinco, pero pueden ser mas. Los valores
#'   plausibles son, en términos generales, valores aleatorios estimados a
#'   partir de la distribución observada de una variable. Tienen como fin
#'   reducir el error de estimación debido al diseño de la medición.
#'
#'   Las variables con valores plausibles son identificadas en Excale y Planea
#'   con el prefijo "PV" seguido de un número, del 1 al total de valores
#'   plausibles. Por ejemplo, en Planea 2018, el logro en Matemáticas, con cinco
#'   valores plausibles se encuentra en las variables "PV1MAT" a "PV5MAT". Deben
#'   ser de tipo numérico.
#'
#'   El peso muestral final (w_final) es un valor único que siempre inicia con
#'   "W" y nunca es seguido por numéros.
#'
#'   Los pesos muestrales replicados (w_rep) siempre se encuentran seguidos por
#'   números, del 1 hasta el total de pesos. Por ejemplo, Planea 2018 se
#'   tuvieron cien pesos replicados, desde "W_FSTR1" hasta "W_FSTR100". El
#'   argumento w_rep requiere los primeros caracteres, sin los números.
#'
#'   El argumento grupo debe ser cualquier variable de tipo cadena de texto o
#'   factor para funcionar correctamente. Si se desean estimaciones para  mas de
#'    dos niveles de agrupacion, se debe proporcionar un vector de texto, por
#'    ejemplo c("SERV", "SEXO").
#'
#' @return Un data frame con las siguientes columnas:
#' \itemize{
#'   \item Variable: Variable para la que se hizo la estimación.
#'   \item Grupo: Variable de agrupación. "Poblacion" si no se especificó
#'     ninguna.
#'   \item Media: Media estimada.
#'   \item EE: Error estándar estimado.
#'   \item Lim_inf y Lim_sup: Límites inferior y superior del intervalo de
#'     confianza estimado al 95\%.
#' }
#'
#' @examples
#' # Data frame con resultados de Planea 2018 de Nayarit.
#' # (Las estimaciones pueden ser imprecisas por ser un fragmento de todos los
#' # datos, se presentan unicamente con fines demostrativos.)
#'
#' p18_nay
#'
#' # Media de Matemáticas.
#' media_pv(x = p18_nay, variable = "MAT", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR")
#'
#' # Media de Matemáticas por Servicio.
#' media_pv(x = p18_nay, variable = "MAT", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR", grupo = "SERV")
#'
#' # Es posible obtener resultados para mas de un nivel de agrupación, pero se
#' # incrementa el error de estimación.
#' # Media de Matematicas por Servicio y Sexo.
#' media_pv(x = p18_nay, variable = "MAT", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR", grupo = c("SERV", "SEXO"))
#'
#' @author Juan Bosco Mendoza Vega
#'
#' @references OECD (2009) PISA Data Analysis Manual: SPSS and SAS, Second
#'   Edition. OECD.

#' @export
media_pv <- function(x, variable, w_final, w_rep, grupo = NULL){
  if (!is.numeric(x[[variable]])) {
    warning(paste0(variable, " no es numerica. Verifica que esto sea correcto"))
  }

  vector_pv <-
    grep(pattern = paste0("^PV.", variable, "$"), x = names(x), value = TRUE)

  if (length(vector_pv) == 0)
  {
    stop(paste0(variable, " no tiene valores plausibles."))
  }

  es_numerico <- vapply(x[vector_pv], is.numeric, logical(1))

  if (!sum(es_numerico) == length(vector_pv)) {
    stop(paste0(variable, " no es numerica."))
  }

  lista_grupo <-
    lapply(vector_pv, function(var_x) {
      media_pob(x, var_x, w_final, w_rep, grupo)
    })

  tabla_grupo <- do.call(rbind, lista_grupo)

  lista_grupo <- split(tabla_grupo, tabla_grupo$Grupo)

  lista_varianza <- lapply(lista_grupo, function(var_x) {
    var_plausible(var_x, variable, vector_pv)
  })

  tabla_final <- do.call(rbind, lapply(lista_grupo, var_plausible, variable, vector_pv))

  tabla_final <- cbind(Grupo = rownames(tabla_final), tabla_final)

  rownames(tabla_final) <- NULL

  tabla_final[, c("Variable", "Grupo", "Media", "EE", "Lim_inf", "Lim_sup")]
}
