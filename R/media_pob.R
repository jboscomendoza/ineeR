#' Estimacion de media poblacional
#'
#' Estima media poblacional y su error estándar asociado con limites inferior y
#' superior del intervalo de confianza al 95\%.
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
#'   La variable para la que se desea estimar una media, debe ser numérica.
#'
#'   El peso muestral final (w_final) es un valor único que siempre inicia con
#'   "W" y nunca es seguido por numéros.
#'
#'   Los pesos muestrales replicados (w_rep) siempre se encuentran seguidos por
#'   numeros, del 1 hasta el total de pesos. Por ejemplo, Planea 2018 se
#'   tuvieron cien pesos replicados, desde "W_FSTR1" hasta "W_FSTR100". El
#'   argumento w_rep requiere los primeros caracteres, sin los numéros.
#'
#'   El argumento grupo puede ser cualquier variable de tipo cadena de texto o
#'   factor para funcionar correctamente. Si se desean mas de dos niveles de
#'   agrupacion, se debe proporcionar un vector de texto, por ejemplo
#'   c("SERV", "SEXO").
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
#' @author Juan Bosco Mendoza Vega
#'
#' @references OECD (2009) PISA Data Analysis Manual: SPSS and SAS, Second
#' Edition. OECD.
#'
#' @examples
#' # Data frame con resultados de Planea 2018 de Nayarit.
#' # (Las estimaciones pueden ser imprecisas por ser un fragmento de todos los
#' # datos, se presentan unicamente con fines demostrativos.)
#'
#' p18_nay
#'
#' # Media de RFAB.
#' media_pob(x = p18_nay, variable = "RFAB", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR")
#'
#' # Media de RFAB por Servicio.
#' media_pob(x = p18_nay, variable = "RFAB", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR", grupo = "SERV")
#'
#' # Es posible obtener resultados para más de un nivel de agrupacion, pero se
#' # incrementa el error de estimación.
#' # Media de RFAB por Servicio y Sexo.
#' media_pob(x = p18_nay, variable = "RFAB", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR", grupo = c("SERV", "SEXO"))

#' @export
media_pob <- function(x, variable, w_final, w_rep, grupo = NULL){
  vector_w_rep <- verifica_w_rep(x, w_rep)

  # Verifica si se ha especificado un grupo
  if(!is.null(grupo)){
    # Si hay un grupo, tabla se divide a partir de este
    x <- split(x, x[grupo])
    # Se aplica ee la tabla dividida. do.call devuelve una mejor presentacion
    tabla_final <- do.call(
      what = rbind,
      lapply(x, function(var_x){
        ee(var_x, variable, w_final, vector_w_rep)
      })
    )
    # Si no hay grupo, se aplica ee a la tabla
  } else {
    tabla_final <- ee(x, variable, w_final, vector_w_rep)
  }

  tabla_final <- cbind(Variable = variable, Grupo = rownames(tabla_final),
                       tabla_final)

  if (is.null(grupo)) tabla_final$Grupo <- "Poblacion"

  rownames(tabla_final) <- NULL

  tabla_final
}
