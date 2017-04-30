#' Estimacion de medias poblacionales para variables con Puntuaciones Plausibles
#'
#' Estima media poblacional, error estandar asociado limites superior e inferior
#' del intervalo de confianza al 95%.
#'
#' @param tabla Data frame con una tabla de datos de Excale o Planea
#' @param sufijo Cadena de texto que identifica un grupo de puntuaciones
#'   plausibles
#' @param prefijo Cadena de texto que identifica a los pesos muestrales
#'   replicados
#' @param peso_final Peso final combinado
#' @param grupo Cadena de texto con la variable de agrupacion. Pueden ser mas de
#'   un nivel de agrupacion. Por defecto, ninguna
#'
#' @details Las puntuaciones poblacionales en Excale y Planea se calculan con el
#'   metodo de puntuaciones plausibles. Para cada estudiante se estiman cinco
#'   puntuaciones plausibles, con el fin de incrementar la precision y reducir
#'   el error de estimacion debido al muestreo. #'
#' @examples
#' # Data frame con resultados de Planea 2015.
#' planea2015
#'
#' # Puntaje en Matematicas.
#' puntaje_plausible(tabla = planea2015, sufijo = "MAT", prefijo = "W_FSTR",
#' peso_final = "W_ALU")
#'
#' # Puntaje en Matematicas por entidad.
#' puntaje_plausible(tabla = planea2015, sufijo = "MAT", prefijo = "W_FSTR",
#' peso_final = "W_ALU", grupo = "NOM_ENT")
#'
#' # Es posible pedir puntajes por más de un nivel de agrupacion, pero esto produce estimaciones con errores altos.
#'
#' @author Juan Bosco Mendoza Vega
#'
#' @references OECD (2009) PISA Data Analysis Manual: SPSS and SAS, Second
#'   Edition. OECD.

#' @export
puntaje_plausible <- function(tabla, sufijo, prefijo, peso_final, grupo = NULL){
  vec_plausible <-
    grep(pattern = paste0("^PV.", sufijo, "$"),
         x = names(tabla),
         value = T)

  lis_grupo <-
    lapply(vec_plausible, function(x){
      media_poblacion(tabla, x, prefijo, peso_final, grupo)
    })

  tab_grupo <-
    do.call(rbind, lis_grupo)

  lis_grupo <-
    split(tab_grupo, tab_grupo$Grupo)

  varianza <- function(df_plausible){

    df_plausible$sample_var <- df_plausible$Error_estandar ^ 2
    df_plausible$imput_dif  <- df_plausible$Media - mean(df_plausible$Media)

    media_plausible <-
      mean(df_plausible$Media)

    error_plausible <-
      sqrt(
        mean(df_plausible$sample_var) +
          ( 1.2 * ( sum(df_plausible$imput_dif) / ( length(vec_plausible) - 1 ) ) )
      )

    data.frame(
      Media = media_plausible,
      Error_estandar = error_plausible,
      Intervalo_inferior = media_plausible - (1.96 * error_plausible),
      Intervalo_superior = media_plausible + (1.96 * error_plausible)
    )

  }

  tabla_final <- do.call(rbind, lapply(lis_grupo, varianza))

  tabla_final <- cbind(Grupo = rownames(tabla_final),
                       tabla_final)

  rownames(tabla_final) <- NULL

  tabla_final
}
