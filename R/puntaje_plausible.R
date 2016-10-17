#' Estimacion de medias poblacionales para variables con Puntuaciones Plausibles
#'
#' Estima media poblacional, error estandar asociado limites superior e inferior
#' del intervalo de confianza al 95%.
#'
#' @param tabla Data frame con una tabla de datos de Excale o Planea
#' @param sufijo Cadena de texto que identifica un grupo de puntuaciones plausibles
#' @param prefijo Cadena de texto que identifica a los pesos muestrales replicados
#' @param peso_final Peso final combinado
#' @param grupo Variable de agrupacion. Ningun grupo por defecto
#'
#' @details Las puntuaciones poblacionales en Excale y Planea se calculan
#' con el metodo de puntuaciones plausibles. Para cada estudiante se estiman
#' cinco puntuaciones plausibles, para incrementar la precision y reducir el
#' error de estimacion debido al muestreo.
#'
#' @author
#' Juan Bosco Mendoza Vega
#'
#' @references
#' OECD (2009) PISA Data Analysis Manual: SPSS and SAS, Second Edition. OECD.

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
    do.call(rbind,
            lapply(lis_grupo, function(x){
              grp <- row.names(x)
              cbind(x, grp)})
    )

  lis_grupo <-
    split(tab_grupo, tab_grupo$grp)

  varianza <- function(df_plausible){

    df_plausible$sample_var <- df_plausible$Error_estandar ^2
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
      Intervalo_superior = media_plausible + (1.96 * error_plausible),
      Intervalo_inferior = media_plausible - (1.96 * error_plausible)
    )

  }

  tabla_final <- do.call(rbind, lapply(lis_grupo, varianza))
    
  tabla_final <- cbind(Grupo = rownames(tabla_final),
                       tabla_final)
    
  rownames(tabla_final) <- c()
    
  tabla_final
  
}
