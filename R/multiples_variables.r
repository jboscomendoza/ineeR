#Funcion creada con R 3.3.1 ----

#' Estimacion de media poblacion para multiples variables
#'
#' Estima medias poblacionales con error estandar e intervalo de confianza para
#' multiples variables. No estima para variables con puntuaciones plausibles.
#'
#' @param tabla Un data frame.
#' @param variables Un vector de texto con variables
#' @param prefijo Prefijo de los pesos muestrales replicados
#' @param peso_final Peso muestral combinado
#' @param grupo Variable de agrupacion
#'
#' @details
#'  Deberia funcionar con Excale y Planea por igual. Pero ya veremos
#'
#' @return Tabla con resultados
#'
#' @author
#' Juan Bosco Mendoza Vega
#'
#' @references
#' OECD (2009) PISA Data Analysis Manual: SPSS and SAS, Second Edition. OECD.

#' @export
multiples_variables <- function(tabla, variables, prefijo, peso_final, grupo = NULL){
  lis_grupos <-
    lapply(variables,
           function(cada_var){
             media_poblacion(tabla, cada_var, prefijo, peso_final, grupo)
           })
  
  names(lis_grupos) <- variables
  
  tabla_final <- do.call(rbind, lis_grupos)
  
  tabla_final <- cbind(
    Variable = rownames(tabla_final),
    tabla_final
  )
  
  if ( !is.null(grupo) ) {
    tabla_final$Variable <- gsub(pattern = ".[[:digit:]]$",
                                 replacement = "",
                                 x = tabla_final$Variable)
  } else {
    tabla_final$Grupo <- "Nacional"
  }
  
  rownames(tabla_final) <- NULL
  
  tabla_final
  
}
