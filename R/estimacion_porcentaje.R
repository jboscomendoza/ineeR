#Funcion creada con R 3.4.0 ----

#' Estimacion de porcentajes
#'
#' Estima porcentajes de variables con su error estandar e intervalos de confianza al 95%.
#'
#' @param tabla Data frame u objeto coercionable a data.frame con datos de Excale o Planea.
#' @param variable Cadena de texto con la variable de la que se estimaran porcentajes.
#' @param prefijo Cadena de texto con el prefijo de las variables que contienen los pesos muestrales replicados.
#' @param peso_final Cadena de texto con el nombre de la variable que contiene el peso muestral combinado.
#' @param grupo Cadena de texto opcional con el nombre de la variable o variables de agrupacion que se usaron para estimar porcentajes.
#' @param cifras_decimales Numero de cifras decimales que se mostraran en los resultados.
#'
#' @return Una Data frame que contiene la estimacion de porcentaje para el argumento variable, la estimacion del error estandar asociado a cada porcentaje y los limites inferior y superior de su intervalo de confianza al 95%.
#'
#' @examples
#' # Una Data frame con los resultados de Planea 2015
#' planea2015
#'
#' # Porcentaje de Sexo ("SEXO") con cuatro cifras decimales
#' estimacion_porcentaje(tabla = planea2015, variable = "SEXO",
#' prefijo = "W_FSTR", peso_final = "W_FSTUWT", cifras_decimales = 4)
#'
#' # Porcentaje de Sexo ("SEXO"), agrupado por servicio educativo ("SERV")
#' estimacion_porcentaje(tabla = planea2015, variable = "SEXO",
#' prefijo = "W_FSTR", peso_final = "W_FSTUWT", grupo = "SERV)
#'
#' @author
#' Juan Bosco Mendoza Vega

#' @export
estimacion_porcentaje <- function(tabla, variable, prefijo, peso_final, grupo = NULL, cifras_decimales = 2) {
    calculos <- function(elemento) {
      elemento[variable][is.na(elemento[variable])] <- "Perdidos"
      elemento[[variable]] <- as.factor(elemento[[variable]])
      vec_pesos <- append(peso_final, grep(x = names(elemento), pattern = prefijo, 
                                           value = T))
      vec_sumatoria <- as.vector(sapply(elemento[vec_pesos], function(peso) sum(peso, 
                                                                                na.rm = T)))
      elemento_split <- split(elemento, elemento[[variable]])
      var_sumatoria_grupos <- lapply(elemento_split, function(grupo) {
        sapply(grupo[, vec_pesos], sum, na.rm = T)
      })
      var_proporcion <- lapply(var_sumatoria_grupos, function(sumatoria) {
        sumatoria/vec_sumatoria
      })
      total_prop <- NULL
      resumen_general <- lapply(var_proporcion, function(prop_parcial) {
        total_prop <- prop_parcial[1]
        error_estandar <- sqrt(sum((prop_parcial[1] - prop_parcial[2:length(vec_pesos)])^2)/25)
        resumen_parcial <- data.frame(Porcentaje = total_prop, 
                                      Error_estandar = error_estandar, Intervalo_inferior = total_prop - 
                                        (error_estandar * 1.96), Intervalo_superior = total_prop + 
                                        (error_estandar * 1.96))
        round(resumen_parcial * 100, cifras_decimales)
      })
      do.call(rbind, resumen_general)
    }
    if (is.null(grupo)) {
      tabla_final <- calculos(tabla)
    } else {
      tabla_final <-  do.call(what = rbind, 
                              args = lapply(split(tabla, tabla[[grupo]]), calculos))
    }
    tabla_final <- cbind(
      Grupo = rownames(tabla_final),
      tabla_final
    )
    rownames(tabla_final) <- NULL
    tabla_final
  }
