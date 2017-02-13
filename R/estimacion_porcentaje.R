#Funcion creada con R 3.3.1 ----

#' Estimacion de porcentajes
#'
#' Estima porcentajes de variables, su error estandar e intervalos de confianza al 95%.
#'
#' @param tabla Data frame u objeto coercionable a data.frame con datos de Excale o Planea.
#' @param variable Cadena de texto con la variable de la que se estimaran porcentajes
#' @param prefijo Cadena de texto con el prefijo de las variables que contienen los pesos muestrales replicados
#' @param peso_final Cadena de texto con el nombre variable que contiene el peso muestral combinado
#' @param cifras_decimales El numero de cifras decimales que se mostraran en los resultados.
#'
#' @return Una Data frame que contiene la estimacion de porcentaje para el argumento variable, la estimacion del error estandar asociado a cada porcentaje y los limites inferior y superior de su intervalo de confianza al 95%.
#'
#' @examples
#' # Una Data frame con los resultados de Planea 2015
#' planea2015
#'
#' estimacion_porcentaje(tabla = planea2015, variable = "SEXO",
#' prefijo = "W_FSTR9", peso_final = "W_ALU", cifras_decimales = 4)
#'
#' @author
#' Juan Bosco Mendoza Vega

#' @export
estimacion_porcentaje <- function(tabla, variable, prefijo, peso_final, cifras_decimales = 2){

  tabla[variable][is.na(tabla[variable])] <- "Perdidos"

  tabla[[variable]] <- as.factor(tabla[[variable]])

  vec_pesos <-append(peso_final,
                     grep(x = names(tabla),
                          pattern = prefijo,
                          value = T))

  vec_sumatoria <- as.vector(
    sapply(tabla[vec_pesos],
           function(peso)
             sum(peso, na.rm = T)))

  tabla_split <- split(tabla, tabla[[variable]])

  var_sumatoria_grupos <- lapply(tabla_split,
                                 function(grupo) {
                                   sapply(grupo[, vec_pesos],
                                          sum, na.rm = T)
                                 })

  var_proporcion <- lapply(var_sumatoria_grupos, function(sumatoria) {
    sumatoria / vec_sumatoria
  })

  total_prop <- NULL

  resumen_general <- lapply(
    var_proporcion, function(prop_parcial){
      total_prop <- prop_parcial[1]

      # proporcion[1] es con peso combinado, de proporcion[2] en adelante con
      # los combinados
      # Todas las diferencias de proporcion[1] con proporcion[2] se elevan al
      # cuadrado, divide entre 25 y obtiene raiz cuadrada
      # Este 25 es igual a 100 * (1 - 0.5) ^ 2, donde 100 es el numero de
      # pesos y 0.05 es el valor fijado para el coeficiente de Fay

      error_estandar <- sqrt(
        sum(
          ( prop_parcial[1] - prop_parcial[2:length(vec_pesos)] ) ^ 2 ) / 25
      )

      resumen_parcial <- data.frame(
        Porcentaje = total_prop,
        Error_estandar = error_estandar,
        # Intervalos al 95%
        Intervalo_inferior = total_prop - (error_estandar * 1.96),
        Intervalo_superior = total_prop + (error_estandar * 1.96)
      )
      round(resumen_parcial * 100, cifras_decimales)
    }
  )

  do.call(rbind, resumen_general)

}
