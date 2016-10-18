#Funcion creada con R 3.3.1 ----

#' Estimacion de porcentajes
#'
#' Estima porcentajes con su error estandar e intervalos de confianza al 95%.
#'
#' @param tabla data.frame con datos de Excale o Planea.
#' @param variable Variable de agrupacion
#' @param prefijo Prefijo de los pesos muestrales replicados
#' @param peso_final Peso muestral combinado
#'
#' @author
#' Juan Bosco Mendoza Vega

#' @export
estimacion_porcentaje <- function(tabla, variable, prefijo, peso_final){
  
  tabla[variable][is.na(tabla[variable])] <- "Perdidos"
  
  tabla[[variable]] <- as.factor(tabla[[variable]])
  
  vec_pesos <-append(peso_final,
                     grep(x = names(tabla),
                          prefijo,
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
      # pesos y 0.05 es el valor fijado para el coeficiente de Fey
      
      error_estandar <- sqrt(
        sum(
          ( prop_parcial[1] - prop_parcial[2:length(vec_pesos)] ) ^ 2 ) / 25
      )
      
      resumen_parcial <- data.frame(
        Porcentaje = total_prop,
        Error_estandar = error_estandar,
        # Intervalos al 95%
        Intervalo_superior = total_prop + (error_estandar * 1.96),
        Intervalo_inferior = total_prop - (error_estandar * 1.96)
      )
      round(resumen_parcial * 100, 2)
    }
  )
  
  tabla_final <- do.call(rbind, resumen_general)
  
  tabla_final <- cbind(
    Grupo = rownames(tabla_final),
    tabla_final
  )
  rownames(tabla_final) <- NULL
  
  tabla_final
}
