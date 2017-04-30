#' Media poblacional
#'
#' Estima la media poblacional con error estandar e intervalos de confianza de
#' de una variable. Es posible estimar medias por subpoblaciones usando una
#' variable de agrupacion.
#'
#' @param tabla Data frame con una tabla de datos de Excale o Planea.
#' @param variable Cadena de texto con el nombre de la  variable a la que se
#'   estimara media poblacional.
#' @param prefijo Cadena de texto con el prefijo de los pesos muestrales
#'   replicados.
#' @param peso_final Cadena de texto con el nombre de la variable con el peso
#'   muestral combinado.
#' @param grupo Cadena de texto con el nombre de la variable de agrupacion.
#'   Pueden ser mas de una variable de agrupacion. Por defecto, ninguna.
#'
#' @return Una Data frame que contiene la media poblacional de la variable
#'   seleccionada , la estimacion del error estandar asociado a cada porcentaje
#'   y los limites inferior y superior de su intervalo de confianza al 95%.
#'
#' @author Juan Bosco Mendoza Vega
#'
#' @references OECD (2009) PISA Data Analysis Manual: SPSS and SAS, Second
#' Edition. OECD.
#'
#' @examples
#' # Una Data frame con los resultados de Planea 2015
#' planea2015
#'
#' # Porcentaje de Edad ("EDAD") con cuatro cifras decimales
#' media_poblacional(tabla = planea2015, variable = "EDAD",
#' prefijo = "W_FSTR", peso_final = "W_FSTUWT", cifras_decimales = 4)
#'
#' # Media de Edad ("EDAD"), agrupado por servicio educativo ("SERV")
#' media_poblacional(tabla = planea2015, variable = "EDAD",
#' prefijo = "W_FSTR", peso_final = "W_FSTUWT", grupo = "SERV")
#'
#' # Es posible estimar medias con mas de una variable de agrupacion, pero esto produce estimaciones con errores altos.
#' #' media_poblacional(tabla = planea2015, variable = "EDAD",
#' prefijo = "W_FSTR", peso_final = "W_FSTUWT", grupo = c("NOM_ENT", "SERV"))

#' @export
media_poblacion <-
  function(tabla, variable, prefijo, peso_final, grupo = NULL){

    # Funcion ee
    ee <- function(tabla, variable, prefijo, peso_final) {
      # Nombre de las columnas con pesos
      var_nombres <- grep(x = names(tabla), pattern = prefijo, value = T)

      # Vector con la variable a estimar
      var_variable <- tabla[[variable]]

      # Imputamos valores usando la media. No es ideal, pero es eficiente
      var_variable[is.na(var_variable)] <- mean(var_variable, na.rm = T)

      # Vector para iniciar media ponderadas
      var_medias <- NULL

      # Loop para generar los valores de var_medias
      lapply(var_nombres, function(nombre) {
        peso <- tabla[[nombre]]
        # Eliminamos NA, usando la media
        peso[is.na(peso)] <- mean(peso, na.rm = T)
        # Valor a var_medias
        var_medias <<- c(var_medias,
                         # Media ponderada de la variable con el peso
                         weighted.mean(var_variable, peso, na.rm = T))
      })

      # Media ponderada con peso combinado
      var_media_combi <-
        weighted.mean(tabla[[variable]], tabla[[peso_final]], na.rm = T)

      # Calculo de error estandar
      var_errorestandar <-
        sqrt(
          sum(
            # Esta operacion esta vectorizada
            ( var_medias - var_media_combi ) ^ 2
            # Este 25 es igual a 100 * (1 - 0.5) ^ 2, donde 100 es el numero de
            # pesos y 0.05 es el valor fijado para el coeficiente de Fey
          ) / 25
        )

      # Tabla resumen
      data.frame(
        Media = var_media_combi,
        Error_estandar = var_errorestandar,
        Intervalo_inferior = var_media_combi - ( 1.96 * var_errorestandar ),
        Intervalo_superior = var_media_combi + ( 1.96 * var_errorestandar )
      )
    }

    # Verifica si se ha especificado un grupo
    if(!is.null(grupo)){
      # Si hay un grupo, tabla se divide a partir de este
      tabla <- split(tabla, tabla[grupo])
      # Se aplica ee la tabla dividida. do.call devuelve una mejor presentacion
      tabla_final <- do.call(
        what = rbind,
        lapply(tabla, function(x){
          ee(x, variable, prefijo, peso_final)
        })
      )
      # Si no hay grupo, se aplica ee a la tabla
    } else {
      tabla_final <- ee(tabla, variable, prefijo, peso_final)
    }

    tabla_final <- cbind(Variable = variable,
                         Grupo = rownames(tabla_final),
                         tabla_final)

    if (is.null(grupo)) {
      tabla_final$Grupo <- "Nacional"
    }

    rownames(tabla_final) <- NULL

    tabla_final

  }

