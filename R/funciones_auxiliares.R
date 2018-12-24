#' @keywords internal
#'
#' @importFrom stats weighted.mean
#' @importFrom graphics arrows axis barplot plot
#' @encoding UTF-8


# Verifica el numero de pesos replicados
verifica_w_rep <- function(tabla_x, nombre_w_rep) {
  vector_w_rep <- grep(x = names(tabla_x), pattern = nombre_w_rep, value = TRUE)
  numero_w_rep <- length(vector_w_rep)

  if (numero_w_rep < 80) {
    warning(paste0(nombre_w_rep, " tiene menos de 80 replicaciones. Verifica que esto sea correcto."))
  }

  vector_w_rep
}

# Estimacion de error estandar para media poblacional
ee <- function(x, variable, w_final, vector_w_rep) {
  # Coeficiente de Fey
  # Este valor es igual a n_pesos_replicados * (1 - 0.5) ^ 2, generalmente  100.
  # 0.05 es el valor fijado de modo arbitrario para el coeficiente de Fey
  coef_fey <- length(vector_w_rep) * (1 - 0.5) ^ 2

  # Vector con la variable a estimar
  vector_var <- x[[variable]]

  # Imputamos valores usando la media. No es ideal, pero es eficiente
  vector_var[is.na(vector_var)] <- mean(vector_var, na.rm = TRUE)

  # Loop para generar los valores de var_medias
  vector_medias <- vapply(vector_w_rep, FUN.VALUE = numeric(1), function(nombre) {
    peso <- x[[nombre]]
    # Eliminamos NA, usando la media
    peso[is.na(peso)] <- mean(peso, na.rm = TRUE)
    # Valor a var_medias
    # Media ponderada de la variable con el peso
    weighted.mean(vector_var, peso, na.rm = TRUE)
  })

  # Media ponderada con peso combinado
  vector_media_final <-
    weighted.mean(x[[variable]], x[[w_final]], na.rm = TRUE)

  # Calculo de error estandar
  error_estandar <- sqrt(sum((vector_medias - vector_media_final) ^ 2) / coef_fey)

  # Tabla resumen
  data.frame(
    Media = vector_media_final,
    EE = error_estandar,
    Lim_inf = vector_media_final - ( 1.96 * error_estandar ),
    Lim_sup = vector_media_final + ( 1.96 * error_estandar )
  )
}

# Estimacion de error estandar para medias con valores plausibles
var_plausible <- function(x, variable, vec){
  num_pesos <- length(vec)
  media_plausible <- mean(x$Media)

  x$sample_var <- x$EE ^ 2
  x$input_dif  <- x$Media - media_plausible

  media_sample_var <-  mean(x$sample_var)
  suma_input_dif <- sum(x$input_dif)

  error_plausible <-
    sqrt(media_sample_var + (1.2 * (suma_input_dif / num_pesos - 1)))

  data.frame(
    Variable = variable,
    Media = media_plausible,
    EE = error_plausible,
    Lim_inf = media_plausible - (1.96 * error_plausible),
    Lim_sup = media_plausible + (1.96 * error_plausible)
  )
}

# Estimacion de proporciones
prop_calc <- function(x, variable, w_final, w_rep) {
  x[[variable]] <- as.factor(x[[variable]])

  vector_w_rep <- verifica_w_rep(x, w_rep)

  vector_pesos <- append(w_final,
                         grep(x = names(x), pattern = w_rep, value = TRUE))

  coef_fey <- (length(vector_pesos) - 1) * (1 - 0.5) ^ 2

  vector_suma <- vapply(x[vector_pesos], sum, numeric(1), na.rm = TRUE)

  tabla_split <- split(x, x[[variable]])

  var_suma_grupo <- lapply(tabla_split, function(grupo) {
    sapply(grupo[, vector_pesos], sum, na.rm = T)
  })

  var_prop <- lapply(var_suma_grupo, function(sumatoria) {
    sumatoria / vector_suma
  })

  total_prop <- NULL

  lista_final <- lapply(var_prop, function(prop_parcial) {
    total_prop <- prop_parcial[1]
    error_estandar <- sqrt(sum((prop_parcial[1] - prop_parcial[2:length(vector_pesos)]) ^ 2) / coef_fey)

    resumen_parcial <- data.frame(
      Porcentaje = total_prop,
      EE = error_estandar,
      Lim_inf = total_prop - (error_estandar * 1.96),
      Lim_sup = total_prop + (error_estandar * 1.96)
    )
    round(resumen_parcial * 100, 4)
  })

  do.call(rbind, lista_final)
}

