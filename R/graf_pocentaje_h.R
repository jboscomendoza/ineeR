#' Grafica de porcentajes, hprizontal
#'
#' Genera una grafica a partir de una tabla creada con \strong{estimacion_porcentaje}.
#'
#' @param tabla_resultado Tabla generada con \strong{estimacion_porcentaje}.
#'
#' @return
#' Un objeto grafico, con orientacion horizontal.
#'
#' @author
#' Juan Bosco Mendoza Vega

#Funcion creada con R 3.3.1 ----

#' @export
graf_porcentaje_h <- function(tabla_porcentaje) {
    barra <- barplot(horiz = T,
                     height = tabla_porcentaje$Porcentaje,
                     names.arg = tabla_porcentaje$Grupo,
                     xlim = c(0,
                              max(tabla_porcentaje$Intervalo_superior)),
                     xlab = "Grupo",
                     ylab = "Porcentaje"
    )
    arrows(y0 = barra,
           y1 = barra,
           x0 = tabla_porcentaje$Intervalo_inferior,
           x1 = tabla_porcentaje$Intervalo_superior,
           code = 3,
           angle = 90,
           length = .15)
}
