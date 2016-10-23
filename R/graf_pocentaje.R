#' Grafica de porcentajes
#'
#' Genera una grafica a partir de una tabla creada con \strong{estimacion_porcentaje}.
#'
#' @param tabla_resultado Tabla generada con \strong{estimacion_porcentaje}.
#'
#' @return
#' Un objeto grafico.
#'
#' @author
#' Juan Bosco Mendoza Vega

#Funcion creada con R 3.3.1 ----

#' @export
graf_porcentaje <- function(tabla_porcentaje) {
    barra <- barplot(
        height = tabla_porcentaje$Porcentaje,
        names.arg = tabla_porcentaje$Grupo,
        ylim = c(0,
                 max(tabla_porcentaje$Intervalo_superior)),
        xlab = "Grupo",
        ylab = "Porcentaje"
    )
    arrows(x0 = barra,
           x1 = barra,
           y0 = tabla_porcentaje$Intervalo_inferior,
           y1 = tabla_porcentaje$Intervalo_superior,
           code = 3,
           angle = 90,
           length = .15)
}
