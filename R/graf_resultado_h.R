#' Grafica de resultados, horizontal
#'
#' Genera una grafica a partir de una tabla creada con media_poblacion. Funciona
#' con resultados de multiples grupos.
#'
#' @param tabla_resultado Tabla generada con \strong{media_poblacion} o
#' \strong{puntuacion_plausible}.
#'
#' @return
#' Un objeto grafico con orientacion horizontal.
#'
#' @author
#' Juan Bosco Mendoza Vega

#Funcion creada con R 3.3.1 ----

#' @export
graf_resultado_h <- function(tabla_resultado) {
    if (
        identical(names(tabla_resultado),
                  c("Grupo",
                    "Media",
                    "Error_estandar",
                    "Intervalo_superior",
                    "Intervalo_inferior"))
    ) {
        plot(y = seq_along(tabla_resultado$Grupo),
             x = tabla_resultado$Media,
             yaxt = "n",
             xlim = c(min(tabla_resultado$Intervalo_inferior),
                      max(tabla_resultado$Intervalo_superior)),
             ylab = "Grupo",
             xlab = "Media",
             pch = 16)
        arrows(y0 = seq_along(tabla_resultado),
               y1 = seq_along(tabla_resultado),
               x0 = tabla_resultado$Intervalo_inferior,
               x1 = tabla_resultado$Intervalo_superior,
               code = 3,
               angle = 90,
               length = .15)
        axis(side = 2,
             at = seq_along(tabla_resultado$Grupo),
             labels = tabla_resultado$Grupo, angle = 90)
    } else {
        stop("Tabla no valida")
    }
}
