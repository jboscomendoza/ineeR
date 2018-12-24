#' Gráficas de proporciones.
#'
#' Genera gráficas para proporciones estimadas con prop_pob.
#'
#' @encoding UTF-8
#'
#' @param x Data frame. Resultados de la funcion prop_pob.
#'
#' @import ggplot2
#'
#' @details
#'   Estas funciones usan ggplot2 para generar gráficas sencillas con los
#'   resultados de la función prop_pob.
#'
#' @return
#'   Un objeto gráfico de tipo ggplot.
#'
#' @author Juan Bosco Mendoza Vega
#'
#' @examples
#' # Data frame con resultados de Planea 2018 de Nayarit.
#' # (Las estimaciones pueden ser imprecisas por ser un fragmento de todos los
#' # datos, se presentan unicamente con fines demostrativos.)
#'
#' p18_nay
#'
#' # Estimamos la proporción de la variable MARGINC y asignamos el resultado a
#' # prop_mar.
#' prop_mar <- prop_pob(x = p18_nay, variable = "MARGINC", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR")
#'
#' # Generamos una gráfica de la proporción estimada con graf_prop.
#' graf_prop(x = prop_mar)
#'
#' @export
graf_prop <- function(x) {
  titulo <- x$Variable[1]

  plot_prop <-
    ggplot2::ggplot(x) +
    ggplot2::aes_string(x = "Grupo", y = "Porcentaje") +
    ggplot2::geom_col(color = "black", fill = "#efefef") +
    ggplot2::geom_errorbar(aes_string(ymin = "Lim_inf", ymax = "Lim_sup"),
                           width = .3, alpha = .3) +
    ggplot2::labs(title = titulo) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(0, 105)) +
    ggplot2::theme_minimal()

  plot_prop
}
