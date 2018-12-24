#' Gráficas de medias-
#'
#' Genera gráficas para medias estimadas con las funciones media_pob y media_pv.
#'
#' @encoding UTF-8
#'
#' @param x Data frame. Resultados de las funciones media_pob o media_pv.
#'
#' @import ggplot2
#'
#' @details
#'   Esta función usa ggplot2 para generar gráficas sencillas con los
#'   resultados de las funciones media_pob y media_pv.
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
#' # Estimamos la media de la variable RFAB y asignamos el resultado a
#' # media_rfab.
#' media_rfab <- media_pob(x = p18_nay, variable = "RFAB", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR")
#'
#' # Generamos una gráfica de la media estimada con graf_media.
#' graf_media(x = media_rfab)
#'
#' # Estimamos la media de la variable MAT, que tiene valores plausibles, y
#' # asignamos el resultado a media_mat.
#' media_mat <- media_pv(x = p18_nay, variable = "MAT", w_final = "W_FSTUWT",
#' w_rep = "W_FSTR")
#'
#' # Generamos una gráfica de la media estimada con graf_media, no hay
#' # diferencia si usamos el resultado de media_pob o media_pv.
#' graf_media(x = media_mat)
#'
#' @export
graf_media <- function(x) {
  titulo <- x$Variable[1]

  plot_media <-
    ggplot2::ggplot(x) +
    ggplot2::aes_string(x = "Grupo", y = "Media") +
    ggplot2::geom_errorbar(aes_string(ymin = "Lim_inf", ymax = "Lim_sup"),
                           width = .2, alpha = .5) +
    ggplot2::geom_point() +
    ggplot2::labs(title = titulo) +
    ggplot2::theme_minimal()

  plot_media
}
