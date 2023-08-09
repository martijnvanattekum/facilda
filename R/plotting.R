#' Creates dot plot using defined columns colored by color_col
#'
#' @param data A data frame or tibble in long format containing the columns
#' specified by x_col, y_col, and color_col
#' @param x_col The column which will be passed as the aes x arg to ggplot
#' @param y_col The column which will be passed as the aes y arg to ggplot
#' @param color_col The column that will be used to color the points
#' @return The plot
#' @examples
#' # plots disp vs mpg colored by gear for mtcars data
#' colored_dot_plot(mtcars, "disp", "mpg", "gear")
#' @importFrom rlang sym
#' @export
colored_dot_plot <- function(data, x_col, y_col, color_col) {

  ggplot2::ggplot(data, ggplot2::aes(x=!!rlang::sym(x_col),
                   y=!!rlang::sym(y_col),
                   color=!!rlang::sym(color_col))) +
    ggplot2::geom_point() +
    ggplot2::theme_bw()

}


#' Creates a grid from plots with a common legend and only prints the legend once
#'
#' Note that no check for equal legends is performed! The legend of the first
#' plot will be used.
#'
#' @param plotlist list of ggplot objects
#' @param ... additional arguments to plot_grid (left panel)
#' @return The plot grid
#' @examples
#' p1 <- p2 <- colored_dot_plot(mtcars, "disp", "mpg", "gear")
#' joint_legends_grid_plot(list(p1,p2))
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 theme
#' @export
joint_legends_grid_plot <- function(plotlist, ...) {

  legends <- map(plotlist, \(this_plot) cowplot::get_legend(this_plot))
  #! stopifnot(all legends have the same labels)
  plotlist_without_legend <- purrr::map(plotlist, ~. + ggplot2::theme(legend.position="none"))
  left_panel <- cowplot::plot_grid(plotlist=plotlist_without_legend, ...)
  right_panel <- legends[[1]]

  cowplot::plot_grid(left_panel, right_panel, rel_widths = c(3, .4))

}


