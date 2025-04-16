#' Plot Histogram for a Numeric Column in a Data Frame
#'
#' This function takes a data frame and a column name and plots a histogram of the selected column.
#'
#' @param data A data frame.
#' @param column A string. The name of the numeric column to plot.
#'
#' @return A ggplot2 histogram object.
#' @import ggplot2
#' @examples
#' plot_histogram(mtcars, "mpg")
#'
#' @export
plot_histogram <- function(data, column, title) {
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!column %in% colnames(data)) stop("column not found in data")
  if (!is.numeric(data[[column]])) stop("column must be numeric")

  ggplot2::ggplot(data, ggplot2::aes_string(x = column)) +
    ggplot2::geom_histogram(fill = "steelblue", color = "white", bins = 30) +
    ggplot2::labs(title = paste(title), x = column, y = "Count") +
    ggplot2::theme_minimal()
}

