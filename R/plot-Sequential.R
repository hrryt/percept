#' @title Sequential Model Plot
#'
#' @description
#' Plots the structure of a [Sequential] model.
#'
#' @param max_nodes Maximum number of nodes plotted for each layer.
#' @param main Title of the plot.
#' @param colors Colors of negative, zero, and positive weights respectively.
#' @param ... Ignored.
#' @return `NULL`.
#'
#' @export
plot.Sequential <- function(x, max_nodes = 30, main = "Sequential Model",
                            colors = c("red", "white", "blue"), ...) {
  stopifnot(
    "all layers must be dense" = all(
      vapply(x$layers, inherits, logical(1), "Dense")
    ),
    "all layers must have weights" = !any(
      vapply(x$layers, \(layer) is.null(layer$weights), logical(1))
    )
  )
  ns_nodes <- vapply(x$layers, \(layer) layer$units, numeric(1))
  in_nodes <- if(is.null(x$input_units)) max_nodes + 1 else x$input_units
  ns_nodes <- c(in_nodes, ns_nodes)
  activators <- vapply(x$layers, \(layer) layer$activation$name, character(1))
  weights <- lapply(x$layers, \(layer) layer$weights)
  plot_network(
    ns_nodes, activators, weights, max_nodes = max_nodes,
    main = main, colors = colors
  )
}
