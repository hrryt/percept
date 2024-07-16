#' @title Input Layer
#'
#' @description
#' An input [Layer] to optionally specify in a neural network model.
#'
#' @export
Input <- R6::R6Class("Input", inherit = Layer, list(

  #' @field units Number of nodes in the layer.
  units = NULL,

  #' @description Creates a new instance of this [R6][R6::R6Class] class.
  #' @param units Number of nodes in the layer.
  #' @return `NULL`.
  #' @examples
  #' model <- Sequential$new(list(
  #'   Dense$new(20, ReLU),
  #'   Dense$new(10, Sigmoid)
  #' ))
  #' model
  #' model$layers
  #' model2 <- Sequential$new(list(
  #'   Input$new(40),
  #'   Dense$new(20, ReLU),
  #'   Dense$new(10, Sigmoid)
  #' ))
  #' model2
  #' model2$layers
  initialize = function(units){
    self$units <- units
  }
))
