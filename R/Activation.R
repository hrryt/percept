#' @title Activation Function
#'
#' @description
#' An activation function to specify for a neural network [Layer].
#'
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
#'
#' @export
Activation <- R6::R6Class("Activation", list(

  #' @field fun Activation function.
  fun = NULL,

  #' @field der Derivative function of the activation function.
  der = NULL,

  #' @description Creates a new instance of this [R6][R6::R6Class] class.
  #' @param fun Activation function.
  #' @param der Derivative function of the activation function.
  #' @return `NULL`.
  initialize = function(fun, der){
    self$fun <- fun
    self$der <- der
    NULL
  }
))

#' @rdname Activation
#' @export
Linear <- Activation$new(\(x) x, \(x) 1)

#' @rdname Activation
#' @export
Sigmoid <- Activation$new(\(x) 1/(1+exp(-x)), \(x) x*(1-x))

#' @rdname Activation
#' @export
ReLU <- Activation$new(\(x) {x[x<0] <- 0; x}, \(x) as.integer(x>0))
