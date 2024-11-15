#' @title Dense Layer
#'
#' @description
#' A fully (densely) connected neural network [Layer].
#'
#' @export
Dense <- R6::R6Class("Dense", inherit = Layer, list(

  #' @field units Number of nodes in the layer.
  units = NULL,

  #' @field activation [Activation] function to apply to the output.
  activation = NULL,

  #' @field weights Matrix of connection weights between this layer
  #' and the previous layer in the model.
  weights = NULL,

  #' @field input The input supplied to the layer upon feed forward.
  input = NULL,

  #' @field output The output of the layer upon feed forward.
  output = NULL,

  #' @field deltas Delta value of each node calculated upon backpropagation.
  deltas = NULL,

  #' @description Creates a new instance of this [R6][R6::R6Class] class.
  #' @param units Number of nodes in the layer.
  #' @param activation [Activation] function to apply to the output.
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
  initialize = function(units, activation = Linear){
    self$units <- units
    self$activation <- activation
    invisible()
  },

  #' @description Randomly initialises weights based on the number of input nodes.
  #' @param input_units Number of input nodes to the layer.
  #' @return `NULL`.
  create_weights = function(input_units){
    self$weights <- sqrt(1/input_units) * matrix(
      runif(input_units * self$units), self$units
    )
    invisible()
  },

  #' @description Feeds an input through the layer.
  #' @param input Numeric array of size `(batch_size, input_units)`.
  #' @return Output of size `(batch_size, output_units)`.
  feed_forward = function(input){ # (B,I)
    self$input <- input
    if(is.null(self$weights)) self$create_weights(dim(input)[2])
    self$output <- self$activation$fun(tcrossprod(input, self$weights)) # (B,O) <- (B,I) %*% t(O,I)
  }
))
