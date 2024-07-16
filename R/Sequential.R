#' @title Sequential Model
#'
#' @description
#' A neural network model for a plain stack of layers where each [Layer] has
#' one input and one output array.
#'
#' @seealso [to_categorical()], [train_split()].
#' @export
Sequential <- R6::R6Class("Sequential", list(

  #' @field layers List of the [Layer]s of the model.
  layers = list(),

  #' @field input_units Number of input nodes to the model.
  input_units = NULL,

  #' @field learning_rate Learning rate used to fit the model.
  learning_rate = NULL,

  #' @description
  #' Creates a new instance of this [R6][R6::R6Class] class.
  #' Calls the `add()` method on each [Layer] in `layers`.
  #'
  #' @param layers List of [Layer]s to add to the model.
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
  initialize = function(layers = list()){
    for (layer in layers) self$add(layer)
    NULL
  },

  #' @description
  #' Adds a [Layer] to the model.
  #'
  #' @param layer [Layer].
  #' @return `NULL`.
  add = function(layer){
    if(inherits(layer, "Input")) return(self$input_units <- layer$units)
    n_layers <- length(self$layers)
    last_units <- if(n_layers == 0) self$input_units else
      self$layers[[n_layers]]$units
    if(!is.null(last_units)) layer$create_weights(last_units)
    self$layers <- c(self$layers, list(layer))
    NULL
  },

  #' @description
  #' Removes a [Layer] from the model.
  #'
  #' @return [Layer] removed.
  pop = function(){
    n_layers <- length(self$layers)
    last <- self$layers[[n_layers]]
    self$layers[[n_layers]] <- NULL
    last
  },

  #' @description Feeds an input forward in the model.
  #' Calls the `feed_forward()` method of each [Layer] in the model.
  #'
  #' @param input Numeric array of size `(batch_size, input_units)`.
  #' @return Output of the last [Layer] in the model, of size `(batch_size, output_units)`.
  feed_forward = function(input){
    for (layer in self$layers) input <- layer$feed_forward(input)
    input
  },

  #' @description Backpropagates to update the weights of each [Layer].
  #'
  #' @param expected Numeric array of size `(batch_size, output_units)`.
  #' @return `NULL`.
  backpropagate = function(expected){
    last <- self$layers[[length(self$layers)]]
    loss_der <- last$output - expected # mean squared error
    last$deltas <- loss_der * last$activation$der(last$output)
    for (layer in rev(self$layers)[-1]){
      layer$deltas <- last$deltas %*% last$weights *
        layer$activation$der(layer$output)
      last <- layer
    }
    batch_size <- nrow(layer$input)
    batch <- seq_len(batch_size)
    for (layer in self$layers){
      dw <- 0
      for(i in batch) dw <- dw + layer$deltas[i, ] %o% layer$input[i, ]
      layer$weights <- layer$weights - self$learning_rate * dw / batch_size
    }
    NULL
  },

  #' @description
  #' Fits the model to inputs and expected outputs.
  #'
  #' @param input Numeric array of size `(>batch_size, input_units)`.
  #' @param expected Numeric array of size `(>batch_size, output_units)`.
  #' @param batch_size Batch size for each `feed_forward()` and
  #' `backpropagate()` call.
  #' @param epochs Number of times a batch is used to update the
  #' weights of the model.
  #' @param learning_rate Rate at which weights are adjusted.
  #' @param verbose How often to print the epoch number.
  #' @return `NULL`.
  fit = function(input, expected, batch_size, epochs, learning_rate,
                 verbose = epochs %/% 10){
    self$learning_rate <- learning_rate
    nrows <- nrow(input)
    for(epoch in seq_len(epochs)){
      if(epoch %% verbose == 0) print(epoch)
      rows <- sample.int(nrows, batch_size)
      self$feed_forward(input[rows, , drop=FALSE])
      self$backpropagate(expected[rows, , drop=FALSE])
    }
    NULL
  }
))
