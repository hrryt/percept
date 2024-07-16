#' @title Categorical Data
#'
#' @description Convert between factor and model input/output batch.
#'
#' @param f Factor.
#' @param x Numeric matrix where each row is an observation.
#' @param labels Passed to [factor()].
#' @returns For `to_categorical()`, numeric matrix. For `to_factor()`, factor.
#' @seealso [Sequential].
#' @export
to_categorical <- function(f){
  x <- matrix(0, length(f), length(levels(f)))
  x[cbind(seq_along(f), as.integer(f))] <- 1
  x
}

#' @rdname to_categorical
#' @export
to_factor <- function(x, labels){
  factor(max.col(x), levels=seq_len(ncol(x)), labels=labels)
}
