#' @title Train and Test Data
#'
#' @description Randomly split rows of a data frame into train and test sets.
#'
#' @param x Data frame.
#' @param train Proportion of rows to allocate to the training set.
#' @returns Logical vector indicating if each row is in the training set.
#' @seealso [to_categorical()], [Sequential].
#' @export
train_split <- function(x, train=0.8){
  nrows <- nrow(x)
  out <- logical(nrows)
  out[sample.int(nrows, train * nrows)] <- TRUE
  out
}
