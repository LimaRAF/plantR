#' @name accessory
#' @aliases my.head
#' @aliases my.tail
#' @aliases rm.tail
#' @aliases my.head.df
#'
#'
#' @title Accessory, internal functions
#'
#' @description These accessory functions work similarly to `R` base functions
#'   `head()` and `tail()` to return the first or last part of a vector or of a
#'   data frame.
#'
#' @details The functions `my.head()`, `my.tail()` and `rm.tail()` works with
#'   vectors only, while function `my.head.df()` works with data frames or
#'   matrices.
#'
#' @param x a vector.
#' @param n a index.
#'
#' @keywords internal
#'
#' @rdname accessory
#' @examples
#' \dontrun{my.head(letters)}
#'
my.head <- function(x, n = 1) {
  if (n > length(x)) n <- length(x)
  x[1:n]
}
#'
#' @rdname accessory
#' @examples
#' \dontrun{my.tail(letters)}
#'
my.tail <- function(x, n = 0) {
  if (n >= length(x)) n <- length(x)
  x[((length(x) - n):length(x))]
}
#'
#' @rdname accessory
#' @examples
#' \dontrun{rm.tail(letters)}
#'
rm.tail <- function(x, n = 0) {
  if (n >= (length(x) - 1)) n <- length(x) - 1
  x[-((length(x) - n):length(x))]
}
#'
#' @rdname accessory
#' @examples
#' \dontrun{my.head.df(iris)}
#'
my.head.df <- function(x, n = 5) {
  if (n > dim(x)[1]) n <- dim(x)[1]
  x[1:n,]
}
