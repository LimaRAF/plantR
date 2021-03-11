#' @title Catch Values, Warnings and Errors
#'
#' @description The function separates the output from another function, its
#'   warning and errors, without printing them in the console.
#'
#' @param fun a function.
#'
#' @keywords internal
#'
#' @return a list with the output of the function in the first element,
#' the warnings in the second and the errors in the third.
#'
#' @details The function was taken from the following stackoverflow question:
#'   https://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function/4952908#4952908
#'
catchAll <- function(fun) {
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(
      tryCatch(fun(...), error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }), warning = function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    list(output = res, warn = warn, err = err)
  }
}
