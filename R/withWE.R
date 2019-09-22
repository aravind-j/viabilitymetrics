#' Evaluate expression and capture all warnings and errors if any along with results
#'
#' Not exported. Strictly internal
#'
#'
#' @keywords internal
#'
#' @param expr The expression to be evaluated.
#'
#' @section Warning(s):
#' Returns the value along with the warning message(s).
#'
#' @section Error:
#' Returns NA as the value along with the error message.
#'
#' @examples
#'
#' foo <- function(){
#'   warning("oops")
#' }
#'
#' withWE(foo)
#'
#' foo <- function(){
#'   warning("oops")
#'   warning("again oops")
#'   1
#' }
#'
#' withWE(foo)
#'
#' foo <- function(){
#'   warning("oops")
#'   log("a")
#' }
#'
#' withWE(foo)
#'
withWE <- function(expr) {
  myWarnings <- NULL
  myError <- NULL

  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }

  val <- withCallingHandlers(tryCatch(expr,error = function(e) e),
                             warning = wHandler)

  if(!is.null(myWarnings)) {
    myWarnings <- paste("WARNING:", myWarnings)
    message <- myWarnings
    # message <- paste(myWarnings, myError,
    #                   collapse = "\n")
  } else {
    message <- NA
  }

  out <- list(value = val, message = message)

  if("simpleError" %in% class(out$value)) {
    out$message <- paste("ERROR:", conditionMessage(out$value))
    out$value <- NA
  }

  message(paste(out$message, "\n"))
  return(out)
}
