#' Prints \ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}} and
#' \ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}} from a
#' \code{FitSigma.batch} object
#'
#' \code{print.FitSigma.batch} prints to console the seed lot constant
#' (\ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}}) and the period to
#' lose unit probit viability
#' (\ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}}).
#'
#' @param x An object of class \code{print.FitSigma.batch}.
#' @param ... Unused
#' @seealso \code{\link[viabilitymetrics]{print.FitSigma.batch}}
#'
#' @return The \ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}} and
#'   \ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}} values (degree Celsius)
#'   in the console.
#'
#' @export
print.FitSigma.batch <- function(x, ...){

  if(attributes(x)$method == "glm") {
    cat("Generalised linear model with probit link function.\n")
  }
  if(attributes(x)$method == "tflm") {
    cat("Linear model after probit transformation.\n")
  }

  if(attributes(x)$cv["logical"] == 1) {
    cat(paste("Control viability = ",
              attributes(x)$cv["value"], "%\n", sep = ""))
  }

  print(x$models[, c("group", "Ki", "sigma")], row.names = FALSE)
  cat("\n")
  if (!any(is.na(x$models$message))) {
    print(x$models[, c("group", "message")], row.names = FALSE)
  }

}
