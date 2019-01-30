#' Prints \ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}} and
#' \ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}} from a \code{FitSigma}
#' object
#'
#' \code{print.FitSigma} prints to console the seed lot constant
#' (\ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}}) and the period to
#' lose unit probit viability
#' (\ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}}).
#'
#' @param x An object of class \code{print.FitSigma}.
#' @param ... Unused
#' @seealso \code{\link[viabilitymetrics]{FitSigma}}
#'
#' @return The \ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}} and
#'   \ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}} values (degree Celsius) in
#'   the console.
#'
#' @export
print.FitSigma <- function(x, ...){

  if (attributes(x)$method == "glm") {
    cat("Generalised linear model with probit link function.\n")
  }
  if (attributes(x)$method == "tflm") {
    cat("Linear model after probit transformation.\n")
  }

  if (attributes(x)$cv["logical"] == 1) {
    cat(paste("Control viability = ",
              attributes(x)$cv["value"], "%\n", sep = ""))
  }

  print(data.frame(Ki = x$Ki, Sigma = x$sigma), row.names = FALSE)

  if (!is.na(x$message)) {
    message(x$message)
  }

}
