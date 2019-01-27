#' Title
#'
#' @param data
#' @param viability.percent
#' @param samp.size
#' @param storage.period
#' @param group
#' @param probit.method
#'
#' @return
#' @importFrom evaluate evaluate
#' @importFrom evaluate new_output_handler
#' @export
#'
#' @examples
FitSigma <- function(data, viability.percent, samp.size, storage.period,
                     group = NULL, probit.method = c("glm", "tflm")) {

  data$viability.count <- (data[, viability.percent] * data[, samp.size]) / 100

  ## multiple warns;
  ## warn + message

  if (probit.method == "glm") {
    probit.model <- withWE(glm(formula(paste("viability.count/",samp.size, " ~ ",
                                             storage.period, sep = "")),
                               family = binomial(link = "probit"),
                               data = data,
                               weights = data[, samp.size]))
  }

  if (method == "lm") {
    probit.model <- withWE(lm(formula(paste("Percent2NED(PercentAdjust(",
                                        viability.percent, ", 100))", " ~ ",
                                         storage.period, sep = "")),
                           data = data))
  }

  # method
  #coeff
  # stats (broom)

  out <- list(model = probit.model$value, message = probit.model$message)

  class(out) <- "FitSigma"
  retrun(out)
}





withWE <- function(expr) {
  myWarnings <- NULL
  myError <- NULL

  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }

  val <- withCallingHandlers(tryCatch(expr, error = function(e) e),
                             warning = wHandler)

  if(!is.null(myWarnings)) {
    myWarnings <- paste("WARNING:", myWarnings)
    message <- paste(myWarnings, myError,
                     collapse = "\n")
  } else {
    message <- NA
  }

 out <- list(value = val, message = message)

 if("simpleError" %in% class(out$value)) {
   out$message <- paste("ERROR:", conditionMessage(out$value))
   out$value <- NA
 }

 return(out)
}
