#' Seed viability curve fitting to estimate
#' \ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}} and
#' \ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}}
#'
#' Fit seed viability/survival curve to estimate the seed lot constant
#' (\ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}}) and the period to
#' lose unit probit viability
#' (\ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}}).
#'
#' This function fits seed survival data to the following seed viability
#' equation \insertCite{ellis_improved_1980}{viabilitymetrics} which models the
#' relationship between probit percentage viability and time period of storage.
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><em>v = K<sub>i</sub>
#' &minus; [ <sup>p</sup> &frasl; <sub>&sigma;</sub> ]
#' </em></p>}}{\deqn{v=K_{i}-\frac{p}{\sigma}}}
#'
#' or
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><em>v = K<sub>i</sub>
#' &minus; <big>(</big><sup>1</sup> &frasl;
#' <sub>&sigma;</sub><big>)</big>&sdot;p</em></p>}}{\deqn{v=K_{i}-\left (
#' \frac{1}{\sigma} \right )\cdot p}}
#'
#' Where, \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} is the probit percentage
#' viability at storage time \ifelse{html}{\out{<i>p</i>}}{\eqn{p}} (final
#' viability),  \ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}} is the
#' probit percentage viability of the seedlot at the beginning of storage
#' (seedlot constant) and
#' \ifelse{html}{\out{<em><sup>1</sup>&frasl;<sub>&sigma;</sub></em>}}{\eqn{\frac{1}{\sigma}}}
#' is the slope.
#'
#' The above equation may be expressed as a generalized linear model (GLM) with
#' a probit (cumulative normal distribution) link function as follows
#' \insertCite{hay_modelling_2014}{viabilitymetrics}.
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><em>y = &phi;(v) =
#' &phi;<big>(</big>K<sub>i</sub> &minus; (<sup>1</sup> &frasl;
#' <sub>&sigma;</sub>) <big>)</big></em></p>}}{\deqn{y = \phi(v) = \phi\left (
#' K_{i}-\left ( \frac{1}{\sigma} \right )p \right )}}
#'
#' Where, \ifelse{html}{\out{<i>y</i>}}{\eqn{y}} is the proportion of seeds
#' viabile after time period \ifelse{html}{\out{<i>p</i>}}{\eqn{p}} and the link
#' function is
#' \ifelse{html}{\out{<em>&phi;<sup>-1</sup></em>}}{\eqn{\phi^{-1}}}, the
#' inverse of the cumulative normal distribution function.
#'
#' The parameters estimated are the intercept
#' \ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K<i>}}, theoretical viability
#' of the seeds at the start of storage or the seed lot constant, and the slope
#' \ifelse{html}{\out{<i>&minus;&sigma;<sup>-1</sup></i>}}{\eqn{-\sigma^{-1}}},
#' where \ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}} is the standard
#' deviation of the normal distribution of seed deaths in time or the period of
#' time to lose unit probit viability.
#'
#' @param data A data frame with the seed viability data recorded periodically.
#'   It should possess columns with data on \itemize{ \item Viability percentage
#'   (to be indicated by the argument \code{viability.percent}), \item Sample
#'   size (to be indicated by the argument \code{samp.size}) and \item Storage
#'   period (to be indicated by the argument \code{storage.period}). }
#' @param viability.percent The name of the column in \code{data} with the
#'   viability percentages as a character string.
#' @param samp.size The name of the column in \code{data} with the sample size
#'   used for calculating viability percentages as a character string.
#' @param storage.period The name of the column in \code{data} with the time
#'   periods at which the viabilty percentages was recorded as a character
#'   string.
#' @param probit.method The method to be used for fitting seed viability curve.
#'   Either as a generalised linear model with a probit link function
#'   (\code{"glm"}, recommended) or as a linear model with probit transformed
#'   viability percentages (\code{"tflm"}).
#'
#' @return A list with the following components:  \item{model}{The
#'   fitted model as an object of class \code{glm} (if \code{probit.method =
#'   "glm"}) or \code{lm} (if \code{probit.method = "lm"}).} \item{parameters}{A
#'   data.frame of parameter estimates, standard errors and p value.}
#'   \item{fit}{A one-row data frame with estimates of model fitness such as log
#'   likelyhoods, Akaike Information Criterion, Bayesian Information Criterion,
#'   deviance and residual degrees of freedom.} \item{Ki}{The estimated seed lot
#'   constant from the model.} \item{sigma}{The estimated period of time to lose
#'   unit probit viability from the model.} \item{message}{Warning or error
#'   messages generated during fitting of model, if any.}
#' @importFrom broom tidy
#' @importFrom broom glance
#' @export
#'
#' @references \insertAllCited{}
#'
#' @examples
#' df <- data.frame(Rep = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
#'                          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
#'                  period = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
#'                             11, 12, 13, 14, 15, 1, 2, 3, 4, 5,
#'                             6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
#'                  sampsize = c(97, 100, 100, 100, 98, 100, 100, 100,
#'                               99, 98, 101, 100, 100, 101, 100, 100,
#'                               99, 97, 100, 97, 99, 98, 100, 98, 99,
#'                               99, 101, 98, 97, 97),
#'                  viabilitypercent = c(98, 96, 91, 83, 70, 55, 39, 24,
#'                                       13, 7, 3, 1, 0, 0, 0, 98, 94, 86,
#'                                       73, 55, 35, 19, 9, 3, 1,
#'                                       0, 0, 0, 0, 0))
#'
#' plot(df$period, df$viabilitypercent)
#'
#' # With generalised linear model with probit link function
#' FitSigma(data = df, viability.percent = "viabilitypercent",
#'          samp.size = "sampsize", storage.period = "period",
#'          probit.method = "glm")
#'
#' # With linear model after probit transformation
#' FitSigma(data = df, viability.percent = "viabilitypercent",
#'          samp.size = "sampsize", storage.period = "period",
#'          probit.method = "tflm")
#'
FitSigma <- function(data, viability.percent, samp.size, storage.period,
                     probit.method = c("glm", "tflm")) {
    # Check if data.frame
    if (!is.data.frame(data)) {
      stop('"data" should be a data frame object.')
    }

    if (any(c("tbl_dataf", "tbl") %in% class(data))) {
      warning('"data" is of type tibble\nCoercing to data frame.')
      data <- as.data.frame(data)
    }

  # Check if viability.percent column present in data
  if (!(viability.percent %in% colnames(data))) {
    stop(paste('Column ', viability.percent,
               ' specified as the viability percentage column',
               ' is not present in "data".',
               sep = ""))
  }

  # Check if samp.size column present in data
  if (!(samp.size %in% colnames(data))) {
    stop(paste('Column ', samp.size,
               ' specified as the sample size column',
               ' is not present in "data".',
               sep = ""))
  }

  # Check if storage.period column present in data
  if (!(storage.period %in% colnames(data))) {
    stop(paste('Column ', storage.period,
               ' specified as the storage time period column',
               ' is not present in "data".',
               sep = ""))
  }

  # Check if viability.percent is of type numeric
  if (!is.numeric(data[, viability.percent])) {
    stop('"viability.percent" is not of type numeric.')
  }

  # Check if samp.size is of type numeric
  if (!is.numeric(data[, samp.size])) {
    stop('"samp.size" is not of type numeric.')
  }

  # Check if storage.period is of type numeric
  if (!is.numeric(data[, storage.period])) {
    stop('"storage.period" is not of type numeric.')
  }

  # Check if viability.percent is within range
  if (any(!findInterval(data[, viability.percent], c(0,100),
                        rightmost.closed = TRUE) == 1)) {
    stop('Data in "viability.percent" is not within range',
         ' (0 < "viability.percent" < 100).')
  }

  # Check probit.method
  probit.method <- match.arg(probit.method)

  data$viability.count <- (data[, viability.percent] * data[, samp.size]) / 100

  if (probit.method == "glm") {
    probit.model <- withWE(glm(formula(paste("viability.count/",samp.size,
                                             " ~ ", storage.period, sep = "")),
                               family = binomial(link = "probit"),
                               data = data,
                               weights = data[, samp.size]))
  }

  if (probit.method == "tflm") {
    probit.model <- withWE(lm(formula(paste("Percent2NED(PercentAdjust(",
                                        viability.percent, ", 100))", " ~ ",
                                         storage.period, sep = "")),
                           data = data))
  }

  parameters <- NA
  fit <- NA
  Ki <- NA
  sigma <- NA

  if(!is.null(probit.model$value)) {
    parameters <- as.data.frame(broom::tidy(probit.model$value))
    parameters$term <- gsub("\\(Intercept\\)", "Ki", parameters$term)
    parameters$term <- gsub("period", "1/sigma", parameters$term)

    fit <- as.data.frame(broom::glance(probit.model$value))

    Ki <- parameters[parameters$term == "Ki", ]$estimate
    sigma <- 1 / parameters[parameters$term == "1/sigma", ]$estimate
  }

  out <- list(model = probit.model$value, parameters = parameters,
              fit = fit, Ki = Ki, sigma = sigma,
              message = probit.model$message)

  attr(out, "method") <- probit.method

  class(out) <- "FitSigma"
  return(out)
}
