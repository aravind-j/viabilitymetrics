#' Half-viability period
#'
#' \code{P50} computes the half-viability period, which is the time taken for
#' 50\% of the seeds to lose viability. \loadmathjax
#'
#' The period to lose 50\% viability (\mjseqn{P_{50}}) is computed according to
#' the relationship between probit percentage viabilities and time of storage
#' described by \insertCite{ellis_improved_1980;textual}{viabilitymetrics} as
#' follows.
#'
#' \mjsdeqn{v=K_{i}-\frac{p}{\sigma}}
#'
#' or
#'
#' \mjsdeqn{v=K_{i}-\left ( \frac{1}{\sigma} \right )\cdot p}
#'
#' Where, \mjseqn{v} is the probit percentage viability at storage time
#' \mjseqn{p} (final viability),  \mjseqn{K_{i}} is the probit percentage
#' viability of the seedlot at the beginning of storage (seedlot constant) and
#' \mjseqn{\dfrac{1}{\sigma}} is the slope.
#'
#' Germination percentages plotted against storage times yield a sigmoid seed
#' survival curve which is converted to a linear relationship by the probit
#' transformation with slope \mjseqn{\dfrac{1}{\sigma}}.
#'
#' When \mjseqn{v} = 0 (equivalent to 50\% viability), \mjseqn{P_{50}} can be
#' computed as follows.
#'
#' \mjsdeqn{P_{50} = K_{i} \times \sigma}
#'
#' If the initial viablity (\code{initial}) is beyond limits (0-100 \%, an error
#' is issued.
#'
#' The value of this computation is appropriate for temperature between -20 to
#' 90 °C and seed moisture content between 5 to 25\%. For values beyond this
#' range, a warning will be displayed.
#'
#' @param initial The initial viability (\%).
#' @inheritParams Sigma
#'
#' @note For initial viability percentage values of 100\%, adjust it according
#'   to sample size using the
#'   \code{\link[viabilitymetrics:Percent2Probit]{PercentAdjust}} function to
#'   avoid infinity values in output.
#'
#' @return The half-viability period in days or years (according to argument
#'   \code{years}).
#'
#' @references
#'
#' \insertAllCited
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' P50(initial = 98, vcindex = 24, mc = 5, temp = -20)
#' P50(initial = 98, vcindex = 24, mc = 5, temp = -20, years = TRUE)
#'
#' # With intial viability of 100%
#' P50(initial = 100, vcindex = 24, mc = 5, temp = -20)
#' P50(initial = 100, vcindex = 24, mc = 5, temp = -20, years = TRUE)
#'
#' # With intial viability of 100%, use of PercentAdjust() to avoid Inf
#' P50(initial = PercentAdjust(100, n = 50), vcindex = 24, mc = 5, temp = -20)
#' P50(initial = PercentAdjust(100, n = 50), vcindex = 24, mc = 5, temp = -20,
#'     years = TRUE)
#'
#' \dontrun{
#' # Error if initial viability is beyond limits (0-100 %)
#' P50(initial = 110, vcindex = 24, mc = 5, temp = -20)
#' }
#'
#' @seealso \code{\link[viabilitymetrics]{Sigma}},
#'   \code{\link[viabilitymetrics:Percent2Probit]{PercentAdjust}}
#'
P50 <- function(initial, vcindex, vcdirect, mc, temp, years = FALSE) {
  # Check if initial is of type numeric with unit length
  if (!is.numeric(initial) || length(initial) != 1){
    stop("'initial' should be a numeric vector of length 1.")
  }

  # Check limits of initial viability
  if (FALSE %in% (findInterval(initial, c(0, 100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"initial" is beyond limits (0 < "initial" < 100).')
  }

  Ki <- Percent2NED(initial)
  sig <- Sigma(vcindex = vcindex, vcdirect = vcdirect, mc = mc, temp = temp,
               years = FALSE)

  if (years == TRUE) {
    out <- (Ki * sig) / 365
  } else {
    out <- Ki * sig
  }

  return(out)
}
