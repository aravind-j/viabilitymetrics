#' Probit transformation
#'
#' These functions transform data between percentage, probit and Normal
#' Equivalent Deviate (NED)
#' \insertCite{bliss_method_1934,finney_probit_1952}{viabilitymetrics}.
#' \loadmathjax
#'
#' Probit transformation can be used to transform a sigmoid curve of percentage
#' data to a linear one. The probit transformation is defined as
#' \mjseqn{\textrm{NED} + 5}. However the two terms probit and NED are used
#' interchangeably in literature.
#'
#' NED function (\mjseqn{\Phi^{-1}}) is is the inverse of the cumulative
#' distribution function (\mjseqn{\Phi}) of the standard normal distribution
#' (\mjseqn{z\sim N(0,1)}) or the quantile function associated with the standard
#' normal distribution.
#'
#' For percentage \mjseqn{p},
#'
#' \mjsdeqn{\textrm{NED}(p)=\Phi^{-1}(p)= \sqrt{2}~\textrm{erf}^{-1}(2p-1)}
#'
#' and
#'
#' \mjsdeqn{\textrm{probit}(p)=\textrm{NED}(p)+5}
#'
#' The \code{PercentAdjust} function adjusts the percentage values of 0 and 100
#' to \mjseqn{100\times \dfrac{0.25}{n}} and \mjseqn{100\times
#' \dfrac{n-0.25}{n}} respectively, according to the sample size \mjseqn{n} to
#' avoid infinity values during probit transformation
#' \insertCite{miller_estimation_1944}{viabilitymetrics}.
#'
#' @param percentage The percentage value.
#' @param probit The probit value
#' @param NED The NED value.
#' @param n Sample size for estimation of percentage.
#'
#' @return The transformed value.
#' @export PercentAdjust
#' @export Percent2NED
#' @export Percent2Probit
#' @export Probit2NED
#' @export Probit2Percent
#' @export NED2Probit
#' @export NED2Percent
#' @importFrom stats pnorm qnorm
#'
#' @references \insertRef{bliss_method_1934}{viabilitymetrics}
#'
#'   \insertRef{miller_estimation_1944}{viabilitymetrics}
#'
#'   \insertRef{finney_probit_1952}{viabilitymetrics}
#'
#' @examples
#' Percent2NED(0:100)
#' Percent2Probit(0:100)
#'
#' Percent2NED(25)
#' Percent2Probit(25)
#' Percent2NED(25) +5
#' NED2Probit(-0.6744898)
#'
#' # Percentage adjustment for 0 and 100
#' Percent2Probit(100)
#' Percent2Probit(0)
#' n = 50
#' Percent2Probit(PercentAdjust(100, n))
#' Percent2Probit(PercentAdjust(0, n))
#'
#' @name Percent2Probit


#' @rdname Percent2Probit
#' @export
PercentAdjust <- function(percentage, n) {
  if (!is.numeric(percentage)) {
    stop('"percentage" is not numeric.')
  }
  if (!is.numeric(n)) {
    stop('"n" is not numeric.')
  }

  # check for n
  AdjPercent <- ifelse(test = percentage == 100,
                       yes = (100 * (n - 0.25)) / n,
                       no = ifelse(test = percentage == 0,
                                   yes = 100 * (0.25 / n),
                                   no = percentage))
  return(AdjPercent)
}

#' @rdname Percent2Probit
#' @export
Percent2NED <- function(percentage) {
  if (!is.numeric(percentage)) {
    stop('"percentage" is not numeric.')
  }

  qnorm(percentage / 100, 0, 1)
}

#' @rdname Percent2Probit
#' @export
Percent2Probit <- function(percentage) {
  if (!is.numeric(percentage)) {
    stop('"percentage" is not numeric.')
  }

  qnorm(percentage / 100, 5, 1)
}

#' @rdname Percent2Probit
#' @export
Probit2NED <- function(probit) {
  if (!is.numeric(probit)) {
    stop('"probit" is not numeric.')
  }

  probit - 5
}

#' @rdname Percent2Probit
#' @export
NED2Probit <- function(NED) {
  if (!is.numeric(NED)) {
    stop('"NED" is not numeric.')
  }

  NED + 5
}

#' @rdname Percent2Probit
#' @export
NED2Percent <- function(NED) {
  if (!is.numeric(NED)) {
    stop('"NED" is not numeric.')
  }

  pnorm(NED, 0, 1) * 100
}

#' @rdname Percent2Probit
#' @export
Probit2Percent <- function(probit) {
  if (!is.numeric(probit)) {
    stop('"probit" is not numeric.')
  }

  pnorm(probit, 5, 1) * 100
}
