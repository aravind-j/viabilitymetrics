#' Viability constant \mjseqn{K_{E}} based on the species-specific temperature
#' coefficients
#'
#' \code{Ke} computes the viability constant \mjseqn{K_{E}} from the
#' species-specific temperature coefficients in case of storage experiments
#' conducted at constant temperature and varying moisture contents. \loadmathjax
#'
#' From seed storage experiments involving storage of seeds at a constant
#' temperature in a range of moisture contents, the effect of moisture content
#' on seed longevity (\mjseqn{\sigma}) can be estimated from the following
#' linear relationship:
#'
#' \mjsdeqn{\log{\sigma} = K - C_{w}\log{m}}
#'
#' Where, \mjseqn{K} is the intercept, \mjseqn{C_{W}} is the slope and
#' \mjseqn{m} is the moisture content.
#'
#' The constant \mjseqn{K} associated with the relationship of temperature with
#' seed longevity as follows.
#'
#' \mjsdeqn{K = K_{E} - C_{H}t - C_{Q}t^{2}}
#'
#' Where, \mjseqn{K_{E}}, \mjseqn{C_{H}} and \mjseqn{C_{Q}} are the
#' species-specific seed viability constants.
#'
#' The constant \mjseqn{K_{E}} can be estimated from the universal temperature
#' constants (\mjseqn{C_{H}} = 0.0329 and \mjseqn{C_{Q}} = 0.000478) in case of
#' seed storage experiments at constant temperature and varying moisture content
#' as follows.
#'
#' \mjsdeqn{K_{E} = K + C_{H}t + C_{Q}t^{2}}
#'
#' @param K The constant \mjseqn{K} associated with the relationship of
#'   temperature with seed longevity (see \strong{Details}).
#' @param temp.coeff The species-specific temperature coefficients
#'   (\mjseqn{C_{H}} and \mjseqn{C_{Q}}) as a numeric vector of length 2.
#' @param temp Temperature in °C.
#'
#' @return The value of species-specific seed viability constant \mjseqn{K_{E}}.
#'
#' @export
#' @encoding UTF-8
#'
#' @references
#'
#' \insertRef{ellis_improved_1980}{viabilitymetrics}
#'
#' \insertRef{pritchard_predicting_2003}{viabilitymetrics}
#'
#' @examples
#' Ke(36, 10)
#'
#' @seealso \code{\link[viabilitymetrics]{Sigma}}
#'
Ke <- function(K, temp, temp.coeff = c(0.0329, 0.000478)){

  # Check if K is of type numeric with unit length
  if (!is.numeric(K) || length(K) != 1){
    stop("'K' should be a numeric vector of length 1.")
  }

  # Check if temp is of type numeric with unit length
  if (!is.numeric(temp) || length(temp) != 1){
    stop("'temp' should be a numeric vector of length 1.")
  }

  # Check limits of temperature
  if (FALSE %in% (findInterval(temp, c(-20, 90),
                               rightmost.closed = TRUE) == 1)) {
    warning('"temp" is beyond limits (-20 < "temp" < 90).')
  }

  # Check if temp.coeff is of type numeric with length 2
  if (!is.numeric(temp.coeff) || length(temp.coeff) != 2){
    stop("'temp.coeff' should be a numeric vector of length 2.")
  }

  Ch <- temp.coeff[1]
  Cq <- temp.coeff[2]

  Ke <- K + (Ch * temp) + (Cq * (temp ^ 2))

  return(Ke)
}
