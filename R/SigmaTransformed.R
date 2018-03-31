#' SigmaTransformed
#'
#' \code{SigmaTransformed} transforms the measured sigma value at a specific
#' temperature to an estimate of sigma at another temperature. This useful in
#' comparitive seed testing protocol to compare seed longevities among species
#' tested at different temperatures (Probert et al., 2009).
#'
#' The transformation is based on the effect of temperature on seed longevity
#' (\ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}}) (identified by storage
#' experiment with constant moisture content and varying temperature) which is
#' as follows:
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><em>log&sigma; = &beta;
#' &minus; C<sub>H</sub>t &minus;
#' C<sub>Q</sub>t<sup>2</sup></em></p>}}{\deqn{\log\sigma = \beta - C_{H}t -
#' C_{H}t^{2}}}
#'
#' Where, \ifelse{html}{\out{<i>C<sub>H</sub></i>}}{\eqn{C_{H}}} and
#' \ifelse{html}{\out{<i>C<sub>Q</sub></i>}}{\eqn{C_{Q}}} are the
#' species-specific temperature coefficients,
#' \ifelse{html}{\out{<i>t</i>}}{\eqn{t}} is the temperature and
#' \ifelse{html}{\out{<i>&beta;</i>}}{\eqn{\beta}} is the constant associated
#' with moisture relations of seed longevity.
#'
#' @param sigma The inverse of slope from the seed viability equation
#'   (\ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}}), estimated at
#'   temperature \code{temp1}.
#' @param temp1 The temperature at which \code{sigma} is estimated in °C.
#' @param temp2 The temperature at which the transformed
#'   \ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}} is to be estimated in °C.
#' @param temp.coeff The species-specific temperature coefficients
#'   (\ifelse{html}{\out{<i>C<sub>H</sub></i>}}{\eqn{C_{H}}} and
#'   \ifelse{html}{\out{<i>C<sub>Q</sub></i>}}{\eqn{C_{Q}}}.) as a numeric
#'   vector of length 2.
#'
#' @return The transformed value of
#'   \ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}} at temperature
#'   \code{temp2}.
#' @export
#' @encoding UTF-8
#'
#' @references \insertRef{ellis_improved_1980}{viabilitymetrics}
#'   \insertRef{probert_ecological_2009}{viabilitymetrics}
#'
#' @examples
#'
#' SigmaTransformed(250, 60, 45)
#'
#' @seealso \code{\link[viabilitymetrics]{Sigma}}
#'
SigmaTransformed <- function(sigma, temp1, temp2,
                      temp.coeff = c(0.0329, 0.000478)){
  # Check if sigma is of type numeric with unit length
  if (!is.numeric(sigma) || length(sigma) != 1){
    stop("'sigma' should be a numeric vector of length 1")
  }

  # Check if temp1 is of type numeric with unit length
  if (!is.numeric(temp1) || length(temp1) != 1){
    stop("'temp1' should be a numeric vector of length 1")
  }

  # Check if temp2 is of type numeric with unit length
  if (!is.numeric(temp2) || length(temp2) != 1){
    stop("'temp2' should be a numeric vector of length 1")
  }

  # Check limits of temperature1
  if (FALSE %in% (findInterval(temp1, c(-20,90),
                               rightmost.closed = TRUE) == 1)) {
    warning('"temp1" is beyond limits (-20 < "temp1" < 90)')
  }

  # Check limits of temperature2
  if (FALSE %in% (findInterval(temp2, c(-20,90),
                               rightmost.closed = TRUE) == 1)) {
    warning('"temp2" is beyond limits (-20 < "temp2" < 90)')
  }

  # Check if temp.coeff is of type numeric with length 2
  if (!is.numeric(temp.coeff) || length(temp.coeff) != 2){
    stop("'temp.coeff' should be a numeric vector of length 2")
  }

  Ch <- temp.coeff[1]
  Cq <- temp.coeff[2]

  beta <- log10(sigma) + Ch*temp1 + (Cq*(temp1^2))

  logsigma <- beta - Ch*temp2 - Cq*(temp2^2)

  Csigma <- 10^logsigma
  #return(c(beta, logsigma, Csigma))
  return(Csigma)

}

