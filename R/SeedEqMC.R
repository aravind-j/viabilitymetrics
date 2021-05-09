#' Seed equilibrium moisture content and equilibrium relative humidity of the
#' seed storage environment
#'
#' Compute the following metrics:\describe{\item{\code{SeedEqMC}}{The seed
#' equilibrium moisture content from known environmental conditions and oil
#' content.} \item{\code{EqRH}}{The equilibrium relative humidity of the seed
#' storage environment from seed equilibrium moisture content, oil content and
#' temperature.} } \loadmathjax
#'
#' This relationship between seed equilibrium moisture content, seed oil
#' content, the  equilibrium relative humidity and temperature of the storage
#' environment was described by
#' \insertCite{cromarty_design_1982;textual}{viabilitymetrics} as follows.
#'
#' \mjsdeqn{(1-R) = \textrm{e}^{-\left( {\bigg[ \frac{M_{e} \times \big(1.1 +
#' \frac{T}{90} \big)}{1 - D_{O}} \bigg]^{2}}\bigg/{440} \right)}}
#'
#' Where, \mjseqn{R} is the relative humidity expressed as decimal,
#' \mjseqn{M_{e}} is the equilibrium percentage moisture content (dry basis),
#' \mjseqn{T} is the temperature in °C of air or the seed equilibrium,
#' \mjseqn{D_{O}} is the oil content of seed (dry basis) expressed as decimal
#' and \mjseqn{e} is the mathematical constant 2.718282.
#'
#' For values of oil content (\code{oilcontent}), relative humidity (\code{rh})
#' and seed equilibrium moisture content (\code{mc}) beyond the limits of 0-100
#' \%, a warning is issued.
#'
#' @param oilcontent The percentage oil content of seed (dry basis).
#' @param rh Relative humidity expressed in percentage.
#' @param mc The seed equilibrium moisture content on wet or dry basis
#'   (according to argument \code{basis}).
#' @param temp Temperature in °C.
#' @param basis The type of estimation of moisture content specified in the
#'   argument \code{mc}. Either \code{"wet"} or \code{"dry"}.
#'
#' @note The above expression by
#'   \insertCite{cromarty_design_1982;textual}{viabilitymetrics} is recommended
#'   for temperature and humidity ranges of 0-40 °C and 10-70\% RH for starchy
#'   seeds (eg. cereals); and 15-25 °C and 10-70\% RH for oilseeds.
#'
#' @return For \code{SeedEqMC}, the seed equilibrium moisture content on wet or
#'   dry basis (according to argument \code{basis}) expressed in percentage.
#'
#'   For \code{EqRH}, the equilibrium relative humidity expressed in percentage.
#'
#' @name SeedEqMC
#'
#' @encoding UTF-8
#' @import Rdpack
#' @references
#'
#' \insertAllCited
#'
#' @seealso \code{\link[viabilitymetrics]{wet2dry}}
#'
#' @examples
#' SeedEqMC(oilcontent = 29, rh = 13, temp = 25, basis = "wet")
#' SeedEqMC(oilcontent = 29, rh = 13, temp = 25, basis = "dry")
#'
#' EqRH(oilcontent = 29, mc = 5, temp = 25, basis = "wet")
#' EqRH(oilcontent = 29, mc = 5, temp = 25, basis = "dry")
#'
#' # Warning if oilcontent is beyond limits (0-100 %)
#' SeedEqMC(oilcontent = 125, rh = 13, temp = 25, basis = "wet")
#' EqRH(oilcontent = 125, mc = 5, temp = 25, basis = "wet")
#'
#' # Warning if relative humidity is beyond limits (0-100 %)
#' SeedEqMC(oilcontent = 29, rh = 115, temp = 25, basis = "wet")
#'
#' # Warning if moisture content is beyond limits (0-100 %)
#' EqRH(oilcontent = 29, mc = 115, temp = 25, basis = "wet")
#'

#' @export
#' @rdname SeedEqMC
SeedEqMC <- function(oilcontent, rh, temp, basis = c("wet", "dry")){

  basis <- match.arg(basis)

  # Check if oilcontent is of type numeric
  if (!is.numeric(oilcontent)){
    stop("'oilcontent' should be of type numeric.")
  }

  # Check limits of oilcontent
  if (FALSE %in% (findInterval(oilcontent, c(0, 100),
                               rightmost.closed = TRUE) == 1)) {
    warning('"oilcontent" is beyond limits (0 < "oilcontent" < 100).')
  }

  # Check if rh is of type numeric
  if (!is.numeric(rh)){
    stop("'rh' should be of type numeric.")
  }

  # Check limits of relative humidity
  if (FALSE %in% (findInterval(rh, c(0, 100), rightmost.closed = TRUE) == 1)) {
    warning('"rh" is beyond limits (0 < "rh" < 100).')
  }

  oc <- oilcontent / 100
  rh <- rh / 100

  mc <- ((1 - oc) * sqrt(-440 * log(1 - rh)))/(1.1 + (temp/90))

  if (basis == "wet") {
    mc <- dry2wet(mc)
  }

  return(mc)
}

#' @export
#' @rdname SeedEqMC
EqRH <- function(oilcontent, mc, temp, basis = c("wet", "dry")){

  basis <- match.arg(basis)

  # Check if oilcontent is of type numeric
  if (!is.numeric(oilcontent)){
    stop("'oilcontent' should be of type numeric.")
  }

  # Check limits of oilcontent
  if (FALSE %in% (findInterval(oilcontent, c(0, 100),
                               rightmost.closed = TRUE) == 1)) {
    warning('"oilcontent" is beyond limits (0 < "oilcontent" < 100).')
  }

  # Check if moisture content is of type numeric
  if (!is.numeric(mc)){
    stop("'mc' should be of type numeric.")
  }

  # Check limits of moisture content
  if (FALSE %in% (findInterval(mc, c(0, 100), rightmost.closed = TRUE) == 1)) {
    warning('"mc" is beyond limits (0 < "mc" < 100).')
  }

  if (basis == "wet") {
    mc <- wet2dry(mc)
  }

  oc <- oilcontent / 100

  sup <- -(mc*(1.1 + temp/90)/(1 - oc))^2/440
  R <- 1 - exp(1)^sup

  return(R*100)
}
