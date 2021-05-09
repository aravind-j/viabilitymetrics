#' Predict variables from the seed viability equation
#'
#' Predict the variables, final viability, storage period, storage temperature
#' and storage period from the improved seed viability equation
#' \insertCite{ellis_improved_1980}{viabilitymetrics}. \describe{
#' \item{\code{FinalViability}}{Compute the final viability after a period of
#' storage at a set of storage conditions (seed moisture content and
#' temperature).} \item{\code{StoragePeriod}}{Compute the storage period from
#' the final viability and the storage conditions (seed moisture content and
#' temperature).} \item{\code{StorageMC}}{Compute the storage moisture content
#' to give final viability at a particular storage temperature.}
#' \item{\code{StorageTemp}}{Compute the storage temperature to give final
#' viability at a particular storage temperature.} } \loadmathjax
#'
#' The improved seed viability equation of
#' \insertCite{ellis_improved_1980}{viabilitymetrics} describes the relationship
#' between final viability, storage period and storage environment conditions as
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
#' The slope is determined as follows.
#'
#' \mjsdeqn{\sigma = 10^{K_{E}-C_{W}\log{m}-C_{H}t-C_{Q}t^2}}
#'
#' Where, \mjseqn{m} is the moisture content (fresh weight basis), \mjseqn{t} is
#' the temperature and \mjseqn{K_{E}}, \mjseqn{C_{W}}, \mjseqn{C_{H}} and
#' \mjseqn{C_{Q}} are the species-specific seed viability constants.
#'
#' On the basis of the the improved seed viability equation, \mjseqn{v},
#' \mjseqn{p}, \mjseqn{m} and \mjseqn{t} can be estimated as follows.
#'
#' \mjsdeqn{v=K_{i}-\frac{p}{\sigma}}
#'
#' \mjsdeqn{p = \sigma(K_{i}-v)}
#'
#' \mjsdeqn{m = 10^{\left[ {\left( K_{E} - C_{H}t - C_{Q}t^{2} -\log \left(
#' \frac{p}{K_{i}-v} \right) \right) } \bigg/ {C_{W}} \right]}}
#'
#' \mjsdeqn{t = \frac{-C_{H} \pm \sqrt{C_{H}^{2}-4C_{Q}\left (
#' C_{W}\log{m}-K_{E}+ \log\left ( \frac{p}{K_{i}-v} \right ) \right
#' )}}{2C_{Q}}}
#'
#' The value of the species-specific seed viability constants can be specified
#' either directly in the arguement  \code{vcdirect} or as the index value of
#' the required seed viability constants from the
#' \code{\link{viabilityconstants}} dataset through the argument \code{vcindex}.
#'
#' The value of this prediction is appropriate for temperature between -20 to 90
#' °C and seed moisture content between 5 to 25\%. For values beyond this range,
#' a warning will be displayed.
#'
#' @param initial The initial viability (\%).
#' @param final The final viability (\%).
#' @param period The time period of storage in days or years according to the
#'   argument \code{years}).
#' @inheritParams Sigma
#' @param unit The unit of temperature.
#'
#' @encoding UTF-8
#' @import Rdpack
#' @import mathjaxr
#'
#' @references
#'
#' \insertAllCited
#'
#' @name FinalViability
#'
#' @note For initial and/or final viability percentage values of 0\% and 100\%,
#'   adjust it according to sample size using the
#'   \code{\link[viabilitymetrics:Percent2Probit]{PercentAdjust}} function to
#'   avoid infinity or extreme values in output.
#'
#' @return For \code{FinalViability}, the final viability (\%).
#'
#'   For \code{StorageMC}, the storage moisture content (\%).
#'
#'   For \code{StorageTemp}, the storage temperature (°C).
#'
#'   For \code{StoragePeriod}, the duration of storage (according to argument
#'   \code{years}).
#'
#' @examples
#' # Fetch the index from viabilityconstants dataset
#' viabilityconstants[grepl("oryza", x = viabilityconstants$Species,
#'                          ignore.case = TRUE),]
#'
#' #----------------------------------------------------------------------------
#' # Final viability
#' #----------------------------------------------------------------------------
#' # Use index  87
#' FinalViability(initial = 98, period = 365, vcindex = 87, mc = 10, temp = 5,
#'                years = FALSE)
#' FinalViability(initial = 98, period = 1, vcindex = 87, mc = 10, temp = 5,
#'                years = TRUE)
#'
#' # Input the viability constants directly
#' FinalViability(initial = 98, period = 365,
#'                vcdirect = c(8.242, 4.345, 0.0307, 0.000501),
#'                mc = 10, temp = 5, years = FALSE)
#' FinalViability(initial = 98, period = 1,
#'                vcdirect = c(8.242, 4.345, 0.0307, 0.000501),
#'                mc = 10, temp = 5, years = TRUE)
#'
#' \dontrun{
#' # Error if initial viability is beyond limits (0-100 %)
#' FinalViability(initial = 110, period = 365, vcindex = 87, mc = 10, temp = 5)
#' }
#'
#' # Warning if moisture content is beyond limits (0-100 %)
#' FinalViability(initial = 98, period = 365, vcindex = 87, mc = 110, temp = 5)
#'
#' # Warning if temperature is beyond limits (-20 to 90 degree C)
#' FinalViability(initial = 98, period = 365, vcindex = 87, mc = 10, temp = 95)
#'
#' # With initial viability 100
#' FinalViability(initial = 100, period = 365, vcindex = 87, mc = 10, temp = 5,
#'                years = FALSE)
#' FinalViability(initial = 100, period = 1, vcindex = 87, mc = 10, temp = 5,
#'                years = TRUE)
#'
#' # With intial viability of 100%, use of PercentAdjust() to avoid extremes
#' FinalViability(initial = PercentAdjust(100, n = 50), period = 365,
#'                vcindex = 87, mc = 10, temp = 5, years = FALSE)
#' FinalViability(initial = PercentAdjust(100, n = 50), period = 1,
#'                vcindex = 87, mc = 10, temp = 5, years = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Storage moisture content
#' #----------------------------------------------------------------------------
#' # Use index  87
#' StorageMC(initial = 98, final = 95, period = 3650, vcindex = 87, temp = 5,
#'           years = FALSE)
#' StorageMC(initial = 98, final = 95, period = 10, vcindex = 87, temp = 5,
#'           years = TRUE)
#'
#' # Input the viability constants directly
#' StorageMC(initial = 98, final = 95, period = 3650,
#'           vcdirect = c(8.242, 4.345, 0.0307, 0.000501),
#'           temp = 5, years = FALSE)
#' StorageMC(initial = 98, final = 95, period = 10,
#'           vcdirect = c(8.242, 4.345, 0.0307, 0.000501),
#'           temp = 5, years = TRUE)
#'
#' \dontrun{
#' # Error if initial viability is beyond limits (0-100 %)
#' StorageMC(initial = 110, final = 95, period = 3650, vcindex = 87, temp = 5)
#'
#' # Error if final viability is beyond limits (0-100 %)
#' StorageMC(initial = 98, final = -10, period = 3650, vcindex = 87, temp = 5)
#' }
#'
#' # Warning if temperature is beyond limits (-20 to 90 degree C)
#' StorageMC(initial = 98, final = 95, period = 3650, vcindex = 87, temp = 95)
#'
#' #----------------------------------------------------------------------------
#' # Storage temperature
#' #----------------------------------------------------------------------------
#' # Use index  87
#'
#' # In Celsius
#' StorageTemp(initial = 98, final = 95, period = 3650, vcindex = 87, mc = 8,
#'           years = FALSE)
#' StorageTemp(initial = 98, final = 95, period = 10, vcindex = 87, mc = 8,
#'           years = TRUE)
#'
#' # In Fahrenheit
#' StorageTemp(initial = 98, final = 95, period = 3650, vcindex = 87, mc = 8,
#'             years = FALSE, unit = "fahrenheit")
#' StorageTemp(initial = 98, final = 95, period = 10, vcindex = 87, mc = 8,
#'             years = TRUE, unit = "fahrenheit")
#'
#' # Input the viability constants directly
#' StorageTemp(initial = 98, final = 95, period = 3650,
#'           vcdirect = c(8.242, 4.345, 0.0307, 0.000501),
#'           mc = 8, years = FALSE)
#' StorageTemp(initial = 98, final = 95, period = 10,
#'           vcdirect = c(8.242, 4.345, 0.0307, 0.000501),
#'           mc = 8, years = TRUE)
#'
#' \dontrun{
#' # Error if initial viability is beyond limits (0-100 %)
#' StorageTemp(initial = 110, final = 95, period = 3650, vcindex = 87, mc = 8)
#'
#' # Error if final viability is beyond limits (0-100 %)
#' StorageTemp(initial = 98, final = -10, period = 3650, vcindex = 87, mc = 8)
#' }
#'
#' # Warning if moisture content is beyond limits (0-100 %)
#' StorageTemp(initial = 98, final = 95, period = 3650, vcindex = 87, mc = 110)
#'
#'
#' #----------------------------------------------------------------------------
#' # Storage period
#' #----------------------------------------------------------------------------
#' # Use index  87
#'  StoragePeriod(initial = 98, final = 95, vcindex = 87, mc = 10, temp = 5,
#'                 years = FALSE)
#'  StoragePeriod(initial = 98, final = 95, vcindex = 87, mc = 10, temp = 5,
#'                 years = TRUE)
#'
#'  # Input the viability constants directly
#'  StoragePeriod(initial = 98, final = 95,
#'                 vcdirect = c(8.242, 4.345, 0.0307, 0.000501),
#'                 mc = 10, temp = 5, years = FALSE)
#'  StoragePeriod(initial = 98, final = 95,
#'                 vcdirect = c(8.242, 4.345, 0.0307, 0.000501),
#'                 mc = 10, temp = 5, years = TRUE)
#'
#'  \dontrun{
#'  # Error if initial viability is beyond limits (0-100 %)
#'  StoragePeriod(initial = 110, final = 95, vcindex = 87, mc = 10, temp = 5)
#'
#'  # Error if final viability is beyond limits (0-100 %)
#'  StoragePeriod(initial = 98, final = -5, vcindex = 87, mc = 10, temp = 5)
#'  }
#'
#'
#'  # Warning if moisture content is beyond limits (0-100 %)
#'  StoragePeriod(initial = 98, final = 95, vcindex = 87, mc = 110, temp = 5)
#'
#'  # Warning if temperature is beyond limits (-20 to 90 degree C)
#'  StoragePeriod(initial = 98, final = 95, vcindex = 87, mc = 10, temp = 95)
#'
#'  # With initial viability 100
#'  StoragePeriod(initial = 100, final = 95, vcindex = 87, mc = 10, temp = 5,
#'                 years = FALSE)
#'  StoragePeriod(initial = 100, final = 95, vcindex = 87, mc = 10, temp = 5,
#'                 years = TRUE)
#'
#'  # With intial viability of 100%, use of PercentAdjust() to avoid extremes
#'  StoragePeriod(initial = PercentAdjust(100, n = 50), final = 95,
#'                 vcindex = 87, mc = 10, temp = 5, years = FALSE)
#'  StoragePeriod(initial = PercentAdjust(100, n = 50), final = 95,
#'                 vcindex = 87, mc = 10, temp = 5, years = TRUE)
#'
#' @seealso \code{\link[viabilitymetrics]{Sigma}},
#'   \code{\link[viabilitymetrics:Percent2Probit]{PercentAdjust}}
#'


#' @rdname FinalViability
#' @export
FinalViability <- function(initial, period, vcindex, vcdirect, mc, temp,
                           years = FALSE) {

  # Check if initial is of type numeric with unit length
  if (!is.numeric(initial) || length(initial) != 1){
    stop("'initial' should be a numeric vector of length 1.")
  }

  # Check limits of initial viability
  if (FALSE %in% (findInterval(initial, c(0, 100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"initial" is beyond limits (0 < "initial" < 100).')
  }

  # Check if period is of type numeric with unit length
  if (!is.numeric(period) || length(period) != 1){
    stop("'period' should be a numeric vector of length 1.")
  }

  if (years == TRUE) {
    period <- period * 365
  } else {
    period <- period
  }
  Ki <- Percent2NED(initial)
  sig <- Sigma(vcindex = vcindex, vcdirect = vcdirect, mc = mc, temp = temp,
               years = FALSE)

  finalv <- Ki - (period / sig)
  finalv <- NED2Percent(finalv)
  return(finalv)

}

#' @rdname FinalViability
#' @export
StorageMC <- function(initial, final, period, vcindex, vcdirect, temp,
                      years = FALSE) {
  # check if both vcindex and vcdirect are given as input
  chk <- c(missing(vcindex), missing(vcdirect))
  if (identical(chk, c(FALSE, FALSE))) {
    stop('Provide only either one of the two arguments\n',
         '"vcindex" or "vcdirect" and not both.')
  }

  # Check if initial is of type numeric with unit length
  if (!is.numeric(initial) || length(initial) != 1){
    stop("'initial' should be a numeric vector of length 1.")
  }

  # Check limits of initial viability
  if (FALSE %in% (findInterval(initial, c(0, 100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"initial" is beyond limits (0 < "initial" < 100).')
  }

  # Check if final is of type numeric with unit length
  if (!is.numeric(final) || length(final) != 1){
    stop("'final' should be a numeric vector of length 1.")
  }

  # Check limits of final viability
  if (FALSE %in% (findInterval(final, c(0, 100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"final" is beyond limits (0 < "final" < 100).')
  }

  # Check if period is of type numeric with unit length
  if (!is.numeric(period) || length(period) != 1){
    stop("'period' should be a numeric vector of length 1.")
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
  if (years == TRUE) {
    period <- period * 365
  } else {
    period <- period
  }

  Ki <- Percent2NED(initial)
  v <- Percent2NED(final)

  # Check if argument years is of type logical with unit length
  if (!missing(years)){
    if (!is.logical(years) || length(years) != 1){
      stop("'years' should be a numeric vector of length 1.")
    }
  }

  #If input is vcindex
  if (!missing(vcindex)) {
    # Extract constants from vcindex
    # check if vcindex is an integer
    if (vcindex %% 1 != 0 && length(n) != 1) {
      stop('"vcindex" is not an integer vector of unit length.')
    }

    # check if vcindex is present in viabilityconstants
    if (!(vcindex %in% viabilityconstants$Index)) {
      stop( paste("specified 'vcindex' is not present in the",
                  " 'viabilityconstants' dataset.\n",
                  "Input a value in the range 1-",
                  max(viabilityconstants$Index), sep = ""))
    }
    vcdirect <- NULL
    vc <- viabilityconstants[vcindex, ]
    Ke <- as.numeric(vc[, 3])
    Cw <- as.numeric(vc[, 4])
    Ch <- as.numeric(vc[, 5])
    Cq <- as.numeric(vc[, 6])
    rm(vc)

  } else {#If input is vcdirect
    # Extract constants from vcdirect
    # check if vcdirect is a numeric vector of length 4 with Ke, Cw, Ch, Cq
    if (length(vcdirect) != 4) {
      stop(paste0(c('"vcdirect" is not of length 4;', '\n',
                    'All four seed viability constants are not specified.')))
    } else {
      # check if vcdirect is numeric vector
      if (!is.numeric(vcdirect)) {
        stop('"vcdirect" is not a numeric vector.')
      }
    }

    vcindex <- NULL
    names(vcdirect) <- c("Ke", "Cw", "Ch", "Cq")
    Ke <- as.numeric(vcdirect[1])
    Cw <- as.numeric(vcdirect[2])
    Ch <- as.numeric(vcdirect[3])
    Cq <- as.numeric(vcdirect[4])

  }

  logmc <- (Ke - Ch*temp - Cq*(temp^2) - log10(period/(Ki - v)))/Cw
  mc <- 10^logmc
  #mc <- exp(logmc*log(10))

  return(mc)

}


#' @rdname FinalViability
#' @export
StorageTemp <- function(initial, final, period, vcindex, vcdirect, mc,
                        years = FALSE, unit = c("celsius", "fahrenheit")) {

  # check if both vcindex and vcdirect are given as input
  chk <- c(missing(vcindex), missing(vcdirect))
  if (identical(chk, c(FALSE, FALSE))) {
    stop('Provide only either one of the two arguments\n',
         '"vcindex" or "vcdirect" and not both.')
  }

  # Check if initial is of type numeric with unit length
  if (!is.numeric(initial) || length(initial) != 1){
    stop("'initial' should be a numeric vector of length 1.")
  }

  # Check limits of initial viability
  if (FALSE %in% (findInterval(initial, c(0, 100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"initial" is beyond limits (0 < "initial" < 100).')
  }

  # Check if final is of type numeric with unit length
  if (!is.numeric(final) || length(final) != 1){
    stop("'final' should be a numeric vector of length 1.")
  }

  # Check limits of final viability
  if (FALSE %in% (findInterval(final, c(0, 100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"final" is beyond limits (0 < "final" < 100).')
  }

  # Check if period is of type numeric with unit length
  if (!is.numeric(period) || length(period) != 1){
    stop("'period' should be a numeric vector of length 1.")
  }

  # Check if mc is of type numeric with unit length
  if (!is.numeric(mc) || length(mc) != 1){
    stop("'mc' should be a numeric vector of length 1.")
  }

  #Check limits of moisture content
  if (FALSE %in% (findInterval(mc, c(0, 100), rightmost.closed = TRUE) == 1)) {
    warning('"mc" is beyond limits (0 < "mc" < 100).')
  }

  # Check if argument years is of type logical with unit length
  if(!missing(years)){
    if(!is.logical(years) || length(years) != 1){
      stop("'years' should be a numeric vector of length 1.")
    }
  }

  unit <- match.arg(unit)
  Ki <- Percent2NED(initial)
  v <- Percent2NED(final)

  # Check if argument years is of type logical with unit length
  if(!missing(years)){
    if(!is.logical(years) || length(years) != 1){
      stop("'years' should be a numeric vector of length 1.")
    }
  }

  if (years == TRUE) {
    period <- period * 365
  } else {
    period <- period
  }

  #If input is vcindex
  if (!missing(vcindex)) {
    # Extract constants from vcindex
    # check if vcindex is an integer
    if (vcindex %% 1 != 0) {
      stop('"vcindex" is not an integer.')
    }

    # check if vcindex is present in viabilityconstants
    if (!(vcindex %in% viabilityconstants$Index)) {
      stop( paste("specified 'vcindex' is not present in the",
                  " 'viabilityconstants' dataset.\n",
                  "Input a value in the range 1-",
                  max(viabilityconstants$Index), sep = ""))
    }
    vcdirect <- NULL
    vc <- viabilityconstants[vcindex, ]
    Ke <- as.numeric(vc[, 3])
    Cw <- as.numeric(vc[, 4])
    Ch <- as.numeric(vc[, 5])
    Cq <- as.numeric(vc[, 6])
    rm(vc)

  } else {#If input is vcdirect
    # Extract constants from vcdirect
    # check if vcdirect is a numeric vector of length 4 with Ke, Cw, Ch, Cq
    if (length(vcdirect) != 4) {
      stop(paste0(c('"vcdirect" is not of length 4;', '\n',
                    'All four seed viability constants are not specified.')))
    } else {
      # check if vcdirect is numeric vector
      if (!is.numeric(vcdirect)) {
        stop('"vcdirect" is not a numeric vector.')
      }
    }

    vcindex <- NULL
    names(vcdirect) <- c("Ke", "Cw", "Ch", "Cq")
    Ke <- as.numeric(vcdirect[1])
    Cw <- as.numeric(vcdirect[2])
    Ch <- as.numeric(vcdirect[3])
    Cq <- as.numeric(vcdirect[4])

  }

  # Solve quadratic for temp
  A <- Cq
  B <- Ch
  C <- (Cw*log10(mc)) - Ke + log10(period/(Ki - v))

  temp <- ((-B) + sqrt(B^2 - (4*A*C)))/(2*A)

  if (unit == "fahrenheit") {
    temp <- (temp * (9/5)) + 32
  }

  return(temp)
}


#' @rdname FinalViability
#' @export
StoragePeriod <- function(initial, final, vcindex, vcdirect, mc, temp,
                          years = FALSE) {

  # Check if initial is of type numeric with unit length
  if (!is.numeric(initial) || length(initial) != 1){
    stop("'initial' should be a numeric vector of length 1.")
  }

  # Check limits of initial viability
  if (FALSE %in% (findInterval(initial, c(0, 100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"initial" is beyond limits (0 < "initial" < 100).')
  }

  # Check if final is of type numeric with unit length
  if (!is.numeric(final) || length(final) != 1){
    stop("'final' should be a numeric vector of length 1.")
  }

  # Check limits of final viability
  if (FALSE %in% (findInterval(final, c(0, 100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"final" is beyond limits (0 < "final" < 100).')
  }


  Ki <- Percent2NED(initial)
  sig <- Sigma(vcindex = vcindex, vcdirect = vcdirect, mc = mc, temp = temp,
               years = FALSE)
  v <- Percent2NED(final)

  period <- sig*(Ki - v)

  if (years == TRUE) {
    period <- period / 365
  } else {
    period <- period
  }

  if (period < 1) {
    warning("Negative storage period computed.")
  }
  return(period)
}
