#' Predict variables from the seed viability equation
#'
#' Predict the variables, final viability, storage period, storage temperature
#' and storage period from the improved seed viability equation (Ellis and
#' Roberts, 1980). \describe{ \item{\code{FinalViability}}{Compute the final
#' viability after a period of storage at a set of storage conditions (seed
#' moisture content and temperature).} \item{\code{StoragePeriod}}{Compute the
#' storage period from the final viability and the storage conditions (seed
#' moisture content and temperature).} \item{\code{StorageMC}}{Compute the
#' storage moisture content to give final viability at a particular storage
#' temperature.} \item{\code{StorageTemp}}{Compute the storage temperature to
#' give final viability at a particular storage temperature.} }
#'
#' The improved seed viability equation of Ellis and Roberts (1980) describes
#' the relationship between final viability, storage period and storage
#' environment conditions as follows:
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
#' Germination percentages plotted against storage times yield a sigmoid seed
#' survival curve which is converted to a linear relationship by the probit
#' transformation with slope \ifelse{html}{\out{<i><sup>1</sup> &frasl;
#' <sub>&sigma;</sub></i>}}{\eqn{\frac{1}{\sigma}}}.
#'
#' The slope is determined as follows:
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><em>&sigma; = 10</em><sup>
#' <em>K<sub>E</sub> &minus; C<sub>W</sub></em> log<em>m &minus; C<sub>H</sub>t
#' &minus; C<sub>Q</sub>t<sup>2</sup></em></sup></p>}}{\deqn{\sigma =
#' 10^{K_{E}-C_{W}\log{m}-C_{H}t-C_{Q}t^2}}}
#'
#' Where, \ifelse{html}{\out{<i>m</i>}}{\eqn{m}} is the moisture content (fresh
#' weight basis), \ifelse{html}{\out{<i>t</i>}}{\eqn{t}} is the temperature and
#' \ifelse{html}{\out{<i>K<sub>E</sub></i>}}{\eqn{K_{E}}},
#' \ifelse{html}{\out{<i>C<sub>W</sub></i>}}{\eqn{C_{W}}},
#' \ifelse{html}{\out{<i>C<sub>H</sub></i>}}{\eqn{C_{H}}} and
#' \ifelse{html}{\out{<i>C<sub>Q</sub></i>}}{\eqn{C_{Q}}} are the
#' species-specific seed viability constants.
#'
#' On the basis of the the improved seed viability equation,
#' \ifelse{html}{\out{<i>v</i>}}{\eqn{v}},
#' \ifelse{html}{\out{<i>p</i>}}{\eqn{p}},
#' \ifelse{html}{\out{<i>m</i>}}{\eqn{m}} and
#' \ifelse{html}{\out{<i>t</i>}}{\eqn{t}} can be estimated as follows:
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><em>v = K<sub>i</sub>
#' &minus; [ <sup>p</sup> &frasl; <sub>&sigma;</sub> ]
#' </em></p>}}{\deqn{v=K_{i}-\frac{p}{\sigma}}}
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><em>p =
#' &sigma;(K<sub>i</sub>-v)</em></p>}}{\deqn{p = \sigma(K_{i}-v)}}
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><em>m =
#' 10<sup><big>[</big><sup><big>[</big>K<sub>E</sub> &minus; C<sub>H</sub>t
#' &minus; C<sub>Q</sub>t<sup>2</sup> &minus; log<big>(</big><sup> p</sup>
#' &frasl; <sub>(K<sub>i</sub> &minus; v)</sub><big>)</big><big>]</big></sup>
#' &frasl; <sub>C<sub>W</sub></sub><big>]</big></sup></em></p>}}{\deqn{m =
#' 10^{\left [ \frac{K_{E}-C_{H}t-C_{Q}t^{2}-\log\left ( \frac{p}{K_{i}-v}
#' \right )}{C_{W}} \right ]}}}
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><em>t =
#' <sup><big>[</big>&minus;C<sub>H</sub> &plusmn;
#' &radic;<big>[</big>C<sub>H</sub><sup>2</sup> &minus;
#' 4C<sub>Q</sub><big>(</big>C<sub>W</sub>logm &minus; K<sub>E</sub> +
#' log<big>(</big><sup>p</sup> &frasl; <sub>(K<sub>i</sub> &minus;
#' v)</sub><big>)</big><big>)</big><big>]</big><big>]</big></sup> &frasl; <sub>
#' 2C<sub>Q</sub></sub></em></p>}}{\deqn{t = \frac{-C_{H} \pm
#' \sqrt{C_{H}^{2}-4C_{Q}\left ( C_{W}\log{m}-K_{E}+ \log\left (
#' \frac{p}{K_{i}-v} \right ) \right )}}{2C_{Q}}}}
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
#' @inheritParams Sigma
#' @param unit The unit of temperature.
#'
#' @encoding UTF-8
#' @import Rdpack
#' @references \insertRef{ellis_improved_1980}{viabilitymetrics}
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
#'                          ignore.case = T),]
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


#' @name FinalViability
#' @export
FinalViability <- function(initial, period, vcindex, vcdirect, mc, temp,
                           years = FALSE) {

  if (FALSE %in% (findInterval(initial, c(0,100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"initial" is beyond limits (0 < "initial" < 100)')
  }

  if (years == TRUE) {
    period = period*365
  } else {
    period = period
  }
  Ki <- Percent2NED(initial)
  sig <- Sigma(vcindex = vcindex, vcdirect = vcdirect, mc = mc, temp = temp,
               years = FALSE)

  finalv <- Ki - (period/sig)
  finalv <- NED2Percent(finalv)
  return(finalv)

}

#' @name FinalViability
#' @export
StorageMC <- function(initial, final, period, vcindex, vcdirect, temp,
                      years = FALSE) {

  if (FALSE %in% (findInterval(initial, c(0,100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"initial" is beyond limits (0 < "initial" < 100)')
  }

  if (FALSE %in% (findInterval(final, c(0,100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"final" is beyond limits (0 < "final" < 100)')
  }

  if (FALSE %in% (findInterval(temp, c(-20,90),
                               rightmost.closed = TRUE) == 1)) {
    warning('"temp" is beyond limits (-20 < "temp" < 90)')
  }
  if (years == TRUE) {
    period = period*365
  } else {
    period = period
  }

  Ki <- Percent2NED(initial)
  v <- Percent2NED(final)

  #If input is vcindex
  if (!missing(vcindex)) {
    # Extract constants from vcindex
    # check if vcindex is an integer
    if (vcindex %% 1 != 0) {
      stop('"vcindex" is not an integer')
    }

    # check if vcindex is present in viabilityconstants
    if (!(vcindex %in% viabilityconstants$Index)) {
      stop( paste("specified 'vcindex' is not present in the 'viabilityconstants' dataset.\n",
                  "Input a value in the range 1-",
                  max(viabilityconstants$Index), sep = ""))
    }
    vcdirect <- NULL
    vc <- viabilityconstants[vcindex,]
    Ke <- as.numeric(vc[,3])
    Cw <- as.numeric(vc[,4])
    Ch <- as.numeric(vc[,5])
    Cq <- as.numeric(vc[,6])
    rm(vc)

  } else {#If input is vcdirect
    # Extract constants from vcdirect
    # check if vcdirect is a numeric vector of length 4 with Ke, Cw, Ch, Cq
    if (length(vcdirect) != 4) {
      stop(paste0(c('"vcdirect" is not of length 4;', '\n',
                    'All four seed viability constants are not specified')))
    } else {
      # check if vcdirect is numeric vector
      if (!is.numeric(vcdirect)) {
        stop('"vcdirect" is not a numeric vector')
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


#' @name FinalViability
#' @export
StorageTemp <- function(initial, final, period, vcindex, vcdirect, mc,
                        years = FALSE, unit = c("celsius", "fahrenheit")) {

  if (FALSE %in% (findInterval(initial, c(0,100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"initial" is beyond limits (0 < "initial" < 100)')
  }

  if (FALSE %in% (findInterval(final, c(0,100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"final" is beyond limits (0 < "final" < 100)')
  }

  #Check limits of moisture content
  if (FALSE %in% (findInterval(mc, c(0,100),
                               rightmost.closed = TRUE) == 1)) {
    warning('"mc" is beyond limits (0 < "mc" < 100)')
  }

  unit <- match.arg(unit)
  Ki <- Percent2NED(initial)
  v <- Percent2NED(final)

  if (years == TRUE) {
    period = period*365
  } else {
    period = period
  }

  #If input is vcindex
  if (!missing(vcindex)) {
    # Extract constants from vcindex
    # check if vcindex is an integer
    if (vcindex %% 1 != 0) {
      stop('"vcindex" is not an integer')
    }

    # check if vcindex is present in viabilityconstants
    if (!(vcindex %in% viabilityconstants$Index)) {
      stop( paste("specified 'vcindex' is not present in the 'viabilityconstants' dataset.\n",
                  "Input a value in the range 1-",
                  max(viabilityconstants$Index), sep = ""))
    }
    vcdirect <- NULL
    vc <- viabilityconstants[vcindex,]
    Ke <- as.numeric(vc[,3])
    Cw <- as.numeric(vc[,4])
    Ch <- as.numeric(vc[,5])
    Cq <- as.numeric(vc[,6])
    rm(vc)

  } else {#If input is vcdirect
    # Extract constants from vcdirect
    # check if vcdirect is a numeric vector of length 4 with Ke, Cw, Ch, Cq
    if (length(vcdirect) != 4) {
      stop(paste0(c('"vcdirect" is not of length 4;', '\n',
                    'All four seed viability constants are not specified')))
    } else {
      # check if vcdirect is numeric vector
      if (!is.numeric(vcdirect)) {
        stop('"vcdirect" is not a numeric vector')
      }
    }

    vcindex <- NULL
    names(vcdirect) <- c("Ke", "Cw", "Ch", "Cq")
    Ke <- as.numeric(vcdirect[1])
    Cw <- as.numeric(vcdirect[2])
    Ch <- as.numeric(vcdirect[3])
    Cq <- as.numeric(vcdirect[4])

  }

  # Solve quadtatic for temp
  A <- Cq
  B <- Ch
  C <- (Cw*log10(mc)) - Ke + log10(period/(Ki - v))

  temp <- ((-B) + sqrt(B^2 - (4*A*C)))/(2*A)

  if (unit == "fahrenheit") {
    temp <- (temp * (9/5)) + 32
  }

  return(temp)
}


#' @name FinalViability
#' @export
StoragePeriod <- function(initial, final, vcindex, vcdirect, mc, temp,
                          years = FALSE) {


  if (FALSE %in% (findInterval(initial, c(0,100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"initial" is beyond limits (0 < "initial" < 100)')
  }

  if (FALSE %in% (findInterval(final, c(0,100),
                               rightmost.closed = TRUE) == 1)) {
    stop('"final" is beyond limits (0 < "final" < 100)')
  }

  Ki <- Percent2NED(initial)
  sig <- Sigma(vcindex = vcindex, vcdirect = vcdirect, mc = mc, temp = temp,
               years = FALSE)
  v <- Percent2NED(final)

  period <- sig*(Ki - v)

  if (years == TRUE) {
    period <- period/365
  } else {
    period <- period
  }

  if (period < 1) {
    warning("Negative storage period computed")
  }
  return(period)
}

