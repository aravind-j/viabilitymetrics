#' Period to lose unit probit viability
#'
#' \code{Sigma} calculates the period to lose one probit viability
#' (\ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}}) under storage at a given
#' moisture content and temperature.
#'
#' This function computes the period to lose one probit viability
#' (\ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}}) according to the improved
#' seed viability equation of
#' \insertCite{ellis_improved_1980;textual}{viabilitymetrics} as follows.
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
#' The slope is determined as follows.
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><em>&sigma; = 10</em><sup>
#' <em>K<sub>E</sub> &minus; C<sub>W</sub></em> log<em>m &minus; C<sub>H</sub>t
#' &minus; C<sub>Q</sub>t<sup>2</sup></em></sup></p>}}{\deqn{\sigma =
#' 10^{K_{E}-C_{W}\log{m}-C_{H}t-C_{Q}t^2}}}
#'
#' Where, \ifelse{html}{\out{<i>v</i>}}{\eqn{v}} is the probit percentage
#' viability at storage time \ifelse{html}{\out{<i>p</i>}}{\eqn{p}} (final
#' viability), \ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}} is the
#' probit percentage viability of the seedlot at the beginning of storage
#' (seedlot constant), \ifelse{html}{\out{<i>m</i>}}{\eqn{m}} is the moisture
#' content (fresh weight basis), \ifelse{html}{\out{<i>t</i>}}{\eqn{t}} is the
#' temperature and \ifelse{html}{\out{<i>K<sub>E</sub></i>}}{\eqn{K_{E}}},
#' \ifelse{html}{\out{<i>C<sub>W</sub></i>}}{\eqn{C_{W}}},
#' \ifelse{html}{\out{<i>C<sub>H</sub></i>}}{\eqn{C_{H}}} and
#' \ifelse{html}{\out{<i>C<sub>Q</sub></i>}}{\eqn{C_{Q}}} are the
#' species-specific seed viability constants.
#'
#' The value of the species-specific seed viability constants can be specified
#' either directly in the arguement \code{vcdirect} or as the index value of the
#' required seed viability constants from the \code{\link{viabilityconstants}}
#' dataset through the argument \code{vcindex}.
#'
#' The value of this prediction is appropriate for temperature between -20 to 90
#' °C and seed moisture content between 5 to 25\%. For values beyond this range,
#' a warning will be displayed.
#'
#' @param vcindex An interger value indicating the index of seed viability.
#'   constants to be used from the \code{viabilityconstants} dataset in the
#'   package.
#' @param vcdirect A numeric vector of length 4 with the four viability
#'   constants \emph{viz.}:
#'   \ifelse{html}{\out{<i>K<sub>E</sub></i>}}{\eqn{K_{E}}},
#'   \ifelse{html}{\out{<i>C<sub>W</sub></i>}}{\eqn{C_{W}}},
#'   \ifelse{html}{\out{<i>C<sub>H</sub></i>}}{\eqn{C_{H}}} and
#'   \ifelse{html}{\out{<i>C<sub>Q</sub></i>}}{\eqn{C_{Q}}}.
#' @param mc Moisture content.
#' @param temp Temperature in °C.
#' @param years If \code{TRUE}, returns the output period in years instead of
#'   days.
#'
#' @return The period to lose one probit in days or years (according to argument
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
#' #----------------------------------------------------------------------------
#' # Days/Years to lose unit probit viability for rice seeds stored at
#' # 5 degree celsius and 10% moisture content.
#' #----------------------------------------------------------------------------
#' # Fetch the index from viabilityconstants dataset
#' viabilityconstants[grepl("oryza", x = viabilityconstants$Species,
#'                          ignore.case = TRUE),]
#' # Use index 87
#' Sigma(vcindex = 87, mc = 10, temp = 5)
#' Sigma(vcindex = 87, mc = 10, temp = 5, years = TRUE)
#'
#' # Input the viability constants directly
#' Sigma(vcdirect = c(8.242, 4.345, 0.0307, 0.000501), mc = 10, temp = 5)
#' Sigma(vcdirect = c(8.242, 4.345, 0.0307, 0.000501), mc = 10, temp = 5,
#'       years = TRUE)
#'
#' # Warning if moisture content is beyond limits (0-100 %)
#' Sigma(vcindex = 87, mc = 110, temp = 5)
#'
#' # Warning if temperature is beyond limits (-20 to 90 degree C)
#' Sigma(vcindex = 87, mc = 10, temp = 95)
#'
#' #----------------------------------------------------------------------------
#' # Days/Years to lose unit probit viability for soybean seeds stored at
#' # -18 degree celsius and 8% moisture content.
#' #----------------------------------------------------------------------------
#' # Fetch the index from viabilityconstants dataset
#' viabilityconstants[grepl("glycine", x = viabilityconstants$Species,
#'                          ignore.case = TRUE),]
#' # Use index  59
#' Sigma(vcindex = 59, mc = 8, temp = -18)
#' Sigma(vcindex = 59, mc = 8, temp = -18, years = TRUE)
#'
#' # Input the viability constants directly
#' Sigma(vcdirect = c(7.292, 3.996, 0.0295, 0.000491), mc = 8, temp = -18)
#' Sigma(vcdirect = c(7.292, 3.996, 0.0295, 0.000491), mc = 8, temp = -18,
#'       years = TRUE)
#'
#' # Warning if moisture content is beyond limits (0-100 %)
#' Sigma(vcindex = 59, mc = 110, temp = 5)
#'
#' # Warning if temperature is beyond limits (-20 to 90 degree C)
#' Sigma(vcindex = 59, mc = 10, temp = 95)
#'
Sigma <- function(vcindex, vcdirect, mc, temp, years = FALSE) {

  # check if both vcindex and vcdirect are given as input
  chk <- c(missing(vcindex), missing(vcdirect))
  if (identical(chk, c(FALSE, FALSE))) {
    stop('Provide only either one of the two arguments\n"vcindex" or "vcdirect" and not both')
  }

  # Check if temp is of type numeric with unit length
  if (!is.numeric(temp) || length(temp) != 1){
    stop("'temp' should be a numeric vector of length 1")
  }

  # Check limits of temperature
  if (FALSE %in% (findInterval(temp, c(-20,90),
                               rightmost.closed = TRUE) == 1)) {
    warning('"temp" is beyond limits (-20 < "temp" < 90)')
  }

  # Check if mc is of type numeric with unit length
  if (!is.numeric(mc) || length(mc) != 1){
    stop("'mc' should be a numeric vector of length 1")
  }

 #Check limits of moisture content
  if (FALSE %in% (findInterval(mc, c(0,100), rightmost.closed = TRUE) == 1)) {
    warning('"mc" is beyond limits (0 < "mc" < 100)')
  }

  # Check if argument years is of type logical with unit length
  if(!missing(years)){
    if(!is.logical(years) || length(years) != 1){
      stop("'years' should be a numeric vector of length 1")
    }
  }

  #If input is vcindex
  if (!missing(vcindex)) {
    # Extract constants from vcindex
    # check if vcindex is an integer
    if (vcindex %% 1 != 0 && length(n) != 1) {
      stop('"vcindex" is not an integer vector of unit length')
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

  if (years == TRUE) {
    sig <- (10 ^ (Ke - (Cw * log10(mc)) - Ch*temp - Cq*(temp^2)))/365
  } else {
    sig <- 10 ^ (Ke - (Cw * log10(mc)) - Ch*temp - Cq*(temp^2))
  }

  return(sig)
}
