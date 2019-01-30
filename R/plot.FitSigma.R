#' Plot the fitted seed viability curve from a \code{FitSigma} object
#'
#' \code{plot.FitSigma} plots the fitted seed viability/survival curve from a
#' \code{FitSigma} object as an object of class \code{ggplot}.
#'
#' @param x An object of class \code{FitSigma} obtained as output from the
#'   \code{\link[viabilitymetrics]{FitSigma}} function.
#' @param limits logical. If \code{TRUE}, set the limits of y axis (viability
#'   percentage) between 0 and 100 in the viability curve plot. If
#'   \code{FALSE}, limits are set according to the data. Default is \code{TRUE}.
#' @param annotate logical. If \code{TRUE},
#'   \ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}} and
#'   \ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}} values are annotated on
#'   the plot. Default is \code{TRUE}.
#' @param \dots Default plot arguments.
#'
#' @return The plot of the seed viability curve as an object of class
#'   \code{ggplot}.
#'
#' @seealso \code{\link[viabilitymetrics]{FitSigma}}
#'
#' @import ggplot2
#' @method plot FitSigma
#' @export
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
#'                  viabilitypercent = c(98, 96, 90, 82, 70, 54, 38, 24,
#'                                       12, 6, 2, 0, 0, 0, 0, 98, 94, 85,
#'                                       72, 54, 35, 18, 8, 2, 0,
#'                                       0, 0, 0, 0, 0))
#'
#' plot(df$period, df$viabilitypercent)
#'
#' #----------------------------------------------------------------------------
#' # Generalised linear model with probit link function (without cv)
#' #----------------------------------------------------------------------------
#' model1a <- FitSigma(data = df, viability.percent = "viabilitypercent",
#'                    samp.size = "sampsize", storage.period = "period",
#'                    probit.method = "glm")
#' plot(model1a)
#'
#' #----------------------------------------------------------------------------
#' # Generalised linear model with probit link function (with cv)
#' #----------------------------------------------------------------------------
#' model1b <- FitSigma(data = df, viability.percent = "viabilitypercent",
#'                    samp.size = "sampsize", storage.period = "period",
#'                    probit.method = "glm",
#'                    use.cv = TRUE, control.viability = 98)
#' plot(model1b)
#'
#' #----------------------------------------------------------------------------
#' # Linear model after probit transformation (without cv)
#' #----------------------------------------------------------------------------
#' model2a <- FitSigma(data = df, viability.percent = "viabilitypercent",
#'                    samp.size = "sampsize", storage.period = "period",
#'                    probit.method = "tflm")
#' plot(model2a)
#'
#' #----------------------------------------------------------------------------
#' # Linear model after probit transformation (with cv)
#' #----------------------------------------------------------------------------
#' model2b <- FitSigma(data = df, viability.percent = "viabilitypercent",
#'                    samp.size = "sampsize", storage.period = "period",
#'                    probit.method = "tflm",
#'                    use.cv = TRUE, control.viability = 98)
#' plot(model2b)
#'
plot.FitSigma <- function(x, limits = TRUE, annotate = TRUE, ...){

  Vplot <- ggplot(data = x$data,
                  aes(x = storage.period, y = viability.percent)) +
    geom_point(alpha = 0.5) +
    stat_function(fun = FitSigma.fun, colour = "red2",
                  args = list(ki = x$Ki, sigma = x$sigma)) +
    labs(x = "Storage period", y = "Viability (%)") +
    theme_bw()

  # plot limits
  if (limits == TRUE) {
    Vplot <- Vplot + coord_cartesian(xlim = c(0, max(x$data$storage.period)),
                                     ylim = c(0, 100))
  }

  if (annotate == TRUE) {
    Vplot <- Vplot + annotate("text", x = quantile(x$data$storage.period)[4],
                   y = quantile(x$data$viability.percent,
                                prob = seq(0, 1, length = 11),
                                type = 5)[10],  parse = TRUE,
                   label = paste("atop(K[i] == ", round(x$Ki, 2), ",",
                                 "~sigma == ", round(x$sigma, 2), ")",
                                 sep = ""))
  }

  Vplot <- Vplot +
    theme(axis.text = element_text(colour = "black"))

  return(Vplot)
}


FitSigma.fun <- function(p, ki, sigma) {

  NED2Percent(ki - (p/sigma))

}

