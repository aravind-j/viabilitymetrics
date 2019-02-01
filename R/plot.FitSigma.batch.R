#' Plot the fitted seed viability curves from a \code{FitSigma.batch} object
#'
#' \code{plot.FitSigma.batch} plots the group-wise fitted seed
#' viability/survival curves from a \code{FitSigma.batch} object as an object of
#' class \code{ggplot}.
#'
#' @param x An object of class \code{FitSigma.batch} obtained as output from the
#'   \code{\link[viabilitymetrics]{FitSigma.batch}} function.
#' @param limits logical. If \code{TRUE}, set the limits of y axis (viability
#'   percentage) between 0 and 100 in the viability curve plot. If
#'   \code{FALSE}, limits are set according to the data. Default is \code{TRUE}.
#' @param grid logical. If \code{TRUE}, a symmetric matrix grid of plots is
#'   produced instead of a single plot with multiple curves. Default is
#'   \code{FALSE}.
#' @param \dots Default plot arguments.
#'
#' @return The plot of the seed viability curves as an object of class
#'   \code{ggplot}.
#'
#' @seealso \code{\link[viabilitymetrics]{FitSigma.batch}}
#'
#' @import ggplot2
#' @method plot FitSigma.batch
#' @export
#'
#' @examples
#'
#' data(seedsurvival)
#' df <- seedsurvival[seedsurvival$moistruecontent == 7 &
#'                      seedsurvival$temperature == 25,
#'                    c("crop", "storageperiod", "rep",
#'                      "viabilitypercent", "sampsize")]
#'
#' #----------------------------------------------------------------------------
#' # Generalised linear model with probit link function (without cv)
#' #----------------------------------------------------------------------------
#' model1a <- FitSigma.batch(data = df, group = "crop",
#'                           viability.percent = "viabilitypercent",
#'                           samp.size = "sampsize",
#'                           storage.period = "storageperiod",
#'                           probit.method = "glm")
#' plot(model1a)
#' plot(model1a, grid = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Generalised linear model with probit link function (with cv)
#' #----------------------------------------------------------------------------
#' model1b <- FitSigma.batch(data = df, group = "crop",
#'                           viability.percent = "viabilitypercent",
#'                           samp.size = "sampsize",
#'                           storage.period = "storageperiod",
#'                           probit.method = "glm",
#'                           use.cv = TRUE, control.viability = 98)
#' plot(model1b)
#' plot(model1b, grid = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Linear model after probit transformation (without cv)
#' #----------------------------------------------------------------------------
#' model2a <- FitSigma.batch(data = df, group = "crop",
#'                           viability.percent = "viabilitypercent",
#'                           samp.size = "sampsize",
#'                           storage.period = "storageperiod",
#'                           probit.method = "tflm")
#' plot(model2a)
#' plot(model2a, grid = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Linear model after probit transformation (with cv)
#' #----------------------------------------------------------------------------
#' model2b <- FitSigma.batch(data = df, group = "crop",
#'                           viability.percent = "viabilitypercent",
#'                           samp.size = "sampsize",
#'                           storage.period = "storageperiod",
#'                           probit.method = "tflm",
#'                           use.cv = TRUE, control.viability = 98)
#' plot(model2b)
#' plot(model2b, grid = TRUE)
#'
plot.FitSigma.batch <- function(x, limits = TRUE, grid = FALSE, ...){

  x$data$group <- as.factor(x$data$group)

  Vplot <- ggplot(data = x$data,
                  aes(x = storage.period, y = viability.percent,
                      group = group)) +
    geom_point(aes(colour = group), alpha = 0.8) +
    labs(x = "Storage period", y = "Viability (%)") +
    theme_bw() +
    theme(axis.text = element_text(colour = "black"),
          legend.title = element_blank())

  # plot grid
  if (grid) {
    Vplot <- Vplot + facet_wrap(~group)
  }

  gcol <- unique(ggplot_build(Vplot)$data[[1]][["colour"]])
  glevels <- levels(x$data$group)

  for(i in seq_along(glevels)){
    if (!grepl("ERROR:", x$models[x$models$group == glevels[i], ]$message)) {
      Vplot <- Vplot +
        stat_function(data = x$data[x$data$group == glevels[i], ],
                      fun = FitSigma.fun, colour = gcol[i],
                      args = list(ki = x$models[x$models$group == glevels[i],
                                               ]$Ki,
                                  sigma = x$models[x$models$group == glevels[i],
                                                  ]$sigma))
    }
  }


  # plot limits
  if (limits == TRUE) {
    Vplot <- Vplot + coord_cartesian(xlim = c(0, max(x$data$storage.period)),
                                     ylim = c(0, 100))
  }

  return(Vplot)
}
