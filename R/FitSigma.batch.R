#' Seed viability curve fitting to estimate multiple values of
#' \ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}} and
#' \ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}} according to a grouping
#' variable
#'
#' Fit seed viability/survival curve to estimate multiple values of the seed lot
#' constant (\ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}}) and the
#' period to lose unit probit viability
#' (\ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}}) according to a grouping
#' variable.
#'
#' @param data A data frame with the seed viability data recorded periodically.
#'   It should possess columns with data on \itemize{ \item Viability percentage
#'   (to be indicated by the argument \code{viability.percent}), \item Sample
#'   size (to be indicated by the argument \code{samp.size}), \item Storage
#'   period (to be indicated by the argument \code{storage.period}) and \item
#'   Grouping variable (to be indicated by the argument \code{group}). }
#' @param group The name of the column in \code{data} with grouping variable as
#'   a character string.
#' @param \dots Arguments to be passed on to \code{FitSigma}.
#'
#' @return A list of class \code{FitSigma.batch} with the following components:
#'   \item{data}{A data frame with the data used for computing the models.}
#'   \item{models}{A data frame with the group-wise values of model parameters,
#'   \ifelse{html}{\out{<i>K<sub>i</sub></i>}}{\eqn{K_{i}}} and
#'   \ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}} and the fit statistics.}
#'
#' @seealso \code{\link[viabilitymetrics]{FitSigma}}
#'
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' df1 <- data.frame(period = 1:15)
#' df1$sampsize <- 50
#'
#' df3 <- df2 <- df1
#'
#' df1$crop <- "Wheat"
#' df1$vcindex <- 136
#' df2$crop <- "Soybean"
#' df2$vcindex <- 59
#' df3$crop <- "Tomato"
#' df3$vcindex <- 78
#'
#' df <- rbind(df1, df2, df3)
#' rm(df1, df2, df3)
#'
#' df$viabilitypercent <- mapply(FinalViability, period = df$period,
#'                               vcindex = df$vcindex,
#'                               MoreArgs = list(initial = PercentAdjust(100, 50),
#'                                               mc = 7, temp = 25,
#'                                               years = TRUE),
#'                               SIMPLIFY = TRUE)
#'
#' df$viabilitypercent <- round(df$viabilitypercent)
#' df$viabilitypercent[df$viabilitypercent == 99] <- 100
#' df$crop <- as.factor(df$crop)
#'
#' df <- df[, c("crop", "period", "viabilitypercent", "sampsize")]
#' df[df$crop == "Wheat",]$viabilitypercent <- df[df$crop == "Wheat",]$viabilitypercent - 2
#'
#'
#' plot(df$period, df$viabilitypercent, col = df$crop)
#' legend(10, 60, legend=levels(df$crop),
#'        col = c("black", "red", "green"), pch = 1)
#'
#' #----------------------------------------------------------------------------
#' # Generalised linear model with probit link function (without cv)
#' #----------------------------------------------------------------------------
#' model1a <- FitSigma.batch(data = df, group = "crop",
#'                           viability.percent = "viabilitypercent",
#'                           samp.size = "sampsize", storage.period = "period",
#'                           probit.method = "glm")
#' model1a
#'
#' #----------------------------------------------------------------------------
#' # Generalised linear model with probit link function (with cv)
#' #----------------------------------------------------------------------------
#' model1b <- FitSigma.batch(data = df, group = "crop",
#'                           viability.percent = "viabilitypercent",
#'                           samp.size = "sampsize", storage.period = "period",
#'                           probit.method = "glm",
#'                           use.cv = TRUE, control.viability = 98)
#' model1b
#'
#' #----------------------------------------------------------------------------
#' # Linear model after probit transformation (without cv)
#' #----------------------------------------------------------------------------
#' model2a <- FitSigma.batch(data = df, group = "crop",
#'                           viability.percent = "viabilitypercent",
#'                           samp.size = "sampsize", storage.period = "period",
#'                           probit.method = "tflm")
#' model2a
#'
#' #----------------------------------------------------------------------------
#' # Linear model after probit transformation (with cv)
#' #----------------------------------------------------------------------------
#' model2b <- FitSigma.batch(data = df, group = "crop",
#'                           viability.percent = "viabilitypercent",
#'                           samp.size = "sampsize", storage.period = "period",
#'                           probit.method = "tflm",
#'                           use.cv = TRUE, control.viability = 98)
#' model2b
FitSigma.batch <- function(data, group, ...) {
  # Check if data.frame
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object.')
  }

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame.')
    data <- as.data.frame(data)
  }

  # Check if group column present in data
  if (!(group %in% colnames(data))) {
    stop(paste('Column ', group,
               ' specified as the sample size column',
               ' is not present in "data".',
               sep = ""))
  }

  outlist <- vector(mode = "list", length = nlevels(data[, group]))
  names(outlist) <- levels(data[, group])

  probitm <- NA
  cvm <- NA


  for(i in seq_along(levels(data[, group]))) {
    pmod <- FitSigma(data = data[data[, group] == levels(data[, group])[i], ],
                     ...)
    if (grepl("ERROR:", pmod$message)) {
      outlist[[i]] <- data.frame(group = names(outlist)[i],
                                 Ki = NA_integer_,
                                 Ki_se = NA_integer_,
                                 Ki_pvalue = NA_integer_,
                                 sigma.inv = NA_integer_,
                                 sigma.inv_se = NA_integer_,
                                 sigma.inv_pvalue = NA_integer_,
                                 sigma = NA_integer_, stringsAsFactors = FALSE)
      outlist[[i]] <- cbind(outlist[[i]], pmod$message,
                            stringsAsFactors = FALSE)
    } else {
      outlist[[i]] <- data.frame(group = names(outlist)[i],
                                 Ki = pmod$Ki,
                                 Ki_se = pmod$parameters[pmod$parameters$term == "Ki", ]$std.error,
                                 Ki_pvalue = pmod$parameters[pmod$parameters$term == "Ki", ]$p.value,
                                 sigma.inv = pmod$parameters[pmod$parameters$term == "1/sigma", ]$estimate,
                                 sigma.inv_se = pmod$parameters[pmod$parameters$term == "1/sigma", ]$std.error,
                                 sigma.inv_pvalue = pmod$parameters[pmod$parameters$term == "1/sigma", ]$p.value,
                                 sigma = pmod$sigma, stringsAsFactors = FALSE)
      outlist[[i]] <- cbind(outlist[[i]], pmod$fit, pmod$message,
                            stringsAsFactors = FALSE)
    }

    probitm <- ifelse(is.na(probitm), attributes(pmod)$method, probitm)
    cvm <- ifelse(is.na(cvm), attributes(pmod)$cv, cvm)

    rm(pmod)
  }

  out <- list(data = data,
              models = data.frame(dplyr::bind_rows(outlist),
                                  stringsAsFactors = FALSE))

  attr(out, "method") <- probitm
  attr(out, "cv") <- cvm

  class(out) <- "FitSigma.batch"
  return(out)
}
