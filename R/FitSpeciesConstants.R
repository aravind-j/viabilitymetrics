
#' @import gnm
FitSpeciesConstants <- function(data, storage.mc, storage.t,
                                viability.percent, samp.size,
                                storage.period, one.step = TRUE,
                                generalised.model = TRUE,
                                universal.temp.constants = FALSE,
                                use.cv = FALSE, control.viability = 100) {

  # Check if data.frame
  if (!is.data.frame(data)) {
    stop('"data" should be a data frame object.')
  }

  if (any(c("tbl_dataf", "tbl") %in% class(data))) {
    warning('"data" is of type tibble\nCoercing to data frame.')
    data <- as.data.frame(data)
  }

  # Check if storage.mc column present in data
  if (!(storage.mc %in% colnames(data))) {
    stop(paste('Column ', storage.mc,
               ' specified as the storage moisture content column',
               ' is not present in "data".',
               sep = ""))
  }

  if (!universal.temp.constants) {
    # Check if storage.t column present in data
    if (!(storage.t %in% colnames(data))) {
      stop(paste('Column ', storage.t,
                 ' specified as the storage temperature column',
                 ' is not present in "data".',
                 sep = ""))
    }
  }

  # Check if viability.percent column present in data
  if (!(viability.percent %in% colnames(data))) {
    stop(paste('Column ', viability.percent,
               ' specified as the viability percentage column',
               ' is not present in "data".',
               sep = ""))
  }

  # Check if samp.size column present in data
  if (!(samp.size %in% colnames(data))) {
    stop(paste('Column ', samp.size,
               ' specified as the sample size column',
               ' is not present in "data".',
               sep = ""))
  }

  # Check if storage.period column present in data
  if (!(storage.period %in% colnames(data))) {
    stop(paste('Column ', storage.period,
               ' specified as the storage time period column',
               ' is not present in "data".',
               sep = ""))
  }

  # Check if storage.mc is of type numeric
  if (!is.numeric(data[, storage.mc])) {
    stop('"storage.mc" is not of type numeric.')
  }

  if (!universal.temp.constants) {
    # Check if storage.t is of type numeric
    if (!is.numeric(data[, storage.t])) {
      stop('"storage.t" is not of type numeric.')
    }
  }

  # Check if viability.percent is of type numeric
  if (!is.numeric(data[, viability.percent])) {
    stop('"viability.percent" is not of type numeric.')
  }

  # Check if samp.size is of type numeric
  if (!is.numeric(data[, samp.size])) {
    stop('"samp.size" is not of type numeric.')
  }

  # Check if storage.period is of type numeric
  if (!is.numeric(data[, storage.period])) {
    stop('"storage.period" is not of type numeric.')
  }

  # Check if viability.percent is within range
  if (any(!findInterval(data[, viability.percent], c(0, 100),
                        rightmost.closed = TRUE) == 1)) {
    stop('Data in "viability.percent" is not within range',
         ' (0 < "viability.percent" < 100).')
  }

  # Check probit.method
  probit.method <- match.arg(probit.method)

  # Check if argument one.step is of type logical with unit length
  if (!is.logical(one.step) || length(one.step) != 1) {
    stop("'one.step' should be a logical vector of length 1.")
  }

  # Check if argument generalised.model is of type logical with unit length
  if (!is.logical(generalised.model) || length(generalised.model) != 1) {
    stop("'generalised.model' should be a logical vector of length 1.")
  }

  # Check if argument use.cv is of type logical with unit length
  if (!is.logical(use.cv) || length(use.cv) != 1) {
    stop("'use.cv' should be a logical vector of length 1.")
  }

  if(use.cv) {
    # Check if control.viability is within range
    if (control.viability > 100 || control.viability < 0) {
      stop('"control.viability" is not within range',
           ' (0 < "control.viability" < 100).')
    }
    # Check if control.viability > any viability.percent
    if (any(data[, viability.percent] > control.viability)) {
      stop('Values > control.viability" exist in "viability.percent".')
    }
  }

  # Check if argument universal.temp.constants is of type logical
  # with unit length
  if (!is.logical(universal.temp.constants) ||
      length(universal.temp.constants) != 1) {
    stop("'universal.temp.constants' should be a logical vector of length 1.")
  }

  if (universal.temp.constants) {
    data <- data.frame(mc = data[, storage.mc],
                       storage.period = data[, c(storage.period)],
                       viability.percent = data[, c(viability.percent)],
                       samp.size = data[, c(samp.size)])
  } else {
    data <- data.frame(mc = data[, storage.mc],
                       temp = data[, storage.t],
                       storage.period = data[, c(storage.period)],
                       viability.percent = data[, c(viability.percent)],
                       samp.size = data[, c(samp.size)])
  }

  data$viability.count <- (data$viability.percent * data$samp.size) / 100


  if (one.step == TRUE) { # One step fitting




    # gnm(cbind(viability.count, samp.size - viability.count) ~ FitSC.nonlin(p = storage.period,
    #                                      mc = mc, temp = temp),
    #     family = binomial(link = "probit"),
    #     data  = data, start = c(6, 3, 0.03, 0.002))


  } else { # Two step fitting

    # Step 1 : Fetch sigmas
    #----------------------
    data$mc_temp <- interaction(as.factor(data$mc), as.factor(data$temp),
                                sep = "_")

    # FitSigma.batch(data = data, group = "mc_temp",
    #                viability.percent = "viability.percent",
    #                samp.size = "samp.size",
    #                storage.period = "storage.period")


  }



}



















# Function of class "nonlin" for gnm
FitSC.nonlin <- function(p, mc, temp) {

  list(predictors = list(ke = 1, cw = 1, ch = 1, cq = 1),
       variables = list(substitute(p), substitute(mc), substitute(temp)),
       term = function(predLabels, varLabels) {
         sprintf("%s / (10 ^ (%s - (%s * log10(%s)) - (%s * %s) - (%s * ((%s) ^ 2))))",
                 varLabels[1], predLabels[1],
                 predLabels[2], varLabels[2],
                 predLabels[3], varLabels[3],
                 predLabels[4], varLabels[3])
         })

}
class(FitSC.nonlin) <- "nonlin"

FitSC.nonlin(p, mc, temp)$term(c("ke", "cw", "ch", "cq"),
                               c("p", "mc", "temp"))

# constraints ?
# - LHS: 0-100% OR 0-CV%
# start ?

# Function for plotting
FitSC.fun <- function(p, mc, temp, ki, ke, cw, ch, cq) {

  NED2Percent(ki - (p / (10 ^ (ke - (cw * log(mc))
                               - (ch * temp) - (cq * (temp ^ 2) )))))

}

# model without t^2

# Function of class "nonlin" for gnm
FitSC.nonlin2 <- function(p, mc, temp) {

  list(predictors = list(ke = 1, cw = 1, ch = 1, cq = 1),
       variables = list(substitute(p), substitute(mc), substitute(temp)),
       term = function(predLabels, varLabels) {
         sprintf("%s / (10 ^ (%s - (%s * log10(%s)) - (%s * %s)))",
                 varLabels[1], predLabels[1],
                 predLabels[2], varLabels[2],
                 predLabels[3], varLabels[3])
       })

}
class(FitSC.nonlin2) <- "nonlin"

FitSC.nonlin2(p, mc, temp)$term(c("ke", "cw", "ch", "cq"),
                               c("p", "mc", "temp"))

# model without any t


#
