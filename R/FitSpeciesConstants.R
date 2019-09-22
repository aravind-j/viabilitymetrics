
#' @import gnm
#' @import minpack.lm
#' @importFrom broom glance
#' @importFrom broom tidy
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

    if (length(unique(data[, storage.t])) != 1) {
      stop("When 'universal.temp.constants == TRUE', values in 'storage.t'",
           " should be unique.")
    } else {
      stemp <- unique(data[, storage.t])
    }

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


    if (generalised.model){ # GLM

    } else { # LM

    }


    # gnm(cbind(viability.count, samp.size - viability.count) ~ FitSC.nonlin(p = storage.period,
    #                                      mc = mc, temp = temp),
    #     family = binomial(link = "probit"),
    #     data  = data, start = c(6, 3, 0.03, 0.002))


  } else { # Two step fitting

    # Step 1 : Fetch sigmas
    # [with & without cv]
    # [with glm or lm]
    #----------------------
    data$mc_temp <- interaction(as.factor(data$mc), as.factor(data$temp),
                                sep = "_")

    sigmadf <- FitSigma.batch(data = data, group = "mc_temp",
                              viability.percent = "viability.percent",
                              samp.size = "samp.size",
                              storage.period = "storage.period",
                              generalised.model = generalised.model,
                              use.cv = use.cv,
                              control.viability = control.viability)

    sigmadf <- sigmadf$models
    sigmadf$mc <- as.numeric(gsub("_\\d+", "", sigmadf$group))
    sigmadf$temp <- as.numeric(gsub("\\d+_", "", sigmadf$group))
    sigmadf$temp2 <- sigmadf$temp ^ 2

    # Step 2 : Fetch species constants
    # [with & without quadratic temp]
    # [with & without universal temp]
    #---------------------------------

    if (temp.quadratic) { # With quadratic temp
      if (!universal.temp.constants) { # without universal temp coefficients
        frmla <- as.formula("log10(sigma) ~ log10(mc) + temp + square(temp)")
      } else { # With universal temp coefficients
        frmla <- as.formula("log10(sigma) ~ log10(mc)")
      }
    } else { # Without quadratic temp
      frmla <- as.formula("log10(sigma) ~ log10(mc) + temp")
    }


    spmodel <- withWE(lm(formula = frmla, data = sigmadf))

    # procession of sp model
    # special procession of model with universal sp constants


  }

return(out)

}


















