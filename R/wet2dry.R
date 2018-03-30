#' Conversion between Moisture content (wet/fresh weight basis) and Moisture
#' content (dry weight basis)
#'
#' These functions convert from moisture content determined on wet/fresh weight
#' basis to equivalent value on dry weight basis and back.
#' \code{MoistureNomograph} plots the nomograph for these conversions.
#'
#' Conversions between moisture content (\%) determined on wet weight basis
#' \ifelse{html}{\out{<i>MC<sub>wb</sub></i>}}{\eqn{MC_{wb}}} and that on dry
#' weight basis \ifelse{html}{\out{<i>MC<sub>db</sub></i>}}{\eqn{MC_{db}}} are
#' computed based on the formulae of Cromarty et al., (1982) as follows:
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><i>MC<sub>db</sub> =
#' <sup>100 &times; MC<sub>wb</sub></sup> &frasl; <sub>100 &minus;
#' MC<sub>wb</sub></sub></i></p>}}{\deqn{MC_{db} = \frac{100 \times
#' MC_{wb}}{100-MC_{wb}}}}
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><i>MC<sub>wb</sub> =
#' <sup>100 &times; MC<sub>db</sub></sup> &frasl; <sub>100 +
#' MC<sub>db</sub></sub></i></p>}}{\deqn{MC_{wb} = \frac{100 \times
#' MC_{db}}{100+MC_{db}}}}
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><i>MC<sub>wb</sub> =
#' <sup>[wi &minus; w<sub>f</sub>]</sup> &frasl;
#' <sub>w<sub>i</sub></sub></i></p>}}{\deqn{MC_{wb} =
#' \frac{w_{i}-w_{f}}{w_{i}}}}
#'
#' \ifelse{html}{\out{<p style="text-align: center;"><i>MC<sub>db</sub> =
#' <sup>[wi &minus; w<sub>f</sub>]</sup> &frasl;
#' <sub>w<sub>f</sub></sub></i></p>}}{\deqn{MC_{db} =
#' \frac{w_{i}-w_{f}}{w_{f}}}}
#'
#' Where, \ifelse{html}{\out{w<i><sub>i</sub></i>}}{\eqn{w_{i}}} is the initial
#' weight and \ifelse{html}{\out{w<i><sub>f</sub></i>}}{\eqn{w_{f}}} is the
#' final weight.
#'
#' If the moisture content (\code{mc}) for conversion is beyond limits (0-50 \%
#' for \code{wet2dry} and 0-100 \% for \code{dry2wet}), a warning is issued.
#'
#' @param mc Moisture content.
#' @param min Minimum value of moisture content to be plotted in nomograph.
#' @param max Minimum value of moisture content to be plotted in nomograph.
#' @param basis The basis on which moisture content is estimated
#' @param horiz If \code{TRUE}, nomograph is plotted horizontally.
#'
#' @return For \code{wet2dry} and \code{dry2wet}, the converted moisture content
#'   (\%).
#'
#'   For \code{MoistureNomograph}, the nomograph as an object of class
#'   \code{ggplot}.
#'
#' @import ggplot2
#' @importFrom plyr round_any
#'
#' @references \insertRef{cromarty_design_1982}{viabilitymetrics}
#' @examples
#' #----------------------------------------------------------------------------
#' # Moisture content (wet basis) to moisture content (dry basis)
#' #----------------------------------------------------------------------------
#' wet2dry(25)
#' # Warning if moisture content is beyond limits (0-50 %)
#' wet2dry(60)
#' wet2dry(-10)
#'
#' #----------------------------------------------------------------------------
#' # Moisture content (dry basis) to moisture content (wet basis)
#' #----------------------------------------------------------------------------
#' dry2wet(30)
#' # Warning if moisture content is beyond limits (0-100 %)
#' dry2wet(-10)
#' dry2wet(110)
#'
#' #----------------------------------------------------------------------------
#' # Nomograph
#' #----------------------------------------------------------------------------
#' # Horizontal
#' MoistureNomograph(min = 0, max = 50, basis = "wet", horiz = TRUE)
#' MoistureNomograph(min = 0, max = 100, basis = "dry", horiz = TRUE)
#'
#' # Vertical
#' MoistureNomograph(min = 0, max = 50, basis = "wet", horiz = FALSE)
#' MoistureNomograph(min = 0, max = 100, basis = "dry", horiz = FALSE)
#'
#' # Nomograph is a "ggplot" object
#' nom <- MoistureNomograph(min = 0, max = 50, basis = "wet", horiz = TRUE)
#' library(ggplot2)
#' nom + geom_hline(aes(yintercept=30), colour = "red")
#' p <- "Scale for converting moisture content values\nbetween dry and wet basis"
#' cap <- "based on Cromarty et al., 1982"
#' nom + labs(title  = p, caption = cap) +
#'   theme(plot.title = element_text(hjust = 0.5))


#' @rdname wet2dry
#' @export
# 0 < mc & 100 >= mc
dry2wet <- function(mc) {
  if (!is.numeric(mc)) {
    stop('"mc" is not numeric')
  }
  if (FALSE %in% (findInterval(mc, c(0,100), rightmost.closed = TRUE) == 1)) {
    warning('"mc" is beyond limits (0 < "mc" < 100)')
  }

  mcwb <- (100 * mc)/(100 + mc)
  return(mcwb)
}

#' @rdname wet2dry
#' @export
# 0 < mc & 50 >= mc
wet2dry <- function(mc) {
  if (!is.numeric(mc)) {
    stop('"mc" is not numeric')
  }
  if (FALSE %in% (findInterval(mc, c(0,50), rightmost.closed = TRUE) == 1)) {
    warning('"mc" is beyond limits (0 < "mc" < 50)')
  }

  mcdb <- (100 * mc)/(100 - mc)
  return(mcdb)
}

#' @rdname wet2dry
#' @export
MoistureNomograph <- function(min, max, basis = c("wet", "dry"), horiz = FALSE) {
  basis <- match.arg(basis)

  if (min > max) {
    stop("'min' is greater than 'max'")
  }

  if (min == max) {
    stop("'min' is equal to 'max'")
  }
  min <- plyr::round_any(min, 10, f = floor)
  max <- plyr::round_any(max, 10, f = ceiling)

  data <- data.frame(Unit = basis,
                     value = min:max,
                     label = c(min:max))

  if (basis == "wet") {
    range <- range(wet2dry(min:max))
    basis2 <- "dry"
    min2 <- round_any(range[1], 10, f = floor)
    max2 <- round_any(range[2], 10, f = ceiling)

    data2 <- data.frame(Unit = "dry",
                        value = dry2wet(min2:max2),
                        label = c(min2:max2))
  } else {
    range <- range(dry2wet(min:max))
    basis2 <- "wet"
    min2 <- round_any(range[1], 10, f = floor)
    max2 <- round_any(range[2], 10, f = ceiling)

    data2 <- data.frame(Unit = basis2,
                        value = wet2dry(min2:max2),
                        label = (min2:max2))
  }

  data <- rbind(data, data2)
  rm(data2)

  data$Unit <- factor(data$Unit)
  data$Unit <- factor(data$Unit, levels = c("wet", "dry"))

  data <- data[data$label %% 1 == 0,]
  data$size <- ifelse(data$label %% 10 == 0, 4,
                      ifelse(data$label %% 5 == 0, 2,
                             1))
  data$R <- ifelse(data$Unit == "wet",
                   as.numeric(data$Unit) - data$size,
                   as.numeric(data$Unit))
  data$L <- ifelse(data$Unit == "dry",
                   as.numeric(data$Unit) + data$size,
                   as.numeric(data$Unit))
  data$Txt <- ifelse(data$Unit == "wet",
                     data$L - 5,
                     data$R + 5)

  levels(data$Unit) <- c("MC[wb]", "MC[db]")

  p <- ggplot(data, aes(x = Unit, y = value)) +
    geom_line(aes(group = Unit)) +
    scale_x_discrete("Unit", labels = parse(text = levels(data$Unit)),
                     expand = c(0,20)) +
    annotate("segment", x = data$L, xend = data$R,
             y = data$value,
             yend = data$value) +
    annotate("text", x = data[data$label %% 10 == 0,]$Txt,
             y = data[data$label %% 10 == 0,]$value,
             label = data[data$label %% 10 == 0,]$label) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  if (horiz) {
    p <- p +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(colour = "black", vjust = c(1,0))) +
      coord_flip()
  } else {
    p <- p +
      theme(axis.text.y = element_blank(),
            axis.text.x = element_text(colour = "black", hjust = c(1,0)))
  }

  return(p)
}
