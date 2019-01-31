#' Seed survival dataset
#'
#' Seed survial data of three crops - groundnut, soybean and wheat stored for 15
#' years at combinations of three different moisture contents (3\%, 5\% and 7\%)
#' and temperatures (0 °C, 10 °C and 25 °C).
#'
#' \if{latex}{\figure{seedsurvival.pdf}}
#'
#' @format A data frame with 7 columns: \describe{ \item{crop}{The crop name.}
#'   \item{mc}{The moisture content (\%).} \item{temp}{The temperature (°C).}
#'   \item{period}{The year at which viability was recorded.} \item{rep}{The
#'   replication.} \item{viabilitypercent}{The viability percentage value
#'   recorded.} \item{sampsize}{The sample size used for estimating viability
#'   percentage.} }
#'
#' @encoding UTF-8
#'
#' @examples
#'
#' library(ggplot2, quietly = TRUE)
#'
#' ggplot(data = seedsurvival, aes(x = period, y = viabilitypercent)) +
#'   geom_point(aes(colour = crop), alpha = 0.5) +
#'   labs(x = "Storage period (years)", y = "Viability (%)") +
#'   facet_grid(crop + mc ~ temp) +
#'   theme_bw() +
#'   theme(axis.text = element_text(colour = "black"),
#'         legend.title = element_blank())
#'
"seedsurvival"
