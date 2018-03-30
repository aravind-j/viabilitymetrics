#' Probit transformation
#'
#' These functions transfrom data between percentage, probit and Normal
#' Equivalent Deviate (NED).
#'
#' Probit transformation can be used to transform a sigmoid curve of percentage
#' data to a linear one. The probit transformation is defined as
#' \ifelse{html}{\out{NED + 5}}{\eqn{NED + 5}}. However the two terms probit and
#' NED are used interchangeably in literature.
#'
#' NED function
#' (\ifelse{html}{\out{<i>&Phi;<sup>-1</sup></i>}}{\eqn{\Phi^{-1}}}) is is the
#' inverse of the cumulative distribution function
#' (\ifelse{html}{\out{<i>&Phi;</i>}}{\eqn{\Phi}}) of the standard normal
#' distribution (\ifelse{html}{\out{<em>z &sim; N(0,1)</em>}}{\eqn{z\sim
#' N(0,1)}}) or the quantile function associated with the standard normal
#' distribution.
#'
#' For percentage \ifelse{html}{\out{<i>p</i>}}{\eqn{p}},
#'
#' \ifelse{html}{\out{<p style="text-align: center;">NED<em>(p)=
#' &Phi;<sup>-1</sup>(p) = &radic;<span style="text-decoration:
#' overline;">2</span></em> erf<sup>-1</sup><em>
#' (2p-1)</em></p>}}{\deqn{\textrm{NED}(p)=\Phi^{-1}(p)=
#' \sqrt{2}~\textrm{erf}^{-1}(2p-1)}}
#'
#' and
#'
#' \ifelse{html}{\out{<p style="text-align: center;">probit<em>(p) =</em>
#' NED<em>(p) + 5</em></p>}}{\deqn{\textrm{probit}(p)=\textrm{NED}(p)+5}}
#'
#' The \code{PercentAdjust} function adjusts the percentage values of 0 and 100
#' to \ifelse{html}{\out{100 &times; [ <sup>0.25</sup> &frasl;
#' <sub><i>n</i></sub> ]}}{\eqn{100\times \frac{0.25}{n}}} and
#' \ifelse{html}{\out{100 &times; [ <sup>(<i>n</i> &minus; 0.25)</sup> &frasl;
#' <sub><i>n</i></sub> ]}}{\eqn{100\times \frac{n-0.25}{n}}} respectively,
#' according to the sample size \ifelse{html}{\out{<i>n</i>}}{\eqn{n}} to avoid
#' infinity values during probit transformation (Miller and Tainter, 1944).
#'
#' @param percentage The percentage value.
#' @param probit The probit value
#' @param NED The NED value.
#' @param n Sample size for estimation of percentage.
#'
#' @return The transformed value.
#' @export PercentAdjust
#' @export Percent2NED
#' @export Percent2Probit
#' @export Probit2NED
#' @export Probit2Percent
#' @export NED2Probit
#' @export NED2Percent
#' @importFrom stats pnorm qnorm
#'
#' @references \insertRef{bliss_method_1934}{viabilitymetrics}
#'
#'   \insertRef{miller_estimation_1944}{viabilitymetrics}
#'
#'   \insertRef{finney_probit_1952}{viabilitymetrics}
#'
#' @examples
#' Percent2NED(0:100)
#' Percent2Probit(0:100)
#'
#' Percent2NED(25)
#' Percent2Probit(25)
#' Percent2NED(25) +5
#' NED2Probit(-0.6744898)
#'
#' # Percentage adjustment for 0 and 100
#' Percent2Probit(100)
#' Percent2Probit(0)
#' n = 50
#' Percent2Probit(PercentAdjust(100, n))
#' Percent2Probit(PercentAdjust(0, n))
#'
#' @name Percent2Probit


#' @rdname Percent2Probit
#' @export
PercentAdjust <- function(percentage, n) {
  # check for n
  AdjPercent <- ifelse(test = percentage == 100,
                       yes = 100*(n - 0.25)/n,
                       no = ifelse(test = percentage == 0,
                                   yes = 100*(0.25/n),
                                   no = percentage))
  return(AdjPercent)
}

#' @rdname Percent2Probit
#' @export
Percent2NED <- function(percentage) {
  qnorm(percentage/100, 0, 1)
}

#' @rdname Percent2Probit
#' @export
Percent2Probit <- function(percentage) {
  qnorm(percentage/100, 5, 1)
}

#' @rdname Percent2Probit
#' @export
Probit2NED <- function(probit) {
  probit - 5
}

#' @rdname Percent2Probit
#' @export
NED2Probit <- function(NED) {
  NED + 5
}

#' @rdname Percent2Probit
#' @export
NED2Percent <- function(NED) {
  pnorm(NED, 0, 1)*100
}

#' @rdname Percent2Probit
#' @export
Probit2Percent <- function(probit) {
  pnorm(probit, 5, 1)*100
}
