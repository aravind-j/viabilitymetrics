% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ke.R
\encoding{UTF-8}
\name{Ke}
\alias{Ke}
\title{Viability constant \ifelse{html}{\out{<i>K<sub>E</sub></i>}}{\eqn{K_{E}}}
based on the species-specific temperature coefficients}
\usage{
Ke(K, temp, temp.coeff = c(0.0329, 0.000478))
}
\arguments{
\item{K}{The constant \ifelse{html}{\out{<i>K</i>}}{\eqn{K}} associated with
the relationship of temperature with seed longevity (see \strong{Details}).}

\item{temp}{Temperature in °C.}

\item{temp.coeff}{The species-specific temperature coefficients
(\ifelse{html}{\out{<i>C<sub>H</sub></i>}}{\eqn{C_{H}}} and
\ifelse{html}{\out{<i>C<sub>Q</sub></i>}}{\eqn{C_{Q}}}.) as a numeric
vector of length 2.}
}
\value{
The value of species-specific seed viability constant
  \ifelse{html}{\out{<i>K<sub>E</sub></i>}}{\eqn{K_{E}}}.
}
\description{
\code{Ke} computes the viability constant
\ifelse{html}{\out{<i>K<sub>E</sub></i>}}{\eqn{K_{E}}} from the
species-specific temperature coefficients in case of storage experiments
conducted at constant temperature and varying moisture contents.
}
\details{
From seed storage experiments involving storage of seeds at a constant
temperature in a range of moisture contents, the effect of moisture content
on seed longevity (\ifelse{html}{\out{<i>&sigma;</i>}}{\eqn{\sigma}}) can be
estimated from the following linear relationship:

\ifelse{html}{\out{<p style="text-align: center;">log<em>&sigma; = K &minus;
C<sub>W</sub></em>log<em>m</em></p>}}{\deqn{\log{\sigma} = K - C_{w}\log{m}}}

Where, \ifelse{html}{\out{<i>K</i>}}{\eqn{K}} is the intercept,
\ifelse{html}{\out{<i>C<sub>W</sub></i>}}{\eqn{C_{W}}} is the slope and
\ifelse{html}{\out{<i>m</i>}}{\eqn{m}} is the moisture content.

The constant \ifelse{html}{\out{<i>K</i>}}{\eqn{K}} associated with the
relationship of temperature with seed longevity as follows.

\ifelse{html}{\out{<p style="text-align: center;"><em>K = K<sub>E</sub>
&minus; C<sub>H</sub>t &minus; C<sub>Q</sub>t<sup>2</sup></em></p>}}{\deqn{K
= K_{E} - C_{H}t - C_{Q}t^{2}}}

Where, \ifelse{html}{\out{<i>K<sub>E</sub></i>}}{\eqn{K_{E}}},
\ifelse{html}{\out{<i>C<sub>H</sub></i>}}{\eqn{C_{H}}} and
\ifelse{html}{\out{<i>C<sub>Q</sub></i>}}{\eqn{C_{Q}}} are the
species-specific seed viability constants.

The constant \ifelse{html}{\out{<i>K<sub>E</sub></i>}}{\eqn{K_{E}}} can be
estimated from the universal temperature constants
(\ifelse{html}{\out{<i>C<sub>H</sub></i>}}{\eqn{C_{H}}} = 0.0329 and
\ifelse{html}{\out{<i>C<sub>Q</sub></i>}}{\eqn{C_{Q}}} = 0.000478) in case of
seed storage experiments at constant temperature and varying moisture content
as follows.

\ifelse{html}{\out{<p style="text-align: center;"><em> K<sub>E</sub> = K +
C<sub>H</sub>t + C<sub>Q</sub>t<sup>2</sup></em></p>}}{\deqn{K_{E} = K +
C_{H}t + C_{Q}t^{2}}}
}
\examples{
Ke(36, 10)

}
\references{
\insertRef{ellis_improved_1980}{viabilitymetrics}

\insertRef{pritchard_predicting_2003}{viabilitymetrics}
}
\seealso{
\code{\link[viabilitymetrics]{Sigma}}
}
