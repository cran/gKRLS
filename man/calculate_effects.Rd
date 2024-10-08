% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AME_function.R
\encoding{UTF-8}
\name{calculate_effects}
\alias{calculate_effects}
\alias{calculate_interactions}
\alias{get_individual_effects}
\alias{print.gKRLS_mfx}
\alias{summary.gKRLS_mfx}
\title{Marginal Effects}
\usage{
calculate_effects(
  model,
  data = NULL,
  variables = NULL,
  continuous_type = c("IQR", "minmax", "derivative", "onesd", "predict",
    "second_derivative"),
  conditional = NULL,
  individual = FALSE,
  vcov = NULL,
  raw = FALSE,
  use_original = FALSE,
  epsilon = 1e-07,
  verbose = FALSE
)

calculate_interactions(
  model,
  variables,
  QOI = c("AMIE", "ACE", "AME", "AIE"),
  ...
)

get_individual_effects(x)

\method{print}{gKRLS_mfx}(x, ...)

\method{summary}{gKRLS_mfx}(object, ...)
}
\arguments{
\item{model}{A model estimated using functions from \code{mgcv} (e.g.,
\code{gam} or \code{bam}).}

\item{data}{A data frame that is used to calculate the marginal effect or set
to \code{NULL} which will employ the data used when estimating the model.
The default is \code{NULL}. Using a custom dataset may have unexpected
implications for continuous and character/factor variables. See "WARNINGS"
for more discussion.}

\item{variables}{A character vector that specifies the variables for which to
calculate effects. The default, \code{NULL}, calculates effects for all
variables.}

\item{continuous_type}{A character value, with a default of \code{"IQR"},
  that indicates the type of marginal effects to estimate when the variable
  is continuous (i.e. not binary, logical, factor, or character). Options are
  \code{"IQR"} (compares the variable at its 25\% and 75\% percentile),
  \code{"minmax"} (compares the variable at its minimum and maximum),
  \code{"derivative"} (numerically approximates the derivative at each
  observed value), \code{"second_derivative"} (numerically approximates the
  second derivative at each observed value), \code{"onesd"} (compares one
  standard deviation below and one standard deviation above the mean of the
  variable). It also accepts a named list where each named element
  corresponds to a continuous variable and has a two-length vector as each
  element. The two values are then compared. If this is used, then all
  continuous variables must have two values specified.

  A special option (\code{"predict"}) produces predictions (e.g.,
  \code{predict(model, type = "response")}) at each observed value and then
  averages them together. This, in conjunction with \code{conditional},
  provides a way of calculating quantities such as predicted probability
  curves using an "observed value" approach (e.g., Hanmer and Kalkan 2013).
  Examples are provided below.}

\item{conditional}{A data.frame or \code{NULL}. This is an analogue of
Stata's \code{at()} option and the \code{at} argument in the \code{margins}
package. For a marginal effect on some variable \code{"a"}, this specifies
fixed values for certain other covariates, e.g. \code{data.frame("b" = 0)}.
If \code{conditional} is \code{NULL}, all other covariates are held at
their observed value. If \code{conditional} is a data.frame, then each row
represents a different combination of covariate values to be held fixed,
and marginal effects are calculated separately for each row. Examples are
provided below.}

\item{individual}{A logical value. \code{TRUE} calculates individual effects (i.e.
an effect for each observation in the \code{data}). The default is
\code{FALSE}.}

\item{vcov}{A matrix that specifies the covariance matrix of the parameters.
The default, \code{NULL}, uses the standard covariance matrix from
\code{mgcv}. This can be used to specify clustered or robust standard
errors using output from (for example) \code{sandwich}.}

\item{raw}{A logical value. \code{TRUE} returns the raw values used to
calculate the effect in addition to the estimated effect. The default is
\code{FALSE}. If \code{TRUE}, an additional column \code{...id} is present
in the estimated effects that reports whether the row corresponds to the
effect (\code{effect}), the first value (\code{raw_0}) or the second value
(\code{raw_1}) where \code{effect=raw_1 - raw_0}. For \code{"derivative"},
this is further scaled by the step size. For \code{"second_derivative"},
\code{effect=raw_2 - 2 * raw_1 + raw_0}, scaled by the step size; see the
discussion for \code{epsilon} for how the step size is calculated.}

\item{use_original}{A logical value that indicates whether to use the
estimation data (\code{TRUE}) or \code{data} (\code{FALSE}) when
calculating quantities such as the IQR for continuous variables or the
levels to examine for factor variables. Default (\code{FALSE}) uses the
provided data; if \code{data = NULL}, this is equivalent to using the
estimation data. The "WARNINGS" section provides more discussion of this
option.}

\item{epsilon}{A numerical value that defines the step size when calculating
  numerical derivatives (default of 1e-7).  For \code{"derivative"}, the step
  size for the approximation is  \eqn{h = \epsilon \cdot \mathrm{max}(1,
  \mathrm{max}(|x|))}{h = \epsilon * max(1, max(|x|))}, i.e. \eqn{f'(x)
  \approx \frac{f(x+h) - f(x-h)}{2h}}{f'(x) ≈ [f(x+h)-f(x-h)]/(2h)}. Please
  see Leeper (2016) for more details.

  For \code{"second_derivative"}, the step size is \eqn{h = [\epsilon \cdot
  \mathrm{max}(1, \mathrm{max}(|x|))]^{0.5}}{h=[\epsilon * max(1,
  max(|x|))]^{0.5}}, i.e. \eqn{f''(x) \approx \frac{f(x+h) - 2 f(x) +
  f(x-h)}{h^2}}{f''(x) ≈ [f(x+h) - 2 f(x) + f(x-h)]/h^2}}

\item{verbose}{A logical value that indicates whether to report progress when
calculating the marginal effects. The default is \code{FALSE}.}

\item{QOI}{A vector of quantities of interest calculate for
\code{calculate_interactions}. Options include \code{"AME"} (average
marginal effect), \code{"ACE"} (average combination effect), \code{"AIE"}
(average interaction effect) and \code{"AMIE"} (average marginal
interaction effect); see "Details" for more information. The default
setting calculates all four quantities.}

\item{...}{An argument used for \code{calculate_interactions} to pass
arguments to \code{calculate_effects}. It is unused for
\code{summary.gKRLS_mfx}.}

\item{x}{An object estimated using \code{calculate_effects}.}

\item{object}{A model estimated using functions from \code{mgcv} (e.g.,
\code{gam} or \code{bam}).}
}
\value{
Both \code{calculate_effects} and \code{calculate_interactions} return
data.frames. \code{calculate_effects} contains attributes---including the
ones noted below---that may be useful for other analyses. \itemize{
\item{"gradient": } This contains the gradients used to calculate the
standard error (via the delta method) for the estimates from
\code{calculate_effects}. There is one column for each quantity calculated in
the main object. The format of this object depends on the family used for
\code{gam} or \code{bam}. This could be used manually to calculate a standard error on the
difference between two estimated marginal effects.
\item{"N_eff": The number of observations (in the estimation data) minus the
effective degrees of freedom. This is used when calculating p-values as the
degrees of freedom for the t-distribution.} \item{"N": The number of
observations.} }
}
\description{
These functions calculate marginal effects or predicted values after
estimating a model with \code{gam} or \code{bam}.
}
\details{
\bold{Overview:} \code{calculate_effects} returns a data.frame of
class \code{"gKRLS_mfx"} that reports the estimated average marginal effects
and standard errors. Other columns include \code{"type"} that reports the
type of marginal effect calculated. For families with multiple predicted
outcomes (e.g., multinomial), the column \code{"response"} numbers the
different outcomes in the same order as \code{predict.gam(object)} for the
specified family. Many (but not all) extended and generalized families from
\code{mgcv} are included.

The \code{conditional} argument while setting \code{continuous_type =
"predict"} can be used to estimate predicted values at different covariate
strata (e.g., to create an "observed value" predicted probability curve for a
logistic regression). The examples provide an illustration.

\bold{Interactions:} \code{calculate_interactions} provides some simple
functions for calculating interaction effects between variables. The default
quantities it can produce are listed below. Egami and Imai (2019) provide a
detailed exposition of these quantities. All marginalization is done using an
"observed value" approach, i.e. over the estimation data or a custom dataset
provided to \code{data}. \itemize{ \item{"AME" or Average Marginal Effect: }
This is the standard quantity reported from \code{calculate_effects}.
\item{"ACE" or Average Combination Effect: } This is the effect of changing
two variables simultaneously on the outcome. \item{"AMIE" or Average Marginal
Interaction Effect: } This is ACE minus each corresponding AME. \item{"AIE"
or Average Interaction Effect:} This has a "conditional effect"
interpretation and reports the difference in average effect of one variable
("A") between two different levels of a second variable ("B"). }

\bold{Other Functions:} \code{get_individual_effects} extracts the
individual-level effects that are estimated if \code{individual=TRUE}.
}
\section{WARNINGS}{


  Using a custom dataset for \code{data}, i.e. a dataset other than the
  estimation data, may have unexpected implications. For continuous and
  character/factor variables, the estimated marginal effects may depend on
  the distribution of the variable in \code{data}. For example, if
  \code{continuous_type="IQR"}, the variable \code{x1} is counterfactually
  set to \code{quantile(data$x1, 0.25)} and \code{quantile(data$x1, 0.75)}
  where \code{data} is provided by \code{calculate_effects} (versus the
  estimation data). To force this range to be set based on the
  \emph{estimation} data, set \code{use_original=TRUE}.

  This default behavior if \code{data} is provided may be undesirable and
  thus \code{calculate_effects} will issue a warning if this situation arises
  and a custom \code{data} is provided. These settings are subject to change
  in future releases.
}

\examples{
set.seed(654)
n <- 50
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
state <- sample(letters[1:5], n, replace = TRUE)
y <- 0.3 * x1 + 0.4 * x2 + 0.5 * x3 + rnorm(n)
data <- data.frame(y, x1, x2, x3, state)

# Make character variables into factors for mgcv
data$state <- factor(data$state)

# A gKRLS model
fit_gKRLS <- mgcv::gam(y ~ state + s(x1, x2, x3, bs = "gKRLS"), data = data)

# calculate marginal effect using derivative
calculate_effects(fit_gKRLS, variables = "x1", continuous_type = "derivative")

# calculate marginal effect by specifying conditional variables
calculate_effects(fit_gKRLS,
  variables = "x1",
  conditional = data.frame(x2 = c(0.6, 0.8), x3 = 0.3)
)

# calculate interaction effects between two variables
# use the default setting ("IQR") for the baseline and
# comparison categories for each variable
calculate_interactions(fit_gKRLS,
   variables = list(c("x1", "x2")),
   QOI = c('AIE', 'AMIE')
)

# calculate marginal effect by specifying a factor conditional variable
# estimate the individual marginal effects
out <- calculate_effects(fit_gKRLS,
  variables = "x1", individual = TRUE,
  conditional = data.frame(state = c("a", "b", "c")), continuous_type = "derivative"
)

# Extract the individual marginal effects:
# shorthand for attr(fit_main, 'individual')
get_individual_effects(out)

# calculated the average expected value across a grid of "x1"
# using an observed value approach for the other covariates
calculate_effects(fit_gKRLS, conditional = data.frame(x1 = c(0, 0.2, 0.4, 0.6)),
  continuous_type = 'predict'
)
}
\references{
Egami, Naoki, and Kosuke Imai. 2019. "Causal Interaction in Factorial
Experiments: Application to Conjoint Analysis." \emph{Journal of the American
Statistical Association}. 114(526):529-540.

Hanmer, Michael J., and Kerem Ozan Kalkan. 2013. "Behind the Curve: Clarifying
the Best Approach to Calculating Predicted Probabilities and Marginal Effects
from Limited Dependent Variable Models." \emph{American Journal of Political
Science} 57(1): 263-277.

Leeper, Thomas J. 2016. "Interpreting Regression Results using Average
Marginal Effects with R's \code{margins}." Working paper available at
\url{https://s3.us-east-2.amazonaws.com/tjl-sharing/assets/AverageMarginalEffects.pdf}.
}
