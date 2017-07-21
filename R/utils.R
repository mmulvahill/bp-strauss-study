
# Lognormal converters -- provide distribution parameters get summary
# statistics
#' @export
lnmean <- function(.mu, .sigma) exp(.mu + (.sigma^2)/2)
#' @export
lnsd   <- function(.mu, .sigma) sqrt((exp(.sigma^2) - 1) * exp(2*.mu + .sigma^2))


