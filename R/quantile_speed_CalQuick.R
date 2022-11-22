# fonction quantile plus rapide vient d'ici : https://gist.github.com/sikli/f1775feb9736073cefee97ec81f6b193
#' quantile_speed
#'
#' @param x
#' @param probs
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
quantile_speed <- function(x, probs = c(0.1, 0.9), na.rm = F) {

  if (na.rm) x <- x[!is.na(x)]

  n <- length(x)
  index <- 1 + (n - 1) * probs

  lo    <- floor(index)
  hi    <- ceiling(index)

  x  <- sort(x, partial = unique(c(lo, hi)))
  qs <- x[lo]

  i     <- 1:length(probs)
  h     <- index - lo
  qs    <- (1 - h) * qs + h * x[hi]
  qs

}
