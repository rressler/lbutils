#' Working-Hotelling bands for simple linear regression.
#'
#' Intervals of the form "fit +/- w * standard-error", where w^2 is
#' found by \code{p * qf(level, p, n - p)}.
#'
#' @param object An object of class "lm".
#' @param new_data A data frame containing the new data.
#' @param level The confidence level of the band.
#' @return a list of 4 elements to include the predicted values as $fit
#' @export
#' @author David Gerard, American University
#' @examples
#' lb_whbands(object = lm(mpg ~ disp, data = mtcars),
#' new_data = data.frame("disp" = seq(from = min(mtcars$disp),
#' to = max(mtcars$disp), length.out = 100)), level = .95)
lb_whbands <- function(object, new_data, level = 0.95) {
  stopifnot(inherits(object, "lm"))
  stopifnot(inherits(new_data, "data.frame"))
  stopifnot(is.numeric(level), length(level) == 1)
  pout <- stats::predict(object = object,
                         newdata = new_data,
                         se.fit = TRUE,
                         interval = "none")
  n <- nrow(stats::model.matrix(object))
  p <- ncol(stats::model.matrix(object))
  w <- sqrt(p * stats::qf(p = level, df1 = p, df2 = n - p))
  lwr <- pout$fit - w * pout$se.fit
  upr <- pout$fit + w * pout$se.fit
  pout$fit <- cbind(fit = pout$fit, lwr = lwr, upr = upr)
  return(pout)
}


lb_whbands2 <- function(object, new_data, level = 0.95) {
  stopifnot(inherits(object, "lm"))
  stopifnot(inherits(new_data, "data.frame"))
  stopifnot(is.numeric(level), length(level) == 1)
  pout <- stats::predict(object = object,
                         newdata = new_data,
                         se.fit = TRUE,
                         interval = "none")
  n <- nrow(stats::model.matrix(object))
  p <- ncol(stats::model.matrix(object))
  w <- sqrt(p * stats::qf(p = level, df1 = p, df2 = n - p))
  lwr <- pout$fit - w * pout$se.fit
  upr <- pout$fit + w * pout$se.fit
  pout$fit <- cbind(fit = pout$fit, lwr = lwr, upr = upr)
  return(pout)
}
