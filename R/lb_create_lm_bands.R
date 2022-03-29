#' Create data frame with confidence band data
#'
#' This function takes input for a linear model and the type of confidence band
#' and alpha level and creates a data frame of values that can be plotted
#' to show the confidence bands.
#'
#' @param df a data frame containing at least three observations of two
#' variables for a linear regression
#' @param x a character name of an explanatory variable from the data frame
#' @param y a character name of a response variable from the data frame
#' @param band_type a character for the type of band desired. Includes:
#' "whbands".
#' @param level a number between 0 and 1 for the confidence level
#'
#' @return a data frame of the 100 x and predicted values along with lower
#' and upper values for the confidence band with level
#' @export
#'
#' @examples
#' lb_create_lm_bands(mtcars, "disp", "mpg")
lb_create_lm_bands <- function(df, x, y, band_type = "whbands", level = .95) {
  temp_df <- data.frame(dfx = df[[x]] , dfy = df[[y]])
  lmout <- stats::lm(dfy ~ dfx, data = temp_df)
  new_df <- data.frame("dfx" = seq(
    from = min(temp_df$dfx),
    to = max(temp_df$dfx),
    length.out = 100
  ))
  # names(new_df) <- "df[[x]]"
  if (band_type == "whbands") {
  whfit <- lb_whbands(object = lmout, new_data = new_df, level)
  whfit$fit  |>
    cbind(new_df) ->
    new_df
  } else {
    stop("Unsupported type of confidence bands.")
  }
  new_df
}

########

lb_create_lm_bandsf <- function(formula, data, band_type = "whbands", level = .95) {
  lmout <- stats::lm(formula , data = data)
  lm_df <- lmout$model
  new_df <- data.frame("dfx" = seq(
    from = min(lm_df[[2]]),
    to = max(lm_df[[2]]),
    length.out = 100
  ))
   names(new_df) <- names(lm_df)[[2]]
  if (band_type == "whbands") {
    whfit <- lb_whbands(object = lmout, new_data = new_df, level)
    whfit$fit  |>
      cbind(new_df) ->
      new_df
  } else {
    stop("Unsupported type of confidence bands.")
  }
  new_df
}
