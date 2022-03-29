#' Plot linear model fitted values with confidence bands
#'
#' Creates a plot object of a fitted linear model with confidence bands and
#' a loess smoother
#'
#'
#' @param df a data frame to be modeled
#' @param x a character name of an explanatory variable from the data frame
#' @param y a character name of a response variable from the data frame
#' @param band_type A character value for the type of confidence band.
#' Includes "whbands".
#' @param level a number between 0 and 1 for the confidence level of the bands
#' @importFrom rlang .data .env
#'
#' @return a ggplot2 object with the fitted line, the confidence bands and
#' a loess smoother
#' @export
#'
#' @examples
#' lb_plot_lm_bands(mtcars, "disp", "mpg")
lb_plot_lm_bands <- function(df, x, y, band_type = "whbands", level = .95){
  temp_df <- data.frame(tdfx = df[[x]] , tdfy = df[[y]])
  lmout <- stats::lm(tdfy ~ tdfx, data = temp_df)
  band_df <- lb_create_lm_bands(df, x, y, band_type, level)
  ggplot2::ggplot(temp_df, ggplot2::aes(x = .data$tdfx, y = .data$tdfy)) +
    ggplot2::geom_smooth(method = lm, se = FALSE) +
    ggplot2::geom_line(data = band_df, ggplot2::aes(x = .data$dfx, y = .data$lwr)) +
    ggplot2::geom_line(data = band_df, ggplot2::aes(x = .data$dfx, y = .data$upr)) +
    ggplot2::geom_smooth(data = temp_df, ggplot2::aes(x = .data$tdfx, y = .data$tdfy),
                                                     method = "loess",
                                                     color = "orange",
                                                     se = FALSE) +
    ggplot2::labs(title = paste0("Linear Model of ", substitute(df),
                                 "  variables ", y, " ~ ", " ", x),
                     subtitle = paste0(band_type,
                                       " confidence bands with level = ",
                                       level),
                  x = x, y = y)
}

lb_plot_lm_bandsf <- function(formula, data, band_type = "whbands", level = .95){
  lmout <- stats::lm(formula , data = data)
  band_df <- lb_create_lm_bandsf(formula, data, band_type, level)
  lm_df <- lmout$model

  ggplot2::ggplot(lm_df, ggplot2::aes(x = lm_df[[2]], y = lm_df[[1]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = lm, formula =  y ~ x, se = FALSE) +
    ggplot2::geom_line(data = band_df, ggplot2::aes(x = band_df[[4]], y = .data$lwr)) +
    ggplot2::geom_line(data = band_df, ggplot2::aes(x = band_df[[4]], y = .data$upr)) +
    ggplot2::geom_smooth(data = lm_df, ggplot2::aes(x = lm_df[[2]], y = lm_df[[1]]),
                         method = "loess", formula =  y ~ x,
                         color = "orange",
                         se = FALSE) +
    ggplot2::labs(title = paste0("Linear Model of ", substitute(data),
                                 " variables: ", deparse(formula)),
                  subtitle = paste0(band_type,
                                    " confidence bands with level = ",
                                    level),
                  x = names(lm_df)[2], y = names(lm_df)[1])
}
