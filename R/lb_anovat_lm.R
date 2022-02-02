#' Create Custom ANOVA Table for Linear Regression
#'
#' @description Create a standard ANOVA table for linear regression
#'
#' @param object a Linear Regression object as returned from stats::lm()
#' @param reg_collapse Logical variable to collapse the elements in the table. Set to FALSE to see each variable and interaction in the model.
#' @param ... other potential arguments
#'
#' @details This function is by Russell Steel for creating an ANOVA table for a full model linear regression.
#' @return a data frame with class anova that shows the Df, SS, MS, F and P for the regression
#'
#' @importFrom stats lm df.residual pf
#' @export
#' @references See https://community.rstudio.com/t/overall-anova-table-in-r/97896 and
#' https://community.rstudio.com/t/anova-table-for-full-linear-model/42074/3
#' @examples
#' #' lb_amovat_lm(object = lm(mpg ~ disp * cyl, data = mtcars), reg_collapse = TRUE)
#' lb_amovat_lm(object = lm(mpg ~ disp * cyl, data = mtcars), reg_collapse = FALSE)
lb_anovat_lm <- function(object, reg_collapse = TRUE, ...) {
  if (length(list(object, ...)) > 1L) {
    return(anova.lmlist(object, ...))
  }

  # check if object inherits from class "lm"
  if (!inherits(object, "lm")) {
    warning("calling anova.lm(<fake-lm-object>) ...")
  }

  w <- object$weights
  ssr <- sum(if (is.null(w)) object$residuals^2 else w * object$residuals^2)
  mss <- sum(if (is.null(w)) object$fitted.values^2 else w * object$fitted.values^2)

  if (ssr < 1e-10 * mss) {
    warning("ANOVA F-tests on an essentially perfect fit are unreliable")
  }

  dfr <- df.residual(object)
  p <- object$rank
  if (p > 0L) {
    p1 <- 1L:p
    comp <- object$effects[p1]
    asgn <- object$assign[stats:::qr.lm(object)$pivot][p1]
    nmeffects <- c("(Intercept)", attr(object$terms, "term.labels"))
    tlabels <- nmeffects[1 + unique(asgn)]
    ss <- c(vapply(split(comp^2, asgn), sum, 1), ssr)
    df <- c(lengths(split(asgn, asgn)), dfr)

    if (reg_collapse) {
      if (attr(object$terms, "intercept")) {
        collapse_p <- 2:(length(ss) - 1)
        ss <- c(ss[1], sum(ss[collapse_p]), ss[length(ss)])
        df <- c(df[1], sum(df[collapse_p]), df[length(df)])
        tlabels <- c(tlabels[1], "Source")
      } else {
        collapse_p <- 1:(length(ss) - 1)
        ss <- c(sum(ss[collapse_p]), ss[length(ss)])
        df <- c(df[1], sum(df[collapse_p]), df[length(df)])
        tlabels <- c("Regression")
      }
    }
  } else {
    ss <- ssr
    df <- dfr
    tlabels <- character()
    if (reg_collapse) {
      collapse_p <- 1:(length(ss) - 1)
      ss < -c(sum(ss[collapse_p]), ss[length(ss)])
      df <- c(df[1], sum(df[collapse_p]), df[length(df)])
    }
  }

  ms <- ss / df
  f <- ms / (ssr / dfr)
  P <- pf(f, df, dfr, lower.tail = FALSE)
  table <- data.frame(df, ss, ms, f, P)
  table <- rbind(table, colSums(table))
  if (attr(object$terms, "intercept")) {
    table$ss[nrow(table)] <- table$ss[nrow(table)] - table$ss[1]
  }
  table$ms[nrow(table)] <- table$ss[nrow(table)] / table$df[nrow(table)]
  table[length(P):(length(P) + 1), 4:5] <- NA
  dimnames(table) <- list(
    c(tlabels, "Error", "Total"),
    c("Df", "SS", "MS", "F", "P")
  )
  if (attr(object$terms, "intercept")) {
    table <- table[-1, ]
    table$MS[nrow(table)] <- table$MS[nrow(table)] *
      (table$Df[nrow(table)]) / (table$Df[nrow(table)] - 1)
    table$Df[nrow(table)] <- table$Df[nrow(table)] - 1
  }
  structure(table,
    heading = c("Analysis of Variance Table\n"),
    class = c("anova", "data.frame")
  )
}
