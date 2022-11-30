#' Residual Analysis
#'
#' @param dataframe name of the data frame
#'
#' @return analysis of MLR assumptions regarding to the error term
#' @importFrom stats cor density qqline qqnorm residuals
#' @export
#'
#' @examples
#' \dontrun{my_residual(fuels)}
#'
my_residual <- function(dataframe){
  fuels <- NULL

  # fit the model - linearity
  ylm <- lm(BurnRate ~ BrakePow + X1 + X2 + BrakePow:X1 + BrakePow:X2, data = dataframe)
  sum = summary(ylm)

  # plot a scatter plot of model 1: residuals vs fitted value
  s20x::trendscatter(residuals(ylm)~fitted(ylm), main="Residuals vs Fitted Plot")

  ## homoscedasticity
  plot(ylm,3)

  ## qq plot to check for normality
  qqnorm(residuals(ylm), main = "Q-Q Plot - Check for normality", datax=TRUE)
  qqline(residuals(ylm), datax = TRUE)

  ## density plot - normality
  plot(density(residuals(ylm)), main = "Density - Residuals", col = c('red'),xlab = "Residuals")

  list(model_summary = sum)
}
