#' Project 3: Function 1 my_validity
#'
#' @param dataframe df
#'
#' @return model summary, reponse surface, residual-fitted value plot, scale-location plot, constant variance test result, shapiro test result
#'
#' @importFrom grDevices rainbow rgb
#' @importFrom stats fitted shapiro.test
#' @export
#'
#' @examples
#' \dontrun{my_validity(dataframe = prodqual)}
my_validity <- function(dataframe){

  # fit the model - linearity
  ylm <- lm(QUALITY ~ TEMP + PRESSURE + TEMP:PRESSURE + I(TEMP^2) + I(PRESSURE^2), data = dataframe)
  sum = summary(ylm)

  # sketch the response surface
  x1_axis = seq(80, 100, 1)
  x2_axis = seq(50 ,60, 1)

  response = function(x,y){coef(ylm)[1] + coef(ylm)[2]*x + coef(ylm)[3]*y + coef(ylm)[4]*x^2 + coef(ylm)[5]*y^2 + coef(ylm)[6]*x*y}

  color = rev(rainbow(100, start = 0/6, end = 4/6))
  zcol  = cut(outer(X = x1_axis,Y = x2_axis, response), 100)

  plot3D::persp3D(x = x1_axis, y = x2_axis, z = outer(X = x1_axis,Y = x2_axis, response), ticktype = "detailed", phi = 25, theta = 50, expand=0.75, col = color[zcol], xlab = "x1: Temperature", ylab = "x2: pressure", zlab = "Quality", clab = "Quality", contour = TRUE, cex.axis = 0.75, cex.lab = 0.75)

  # plot a scatter plot: residuals vs fitted value
  plot(fitted(ylm), residuals(ylm), pch = 19, col = rgb(0, 0, 0, 0.5), main = "Residuals vs Fitted Plot", xlab = "Fitted Values", ylab = "Residual Values")
  abline(h=0, col = "red")

  ## homoscedasticity
  plot(ylm,3, pch = 19, col = rgb(0, 0, 1, 0.25))
  ncv = car::ncvTest(ylm)

  ## shapiro wilk test to check for normality
  s20x::normcheck(ylm, shapiro.wilk = TRUE, main="Multiple Linear Regression Model")
  st = shapiro.test(residuals(ylm))

  list(model_summary = sum, p_value_for_constant_variance = ncv, shapiro_test_for_normality = st)
}
