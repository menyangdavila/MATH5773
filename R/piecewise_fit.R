#' Piecewise_fit
#'
#' @param data name of the data set
#'
#' @return plots of r squared and aic
#'
#' @importFrom stats AIC coefficients lm
#'
#' @export
#'
#' @examples
#' \dontrun{mypiece_fit(data = ddt)}

mypiece_fit <- function(data){
  Rsq <- vector(mode = "numeric", length = length(data$WEIGHT))
  aic <- vector(mode = "numeric", length = length(data$WEIGHT))

  for(i in seq_along(data$WEIGHT)){
    xk <- data$WEIGHT[i]
    x <- (data$WEIGHT > xk)*(data$WEIGHT - xk)
    ylm <- lm(data$LENGTH ~ data$WEIGHT + x)
    smm <- summary(ylm)
    Rsq[i] <- smm$r.squared
    cf <- coefficients(smm)
    aic[i] <- AIC(ylm)
  }

  RSQ_plot <- plot(data$WEIGHT,
                   Rsq,
                   xlab = expression(x[k]),
                   type = "p",
                   pch = 21,
                   bg = "blue",
                   cex = 1.2)

  AIC_plot <- plot(data$WEIGHT,
                   aic,
                   xlab = expression(x[k]),
                   type = "p",
                   pch = 21,
                   bg = "blue",
                   cex = 1.2)
}
