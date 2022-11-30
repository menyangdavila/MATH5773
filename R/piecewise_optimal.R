#' Piecewise_optimal
#'
#' @param data name of the data set
#'
#' @return x_k y_k r_sq aic
#'
#' @importFrom stats coef
#'
#' @export
#'
#' @examples
#'  \dontrun{mypiece_optimal(data = ddt)}
#'

mypiece_optimal = function(data){
  WEIGHT <- NULL

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

  n = which.max(Rsq)
  weight_k = data$WEIGHT[n]
  length_k = data$LENGTH[n]
  max_rsq = Rsq[n]
  min_aic = aic[n]

  df=within(data, {knot = (WEIGHT - weight_k)*(WEIGHT > weight_k)})
  lm_piecewise = lm(LENGTH ~ WEIGHT + knot, data=df)
  betas = coef(lm_piecewise)

  piece_fuction = function(x){betas[1]+betas[2]*(x) + betas[3]*(x-weight_k)*(x - weight_k>0)}

  with(data,plot(WEIGHT,LENGTH,
                 pch = 21,
                 cex=1,
                 bg="pink",
                 main="LENGTH Vs WEIGHT - Piecewise Regression"))

  curve(piece_fuction(x), add=TRUE, lwd=2,col="Black")

  list(x_k = weight_k, y_k = length_k, optimized_rsq = max_rsq, optimized_aic = min_aic)
}
