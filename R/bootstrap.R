#' Bootstrapping estimates
#'
#' @param df name of the data frame
#' @param model model specification
#' @param iter iteration times
#' @param alpha significance level
#'
#' @return invisible named list containing a data frame of iterations, the CIs, and point estimates
#'
#' @importFrom graphics hist layout
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' \dontrun{myboot(df = bubble, model = "model1", iter = 1000, alpha = .95)}
#'
myboot<- function(df, model, iter, alpha){
  # Specify two dependent variables
  switch(model,
         model1= Y<-"Diameter",
         model2= Y<-"Density")

  # Point estimate
  mx0 <- matrix(c(1),nrow = 18,ncol = 1)
  mx <- matrix(c(mx0,df$MassFlux,df$HeatFlux),nrow=18,ncol=3)
  point <- solve(t(mx)%*%mx)%*%t(mx)%*%as.matrix(df[Y])

  # Bootstrap estimates
  hbetas <- matrix(NA, nrow = iter, ncol = 3)

  for(j in 1:iter){
    ind <- sample(1:18,18, replace = TRUE )

    # Create matrices using the data
    mx0 <- matrix(c(1),nrow = 18,ncol = 1)
    mx <- matrix(c(mx0,df[ind,]$MassFlux,df[ind,]$HeatFlux),nrow=18,ncol=3)

    # Use matrices to calculate estimates
    bhat <- solve(t(mx)%*%mx)%*%t(mx)%*%as.matrix(df[ind,Y])
    hbetas[j,] <- bhat
  }
  layout(matrix(1:3, nrow = 1,ncol = 3))

  # Display output
  hist(hbetas[,1],
       xlab = expression(widehat(beta)[0]),
       main = "Histogram of intercept estimate")

  hist(hbetas[,2],
       xlab = expression(widehat(beta)[1]),
       main = "Histogram of slope 1 estimate")

  hist(hbetas[,3],
       xlab = expression(widehat(beta)[2]),
       main = "Histogram of slope 2 estimate")

  beta0 <- hbetas[,1]
  ci1=quantile(beta0,c(alpha/2,1-alpha/2),na.rm=TRUE)

  beta1 <- hbetas[,2]
  ci2=quantile(beta1,c(alpha/2,1-alpha/2),na.rm=TRUE)

  beta2 <- hbetas[,3]
  ci3=quantile(beta2,c(alpha/2,1-alpha/2),na.rm=TRUE)

  invisible(list(hat_betas = hbetas, point_estimate  = point, "CI for Intercept"=ci1,"CI for Beta1"=ci2,
                 "CI for Beta2"=ci3, model = model, iteration_times = iter, alpha = alpha))
}
