#' Project 3: Function 2 my_bootstrap
#'
#' @param df dataframe
#' @param iter iteration time
#' @param alpha significance level
#'
#' @return point estimate from both mlr and bootstrapping
#'
#' @importFrom stats confint model.matrix
#' @export
#'
#' @examples
#' \dontrun{my_bootstrap(df = prodqual, iter = 10000, alpha = 0.05)}
my_bootstrap<- function(df, iter, alpha){

  # Point estimate and interval estimate using lm
  ylm = lm(QUALITY ~ TEMP + PRESSURE + TEMP:PRESSURE + I(TEMP^2) + I(PRESSURE^2), data = df)
  point_lm <- coef(ylm)
  ci_lm <- confint(ylm)
  sum <- summary(ylm)

  # Point estimate and interval estimate using bootstrap
  X <- model.matrix(ylm)
  y <- scale(df$QUALITY)
  y <- as.matrix(y)

  n <- dim(X)[1]
  nb <- dim(X)[2]
  hbetas <- matrix(NA, nrow = nb, ncol = iter )

  singular <- -1
  ginverse <- function(){

    indx <- sample(1:n, n, replace = TRUE)
    X <- X[indx,]           # re-sampling
    y <- y[indx,]

    pp <- t(X)%*%X
    solve(pp)
  }

  f <- function(x){# x is the iteration number

    attempt <- "fail"

    while(attempt[1] == "fail"){
      singular <<- singular + 1
      attempt <- tryCatch(
        error = function(cnd) "fail",
        ginverse(),
        finally = NULL
      )
    }
    hbetas[,x] <<- attempt%*%t(X)%*%y
  }

  purrr::map(1:iter, ~f(.x))

  if(nb %% 2 == 0){
    layout(matrix(1:nb,
                  nrow = nb/2,
                  ncol = 2,
                  byrow = TRUE))}
  else{
    layout(matrix(c(1:(nb-1), c(nb,nb)),
                  nrow = (nb+1)/2,
                  ncol = 2,
                  byrow = TRUE))
  }

  for(i in 1:nb){
    j <- i-1
    h <- hist(hbetas[i,],
              plot = FALSE)

    hist(hbetas[i,],
         xlab = substitute(hat(beta)[j]),
         main = "Bootstrap Estimates",
         col = rgb(0.6, (h$counts/max(h$counts)), 0.8),
         freq = TRUE, breaks = seq(min(hbetas[i,]), max(hbetas[i,]), length.out = 20))


  }
  list(singular = singular, Model_Smuuary = sum, MLR_point_estimate = point_lm, MLR_CI = ci_lm, Bootstrap_CI_b0 = quantile(hbetas[1,], c(alpha/2,1-alpha/2)), Bootstrap_CI_b1 = quantile(hbetas[2,], c(alpha/2,1-alpha/2)),Bootstrap_CI_b2 = quantile(hbetas[3,], c(alpha/2,1-alpha/2)),Bootstrap_CI_b3 = quantile(hbetas[4,], c(alpha/2,1-alpha/2)),Bootstrap_CI_b4 = quantile(hbetas[5,], c(alpha/2,1-alpha/2)),Bootstrap_CI_b5 = quantile(hbetas[6,], c(alpha/2,1-alpha/2)))
}

