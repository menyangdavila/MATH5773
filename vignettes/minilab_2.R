## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=7.5)

## ----setup--------------------------------------------------------------------
library(MATH5773FALLdavi0682)

## -----------------------------------------------------------------------------
ntickets <- function(N,gamma,p) {              
  par(mfrow=c(2, 1))
  
  n1=seq(N, N*1.1, 1)
  n_nd=which.min(abs(1-gamma-pnorm(N+0.5, n1*p, sqrt(n1*p*(1-p))))) #endpoint correction
  nd = n_nd+N
                         
  plot(n1, 1-gamma-pnorm(N+0.5, n1*p, sqrt(n1*p*(1-p))), #endpoint correction
       col = 'black', type='b', pch = 20, lwd = 1.5, cex = 1.5,
       main=paste("Objective versus n to find optimal tickets sold", "\n", "n = ", nd, "gamma = ", gamma, "N = ", N, "discrete"), col.main='black',
       xlab = "n", ylab = "Objective")
  par(new = TRUE)
  points(n1, 1-gamma-pnorm(N+0.5, n1*p, sqrt(n1*p*(1-p))),col = 'darkblue', pch = 16 )
  abline(h=0, col="red", lwd=3)
  abline(v=nd, col="red", lwd=3)

  
  
  n2=seq(N, 1.1*N, length = 20000)
  n_nc=which.min(abs(1-gamma-pnorm(N+0.5, n2*p, sqrt(n2*p*(1-p))))) #endpoint correction
  nc = n_nc*0.1*N/20000+N
  
  plot(n2, 1-gamma-pnorm(N+0.5, n2*p, sqrt(n2*p*(1-p))), 
       col = 'black', type='l', pch=20, lwd=2, cex=1.5,
       main=paste("Objective versus n to find optimal tickets sold", "\n", "n = ", nc, "gamma = ", gamma, "N = ", N, "continuous"),col.main='black',
       xlab = "n", ylab = "Objective")
  abline(h=0, col="blue", lwd=3)
  abline(v=nc, col="blue", lwd=3)
  
  
  out_list = list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  return(out_list)
  
}

## -----------------------------------------------------------------------------
ntickets(400,0.02,0.95)

