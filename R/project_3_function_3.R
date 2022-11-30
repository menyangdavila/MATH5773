#' Project 3: Function 3 my_bayesian
#'
#' @param df data frame
#'
#' @return model summary for both classical and Bayesian regression, including trace and history plots and the convergence diagnostic
#' @export
#'
#' @examples
#' \dontrun{my_bayesian(df = prodqual)}
my_bayesian <- function(df) {

  TEMP <- NULL
  PRESSURE <- NULL

  fa.df<-within(df,{
    TEMP.F<-factor(TEMP)
    PRESSURE.F<-factor(PRESSURE)})

  ylm.reg <- lm(QUALITY ~ TEMP.F + PRESSURE.F + TEMP.F*PRESSURE.F, data = fa.df)
  ci.im <- confint(ylm.reg)
  beta.lm <- coef(ylm.reg)

  Bayes.reg <- MCMCpack::MCMCregress(QUALITY ~ TEMP.F + PRESSURE.F + TEMP.F*PRESSURE.F, data = fa.df, mcmc = 10000, burnin = 1000,  b0 = 0, B0 = 0, c0 = 0.001, d0 = 0.001)
  sum.bay <- summary(Bayes.reg)

  con_test1 <- coda::heidel.diag(Bayes.reg, eps=0.1, pvalue=0.05)
  con_test2 <- coda::geweke.diag(Bayes.reg, frac1=0.1, frac2=0.5)
  plot0 <- coda::geweke.plot(Bayes.reg)
  print(plot0)

  posterior <- as.array(Bayes.reg)

  bayesplot::color_scheme_set(scheme = "blue")
  plot <- bayesplot::mcmc_trace(posterior, pars = c("(Intercept)","TEMP.F90","TEMP.F100","PRESSURE.F55","PRESSURE.F60","TEMP.F90:PRESSURE.F55","TEMP.F100:PRESSURE.F55","TEMP.F90:PRESSURE.F60","TEMP.F100:PRESSURE.F60"))
  print(plot)

  bayesplot::color_scheme_set(scheme = "purple")
  plot1 <- bayesplot::mcmc_hist(posterior, binwidth = 0.5, pars = c("(Intercept)","TEMP.F90","TEMP.F100","PRESSURE.F55","PRESSURE.F60","TEMP.F90:PRESSURE.F55","TEMP.F100:PRESSURE.F55","TEMP.F90:PRESSURE.F60","TEMP.F100:PRESSURE.F60"))
  print(plot1)

  bayesplot::color_scheme_set(scheme = "green")
  plot2 <- bayesplot::mcmc_dens(posterior, pars = c("(Intercept)","TEMP.F90","TEMP.F100","PRESSURE.F55","PRESSURE.F60","TEMP.F90:PRESSURE.F55","TEMP.F100:PRESSURE.F55","TEMP.F90:PRESSURE.F60","TEMP.F100:PRESSURE.F60"))
  print(plot2)

  list(Classical_CI = ci.im, Classical_Estimate = beta.lm, Bayesian_Summary = sum.bay, Convergence_Test_1 = con_test1, Convergence_Test_2 = con_test2)
}
