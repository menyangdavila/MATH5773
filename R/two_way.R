#' Two Way Table Analysis
#'
#' @param filename name of the data file
#' @param alpha significance level
#' @importFrom ggplot2 ggplot aes geom_bar
#' @importFrom grDevices adjustcolor
#' @importFrom graphics curve polygon
#' @importFrom stats addmargins chisq.test dchisq qchisq xtabs
#'
#' @return table, chisq
#' @export
#'
#' @examples
#' \dontrun{my_one_way("HYBRID")}

my_two_way = function (filename, alpha = 0.05){

  # Import the data
  hybrid <- readxl::read_excel(paste0("C:/Users/sergi/OneDrive/Desktop/MATH5773/Excel/",filename,'.xls'))

  Number <- hybrid$Number
  Model <- hybrid$Model
  Claim <- hybrid$Claim

  # Form the contingency table
  tab <- xtabs(Number ~ Model + Claim, data = hybrid)
  margin_tab <- addmargins(tab)

  nr = nrow(tab)
  nc = ncol(tab)
  n = sum(tab)

  margin_row = apply(tab,1,sum)
  margin_col = apply(tab,2,sum)

  # Bar plot
  gg1 <- ggplot2::ggplot(hybrid, ggplot2::aes(x = factor(Model), y = Number, fill = Claim, colour = Claim)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge")

  gg2 <- ggplot2::ggplot(hybrid, ggplot2::aes(x = factor(Model), y = Number, fill = Claim, colour = Claim)) +
    ggplot2::geom_bar(stat = "identity")

  # Expected frequency
  expect_freq = matrix(rep(margin_col,nr), nrow=nr, ncol=nc,byrow = TRUE)*margin_row/n

  # Individual chi squared
  ind_chiq = matrix((tab-expect_freq)^2/expect_freq, nrow=nr, ncol=nc)

  # Sum of chi square
  chsqsum = sum(ind_chiq)
  chsq = chisq.test(tab)

  # Find the rejection region
  rej = qchisq(1-alpha,chsq$parameter)

  # Plot the rejection area

  # Create density curve
  x <- NULL
  curve(dchisq(x, df = chsq$parameter), from = 0, to = 8, main = 'Chi-Square Distribution', ylab = 'Density',lwd = 2)

  # Create vector of x values
  x_vector <- seq(qchisq(1-alpha,chsq$parameter), 8)

  # Create vector of chi-square density values
  p_vector <- dchisq(x_vector, df = chsq$parameter)

  # Fill in portion of the density plot from 0 to 8
  polygon(c(x_vector, rev(x_vector)), c(p_vector, rep(0, length(p_vector))), col = adjustcolor('red', alpha.f = 0.3), border = NA)

  # Output the list
  list(tab = tab, bar_plot_1 = gg1, bar_plot_2 = gg2, exp_freq = expect_freq, chi_sq = round(ind_chiq, digits = 4), chi_sq_sum = chsqsum, p_value = chsq$p.value, degree_of_freedom = chsq$parameter, reject_region = rej)

}
