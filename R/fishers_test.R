#' Fisher's test
#'
#' @param filename filename name of the data file
#'
#' @return table chi_squared_test fishers_test
#' @importFrom ggplot2 ggplot aes geom_bar
#' @importFrom graphics mosaicplot
#' @importFrom stats fisher.test
#' @export
#'
#' @examples
#' \dontrun{my_fishser("BONDING")}
#'
my_fishser = function (filename){

  # Import the data
  bonding <- readxl::read_excel(paste0("C:/Users/sergi/OneDrive/Desktop/MATH5773/Excel/",filename,'.xls'))

  NUMBER <- bonding$NUMBER
  ADHESIVE <- bonding$ADHESIVE
  ARIScore <- bonding$ARIScore

  # Form the contingency table
  tab <- xtabs(NUMBER ~ ADHESIVE + ARIScore, data = bonding)
  margin_tab <- addmargins(tab)

  # Bar plot
  gg1 <- ggplot2::ggplot(bonding, ggplot2::aes(x = factor(ARIScore), y = NUMBER, fill = ADHESIVE, colour = ADHESIVE)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge")

  gg2 <- ggplot2::ggplot(bonding, ggplot2::aes(x = factor(ARIScore), y = NUMBER, fill = ADHESIVE, colour = ADHESIVE)) +
    ggplot2::geom_bar(stat = "identity")

  # Mosaic plot
  mos <- mosaicplot(tab, main = "Mosaic plot", color = TRUE)

  # Chi-squared test
  chi_test <- chisq.test(tab)

  # Fisher's test
  fisher_test <- fisher.test(tab)


  #Output the list
  list(tab = margin_tab, bar_plot_1 = gg1, bar_plot_2 = gg2, mpsaic_plot = mos, exp_freq = chi_test$expected, fishers_test = fisher_test, p_value = fisher_test$p.value)

}
