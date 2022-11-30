#' Model plotting
#'
#' @param dataframe name of the data frame
#'
#' @return model summaries and plots of both full sample and sub samples
#' @importFrom corrplot corrplot
#' @export
#'
#' @examples
#' \dontrun{my_model_plot(fuels)}
#'
my_model_plot <- function(dataframe){
  BrakePow <- NULL
  X1 <- NULL
  X2 <- NULL
  BurnRate <- NULL
  FuelType <- NULL

  # correlation - independence
  C = cor(dataframe[c("BrakePow","X1","X2","BurnRate")])
  correlation = corrplot::corrplot(C, method = 'number')

  # fit the model
  ylm1 <- lm(BurnRate ~ BrakePow + X1 + X2 + BrakePow:X1 + BrakePow:X2,data = dataframe)
  sum1 = summary(ylm1)

  ylm2 <- lm(BurnRate ~ BrakePow,data = dataframe)
  sum2 = summary(ylm2)

  # subset data based on the type of fuels
  sub_df2 <- subset(dataframe, X1 == 1)
  sub_blended <- subset(dataframe, X2 == 1)
  sub_adv <- subset(dataframe, X1 == 0 & X2 == 0)

  # fit the model for each subset
  ylm_df2 <- lm(BurnRate ~ BrakePow, data = sub_df2)
  ylm_blended <- lm(BurnRate ~ BrakePow, data = sub_blended)
  ylm_adv <- lm(BurnRate ~ BrakePow, data = sub_adv)

  # plot the model
  g1 <- ggplot2::ggplot(dataframe, ggplot2::aes(BrakePow, BurnRate, col=FuelType, lty=FuelType)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(formula = y ~ x, method="lm", se=FALSE) +
    ggplot2::ggtitle("Burn Rate vs. Brake Pow")

  g2 <- ggplot2::ggplot(dataframe, ggplot2::aes(BrakePow, BurnRate)) +
    ggplot2::geom_point(ggplot2::aes(colour = FuelType)) +
    ggplot2::geom_smooth(formula = y ~ x, method="lm", se=FALSE) +
    ggplot2::ggtitle("Burn Rate vs. Brake Pow")

  list(model_1_summary = sum1, model_2_summary = sum2, correlation = correlation, SLR_R_Squared = sum2$r.squared, MLR_R_Squared = sum1$r.squared, plot_sub_sample = g1, plot_full_sample = g2)
}
