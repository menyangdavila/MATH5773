#' One Way Table Analysis
#'
#' @param filename name of the data file
#' @param alpha significance level
#' @importFrom ggplot2 ggplot aes geom_bar geom_text
#' @return table, frequency
#' @export
#'
#' @examples
#' \dontrun{my_one_way("SCAN")}

my_one_way <- function (filename, alpha = 0.05){
  # Import the data
  scan <- readxl::read_excel(paste0("C:/Users/sergi/OneDrive/Desktop/MATH5773/Excel/",filename,'.xls'))

  # Form the table
  Number <- scan$NUMBER
  Category <- scan$CATEGORY
  margin_tab <- addmargins(xtabs(Number ~ CATEGORY, data = scan))

  # Bar plot
  gg <- ggplot2::ggplot(data = scan, mapping = ggplot2::aes(x = Category, y = Number, fill = Category)) +
    ggplot2::geom_bar(stat="identity", position = "dodge") +
    ggplot2::geom_text(ggplot2::aes(label = Number), vjust = -0.2, size = 5, position = ggplot2::position_dodge(0.9)) +
    ggplot2::ylim(0, max(Number)*1.1) +
    ggplot2::labs(title = "Equifax/Harris Consumer Privacy Survey", subtitle = "Scan",
                  x = "Response Category", y = "Number of Users")

  # Hypothesis test
  tab <- table(rep(Category, Number))
  freq1way <- s20x::freq1way(tab,hypothprob = rep(1/length(Category),length(Category)), conf.level = alpha)

  df <- as.data.frame(rep(Category, Number))
  names(df) <- "Response_Category"

  # Z statistics
  z = qnorm(1-alpha/2)
  phat = Number/sum(Number)

  # Confidence interval
  cil = phat-z*sqrt(phat*(1-phat)/sum(Number))
  ciu = phat+z*sqrt(phat*(1-phat)/sum(Number))
  ci = list(cil,ciu)

  conf_int = paste0("The approximate confidence interval for the frequency is", Category, " is: (",
                    round(as.numeric(unlist(ci[1])),3), ", ",round(as.numeric(unlist(ci[2])),3), ").")

  # Output list
  list(table = margin_tab, plot = gg, hypothesis = freq1way, confidence_interval = conf_int)
}
