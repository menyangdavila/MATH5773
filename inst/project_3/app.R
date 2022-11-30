library(shiny)
library(car)
library(s20x)
library(purrr)
library(shinyscreenshot)
prodqual <- readxl::read_excel(paste0("C:/Users/sergi/OneDrive/Desktop/MATH5773/Excel/","PRODQUAL",'.xls'))
Quality <- prodqual$QUALITY
Temp <- prodqual$TEMP
Pressure <- prodqual$PRESSURE
fit1 = lm(QUALITY ~ TEMP + PRESSURE + TEMP*PRESSURE, data = prodqual)
fit2 = lm(QUALITY ~ TEMP + PRESSURE + TEMP*PRESSURE + I(TEMP^2) + I(PRESSURE^2), data = prodqual)

fit_t = lm(QUALITY ~ TEMP + I(TEMP^2), data = prodqual)
fit_p = lm(QUALITY ~ PRESSURE + I(PRESSURE^2), data = prodqual)

fa.prodqual<-within(prodqual,{
  TEMP.F<-factor(TEMP)
  PRESSURE.F<-factor(PRESSURE)})
factor.fit = lm(QUALITY ~ TEMP.F + PRESSURE.F + TEMP.F*PRESSURE.F, data = fa.prodqual)

ui <- fluidPage(actionButton("screeshot","Take a screen shot"),
                navbarPage("Tabs:",tabPanel("Data Summary",
                                            titlePanel("Product Quality Dataset"),
                                            sidebarLayout(
                                              sidebarPanel(

                                                checkboxInput("header", "Header", TRUE),
                                                radioButtons("sep", "Separator",
                                                             choices = c(Comma = ",",
                                                                         Semicolon = ";",
                                                                         Tab = "\t"),
                                                             selected = ","),
                                                tags$hr(),
                                                radioButtons("disp", "Display",
                                                             choices = c(Head = "head",
                                                                         All = "all"),
                                                             selected = "head"),
                                                tags$hr(),
                                                radioButtons("quote", "Quote",
                                                             choices = c(None = "",
                                                                         "Double Quote" = '"',
                                                                         "Single Quote" = "'"),
                                                             selected = '"')),
                                              mainPanel(
                                                verbatimTextOutput("summary"),
                                                tableOutput("contents")
                                              ))),
                           tabPanel("Group Comparison",
                                    titlePanel("Interaction Plot"),
                                    sidebarLayout(
                                      sidebarPanel( uiOutput("factor_1"),
                                                    uiOutput("factor_2")),
                                      mainPanel(
                                        h3(textOutput("caption")),
                                        plotOutput("group_comparison"),
                                        plotOutput("plot1"),
                                        plotOutput("plot2")
                                      )
                                    )),

                           tabPanel("ANOVA",
                                    titlePanel("Product Quality: ANOVA Analysis"),
                                    sidebarLayout(
                                      sidebarPanel(

                                        radioButtons("anova", "ANOVA",
                                                     choices = c(Table = "table",
                                                                 Comparison = "interaction"),
                                                     selected = "table")),
                                      mainPanel(
                                        verbatimTextOutput("anova_result")
                                      ))),


                           tabPanel("Model",
                                    titlePanel("Model Summary"),
                                    sidebarLayout(
                                      sidebarPanel(

                                        radioButtons("model", "Choose Model",
                                                     choices = c(Model_1 = "model1",
                                                                 Model_2 = "model2"),
                                                     selected = "model1")),
                                      mainPanel(
                                        verbatimTextOutput("model_summary"),
                                        verbatimTextOutput("anova_summary")
                                      )))
                ))

server <- function(input, output, session) {
  onSessionEnded(stopApp)
  data <- reactive({
    df <- read.csv("C:/Users/sergi/OneDrive/Desktop/MATH5773/Excel/PRODQUAL.csv", header = input$header, sep = input$sep, quote = input$quote)
    return(df)
  })

  output$contents <- renderTable({
    if (input$disp == "head") {
      return(head(data()))
    }
    else {
      return(data())
    }
  })

  output$summary <- renderPrint({
    summary(data())
  })

  output$factor_1 <- renderUI({
    selectInput("factor_1", label = "Factor_1", choices = names(data()))
  })
  output$factor_2 <- renderUI({
    selectInput("factor_2", label = "Factor_2", choices = names(data()) )
  })


  dat <- reactive({
    test <- data.frame(Quality, data()[[input$factor_1]], data()[[input$factor_2]])
    colnames(test) <- c("Quality","X1", "X2")
    return(test)
  })

  output$group_comparison <- renderPlot({
    interactionPlots(Quality ~ X1 + X2, data=dat())
  })

  output$plot1 <- renderPlot({
    scatterplot(QUALITY ~ TEMP | PRESSURE, data=prodqual, lwd=2,
                main="Plot of Quality vs. Temp",
                xlab="Temperature",
                ylab="Quality")
  })

  output$plot2 <- renderPlot({
    scatterplot(QUALITY ~ PRESSURE | TEMP, data=prodqual, lwd=2,
                main="Plot of Quality vs. Pressure",
                xlab="Pressure",
                ylab="Quality")
  })


  output$anova_result <- renderPrint({
    if (input$anova == "table") {
      return(anova(fit1))
    }
    else {
      return(summary2way(factor.fit, page="interaction"))
    }
  })

  output$model_summary <- renderPrint({
    if (input$model == "model1") {
      return(summary(fit1))
    }
    else {
      return(summary(fit2))
    }
  })

  output$anova_summary <- renderPrint({
    if (input$model == "model1") {
      return(anova(fit1))
    }
    else {
      return(anova(fit2))
    }
  })

  observeEvent(input$screeshot,{screenshot()})

}

shinyApp(ui, server)
