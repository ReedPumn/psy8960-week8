library(tidyverse)
library(shiny)

# Create the ui
ui <- fluidPage(
  # Title the Shiny to orient the reader to the shiny page.
  titlePanel("Priest PSY 8960 Week 8"),
  # COMMENT
  sidebarLayout(
    sidebarPanel(
      radioButtons("radiogender", "gender", choices = c("Male", "Female", "All"), selected = "All"),
      radioButtons("radiose", "se", choices = c("With SE bar", "Without SE bar"), selected = "With SE bar"),
      radioButtons("radiotime", "time", choices = c("Include all Ps", "Exclude Ps sampled before 08-01-2017"), selected = "Include all Ps")
    ),
    # COMMENT
    mainPanel(
      plotOutput("plot"))))

# Create the server
server <- function(input, output) {
  # COMMENT
  shinydata_tbl <- readRDS(file = "../shiny_week8/shinydata_tbl.rds")
  # COMMENT
  gg_se <- ggplot(shinydata_tbl, aes(x = q1_6, y = q8_10)) +
    geom_jitter() +
    geom_smooth(method = "lm", se = TRUE, color = "purple") +
    labs(x = "Mean of Questions 1-6", y = "Mean of Questions 8-10")
  gg_no_se <- ggplot(shinydata_tbl, aes(x = q1_6, y = q8_10)) +
    geom_jitter() +
    geom_smooth(method = "lm", se = FALSE, color = "purple") +
    labs(x = "Mean of Questions 1-6", y = "Mean of Questions 8-10")
  
  # COMMENT
  output$plot <- renderPlot({
    if (input$radiogender == c("Male", "Female")) {
      shinydata_tbl <- shinydata_tbl %>%
        filter(gender == input$radiogender)
    } 
    # COMMENT
    if (input$radiotime == "Include all Ps") {
      shinydata_tbl <- shinydata_tbl %>%
        filter(timeEnd >= ymd_hms("2017-08-01 00:00:00"))
    }
    # COMMENT
    if (input$radiose == "Without SE bar") {
      gg_no_se
    } else if (input$radiose == "With SE bar") {
      gg_se
    } 
  })
}

#Finally create the fourth component
shinyApp(ui = ui, server = server)
