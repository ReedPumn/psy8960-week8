library(tidyverse)
library(shiny)

# Create the ui
ui <- fluidPage(
  # Title the Shiny to orient the reader to the shiny page.
  titlePanel("Priest PSY 8960 Week 8"),
  # Add settings to the side bar panel. The radio buttons were chosen to let readers easily toggle between settings.
  sidebarLayout(
    sidebarPanel(
      radioButtons("radiogender", "gender", choices = c("Male", "Female", "All"), selected = "All"),
      radioButtons("radiose", "se", choices = c("With SE bar", "Without SE bar"), selected = "With SE bar"),
      radioButtons("radiotime", "time", choices = c("Include all Ps", "Exclude Ps sampled before 08-01-2017"), selected = "Include all Ps")
    ),
    # The main panel displays the main plot. This main plot automatically updates in response to the chosen radio buttons to let the reader see the section of data they prefer.
    mainPanel(
      plotOutput("plot"))))

# Create the server
server <- function(input, output) {
  # Bring in the main data file. Selected parts of this data file will create the main plot depending on the radio buttons chosen.
  shinydata_tbl <- readRDS(file = "../shiny_week8/shinydata_tbl.rds")
  # I want to keep the main output code within the server readable, so I've included the lengthy ggplot code here. For the standard error radio button, I created two plots, differing only on whether they display the standard error.
  gg_se <- ggplot(shinydata_tbl, aes(x = q1_6, y = q8_10)) +
    geom_jitter() +
    geom_smooth(method = "lm", se = TRUE, color = "purple") +
    labs(x = "Mean of Questions 1-6", y = "Mean of Questions 8-10")
  gg_no_se <- ggplot(shinydata_tbl, aes(x = q1_6, y = q8_10)) +
    geom_jitter() +
    geom_smooth(method = "lm", se = FALSE, color = "purple") +
    labs(x = "Mean of Questions 1-6", y = "Mean of Questions 8-10")
  
  # This code tells the ui which data to display depending on the filtering. The first filter is the gender variable, where filtering only occurs if the "Male" or "Female" buttons are selected.
  output$plot <- renderPlot({
    if (input$radiogender == c("Male", "Female")) {
      shinydata_tbl <- shinydata_tbl %>%
        filter(gender == input$radiogender)
    } 
    # This code similarly filters the displayed data depending on the time at which data sampling was completed. To keep things simple, no filtering is done if "Include all Ps" is selected.
    if (input$radiotime == "Include all Ps") {
      shinydata_tbl <- shinydata_tbl %>%
        filter(timeEnd >= ymd_hms("2017-08-01 00:00:00"))
    }
    # This last filter refers back to the two ggplots created previously and displays one of them depending on whether we want to include or exclude our standard error bar. By including the brunt of the plots' code above, this important section of code is readable.
    if (input$radiose == "Without SE bar") {
      gg_no_se
    } else if (input$radiose == "With SE bar") {
      gg_se
    } 
  })
}

#Finally create the fourth component
shinyApp(ui = ui, server = server)
