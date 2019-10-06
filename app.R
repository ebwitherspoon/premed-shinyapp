# Pre Med Shiny App 
#### Start Up ##### 
# Load required packages #
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "data.table", "psych", "summarytools", "shiny", "shinythemes")

# Import App dataset #
df_shiny <- read.csv("PreMed_ShinyApp.csv")

#### Build Shiny App ####
# Define UI ####
ui <- fluidPage(
  titlePanel("Retention by Gender"),
  sidebarLayout(
    sidebarPanel(
      #Select course to plot
      selectInput(inputId = "course", 
                  label = "Select a course:",
                  choices = unique(df_shiny$course), 
                  selected = "Bio")
      ),
    #Output description
    mainPanel(
      plotOutput("linePlot"))
  )
)

# Define Server ####
server <- function(input, output) {
  #Subset data
  chartData <- reactive({
    df_shiny %>%
      filter(
        course == input$course)
  })
  
  output$linePlot <- renderPlot({ 
    ggplot(data=chartData(), aes(x=grade, y=ret, group=gender, color = gender))+ 
      geom_point(stat = 'summary', fun.y = 'mean') + 
      geom_smooth(method = "glm",
      method.args = list(family = binomial), se = TRUE)
  })
}
  
  # Launch Shiny App ####
  shinyApp(ui = ui, server = server)
