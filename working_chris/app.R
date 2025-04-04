# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)

# Function to analyze sentiment based on demographic column
analyze_demographics <- function(df, demographic_col) {
  df %>%
    group_by(.data[[demographic_col]]) %>%
    summarize(avg_sentiment = mean(Support_Notes_Sentiment, na.rm = TRUE),
              count = n()) %>%
    arrange(desc(avg_sentiment))
}

# Define UI for the Shiny app
ui <- fluidPage(
  
  # Application title
  titlePanel("Demographic Sentiment Analysis"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Input: Choose the demographic column for analysis
      selectInput("demographic", 
                  "Choose Demographic:", 
                  choices = c(
                    "Big Age", 
                    "Big Gender", 
                    "Big Race/Ethnicity", 
                    "Big Level of Education", 
                    "Big Occupation", 
                    "Big County", 
                    "Big Languages", 
                    "Big Employer", 
                    "Big Military", 
                    "Big Car Access", 
                    "Little Gender", 
                    "Little Participant: Race/Ethnicity", 
                    "Big Contact: Marital Status", 
                    "Program", 
                    "Match Length"
                  ))
    ),
    
    mainPanel(
      # Output: Plot for sentiment analysis
      plotOutput("sentimentPlot", height = "900px")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  # Load the saved RDS files for the datasets
  train_data_sent <- readRDS("train_data_sent.rds")
  test_data_sent <- readRDS("test_data_sent.rds")
  
  # Reactive expression to calculate sentiment analysis based on the selected demographic
  sentiment_analysis <- reactive({
    analyze_demographics(train_data_sent, input$demographic)
  })
  
  # Render the plot for sentiment analysis based on demographic
  output$sentimentPlot <- renderPlot({
    demographic_data <- sentiment_analysis()
    
    ggplot(demographic_data, aes(x = .data[[input$demographic]], 
                                 y = avg_sentiment, fill = .data[[input$demographic]])) +
      geom_col() +
      coord_flip() +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Average Sentiment Score by", input$demographic),
        x = input$demographic, 
        y = "Average Sentiment"
      ) +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
