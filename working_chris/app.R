# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(ggridges)

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
      selectInput("demographic", "Choose Demographic Variable:",
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
                  )),
      selectInput("plot_type", "Choose Plot Type:",
                  choices = c(
                    "Bar Plot (Mean)" = "bar",
                    "Box Plot" = "box",
                    "Violin Plot" = "violin",
                    "Jitter + Box Plot" = "jitterbox",
                    "Density Ridge Plot" = "ridge"))
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
    req(input$demographic)
    df <- train_data_sent
    demo_col <- input$demographic
    plot_type <- input$plot_type
    
    if (plot_type == "bar") {
      summary_df <- analyze_demographics(df, demo_col)
      
      ggplot(summary_df, aes(x = reorder(.data[[demo_col]], avg_sentiment), y = avg_sentiment, fill = .data[[demo_col]])) +
        geom_col() +
        coord_flip() +
        theme_minimal(base_size = 14) +
        labs(
          title = paste("Average Sentiment Score by", demo_col),
          x = demo_col,
          y = "Average Sentiment"
        )
      
    } else if (plot_type == "box") {
      ggplot(df, aes(x = .data[[demo_col]], y = Support_Notes_Sentiment, fill = .data[[demo_col]])) +
        geom_boxplot() +
        coord_flip() +
        theme_minimal(base_size = 14) +
        labs(
          title = paste("Sentiment Distribution by", demo_col),
          x = demo_col,
          y = "Sentiment Score"
        )
      
    } else if (plot_type == "violin") {
      ggplot(df, aes(x = .data[[demo_col]], y = Support_Notes_Sentiment, fill = .data[[demo_col]])) +
        geom_violin(trim = FALSE) +
        coord_flip() +
        theme_minimal(base_size = 14) +
        labs(
          title = paste("Sentiment Density by", demo_col),
          x = demo_col,
          y = "Sentiment Score"
        )
      
    } else if (plot_type == "jitterbox") {
      ggplot(df, aes(x = .data[[demo_col]], y = Support_Notes_Sentiment, color = .data[[demo_col]])) +
        geom_boxplot(outlier.shape = NA, fill = NA) +
        geom_jitter(width = 0.2, alpha = 0.5) +
        coord_flip() +
        theme_minimal(base_size = 14) +
        labs(
          title = paste("Box + Jitter Sentiment Plot by", demo_col),
          x = demo_col,
          y = "Sentiment Score"
        )
      
    } else if (plot_type == "ridge") {
      # Load ggridges if not already loaded
      if (!requireNamespace("ggridges", quietly = TRUE)) {
        stop("The 'ggridges' package is required for ridge plots. Please install it.")
      }
      library(ggridges)
      
      ggplot(df, aes(y = .data[[demo_col]], x = Support_Notes_Sentiment, fill = .data[[demo_col]])) +
        ggridges::geom_density_ridges(scale = 1) +
        theme_minimal(base_size = 14) +
        labs(
          title = paste("Ridge Plot of Sentiment by", demo_col),
          y = demo_col,
          x = "Sentiment Score"
        )
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
