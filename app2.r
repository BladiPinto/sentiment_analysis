library(shiny)
library(tm)
library(wordcloud)
library(httr)
library(dplyr)
library(ggplot2)

url <- "https://text-analysis12.p.rapidapi.com/sentiment-analysis/api/v1.1"

ui <- fluidPage(
  titlePanel("Sentiment Analysis for surveys"),
  sidebarLayout(
    sidebarPanel(
      # Sentiment Analysis
      radioButtons("option", "Option:", choices = c("Text", "Upload CSV")),
      conditionalPanel(
        condition = "input.option == 'Text'",
        textInput("text", "Enter the text:")
      ),
      conditionalPanel(
        condition = "input.option == 'Upload CSV'",
        fileInput("file", "Upload CSV file for Sentiment Analysis",accept = c(".csv")),
        selectInput("text_column_sa", "Select text column for Sentiment Analysis", choices = NULL),
        selectInput("text_column_wc", "Select text column for Word Cloud", choices = NULL),
        selectInput("category_column", "Select categorical column", choices = NULL)
      ),
      actionButton("analyze", "Perform Analysis"),
      # Word Cloud Generator
      actionButton("generateBtn", "Generate Word Cloud"),
      verbatimTextOutput("result"),
      selectInput("selected_category", "Select category:", choices = c("General", NULL)),
      uiOutput("category_selector"),
      
      
    ),
    mainPanel(
      plotOutput("plot"),
      dataTableOutput("table"),
      plotOutput("wordcloud"),
    )
  )
)

server <- function(input, output, session) {
  # Vincular el archivo CSS

  
  # Word Cloud Generator - Load CSV and get columns
  data_wc <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observe({
    shiny::includeCSS("styles.css")
    if (!is.null(data_wc())) {
      updateSelectInput(session, "text_column_wc", choices = colnames(data_wc()))
    }
  })
  
  # Word Cloud Generator - Generate Word Cloud
  output$wordcloud <- renderPlot({
    req(input$generateBtn, input$text_column_wc)
    
    text_column <- data_wc()[[input$text_column_wc]]
    text_column <- as.character(text_column)
    
    # Preprocess text
    corpus <- Corpus(VectorSource(text_column))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    # Create Word Cloud
    wordcloud(words = corpus,
              random.order = FALSE,
              colors = brewer.pal(8, "Dark2")
    )
  })
  
  # Sentiment Analysis - Update column choices after uploading CSV
  observeEvent(input$file, {
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    updateSelectInput(session, "text_column_sa", choices = names(data))
    updateSelectInput(session, "category_column", choices = names(data))
  })
  
  # Sentiment Analysis - Perform Analysis
  analyzeSentiment <- function(text) {
    payload <- paste0('{"language": "english", "text": "', text, '"}')
    response <- POST(url,
                     body = payload,
                     add_headers('X-RapidAPI-Key' = '5723f5d0e3msh0c11d897b7e0f79p155556jsnda522ee5b848', 'X-RapidAPI-Host' = 'text-analysis12.p.rapidapi.com'),
                     content_type("application/json"))
    sentiment <- content(response, "parsed")$sentiment
    return(sentiment)
  }
  
  observeEvent(input$analyze, {
    if (input$option == "Text") {
      req(input$text)
      text <- input$text
      if (text != "") {
        sentiment <- analyzeSentiment(text)
        output$result <- renderPrint({
          paste("Sentiment:", sentiment)
        })
        output$plot <- renderPlot(NULL)
        output$table <- NULL
        output$selected_category <- renderUI(NULL)
        output$category_selector <- renderUI(NULL)
      } else {
        output$result <- renderPrint("Enter a valid text.")
        output$plot <- renderPlot(NULL)
        output$table <- NULL
        output$selected_category <- renderUI(NULL)
        output$category_selector <- renderUI(NULL)
      }
    } else if (input$option == "Upload CSV") {
      req(input$file, input$text_column_sa, input$category_column)
      data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      
      if (!is.null(input$text_column_sa) && input$text_column_sa %in% colnames(data) &&
          !is.null(input$category_column) && input$category_column %in% colnames(data)) {
        text <- data[[input$text_column_sa]]
        sentiment <- sapply(text, analyzeSentiment)
        category <- data[[input$category_column]]
        
        output$result <- renderPrint({
          paste("Sentiments:", sentiment)
        })
        
        table_data <- data.frame(Sentiment = sentiment, Category = category)
        table_data <- table_data %>%
          group_by(Sentiment, Category) %>%
          summarise(Frequency = n()) %>%
          mutate(Percent = round(Frequency / sum(Frequency) * 100, 1))
        
        output$table <- renderDataTable({
          if (input$selected_category == "General") {
            general_data <- table_data %>% group_by(Sentiment) %>% summarise(Frequency = sum(Frequency))
            general_data$Percent <- round(general_data$Frequency / sum(general_data$Frequency) * 100, 1)
            general_data
            total_row <- c("Total", sum(general_data$Frequency), 100)
            rbind(general_data, total_row)
          } else {
            category_total <- sum(table_data$Frequency[table_data$Category == input$selected_category])
            table_data %>%
              filter(Category == input$selected_category) %>%
              mutate(Percent = round(Frequency / category_total * 100, 1))
          }
        })
        
        output$plot <- renderPlot({
          if (input$selected_category == "General") {
            general_data <- table_data %>% group_by(Sentiment) %>% summarise(Frequency = sum(Frequency))
            general_data$Percent <- round(general_data$Frequency / sum(general_data$Frequency) * 100, 1)
            plot_data <- general_data
          } else {
            category_total <- sum(table_data$Frequency[table_data$Category == input$selected_category])
            plot_data <- table_data %>%
              filter(Category == input$selected_category) %>%
              mutate(Percent = round(Frequency / category_total * 100, 1))
          }
          
          ggplot(plot_data, aes(x = Sentiment, y = Percent, fill = Sentiment)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(Percent, "%")), vjust = -0.5, size = 3) +
            labs(x = "Sentiment", y = "Percentage", fill = "Sentiment") +
            theme_minimal()
        })
        
        categories <- unique(category)
        updateSelectInput(session, "selected_category", choices = c("General", categories))
        updateSelectInput(session, "category_selector", choices = c("General", categories))
        
      } else {
        output$result <- renderPrint("Select a valid column.")
        output$table <- NULL
        output$plot <- NULL
        output$selected_category <- renderUI(NULL)
        output$category_selector <- renderUI(NULL)
      }
    }
  })
}

shinyApp(ui = ui, server = server)
