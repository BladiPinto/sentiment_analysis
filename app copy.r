library(shiny)
library(httr)
library(dplyr)
library(ggplot2)

url <- "https://text-analysis12.p.rapidapi.com/sentiment-analysis/api/v1.1"

ui <- fluidPage(
  titlePanel("Análisis de Sentimientos"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("option", "Opción:", choices = c("Texto", "Cargar CSV")),
      conditionalPanel(
        condition = "input.option == 'Texto'",
        textInput("text", "Ingresa el texto:")
      ),
      conditionalPanel(
        condition = "input.option == 'Cargar CSV'",
        fileInput("file", "Cargar archivo CSV"),
        selectInput("text_column", "Seleccionar columna de texto", choices = NULL),
        selectInput("category_column", "Seleccionar columna categórica", choices = NULL)
      ),
      actionButton("analyze", "Realizar análisis"),
      verbatimTextOutput("result"),
      selectInput("selected_category", "Seleccionar categoría:", choices = c("General", NULL)),
      uiOutput("category_selector")
    ),
    mainPanel(
      plotOutput("plot"),
      dataTableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  # Actualizar las opciones del selector de columnas después de cargar el archivo CSV
  observeEvent(input$file, {
    data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    updateSelectInput(session, "text_column", choices = names(data))
    updateSelectInput(session, "category_column", choices = names(data))
  })
  
  # Función para realizar el análisis de sentimientos en un texto
  analyzeSentiment <- function(text) {
    payload <- paste0('{"language": "english", "text": "', text, '"}')
    response <- POST(url,
                     body = payload,
                     add_headers('X-RapidAPI-Key' = '5723f5d0e3msh0c11d897b7e0f79p155556jsnda522ee5b848', 'X-RapidAPI-Host' = 'text-analysis12.p.rapidapi.com'),
                     content_type("application/json"))
    sentiment <- content(response, "parsed")$sentiment
    return(sentiment)
  }
  
  # Realizar el análisis de sentimientos al hacer clic en el botón "Realizar análisis"
  observeEvent(input$analyze, {
    if (input$option == "Texto") {
      req(input$text)
      text <- input$text
      if (text != "") {
        sentiment <- analyzeSentiment(text)
        output$result <- renderPrint({
          paste("Sentimiento:", sentiment)
        })
        output$plot <- renderPlot(NULL)
        output$table <- NULL
        output$selected_category <- renderUI(NULL)
        output$category_selector <- renderUI(NULL)
      } else {
        output$result <- renderPrint("Ingresa un texto válido.")
        output$plot <- renderPlot(NULL)
        output$table <- NULL
        output$selected_category <- renderUI(NULL)
        output$category_selector <- renderUI(NULL)
      }
    } else if (input$option == "Cargar CSV") {
      req(input$file, input$text_column, input$category_column)
      data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      
      if (!is.null(input$text_column) && input$text_column %in% colnames(data) &&
          !is.null(input$category_column) && input$category_column %in% colnames(data)) {
        text <- data[[input$text_column]]
        sentiment <- sapply(text, analyzeSentiment)
        category <- data[[input$category_column]]
        
        output$result <- renderPrint({
          paste("Sentimientos:", sentiment)
        })
        
        # Crear una tabla de frecuencias
        table_data <- data.frame(Sentimiento = sentiment, Categoría = category)
        table_data <- table_data %>%
          group_by(Sentimiento, Categoría) %>%
          summarise(Frequency = n()) %>%
          mutate(Percent = round(Frequency / sum(Frequency) * 100, 1))
        
        output$table <- renderDataTable({
          if (input$selected_category == "General") {
            general_data <- table_data %>% group_by(Sentimiento) %>% summarise(Frequency = sum(Frequency))
            general_data$Percent <- round(general_data$Frequency / sum(general_data$Frequency) * 100, 1)
            general_data
            total_row <- c("Total", sum(general_data$Frequency), 100)
            rbind(general_data, total_row)
          } else {
            category_total <- sum(table_data$Frequency[table_data$Categoría == input$selected_category])
            table_data %>%
              filter(Categoría == input$selected_category) %>%
              mutate(Percent = round(Frequency / category_total * 100, 1))
          }
        })
        
        # Crear un gráfico de barras
        output$plot <- renderPlot({
          if (input$selected_category == "General") {
            general_data <- table_data %>% group_by(Sentimiento) %>% summarise(Frequency = sum(Frequency))
            general_data$Percent <- round(general_data$Frequency / sum(general_data$Frequency) * 100, 1)
            plot_data <- general_data
          } else {
            category_total <- sum(table_data$Frequency[table_data$Categoría == input$selected_category])
            plot_data <- table_data %>%
              filter(Categoría == input$selected_category) %>%
              mutate(Percent = round(Frequency / category_total * 100, 1))
          }
          
          ggplot(plot_data, aes(x = Sentimiento, y = Percent, fill = Sentimiento)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(Percent, "%")), vjust = -0.5, size = 3) +  # Agregar etiquetas en el medio de las barras
            labs(x = "Sentimiento", y = "Porcentaje", fill = "Sentimiento") +
            theme_minimal()
        })
        
        # Actualizar las opciones del selector de categorías
        categories <- unique(category)
        updateSelectInput(session, "selected_category", choices = c("General", categories))
        
        # Actualizar las opciones del selector de categorías según la columna categórica seleccionada
        updateSelectInput(session, "category_selector", choices = c("General", categories))
        
      } else {
        output$result <- renderPrint("Selecciona una columna válida.")
        output$table <- NULL
        output$plot <- NULL
        output$selected_category <- renderUI(NULL)
        output$category_selector <- renderUI(NULL)
      }
    }
  })
}

shinyApp(ui = ui, server = server)
