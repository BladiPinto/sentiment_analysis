library(shiny)
library(tm)
library(wordcloud)

ui <- fluidPage(
  titlePanel("Word Cloud Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Subir archivo CSV",
                accept = c(".csv")
      ),
      selectInput("text_column", "Seleccionar columna de texto",
                  choices = NULL
      ),
      actionButton("generateBtn", "Generar Word Cloud")
    ),
    mainPanel(
      plotOutput("wordcloud")
    )
  )
)

server <- function(input, output, session) {
  # Cargar archivo CSV y obtener las columnas
  data1 <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observe({
    if (!is.null(data1())) {
      updateSelectInput(session, "text_column", choices = colnames(data1()))
    }
  })
  
  # Generar el word cloud
  output$wordcloud <- renderPlot({
    req(input$generateBtn, input$text_column)
    
    text_column <- data1()[[input$text_column]]
    text_column <- as.character(text_column)
    
    # Preprocesar el texto
    corpus <- Corpus(VectorSource(text_column))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    # Crear el word cloud
    wordcloud(words = corpus,
              random.order = FALSE,
              colors = brewer.pal(8, "Dark2")
    )
  })
}

shinyApp(ui, server)