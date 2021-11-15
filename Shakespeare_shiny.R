library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)
library(dplyr)

getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./Data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing

# task6: add in shinythemes function

ui <- fluidPage(
  theme=shinytheme("sandstone"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(
      selectInput("choosebook", "Choose a book:", books),
      checkboxInput("stopwords", "Stop words:", value=TRUE),
      actionButton("run", "Run"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput("maxwords", "Max # of Words", min=10, max=200, value=100, step=10),
      sliderInput("largestwords", "Size of largest words", min=1, max=8, value=4),
      sliderInput("smallestwords", "Size of smallest words", min=0.1, max=4, value=0.5),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput("minwords", "Minimum words for Counts Chart", min=10, max=100, value=25),
      sliderInput("wordsize", "Word size for Counts Chart", min=8, max=30, value=14)
    ),
    mainPanel(
      tabsetPanel(tabPanel(title="Word Cloud",
                           plotOutput("cloud", height="600px")),
                  tabPanel(title="Word Counts",
                           plotOutput("freq", height="600px"))
                  ))
  ))
    
  
  
  # task2: add in the inputs in the sidebarPanel
  
  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
  
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights



server <- function(input, output) {
    freq <- eventReactive(input$run, {
      
        withProgress({
          setProgress(message = "Processing corpus...")
          getFreq(input$choosebook, input$stopwords)
        })
    })
    output$cloud <- renderPlot({
      v <- freq()
      pal <- brewer.pal(8,"Dark2")
      v %>% 
        with(
          wordcloud(
            word, 
            n, 
            scale = c(input$largestwords, input$smallestwords),
            random.order = FALSE, 
            max.words = input$maxwords, 
            colors=pal))
    })
    output$freq <- renderPlot({
        v <- freq()
        v %>% 
            filter(n>input$minwords) %>%
            ggplot(aes(reorder(word, n), n))+
            geom_col() + 
          coord_flip()+
            theme(text = element_text(size = input$wordsize),
                  axis.title.x = element_blank(),
                  axis.title.y =  element_blank()
            )
        })
        
        
    }

shinyApp(ui = ui, server = server)

