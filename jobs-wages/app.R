mega <- readRDS("data/mega_wages.rds")

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Jobs and wages by state"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("STATE", label = h3("Select state"), 
                  unique(mega$STATE), selected = unique(mega$STATE[1])),
      selectInput("OCCUPATION", label = h3("Select occupation"), 
                  unique(mega$OCC_TITLE), selected = unique(mega$OCC_TITLE[1]))
    ),
    mainPanel(
      h1("First level title"),
      h2("Second level title"),
      h3("Third level title"),
      h4("Fourth level title"),
      h5("Fifth level title"),
      h6("Sixth level title"),
      textOutput("text1"),
      textOutput("text2")
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  the_state <- input$STATE
  the_occu <- input$OCCUPATION
  
  output$text1 <- renderText({ 
    paste("You have selected", the_state, "and", the_occu)
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

