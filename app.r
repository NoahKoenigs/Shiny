library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("CSV Linear Reader"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Input: Select a file 
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line 
      tags$hr(),
      # Input: Checkbox if file has header 
      checkboxInput("header", "Header", TRUE),
      # Input: Select separator 
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      # Input: Select quotes 
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      # Horizontal line ----
      tags$hr(),
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "Preview",
                               None = "None",
                               All = "All"),
                   selected = "None"),
      # Adding a reset button
      actionButton("reset_btn", "Reset"),
      
      # Adding a histogram button
      actionButton("histogram_btn", "Histogram")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      tableOutput("contents")
    )
  )
)

# Define server logic required to draw a histogram (not shown on this Shiny app)
server <- function(input, output, session) {
  dataInput <- reactive({
    if (is.null(input$file1))
      return(NULL)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    return(df)
  })
  
  # Observe Tag for Reset button
  observeEvent(input$reset_btn, {
    updateTextInput(session, "dem", "Reset")
    
    # Reset the input values to their default state
    updateRadioButtons(session, "disp", selected = "None")
    updateRadioButtons(session, "sep", selected = ",")
    updateRadioButtons(session, "quote", selected = '"')
    updateCheckboxInput(session, "header", value = TRUE)
    updateFileInput(session, "file1", NULL)
  })
  
  # Show histogram when the "Histogram" button is clicked
  observeEvent(input$histogram_btn, {
    if (input$histogram_btn > 0) {
      output$plot <- renderPlot({
        if (is.null(dataInput()))
          return(NULL)
        
        x <- dataInput()[["x"]]
        bins <- seq(min(x), max(x), length.out = 10)  # Adjust the number of bins as needed
        hist(x, breaks = bins, col = 'orange', border = 'white', main = "Histogram")
      })
    }
  })
  
  # Show regression plot by default
  output$plot <- renderPlot({
    if (is.null(dataInput()))
      return(NULL)
    
    coefs <- coef(lm(y ~ x, data = dataInput()))
    intercept <- round(coefs[1], 2)
    slope <- round(coefs[2], 2)
    r2 <- round(summary(lm(y ~ x, data = dataInput()))$r.squared, 2)
    
    ggplot(dataInput(), aes(x = x, y = y)) +
      geom_point(colour = 'red') +
      geom_smooth(method = "lm", se = FALSE, color = 'blue') +
      ggtitle('X vs Y') +
      xlab('X') +
      ylab('Y') +
      geom_text(aes(x = 10, y = 21, label = paste("Intercept =", intercept))) +
      geom_text(aes(x = 10, y = 20, label = paste("Slope =", slope))) +
      geom_text(aes(x = 10, y = 19, label = paste("R-squared =", r2)))
  })
  
  output$contents <- renderTable({
    if (is.null(dataInput()))
      return(NULL)
    
    if (input$disp == "Preview") {
      return(head(dataInput()))
    } else if (input$disp == "All") {
      return(dataInput())
    } else {
      return(NULL)
    } 
  })       
}

# Run the application 
shinyApp(ui = ui, server = server)





