#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("CSV Linear Reader"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # Horizontal line ----
            tags$hr(),
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            # Input: Select quotes ----
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
                         selected = "None")
            #Adding Action Button
            actionButton("btn", "Click Me")
        ),
            tags$head(tags$style(HTML("
                #dem {
                    title: "text/csv";
                    width:150px;
                    height:60px;
                    position: relative;
                }
                ")))
            tags$div(id= "dem", "Initial Text")
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("scatter"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    dataInput <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    #Observe Tag
    observeEvent(input$btn, {
        updateTextInput(session, "dem", "Text Updated")
    })
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    output$scatter <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    output$lmtPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    output$contents <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        if(input$disp == "Preview") {
            return(head(dataInput()))
        } else if (input$disp =="All") {
            return(dataInput())
        } else {
            return(NULL)
        }   
    })       
}
# Run the application 
shinyApp(ui = ui, server = server)
