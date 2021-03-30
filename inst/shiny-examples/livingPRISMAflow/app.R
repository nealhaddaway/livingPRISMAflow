library(shiny)
library(ggplot2)
library(shinybusy)
library(statebins)

source("LSRPrisma_data.R")
source("LSRPrisma_flow.R")


#template <- read.csv("www/PRISMA.csv",stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Living PRISMA Flow Diagram",
                         
                         # Tab 1 ----
                         tabPanel("Home",
                                  fluidRow(
                                      column(10,
                                             'Systematic reviews should be described in a high degree of methodological detail. ', tags$a(href="http://prisma-statement.org/", "The PRISMA Statement"), 
                                             'calls for a high level of reporting detail in systematic reviews and meta-analyses. An integral part of the methodological description of a review is a flow diagram.',
                                             br(),
                                             br(),
                                             'This tool allows you to produce a flow diagram for your own living systematic review based on ', tags$a(href="https://osf.io/preprints/metaarxiv/v7gm2/", "the PRISMA2020 Statement."), 
                                             'You can provide the numbers and texts for the boxes by adding your own values using the template file below. The four different formats described in ', tags$a(href="https://doi.org/10.12688/f1000research.51723.2", "the preprint by Kahale et al. (2021)"), ' are possible.',
                                             br(),
                                             br(),
                                             "At present, this version of the tool doesn't support embedding tooltips and hyperlinks in the plot. For this functionality, please use the", 
                                             tags$a(href="https://github.com/nealhaddaway/livingPRISMAflow", "livingPRISMAflow R package on Github."),
                                             br(),
                                             br(),
                                             'Please let us know if you have any feedback or if you encounter an error by sending an email to ', tags$a(href="mailto:neal.haddaway@sei.org", "neal.haddaway@sei.org"),
                                             br(),
                                             br(),
                                             HTML('<a href="template.csv"><button style = "border-radius:8px;background-color:#b6cce6;border: none;color: white;padding: 10px 25px;text-align: center;text-decoration: none;display: inline-block;font-size: 14px;margin: 4px 2px;cursor:pointer;">Download the template CSV file</button></a>'),
                                             #tags$a(href="template.csv", "Download the template CSV file", download=NA, target="_blank"),
                                             br(),
                                             br(),
                                             'See the four different formats possible by making a selection from this dropdown box:',
                                             br(),
                                             selectInput("format", "Select a format", choices = c('Approach 1: Base review and each update separately' = 'one',
                                                                                                  'Approach 2: Combined base and update' = 'two',
                                                                                                  'Approach 3: Base review separately and combined updates' = 'three',
                                                                                                  'Approach 4: Base review and updates combined, latest update separate' = 'four'), width='100%'),
                                             br(),
                                             uiOutput('frame'),
                                             br(),
                                             hr(),
                                             'Please cite as:',
                                             br(),
                                             'Neal R Haddaway. (2021). livingPRISMA_flow: R package and ShinyApp for producing PRISMA-style flow diagrams for living systematic reviews (Version 0.0.1). Zenodo.', 
                                             tags$a(href="xxx", "xxx"),
                                             br(),
                                             tags$a(href="Haddaway_2021.ris", "Download citation (.ris)", download=NA, target="_blank")
                                      ),
                                      column(2,
                                             tags$img(height = 170, src = "https://github.com/nealhaddaway/livingPRISMAflow/blob/master/inst/extdata/livingPRISMAflow_hex.png?raw=true"),
                                      )
                                  ),
                                  
                                  # Show a plot of the generated distribution
                                  fluidRow(
                                      column(10, 
                                             br(),
                                             'Credits:',
                                             br(),
                                             'Neal R Haddaway (creator, author)', br(),
                                             'Lara Kahale (concept, advisor)', br(),
                                             'Elie A Akl (concept, advisor)', br(),
                                             'Luke A McGuinness (coder, contributor)', br(),
                                             br(),
                                             tags$a(href="https://github.com/nealhaddaway/livingPRISMAflow", tags$img(height = 40, width = 40, src = "https://pngimg.com/uploads/github/github_PNG40.png")), 
                                             'Created March 2021'
                                      )
                                  )
                         ),
                         
                         # Tab 2 ----
                         tabPanel("The flow diagram",
                                  sidebarLayout(
                                      sidebarPanel(width=3,
                                          #style = "overflow-y:scroll; max-height: 900px; position:relative;",
                                          'Upload your edited file here:',
                                          br(),
                                          fileInput("data_upload", "Choose CSV File",
                                                    multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
                                          h3("Options"),
                                          selectInput("previous", "Previous studies", choices = c('Not included', 'Included')),
                                          selectInput("other", "Other searches for studies", choices = c('Included', 'Not included')),
                                          hr(),
                                          actionButton("generate", "Click to show flow diagram"),
                                          hr(),
                                          h3("Download"),
                                          downloadButton('downloadPlot', 'Download PDF')
                                      ), 
                                      mainPanel(
                                          plotOutput("plot", width = '100%'),
                                          add_busy_spinner(spin = "fading-circle", color = "#ffc125", position = "full-page"))
                                  ))
))



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Define reactive values
    rv <- reactiveValues()
    
    # Data Handling ----
    
    # Use template data to populate editable table
    observeEvent(input$data_upload,{
        rv$data <- read.csv(input$data_upload$datapath)
        rv$flowdata <- LSRPrisma_data(rv$data)
    })
    
    # Create plot
    plot <- reactive({
        if (input$previous == 'Included'){
            include_previous = TRUE
        } else {
            include_previous = FALSE
        }
        if (input$other == 'Included'){
            include_other = TRUE
        } else {
            include_other = FALSE
        }
        plot <- LSRPrisma_flow(rv$flowdata,
                       previous = include_previous,
                       other = include_other)
        rv$plot <- plot
        plot
    })
    
    observeEvent(input$generate, {
        output$plot <- renderPlot({
            plot()
        })
    })
    
    # Demo reactive imgage
    output$frame <- renderUI({
        format <- paste0('https://github.com/nealhaddaway/livingPRISMAflow/blob/master/inst/shiny-examples/livingPRISMAflow/www/', input$format, '.png?raw=true')
        my_test <- tags$img(src=format, width='70%', height = '70%')
        my_test
    })
    
    # Handle downloads ----
    output$downloadPlot <- downloadHandler(
        filename = 'PRISMA.pdf',
        content = function(file){
            pdf(file = file, width = 10, height = 5.354)
            print(plot())
            dev.off()
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)


