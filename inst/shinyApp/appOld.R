#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Although against convention, nearly all of the ui uses shiny so this avoids very repetative calls
library(shiny)

# Load global variable from the config file
# Temporary solution to load the db, Will need a menu
source(system.file('config', package = 'floodnetRfa'))

# UI Section ---------------------------------------------------------------
ui <- fluidPage(theme = "mystyle.css",

                # Application title
                titlePanel("FloodnetApp"),

                sidebarLayout(
                    sidebarPanel(
                        tags$head(tags$style(type = "text/css",
                                             "#loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
            ")),
                        selectInput("station", label = h3("Target Site"),
                                    choices = list("01AF009" = "01AF009",
                                                   "01AD003" = '01AD003',
                                                   "01AF007" = '01AF007',
                                                   "01AJ003" = '01AJ003',
                                                   "01AJ004" = '01AJ004',
                                                   "01AJ010" = '01AJ010'
                                    ), selected = "01AF009"),
                        textInput("periodString", label = h3("Return Period"), value = "10,100"),
                        selectInput("method", label = h3("Method"),
                                    choices = list("AMAX" = "amax",
                                                   "POT" = "pot",
                                                   "RFA AMAX" = "rfaAmax",
                                                   "RFA POT" = "rfaPot"
                                    ), selected = "amax"),

                        ## The option to select the distribution method is only available for AMAX
                        ## Therefore this selectInput is hidden for POT
                        conditionalPanel(condition = "input.method == 'amax' || input.method == 'rfaAmax'",
                                         selectInput("distr", label = h3("Distribution"),
                                                     choices = list("Default" =  "Default",
                                                                    "gev" = "gev",
                                                                    "glo" = "glo",
                                                                    "gno" = "gno",
                                                                    "pe3" = "pe3"
                                                     ), selected = "Default"),
                        ),

                        actionButton(inputId = 'update', label = 'Update'),
                        # textOutput("loading", inline = TRUE)

                        # loading message and css taken from user1603038's post at https://stackoverflow.com/questions/17325521/r-shiny-display-loading-message-while-function-is-running
                        # will be customized in the future - a placeholder for now
                        conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                         tags$div("Loading...", id = "loadmessage"))
                    ),


                    mainPanel(

                        tags$h3("Plot"),
                        fluidRow(
                            # imageOutput("loading"),
                            plotOutput("plot")
                        ),
                        tags$h3("Estimated Flood Quantile"),
                        fluidRow(
                            # imageOutput("loading"),
                            tableOutput("table")
                        )
                    )


                )

)

# SERVER Function ------------------------------------------------------------------------
server <- function(input, output) {

    # Making eventReactive so table/plot updates with button instead of automatically
    # Storing values in result so each function is only run once
    result <- shiny::eventReactive(input$update, floodnetRfa::.ClickUpdate(input, db = DB_HYDAT))

    # output functions to table/plot
    output$table <- shiny::renderTable(result()$qua)
    output$plot <- shiny::renderPlot(plot(result()$fit) )

}

# Run the application
shinyApp(ui = ui, server = server)
