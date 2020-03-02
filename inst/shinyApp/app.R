#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(floodnetRfa)

# Load global variable from the config file
# Temporary solution to load the db, Will need a menu
source(system.file('config', package = 'floodnetRfa'))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "mystyle.css",

    # Application title
    titlePanel("FloodnetApp"),

    sidebarLayout(
        sidebarPanel(
            tags$head(tags$style(type="text/css",
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
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading...",id="loadmessage"))
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
    # sidebarLayout(
    #     sidebarPanel(tags$p("test")),
    #     mainPanel()
    # )


    ## Custom-css method, if we want sidebar split up, designed for landscape, etc.
    # fluidRow(
    #     column(4,HTML("<div class='inputBox'>
    #          "),
    #            column(6,
    #                   selectInput("station", label = h3("Select Station"),
    #                               choices = list("01AF009" = "01AF009",
    #                                              "01AD003" = '01AD003',
    #                                              "01AF007" = '01AF007',
    #                                              "01AJ003" = '01AJ003',
    #                                              "01AJ004" = '01AJ004',
    #                                              "01AJ010" = '01AJ010'
    #                               ))
    #                   ),
    #           column(6,
    #                  textInput("periodString", label = h3("Return Period"), value = "10,100"),
    #                  ),
    #           fluidRow(
    #               actionButton(inputId = 'update', label = 'Update'),
    #               HTML("</div>")
    #           )
    #
    #
    #
    #     )
    #
    # ),
    #
    #
    #
    #
    #     # column(6,
    #            #inputPanel(# Input station selector
    #               # fluidRow(
    # #               column(6,
    # #                     selectInput("station", label = h3("Select Station"),
    # #                                   choices = list("Iroquois River at Moulin Morneault" = "01AF009",
    # #                                                  "ST. FRANCIS RIVER AT OUTLET OF GLASIER LAKE" = '01AD003',
    # #                                                  "GRANDE RIVIERE AT VIOLETTE BRIDGE" = '01AF007',
    # #                                                  "MEDUXNEKEAG RIVER NEAR BELLEVILLE" = '01AJ003',
    # #                                                  "BIG PRESQUE ISLE STREAM AT TRACEY MILLS" = '01AJ004',
    # #                                                  "BECAGUIMEC STREAM AT COLDSTREAM" = '01AJ010'
    # #                                   ))
    # #                 ),
    # #                 column(6,
    # #                     textInput("periodString", label = h3("Return Period"), value = "Enter period. Eg 10,100...")
    # #                 ),
    # #
    # #
    # #                 actionButton(inputId = 'update', label = 'Update')
    # #            )
    # #
    # #            )
    # # ),
    # #fluidRow(tags$h3("In-place Amax")),
    # fluidRow(
    #     headerPanel("In-place Amax"),
    #     column(width = 6, tableOutput("amaxTable")),
    #     column(width = 6, plotOutput("amaxPlot"))
    # ),
    # fluidRow("In-place POT"),
    # fluidRow(
    #     column(width = 6, tableOutput("potTable")),
    #     column(width = 6, plotOutput("potPlot"))
    # ),
    # fluidRow("RFA Amax"),
    # fluidRow(
    #     column(width = 6, tableOutput("rfaAmaxTable")),
    #     column(width = 6, plotOutput("rfaAmaxPlot"))
    # ),
    # fluidRow("RFA POT"),
    # fluidRow(
    #     column(width = 6, tableOutput("rfaPotTable")),
    #     column(width = 6, plotOutput("rfaPotPlot"))
    # )


)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # You can access the value of the widget with input$select, e.g.

    # observeEvent(input$update, {
    #     output$loading <- list(
    #         src = "www/loadingSpinnger.svg",
    #         contentType = "image/png",
    #         alt = "Spinner"
    #     )
    # })

    # observeEvent(input$update, {
    #     output$loading <- renderText("Loading...")
    # })


    # Making eventReactive so table/plot updates with button instead of automatically
    # Storing values in result so each function is only run once
    result <- eventReactive(input$update, .ClickUpdate(input, db = DB_HYDAT))

    # output functions to table/plot
    output$table <- renderTable(as.data.frame(result()))
    output$plot <- renderPlot(plot(result()) )

}

# Run the application
shinyApp(ui = ui, server = server)
