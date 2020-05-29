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
library(shinydashboard)
library(shinyjs)
library(DT)


# Load global variable from the config file
# Temporary solution to load the db, Will need a menu
source(system.file('config', package = 'floodnetRfa'))

sidebar <- dashboardSidebar(

)

body <- dashboardBody(

	#Use custom css
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css"),
		tags$style(class = "body")
	),
	useShinyjs(),

	fluidRow(
		column(5,
					 ## -- Start of Model Configuration box --
					 tags$div(class = "background-box fixed-height",
					 				 ## Headline
					 				 tags$h2("Model Configuration"),
					 				 ## Left side -------------------------------
					 				 tags$div(class = "left-item",
					 				 				 textInput("station", label = h3("Target Site"),
					 				 				 					placeholder = "Enter Station ID..."),
					 				 				 textInput("periodString", label = h3("Return Period"),
					 				 				 					value = "2, 5, 10, 20, 50, 100"),
					 				 				 selectInput("method", label = h3("Method"),
					 				 				 						choices = list("AMAX" = "amax",
					 				 				 													 "POT" = "pot",
					 				 				 													 "RFA AMAX" = "rfaAmax",
					 				 				 													 "RFA POT" = "rfaPot"
					 				 				 						), selected = "amax")
					 				 ),
					 				 ## Right side --------------------------------
					 				 tags$div(class = "right-item",
					 				 				 conditionalPanel(condition = "input.method == 'rfaAmax' || input.method == 'rfaPot'",
					 				 				 								 selectInput("supReg", label = h3("Super Region"),
					 				 				 								 						choices = list("Province" = "province",
					 				 				 								 													 "User-Made" = "userMade"),
					 				 				 								 						selected = "Province")
					 				 				 ),

					 				 				 ## The option to select the distribution method is only available for AMAX
					 				 				 ## Therefore this selectInput is hidden for POT
					 				 				 conditionalPanel(condition = "input.method == 'amax' || input.method == 'rfaAmax'",
					 				 				 								 # disthresh used instead of seperate distr and thresh ... otherwise cannot merge in table
					 				 				 								 selectInput("disthresh", label = h3("Distribution"),
					 				 				 								 						choices = list("Default" =  "Default",
					 				 				 								 													 "gev" = "gev",
					 				 				 								 													 "glo" = "glo",
					 				 				 								 													 "gno" = "gno",
					 				 				 								 													 "pe3" = "pe3"
					 				 				 								 						), selected = "Default")
					 				 				 ),

					 				 				 ## The option to select the threshold is only available for AMAX
					 				 				 ## Therefore this selectInput is hidden for POT
					 				 				 conditionalPanel(condition = "input.method == 'pot' || input.method == 'rfaPot'",
					 				 				 								 selectInput("disthresh", label = h3("Threshold"),
					 				 				 								 						choices = list("Default" =  "Default",
					 				 				 								 													 "Read from List..." = "etc"
					 				 				 								 						), selected = "Default")
					 				 				 ),

					 				 				 ## Action button for running the model - always on bottom right
					 				 				 actionButton("fitModel", class = "bottom-button red-button right-button", label = "Fit")
					 				 )
					 )

		), ## -- End of Model Configuration box --

		## Right side - Return Plot
		column(5, offset = 1,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Return Level Plot"),
					 				 # imageOutput("loading"),
					 				 plotOutput("plot")
					 ),
		 )

	), ## -- End of 1st Row --

	fluidRow( ## 2nd Row
		## -- Fitted Models box --
		column(5,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Fitted Models"),
					 				 DT::dataTableOutput("modelsTable"),
					 				 ## Action button for removing selected models from datatable
					 				 actionButton("removeButton", class = "bottom-button blue-button left-button", label = "Remove"),
					 				 ## Action button for showing selected models from datatable in graphics tab
					 				 actionButton("showButton", class = "bottom-button red-button right-button", label = "Show")
					 )
	  ),
		## -- Flood Quantiles box --
		column(5, offset = 1,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Flood Quantiles"),
					 				 # imageOutput("loading"),
					 				 DT::dataTableOutput("table")
					 ))
	)
)

graphicsSidebar <- dashboardSidebar(

)

graphicsBody <- dashboardBody(

	#Use custom css
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css"),
		tags$style(class = "body")
	),

	fluidRow( ## 1st row graphics
		## -- Flood Quantiles box --
		column(5,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Flood Quantiles"),
					 				 # imageOutput("loading"),
					 				 DT::dataTableOutput("graphicsQuantiles")
					 )),
		## -- Return plot box --
		column(5, offset = 1,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Return Level Plot"),
					 				 # imageOutput("loading"),
					 				 plotOutput("graphicsReturnPlot")
					 ))
		),

	fluidRow( ## 2nd row graphics
		## -- Confidence Intervals box --
		column(5,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Confidence Intervals"),
					 				 # imageOutput("loading"),
					 				 plotOutput("confIntervals")
					 )),
		## -- Coefficient of Variation box --
		column(5, offset = 1,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Coefficient of Variation"),
					 				 # imageOutput("loading"),
					 				 plotOutput("ceoffVariation")
					 ))
	),

	fluidRow( ## 3rd row graphics
		## -- Histogram box --
		column(5,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Histogram"),
					 				 # imageOutput("loading"),
					 				 plotOutput("histogram")
					 )),
		## -- L-moment Ratio Diagram box --
		column(5, offset = 1,
					 tags$div(class = "background-box fixed-height",
					 				 h2("L-Moment Ratio Diagram"),
					 				 # imageOutput("loading"),
					 				 plotOutput("momPlot")
					 ))
	),

	fluidRow( ## 4th row graphics
		## -- Coordinates box --
		column(5,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Coordinates"),
					 				 # imageOutput("loading"),
					 				 plotOutput("coordinatesPlot")
					 )),
		## -- Descriptor Space box --
		column(5, offset = 1,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Descriptor Space"),
					 				 # imageOutput("loading"),
					 				 plotOutput("descriptorPlot")
					 ))
	),

	fluidRow( ## 5th row graphics
		## -- Seasonal Space box --
		column(5,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Seasonal Space"),
					 				 # imageOutput("loading"),
					 				 plotOutput("seasonalPlot")
					 )
		)
	)
)

ui <- navbarPage("FloodNet RFA", id="pageId",
								 tabPanel("Models",
								 				 dashboardPage(
								 				 	dashboardHeader(disable = TRUE,
								 				 									#For some reason, color styling had to be done like this and not in css
								 				 									title = tags$div(tags$span(id = "floodnetText", "FloodNet"),
								 				 																	 tags$span(id = "rfaText", "RFA"))
								 				 	),
								 				 	sidebar,
								 				 	body)
								 ),
								 tabPanel("Graphics",
								 				 dashboardPage(
								 				 	dashboardHeader(disable = TRUE,
								 				 									#For some reason, color styling had to be done like this and not in css
								 				 									title = tags$div(tags$span(id = "floodnetText", "FloodNet"),
								 				 																	 tags$span(id = "rfaText", "RFA"))
								 				 	),
								 				 	graphicsSidebar,
								 				 	graphicsBody
								 				 )
								 )
)


# ----------  SERVER Function  ------------------------------------------------------------------------
server <- function(input, output, session) {

	# ------ Models Page Functions ------
	# Making eventReactive so table/plot updates with button instead of automatically
	# Storing values in result so each function is only run once
	result <- shiny::eventReactive(input$fitModel, floodnetRfa::.ClickUpdate(input, db = DB_HYDAT))

	# Add fitted model to list
	values <- reactiveValues() # Found a similar solution on stackoverflow (23236944), but do we need all of values just for df?
	#values$df <- data.frame(Column1 = NA, Column2 = NA, Column3 = NA) # Column names need to be the same else match.names error .. only needed to initialize table
  mList <- reactiveValues() #list of models to be shown in graphics page ... unsure if should be reactiveValues or some other kind of data structure?
  graphicsModel <- reactiveValues() #current (temp) model to be displayed in graphics


	newModel <- observeEvent(input$fitModel, {
		# When a model is fit, a new line is made for the Fitted Models datatable and contains the model info
		if (input$method == "amax" || input$method == "pot") { #need to create NA for superregion for non-RFA methods
			values$supReg <- "N/A"   ## Can't modify input... have to create new reactive value
		} else {
			values$supReg <- input$supReg # Store input supreg in same reactive value as non-rfa, so it can be used together in DT
		}
		newLine <- isolate(cbind.data.frame(input$station, input$periodString, input$method, input$disthresh, values$supReg))
		isolate(values$df <- rbind.data.frame(values$df, newLine))
	})



	# Keep hidden data table of 'result's, tied to visable list of models

	# output functions to table/plot
	output$table <- renderDT(
		as.data.frame(result()), options = list(
			pageLength = 6
			#paging = FALSE #FALSE = becomes one long list instead of multiple properly-sized lists
		)
	)
	output$plot <- shiny::renderPlot(plot(result()), height = 327 ) #327 height leaves 20px bottom margin - same as buttons

	# List of Fitted Models
	output$modelsTable <- renderDT(
		values$df,
		colnames = c("Site", "Period", "Method", "Distribution/Threshold", "Super Region"),
		options = list(
			pageLength = 4

		)
	)

	# # Table button functions  ## Most likely not going to use these - showing an error message instead
	# shiny::observe(
	# 	if (length(input$modelsTable_rows_selected) > 0) {
	# 		shinyjs::enable("showButton")
	# 		shinyjs::enable("removeButton")
	# 	} else {
	# 		shinyjs::hide("showButton")
	# 		shinyjs::hide("removeButton")
	# 	}
	# )

# 	 shiny::observeEvent(length(input$modelsTable_rows_selected) > 0, {
#  		shinyjs::enable("showButton")
# 	 	shinyjs::enable("removeButton")
# 	 })
#
# 	 shiny::observeEvent(length(input$modelsTable_rows_selected) == 0, {
# 	 	shinyjs::hide("showButton")
# 	 	shinyjs::hide("removeButton")
# 	 })

	# When "Remove" button is pressed to remove selected models from table
	observeEvent(input$removeButton, {
		# get selected rows
		selectedRows <- input$modelsTable_rows_selected

		if (!is.null(selectedRows)) {
			values$df <- values$df[-as.numeric(selectedRows),]
		} else {
			showNotification("Please select a model from the list first.", type = "warning")
		}
	})


	# When "Show" button pressed to compare models in table
	observeEvent(input$showButton, {
		# get selected rows
		selectedRows <- input$modelsTable_rows_selected

		if (length(selectedRows) == 0) {
			showNotification("Please select a model from the list first.", type = "warning")
			return()
		}

		for (i in length(selectedRows)) {
			# load all data from selected models into mList
			mList$station[i] <- as.character(values$df[selectedRows[i],"input$station"])  #read values list #as.character() was needed!!!
			mList$periodString[i] <- as.character(values$df[selectedRows[i],"input$periodString"])
			mList$method[i] <- as.character(values$df[selectedRows[i],"input$method"])
			mList$disthresh[i] <- as.character(values$df[selectedRows[i],"input$disthresh"])
			mList$supReg[i] <- as.character(values$df[selectedRows[i],"values$supReg"])
		}

		# --- generate plots for first model in list ---
		# load temp model variable
		graphicsModel$station <- mList$station[1]
		graphicsModel$periodString <- mList$periodString[1]
		graphicsModel$method <- mList$method[1]
		graphicsModel$disthresh <- mList$disthresh[1]
		graphicsModel$supReg <- mList$supReg[1]

		#output$tableTest <- renderTable(graphicsModel$periodString)

		# calculate result
		resultGraphics <- shiny::reactive(floodnetRfa::.ClickUpdate(graphicsModel, db = DB_HYDAT))

		# plot result
		output$graphicsQuantiles <- renderDT(
			as.data.frame(resultGraphics()), options = list(
				pageLength = 6
				#paging = FALSE #FALSE = becomes one long list instead of multiple properly-sized lists
			)
		)
		output$graphicsReturnPlot <- shiny::renderPlot(plot(resultGraphics()), height = 327 )


		# Switch view to Graphics tab
		updateTabsetPanel(session, inputId = "pageId", selected = "Graphics")
	})

	# ----- End of Models Page -----
	# ----- Start of Graphics Page -----



}

# Run the application
shinyApp(ui = ui, server = server)
