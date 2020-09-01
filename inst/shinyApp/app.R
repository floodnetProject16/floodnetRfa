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
library(shinyFiles)
library(DT)
library(floodnetRfa) #needed for supporting functions... I think the problem was ggplots wasn't loading but is loaded through floodnetRfa?
library(gridExtra) #needed for outputting dataframes to pdf



# Load global variable from the config file
# Temporary solution to load the db, Will need a menu
# source(system.file('config', package = 'floodnetRfa'))

options(shiny.maxRequestSize = 10000*1024^2) # Arbitrarily-large (~10GB) max upload file size since this will be done locally

sidebar <- dashboardSidebar(

	# --- Button box ---
	fluidRow(
		tags$div(class = "sidebar-box button-box",
			# Red buttons - open and save
			shinyFilesButton(id = "openButton", label = "Open" , title = "Fit Models Data", class = "sidebar-button red-button left-sidebar-button top-sidebar-button", multiple = FALSE, buttonType = "data"),
			shinySaveButton("saveButton", label = "Save", title = "Save Models", class = "sidebar-button red-button right-sidebar-button top-sidebar-button", filetype =  ".Rdata"),

			# Blue buttons - Reset and Quit
			actionButton("resetButton", class = "sidebar-button blue-button left-sidebar-button bottom-sidebar-button", label = "Reset"),
			actionButton("quitButton", class = "sidebar-button blue-button right-sidebar-button bottom-sidebar-button", label = "Quit")
		)
	),

	# --- Data Box ---
	fluidRow(
		tags$div(class = "sidebar-box data-box",
			 ## Headline
			 tags$h2("Data"),
			 shinyFilesButton(id = "hydroData", label = "Hydrometric Data" , title = "Hydrometric Data:", multiple = FALSE, buttonType = "data", class = NULL),
			 textOutput("hydroFile"),
			 shinyFilesButton(id = "stationData", label = "Station Data" , title = "Station Data:", multiple = FALSE, buttonType = "data", class = NULL),
			 textOutput("stationFile")
		)
	),

	# --- Options Box ---
	fluidRow(
		tags$div(class = "sidebar-box options-box",
						 ## Headline
						 tags$h2("Options"),
						 # Confidence Level - Corresponds to argument `level` in `FloodnetAmax`, `FloodnetPOT` and `FloodnetPool`
						 numericInput(inputId = 'confidenceLevel', label = "Confidence Level", value = 0.95, min = 0, max = 1, step = NA, width = NULL),
						 # Simulations - size of bootstrap sample - Corresponds to argument `nsim` in `FloodnetAmax`, `FloodnetPOT` and `FloodnetPool`
						 numericInput(inputId = 'simulations', label = "Simulations", value = 1000, min = 1, max = NA, step = 1, width = NULL),
						 # Heterogeneity - Corresponds to argument `tol.H` in `FloodnetPool`
						 numericInput(inputId = 'heterogeneity', label = "Heterogeneity", value = 2, min = 0, max = NA, step = NA, width = NULL),
						 # Pooling group - Corresponds to argument `size` in `AmaxData`, `DailyData` and `DailyPeaksData`
						 numericInput(inputId = 'pool', label = "Pooling Group", value = 25, min = 0, max = NA, step = 1, width = NULL),
						 # Intersite Correlation  - Corresponds to argument `corr` in `FloodnetPool`
						 numericInput(inputId = 'intersite', label = "Intersite Correlation", value = 0, min = -1, max = 1, step = NA, width = NULL),
						 # Graphical theme (Character): Possibility to chose a theme for the graphical output. See `ggplot2::ggtheme`. To be discussed.
						 selectInput(inputId = "theme", label = "Graphical Theme",
						 					 choices = list("Light" = "light"
						 					 							 #"POT" = "pot",
						 					 							 #"RFA AMAX" = "rfaAmax",
						 					 							 #"RFA POT" = "rfaPot"
						 					 ), selected = "light")
		)
	)
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
					 				 				 textInput("mID", label = h3("Model ID"),
					 				 				 					placeholder = "Enter Unique Name for Model..."),
					 				 				 textInput("station", label = h3("Target Site"),
					 				 				 					placeholder = "Enter Station ID..."),
					 				 				 textInput("periodString", label = h3("Return Period"),
					 				 				 					value = "2, 5, 10, 20, 50, 100")

					 				 ),
					 				 ## Right side --------------------------------
					 				 tags$div(class = "right-item",
					 				 				 selectInput("method", label = h3("Method"),
					 				 				 						choices = list("AMAX" = "amax",
					 				 				 													 "POT" = "pot",
					 				 				 													 "RFA AMAX" = "rfaAmax",
					 				 				 													 "RFA POT" = "rfaPot"
					 				 				 						), selected = "amax"),

					 				 				 conditionalPanel(condition = "input.method == 'rfaAmax' || input.method == 'rfaPot'",
					 				 				 								 selectInput("supReg", label = h3("Super Region"),
					 				 				 								 						choices = list("HC6" = "supreg_hc6",
					 				 				 								 													 "HC12" = "supreg_hc12",
					 				 				 								 													 "KM6" = "supreg_km6",
					 				 				 								 													 "KM12" = "supreg_km12"
					 				 				 								 													 ), selected = "KM12")
					 				 				 ),

					 				 				 ## The option to select the distribution method is only available for AMAX
					 				 				 ## Therefore this selectInput is hidden for POT
					 				 				 conditionalPanel(condition = "input.method == 'amax' || input.method == 'rfaAmax'",
					 				 				 								 # disthresh used instead of seperate distr and thresh ... otherwise cannot merge in table
					 				 				 								 selectInput("disthresh", label = h3("Distribution"),
					 				 				 								 						choices = list("Auto" =  "Default",
					 				 				 								 													 "gev" = "gev",
					 				 				 								 													 "glo" = "glo",
					 				 				 								 													 "gno" = "gno",
					 				 				 								 													 "pe3" = "pe3"
					 				 				 								 						), selected = "Auto")
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
	## --- Back Button ---
	actionButton("backButton", class = "back-button blue-button", label = "← Back"),

	## --- Model Selector ---
	tags$div(class = "sidebar-box model-box",
					 selectInput("modelSelect", label = "Display Model", choices = NULL, selected = NULL)
	),

	## --- Export Settings ---
	tags$div(class = "sidebar-box export-box",
					 # Options to export
					 checkboxGroupInput("exportPlots", label = h2("Export Settings"),
					 									 choices = list("Flood Quantiles (PDF)" = "quantilesPdf",
					 									 							 "Flood Quantiles (CSV)" = "quantilesCsv",
					 									 							 "Return Level Plot" = "returnPlot",
					 									 							 "Condifence Intervals" = "intervalsPlot",
					 									 							 "Coefficient of Variations" = "variationsPlot",
					 									 							 "Histogram" = "histogramPlot",
					 									 							 "L-Moment Ratio Diagram" = 'lMomentPlot',
					 									 							 #"Coordinates" = "coordinates",
					 									 							 "Seasonal Space" = "seasonalPlot",
					 									 							 "Descriptor Space" = "descriptorPlot")
					 									 ),
					 shinySaveButton("exportButton", label = "Export", title = "Export Plots", class = "sidebar-button red-button bottom-sidebar-button", filetype = ".pdf")
					 )
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
					 				 id = "lMomentBox",
					 				 h2("L-Moment Ratio Diagram"),
					 				 # imageOutput("loading"),
					 				 plotOutput("lMomentPlot")
					 ))
	),

	fluidRow( ## 4th row graphics
		# ## -- Coordinates box --
		# column(5,
		# 			 tags$div(class = "background-box fixed-height",
		# 			 				 h2("Coordinates"),
		# 			 				 # imageOutput("loading"),
		# 			 				 plotOutput("coordinatesPlot")
		# 			 )),

		## -- Seasonal Space box --
		column(5,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Seasonal Space"),
					 				 # imageOutput("loading"),
					 				 plotOutput("seasonalPlot")
					 )
		),
		## -- Descriptor Space box --
		column(5, offset = 1,
					 tags$div(class = "background-box fixed-height",
					 				 h2("Descriptor Space"),
					 				 # imageOutput("loading"),
					 				 plotOutput("descriptorPlot")
					 ))
	),
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

	# Initialize various reactive variables (lists mostly)
	# Add fitted model to list
	values <- reactiveValues() # Found a similar solution on stackoverflow (23236944), but do we need all of values just for df?
	#values$df <- data.frame(Column1 = NA, Column2 = NA, Column3 = NA) # Column names need to be the same else match.names error .. only needed to initialize table
	resultList <- reactiveValues() # Store each result here with the "key" being the unique identifier mID
#	resultListKeys <- reactive
	PLOTHEIGHT <- 327 #Constant value - Height for plots
	db_hydat <- "" #Initialize DB and GUAGED as empty strings, so they can be checked before fitting model
	gaugedSites <- ""
	spacePlots <- reactiveValues()
	savePath <- "NA"
	exportPath <- "NA"
	mListMIDs <- reactiveVal()
	mListMIDsCopy <- reactiveVal()

	# --- ShinyFiles File Selection ---
	volumes <- getVolumes()
	shinyFileChoose(input,'hydroData', roots=volumes, filetypes = c('csv', 'sqlite3'))
	observeEvent(input$hydroData, {
		output$hydroFile <- renderText(as.character(parseFilePaths(volumes,input$hydroData)$datapath)) # Display the hydro data file loaded
	})
	shinyFileChoose(input,'stationData', roots=volumes, filetypes = c('csv'))
	observeEvent(input$stationData, {
		output$stationFile <- renderText(as.character(parseFilePaths(volumes,input$stationData)$datapath)) # Display the station data file loaded
	})
#	shinyFileSave(input, "saveButton", roots=volumes, session = session)
	shinyFileChoose(input, "openButton", roots=volumes, filetypes = c('Rdata'))

	# --- Save Button Functions ---
	observe({
		shinyFileSave(input, "saveButton", roots=volumes, session=session)
		savePath <- parseSavePath(volumes, input$saveButton) #get path for file
		#if (length(resultList) == 0) { -- I cannot find a way to get this to work.. resultList doesn't seem to actually delete entries when set to null
	#		showNotification("No fitted models to save. Please Fit a model before saving.", type = "warning")
	#	} else {
		if (nrow(savePath) > 0) {
			savedValues <- values
			savedResultList <- resultList
			save(savedValues, savedResultList, file = savePath$datapath)
		}
#		}
	})

	# --- Load Rdata File ---
	observeEvent(input$openButton,{ # observeEvent needed over observe so that values/resultList can be updated
		loadPath <- parseFilePaths(volumes, input$openButton) #get path for file
		if (nrow(loadPath) > 0) {
			load(loadPath$datapath)
			# Remove values in resultList first, since we can't just set resultList <- NULL
			for (eachResult in (values$df[1])){ #gives "list"(integer..) of model IDs
				for (eachName in as.character(eachResult)){
					resultList[[eachName]] <- NULL
				}
			}

			# Now that resultList is cleaned up, we can repopulate it from the saved list
			for (eachResult in (savedValues$df[1])){ #gives "list"(integer..) of model IDs
				for (eachName in as.character(eachResult)){
					resultList[[eachName]] <- savedResultList[[eachName]]
				}
			}

			values$df <- savedValues$df # Finally, overwrite values with the new savedValues
		}
	})

# 	observeEvent(input$saveButton, {
# 		savePath <- as.character(parseSavePath(volumes, input$saveButton)) #get path for file
# 	})
#
# 	# -- Saving Data to File --
#   observeEvent(savePath, {
#   	print(savePath)
#   	# if (length(resultList) == 0) {
#   	# 	showNotification("No fitted models to save. Please Fit a model before saving.", type = "warning")
#   	# } else {
#   	# 	if (nrow(savePath) > 0) {
#   	# 		save(values, resultList, file = savePath$datapath)
#   	# 	}
#   	# }
#   }
#   )



	# Making eventReactive so table/plot updates with button instead of automatically
	# Storing values in result so each function is only run once
	# result <- shiny::eventReactive(input$fitModel, # Checks to see if ID isn't already in list or blank
	# 															 if ( is.null(resultList[[input$mID]]) & (input$mID != "")) {floodnetRfa::.ClickUpdate(input, db = DB_HYDAT)}
	# 															 else {reactiveValuesToList(resultList)[[input$mID]]})

	observeEvent(input$fitModel, {
		# Check that fields are filled in
		if ((input$mID != "") & (input$station != "") & (input$periodString != "")) {

		# Check that DB_HYDAT and GAUGEDSITES are selected - they are initialized as integer, but once files selected become lists!
		if (typeof(input$hydroData) == "list" & typeof(input$stationData) == "list"){

		# Check that this model ID hasn't already been used
		if ( is.null(resultList[[input$mID]]) ) {

			# Setting DB_HYDAT when file selected with Hydrometric Data button
			db_hydat <- as.character(parseFilePaths(volumes,input$hydroData)$datapath)
			# Setting secondary Station Data filepath (I believe GAUGEDSITES is the correct name for this one? Or should it be DESCRIPTORS? Or something else entirely?)
			gaugedSites <- read.csv(as.character(parseFilePaths(volumes,input$stationData)$datapath))

			result <- floodnetRfa::.ClickUpdate(input, db = db_hydat, gaugedSites)

			# When a model is fit, a new line is made for the Fitted Models datatable and contains the model info
			if (input$method == "amax" || input$method == "pot") { #need to create NA for superregion for non-RFA methods
				values$supReg <- "N/A"   ## Can't modify input... have to create new reactive value
			} else {
				values$supReg <- input$supReg # Store input supreg in same reactive value as non-rfa, so it can be used together in DT
			}
			newLine <- isolate(cbind.data.frame(input$mID, input$station, input$periodString, input$method, input$disthresh, values$supReg))
			isolate(values$df <- rbind.data.frame(values$df, newLine))


			# store result in resultList
			resultList[[input$mID]] <- result
#			resultListKeys <- c(resultListKeys, input$mID) #add unique ID to list of keys... right now used to check if there are any
			#resultList[[input$mID]] <- floodnetRfa::.ClickUpdate(input, db = DB_HYDAT)()

			# Reset text box
			updateTextInput(session, "mID", value="")

			# output functions to table/plot
			output$table <- renderDT(
				as.data.frame(result), options = list(
					pageLength = 6,
					scrollX = TRUE
					#paging = FALSE #FALSE = becomes one long list instead of multiple properly-sized lists
				)
			)
			output$plot <- shiny::renderPlot(plot(result) + ggplot2::ggtitle(isolate(input$station)), height = PLOTHEIGHT ) #327 height leaves 20px bottom margin - same as buttons

		} #end of Check that this model ID hasn't already been used
			else {
				showNotification("Model ID has already been used. Please enter a unique Model ID.", type = "warning")
		}} #end of check for data selected
			else {
				showNotification("Please select files for Hydrometric Data and Station Data before fitting a model.", type = "warning")
		}} #end of  Check that fields are filled in
			else {
				showNotification("One or more fields are blank. Please ensure Model ID, Target Site, and Return Period are filled in.", type = "warning")
	}})




	# List of Fitted Models
	# observe(
	output$modelsTable <- renderDT(
		values$df,
		colnames = c("Model ID", "Site", "Period", "Method", "Distribution/Threshold", "Super Region"),
		options = list(
			pageLength = 4,
			scrollX = TRUE
		)
	)
	# )

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
			# Remove models from resultList
			for (i in selectedRows) {
				modelName <- as.character(values$df[i,"input$mID"])  #read values list #as.character() was needed!!!
				resultList[[modelName]] <- NULL
			#	resultListKeys[[modelName]] <- NULL
			}

			values$df <- values$df[-as.numeric(selectedRows),]
		} else {
			showNotification("Please select a model from the list first.", type = "warning")
		}
	})


	# When "Show" button pressed to compare models in table
	observeEvent(input$showButton, {

		# Initialize list of models to be shown in graphics page
		mList <- reactiveValues()  # Needs to be re-initialized in this loop else problems will occur with multiple "Show" attempts

		# Re-grab gaugedSites since it is local to Fit button... (it could have changed too, and maybe show will be used without Fit button on a load, so good to do anyways)
		gaugedSites <- read.csv(as.character(parseFilePaths(volumes,input$stationData)$datapath))

		# get selected rows
		selectedRows <- input$modelsTable_rows_selected

		if (length(selectedRows) == 0) {
			showNotification("Please select a model from the list first.", type = "warning")
			return()
		}

		for (i in selectedRows) {
			# Load results for each mID for each selected row into mList
			modelName <- as.character(values$df[i,"input$mID"])  #read values list #as.character() was needed!!!

			mList[[modelName]] <- resultList[[modelName]]
			mListMIDs <- c(mListMIDs, modelName) #add modelName to list - for model selection list
		}

		# --- Generate selectInput for list of models, select 1st from list to display by default

		mListMIDs <- mListMIDs[-1]
		modelName <- as.character(mListMIDs[1])

		mListMIDsCopy <<- mListMIDs #I have no idea why mListMIDs is innacessible outside of this observeEvent, but this lets us see it elsewhere..

		updateSelectInput(session = session, inputId = "modelSelect", label = "Display Model", choices = mListMIDs,
											selected = modelName)

		# Switch view to Graphics tab
		updateTabsetPanel(session, inputId = "pageId", selected = "Graphics")
	}) ## End of Show Button

	# ----- End of Models Page -----
	# ----- Start of Graphics Page -----

	observeEvent(input$backButton, {
	# Switch view to Models tab
	updateTabsetPanel(session, inputId = "pageId", selected = "Models")
	})

 # Update plots when model is selected
	observeEvent(input$modelSelect, {
		siteList <- c()
		if(input$modelSelect != "") {
			gaugedSites <- read.csv(as.character(parseFilePaths(volumes,input$stationData)$datapath))  # Needs to be re-initialized here
			mList <- reactiveValues()  # Needs to be re-initialized here
			 for (eachModel in mListMIDsCopy) {
				 	mList[[eachModel]] <- resultList[[eachModel]]
				 	if ((mList[[eachModel]][1]$site %in% siteList) == FALSE) {siteList <- c(siteList, mList[[eachModel]][1]$site)} # Making list of each station in comparison
				 #	print(mList[[eachModel]][1]$site)
			 }

			# --- generate plots for first model in list ---
			# Need modelName of 1st to display (for some reason $mID is part of copied result list)
			resultGraphics <- reactiveValuesToList(resultList)[[input$modelSelect]] # grab the model selected by modelSelect

			# Create a compareModels list with each selected model from the table (for comparative plots)
			lst.fit <- do.call(floodnetRfa::CompareModels, reactiveValuesToList(mList)) # compare all models in mList

			# --- Plot result ---
			# Flood quantiles
			output$graphicsQuantiles <- renderDT(
				as.data.frame(resultGraphics), options = list(
					pageLength = 6,
					# autoWidth = TRUE,
					# columnDefs = list(list(width = '10', visible = TRUE, targets = "_all")),
					scrollX = TRUE
					#paging = FALSE #FALSE -= becomes one long list instead of multiple properly-sized lists
				)
			)
			# Return level plot
			output$graphicsReturnPlot <- shiny::renderPlot(plot(resultGraphics), height = PLOTHEIGHT)
			# Confidence intervals plot
			output$confIntervals <- shiny::renderPlot(plot(lst.fit), height = PLOTHEIGHT)
			# Coefficient of variation plot
			output$ceoffVariation <- shiny::renderPlot(plot(lst.fit, 'cv'), height = PLOTHEIGHT)
			# Histogram
			output$histogram <- shiny::renderPlot(hist(resultGraphics, histogram.args = list( bins = 15)), height = PLOTHEIGHT)

			#print(mList[[input$modelSelect]][1])

			# L-Moment Ratio Diagram --- only display when model is RFA AMAX
			if (mList[[input$modelSelect]][2]$method == "pool_amax") {
				shinyjs::show("lMomentBox")
				output$lMomentPlot <- shiny::renderPlot(plot(resultGraphics, 'l'), height = PLOTHEIGHT)
			} else {
				shinyjs::hide("lMomentBox")
			}

			# Space diagrams --- may be best to make another helper function like ClickUpdate to make these - check vignette pdf for help
			spacePlots <- floodnetRfa::.spacePlots(gaugedSites, siteList)
			# ## Geographical Space
			# output$coordinatesPlot <-shiny::renderPlot(spacePlots$coordinates, height = PLOTHEIGHT)
			## Seasonal space
			output$descriptorPlot <-shiny::renderPlot(spacePlots$descriptor, height = PLOTHEIGHT)
			## Descriptor space
			output$seasonalPlot <-shiny::renderPlot(spacePlots$seasonal, height = PLOTHEIGHT)


		} # END of Display Model select-actions
	})

	# --- Export Button Functions ---
	observe({
		shinyFileSave(input, "exportButton", roots=volumes, session=session)
		exportPath <- parseSavePath(volumes, input$exportButton) #get path for file
		isolate( #isolating everything so updating tick-box after selecting save path doesn't end up rewriting pdf
			if (nrow(exportPath) > 0) {

						siteList <- c()
						gaugedSites <- read.csv(as.character(parseFilePaths(volumes,isolate(input$stationData))$datapath))  # Needs to be re-initialized here
						mList <- reactiveValues()  # Needs to be re-initialized here
						for (eachModel in mListMIDsCopy) { #get list of models into local memory
							mList[[eachModel]] <- resultList[[eachModel]]
							if ((mList[[eachModel]][1]$site %in% siteList) == FALSE) {siteList <- c(siteList, mList[[eachModel]][1]$site)} # Making list of each station in comparison
						}

						pdf(file = exportPath$datapath) #open pdf

						# --- Plots for individual models ---
						for (eachModel in mListMIDsCopy) { #get list of models into local memory
							resultGraphics <- reactiveValuesToList(resultList)[[eachModel]] #get the result for eachModel from resultList

							# --- CSV Output ---
							csvFile <- paste(substring(exportPath$datapath,1,nchar(exportPath$datapath)-4), eachModel, sep = "_") #Make unique name for each model, extract .pdf out of name
							csvFile <- paste(csvFile, ".csv", sep = "") #Add .csv to end
							print(csvFile)
							write.csv(as.data.frame(resultGraphics), file = csvFile)

							modelTitle <- paste(eachModel, mList[[eachModel]][1]$site, mList[[eachModel]][2]$method,  sep = " - ")
							print(modelTitle) #Print ID of model as a title for the model-section .. any way to do this like a title in pdf?

							#quantilesPdf #quantilesCsv -- do seperate?
							if ("quantilesPdf" %in% input$exportPlots) {
								plot.new()
								print(gridExtra::grid.table(as.data.frame(resultGraphics)))
							}

							#returnPlot
							if ("returnPlot" %in% input$exportPlots) {
								print(plot(resultGraphics) + ggplot2::ggtitle(paste("Return Levels: ", modelTitle)))
							}


							#histogramPlot
							if ("histogramPlot" %in% input$exportPlots) {
								print(hist(resultGraphics, histogram.args = list( bins = 15)) + ggplot2::ggtitle(paste("Histogram (better name for this?): ",modelTitle)))
							}


							#lMomentPlot (check if method == "pool_amax")
							if ("lMomentPlot" %in% input$exportPlots) {
								if (mList[[eachModel]][2]$method == "pool_amax") {
								print(plot(resultGraphics, 'l') + ggplot2::ggtitle(paste("L-Moment Ratio Diagram: ",modelTitle)))
							}
							}


						} #end of individual plots section

						# --- Group plots ---
						# Create a compareModels list with each selected model from the table (for comparative plots)
						lst.fit <- do.call(floodnetRfa::CompareModels, reactiveValuesToList(mList)) # compare all models in mList
						spacePlots <- floodnetRfa::.spacePlots(gaugedSites, siteList)

						if ("intervalsPlot" %in% input$exportPlots) {
							print(plot(lst.fit) + ggplot2::ggtitle("Confidence Intervals"))
						}

						if ("variationsPlot" %in% input$exportPlots) {
							print(plot(lst.fit, 'cv') + ggplot2::ggtitle("Coefficients of Variation"))
						}

						if ("coordinates" %in% input$exportPlots) {
							print(spacePlots$coordinates + ggplot2::ggtitle("Coordinates of Stations"))
						}

						if ("seasonalPlot" %in% input$exportPlots) {
							print(spacePlots$seasonal + ggplot2::ggtitle("Seasonal Space"))
						}

						if ("descriptorPlot" %in% input$exportPlots) {
							print(spacePlots$descriptor + ggplot2::ggtitle("Descriptor Space"))
						}


						dev.off() # End pdf-printing session

						# NEED some way to stop printing pdf once complete... otherwise any changes in boxes will change what's been printed to pdf when button pressed
						# will something like this work? exportPath <- NULL ??
					}
		)

		#		}
	}) # END of Export Button Functions
}

# Run the application
shinyApp(ui = ui, server = server)
