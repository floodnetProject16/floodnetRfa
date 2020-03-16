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

						fluidRow(
							column(5,
								## -- Start of Model Configuration box --
								tags$div(class = "background-box",
									tags$div(class = "left-item",
										textInput("station", label = h3("Target Site"),
															 placeholder = "Enter Station ID..."),
										textInput("returnPeriod", label = h3("Return Period"),
															 value = "2, 5, 10, 20, 50, 100"),
										selectInput("method", label = h3("Method"),
																choices = list("AMAX" = "amax",
																							 "POT" = "pot",
																							 "RFA AMAX" = "rfaAmax",
																							 "RFA POT" = "rfaPot"
																), selected = "amax")
									),
									tags$div(class = "right-item",
										selectInput("superRegion", label = h3("Super Region"),
															 choices = list("Province" = "province",
															 							  "User-Made" = "userMade"),
															 							  selected = "Province")
									)
								),
								## -- Start of Fitted Models box --
								tags$div(class = "background-box",
									h2("Fitted Models")

								)
							)



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

	tags$p(class = "bodyText", "Graphics Tab")
)

ui <- navbarPage("FloodNet RFA",
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





# UI Section ---------------------------------------------------------------
# ui <- navbarPage("FloodNet RFA",
#
# 								 #Tab 1: Models
# 								 tabPanel("Models",
# 								 # Header ---------------------------------------
# 								 dashboardHeader(disable = TRUE,
# 								 	#For some reason, color styling had to be done like this and not in css
# 								 	title = tags$div(tags$span(id = "floodnetText", "FloodNet"),
# 								 									 tags$span(id = "rfaText", "RFA"))
# 								 ),
#
# 								 # Sidebar --------------------------------------
# 								 dashboardSidebar(
#
#
# 								 ),
#
# 								 # Body -----------------------------------------
# 								 dashboardBody(
# 								 	#Use custom css
# 								 	tags$head(
# 								 		tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css"),
# 								 		tags$style(class = "body")
# 								 	),
#
# 								 	tags$p(class = "bodyText", "FloodNet RFA")
# 								 )
# 								 ),
#
# 								 #Tab 2: Graphics
# 								 tabPanel("Graphics",
# 								 				 dashboardPage(
#
# 								 				 	# Header ---------------------------------------
# 								 				 	dashboardHeader(
# 								 				 		#For some reason, color styling had to be done like this and not in css
# 								 				 		title = tags$div(tags$span(id = "floodnetText", "FloodNet"),
# 								 				 										 tags$span(id = "rfaText", "RFA"))
# 								 				 	),
#
# 								 				 	# Sidebar --------------------------------------
# 								 				 	dashboardSidebar(
#
#
# 								 				 	),
#
# 								 				 	# Body -----------------------------------------
# 								 				 	dashboardBody(
# 								 				 		#Use custom css
# 								 				 		tags$head(
# 								 				 			tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css"),
# 								 				 			tags$style(class = "body")
# 								 				 		),
#
# 								 				 		tags$p(class = "bodyText", "FloodNet RFA")
# 								 				 	)
# 								 				 )
#
#
# )
# )

# SERVER Function ------------------------------------------------------------------------
server <- function(input, output) {

	# Making eventReactive so table/plot updates with button instead of automatically
	# Storing values in result so each function is only run once
	result <- shiny::eventReactive(input$update, .ClickUpdate(input, db = DB_HYDAT))
    # output functions to table/plot
    output$table <- renderTable(as.data.frame(result()))
    output$plot <- renderPlot(plot(result()) )

}

# Run the application
shinyApp(ui = ui, server = server)
