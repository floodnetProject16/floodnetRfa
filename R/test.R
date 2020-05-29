library(shiny)
library(DT)

ui <- basicPage(
	mainPanel(DT::dataTableOutput('mytable')),
	textOutput("selected")
)

server <- function(input, output,session) {

	mydata <- reactive({mtcars})

	output$mytable = DT::renderDataTable(
		datatable(mydata())
	)

	selectedRow <- eventReactive(input$mytable_rows_selected,{
		row.names(mtcars)[c(input$mytable_rows_selected)]
	})

	output$selected <- renderText({
		selectedRow()
	})
}
runApp(list(ui = ui, server = server))
