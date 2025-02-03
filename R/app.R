# library(shiny)
# library(bslib)
# library(ggplot2)

closest_taxon = function(df, input) {
	diff_range_x = abs(input$domain$left - input$domain$right)
	diff_range_y = abs(input$domain$top - input$domain$bottom)
	x <- input$x
	y <- input$y
	dist_x = df$label_x - x
	dist_y = df$y - y
	dist = sqrt((dist_x/diff_range_x)^2 + (dist_y/diff_range_y)^2)
	df$taxon[which.min(dist)]
}

tree_app <- function(taxon) {

	df = load_tree_data()
	input_taxon_choices = df$taxon[!df$is_tip]
	input_taxon_selected = if (missing(taxon)) df$taxon[df$is_root] else taxon

	ui <- bslib::page_sidebar(
		theme = bslib::bs_theme(bootswatch = "spacelab"),

		# title = paste("arboretum", packageVersion('arboretum')),

		sidebar = bslib::sidebar(
			paste("arboretum", packageVersion('arboretum')),
			bslib::accordion(
				open = c("Plot controls", "Info"),
				bslib::accordion_panel(
					"Plot controls",
					selectInput(
						inputId = 'taxon',
						label = 'taxon',
						selected = input_taxon_selected,
						choices = input_taxon_choices
					)
				),
				bslib::accordion_panel(
					"Info",
					htmlOutput(
						# textOutput(
						# verbatimTextOutput(
						outputId = 'info'
					)
				)
			)
		),

		plotOutput(
			outputId = "plot",
			click = "plot_click",
			dblclick = "plot_dblclick",
			hover = "plot_hover"
		),
	)

	server <- function(input, output, session) {

		thematic::thematic_shiny()

		vals = reactiveValues()

		observeEvent(input$taxon, {
			vals$df = tree_data(taxon=input$taxon)
		})

		observeEvent(vals$df, {
			vals$plot = plot_tree_data(vals$df)
		})

		output$plot <- renderPlot({
			vals$plot
		}, res = 96)

		output$info <- renderUI(HTML({
			req(input$plot_hover)
			df = isolate(vals$plot$data)
			taxon = closest_taxon(df, input$plot_hover)
			get_info(df, taxon) |> stringr::str_replace_all("\n", "<br/>")
		}))

	}

	shinyApp(ui, server)
}
