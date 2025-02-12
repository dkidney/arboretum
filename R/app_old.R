#' shiny app
#' @param taxon TODO
#' @import shiny
#' @export
tree_app_old <- function(taxon='tetrapodomorpha') {

	df = load_tree_data()
	input_taxon_choices = df$taxon[!df$is_tip]
	input_taxon_selected = if (missing(taxon)) df$taxon[df$is_root] else taxon

	ui <- bslib::page_sidebar(
		# theme = bslib::bs_theme(bootswatch = "cerulean"), # baby blue
		# theme = bslib::bs_theme(bootswatch = "cosmo"), # light sky blue
		# theme = bslib::bs_theme(bootswatch = "flatly"), # grey
		theme = bslib::bs_theme(bootswatch = "spacelab"), # grey blue
		# theme = bslib::bs_theme(bootswatch = "yeti"), # baby blue with a hint of green?
		# theme = bslib::bs_theme(bootswatch = "zephyr"), # light sky blue

		title = paste("arboretum", utils::packageVersion('arboretum')),

		sidebar = bslib::sidebar(
			bslib::accordion(
				open = c(
					"Configure tree",
					"Taxon info",
					# "Plot settings",
					"Plot summary",
					"About",
					NA_character_
				),
				bslib::accordion_panel(
					"Configure tree",
					selectInput(
						inputId = 'taxon',
						label = 'taxon:',
						selected = input_taxon_selected,
						choices = input_taxon_choices
					),
					actionButton('expand', 'expand all'),
					HTML("<br/>"),
					actionButton('collapse', 'collapse default')
				),
				bslib::accordion_panel(
					"Taxon info",
					htmlOutput(outputId = 'info')
				),
				bslib::accordion_panel(
					"Plot summary",
					verbatimTextOutput(outputId = 'summary'),
				),
				bslib::accordion_panel(
					"Plot settings",
					numericInput("height", "height", min = 100, max = 5000, value = 900, step=50),
					numericInput("width", "width", min = 100, max = 5000, value = 1250, step=50),
					numericInput("xmin", "xmin", min = 0, max = 538.8, value = NA, step=10),
					numericInput("xmax", "xmax", min = 0, max = 538.8, value = 0, step=10),
				),
				bslib::accordion_panel(
					"About",
					HTML('<a href="https://github.com/dkidney/arboretum/blob/main/README.md">Github homepage</a>'),
					HTML("<br/>"),
					textOutput(outputId = 'about'),
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

		# bslib::bs_themer()

		# thematic::thematic_shiny()

		vals = reactiveValues()

		observeEvent(c(input$taxon, input$collapse), {
			vals$df = tree_data(taxon=input$taxon, collapse = 'default')
		})

		observeEvent(input$expand, {
			vals$df = tree_data(taxon=input$taxon, collapse = 'none')
		})

		# collapse
		observeEvent(input$plot_click, {
			# browser()
			df = vals$plot$data
			taxon = closest_taxon(df, input$plot_click)
			if (!is_root(df, taxon)) {
				collapsed = df$taxon[df$is_collapsed]
				if (!taxon %in% collapsed) {
					collapse = c(collapsed, taxon)
					vals$df = tree_data(taxon=input$taxon, collapse = collapse)
				}
			}
		})

		# expand
		observeEvent(input$plot_dblclick, {
			# browser()
			df = vals$plot$data
			taxon = closest_taxon(df, input$plot_dblclick)
			if (!is_root(df, taxon)) {
				collapsed = df$taxon[df$is_collapsed]
				if (taxon %in% collapsed) {
					collapse = collapsed[collapsed != taxon]
					collapse = c(collapse, get_children(load_tree_data(), taxon)$taxon)
					if (length(collapse) == 0) collapse = 'none'
					vals$df = tree_data(taxon=input$taxon, collapse = collapse)
				}
			}
		})

		# observeEvent(vals$df, {
		# 	vals$plot = plot_tree_data(vals$df)
		# })

		# observeEvent(c(vals$df, input$xmin, input$xmax), {
		# 	vals$plot = plot_tree_data(
		# 		vals$df,
		# 		xmin=input$xmin,
		# 		xmax=input$xmax
		# 	)
		# })

		observe({
			vals$plot = plot_tree_data(
				vals$df,
				xmin=input$xmin,
				xmax=input$xmax
			)
		})

		output$plot <- renderPlot({
			vals$plot
		},
		width = function() input$width,
		height = function() input$height,
		res = 115
		)

		output$info <- renderUI(HTML({
			req(input$plot_hover)
			df = isolate(vals$plot$data)
			taxon = closest_taxon(df, input$plot_hover)
			get_info(df, taxon) |> stringr::str_replace_all("\n", "<br/>")
		}))

		output$summary <- renderText({
			req(vals$df)
			print(summary(vals$df))
		})

		output$about = renderText({
			paste("arboretum", utils::packageVersion('arboretum'))
		})

	}

	shinyApp(ui, server)
}

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



