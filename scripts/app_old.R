# library(shiny)
# library(arboretum)
#
# # if collapse selections are all manually deleted the plot doesn't change
#
# options(shiny.reactlog = TRUE)


#' launch the app
#' @param taxon TODO
#' @param collapse TODO
#' @export
#' @import shiny
tree_app_old <- function(taxon, collapse) {

	cat('pre-lauch code start ------------------------------------------------------\n')
	# taxon = 'sauropoda'
	# taxon = 'dinosauria'
	# taxon = 'tetrapodomorpha'
	# taxon = 'archosauria'
	# taxon = 'pterosauria'
	df = load_tree_data()
	if (is.null(taxon)) {
		taxon = get_roots(df)$taxon[1]
	}
	# tree_data_summary(df)
	print(df)
	node_taxa = get_nodes(df)$taxon
	# df = subset_taxon(df, taxon)
	df = subset(df, taxon)
	# default_collapsed = get_default_collapsed(df)
	df <- collapse(df, default_collapsed)
	print(df)
	# tree_data_summary(df)
	# default_uncollapsed = get_nodes(df, ignore_roots = TRUE)$taxon

	# default app settings
	input_taxon_selected_default = taxon
	input_taxon_choices = node_taxa
	input_collapse_selected_default = default_collapsed
	input_collapse_choices_default = c(default_collapsed, default_uncollapsed)

	cat('pre-lauch code end --------------------------------------------------------\n')

	ui <- fluidPage(
		fluidRow(
			column(
				width = 2,
				selectInput(
					inputId = 'taxon',
					label = 'taxon',
					selected = input_taxon_selected_default,
					choices = input_taxon_choices
				)
				# verbatimTextOutput("taxon_check")
			),
			column(
				width = 6,
				selectInput(
					inputId = 'collapse',
					label = 'collapse',
					choices = input_collapse_choices_default,
					selected = input_collapse_selected_default,
					# choices = '',
					multiple = TRUE,
					width = '100%'
					# )
					# verbatimTextOutput("collapse_check")
				)
			),
			column(
				width = 1,
				numericInput("height", "height", min = 100, max = 5000, value = 400, step=50)
				# numericInput("height", "height", min = 100, max = 5000, value = 800, step=50)
			),
			column(
				width = 1,
				numericInput("width", "width", min = 100, max = 5000, value = 1500, step=50)
			),
			column(
				width = 1,
				actionButton(
					inputId = 'expand_all',
					label = 'expand all'
				),
				actionButton(
					inputId = 'collapse_default',
					label = 'collapse default'
				)
			),
			column(
				width = 1,
				verbatimTextOutput(
					outputId = 'summary'
				)
			)
		),
		fluidRow(
			plotOutput(
				outputId = 'tree',
				hover = "tree_hover",
				click = "tree_click",
				fill = TRUE
			)
		),
		fluidRow(
			verbatimTextOutput("click_info"),
			verbatimTextOutput("hover_info")
		)
	)

	server <- function(input, output, session) {

		vals = reactiveValues()

		collapse = function(how) {
			stopifnot(how %in% c('input', 'default', 'none'))

			# load data and collapse taxa
			df = load_tree_data()
			df = subset_taxon(df, input$taxon)
			collapsed = how |> switch(
				'input'   = input$collapse,
				'default' = get_default_collapsed(df),
				'none'    = character(0)
			)
			df <- collapse_taxa(df, collapsed)
			df = arrange_tree(df)
			# uncollapsed = get_nodes(df, ignore_roots = TRUE)$taxon

			# update vals
			vals$collapse_selected = collapsed
			vals$collapse_choices = c(collapsed, uncollapsed)
			vals$taxon_selected = input$taxon
			vals$tree_data = df

			# update collapse selectInput
			freezeReactiveValue(input, "collapse")
			updateSelectInput(
				inputId = 'collapse',
				selected = vals$collapse_selected,
				choices = vals$collapse_choices
			)
		}

		observeEvent(input$expand_all, {
			collapse('none')
		})

		observeEvent(input$collapse_default, {
			collapse('default')
		})

		observeEvent(input$taxon, {
			collapse('default')
		})

		observeEvent(input$collapse, {
			collapse_updated = !(length(input$collapse) == length(vals$collapse_selected) &&
								 	all(input$collapse %in% vals$collapse_selected))
			if(collapse_updated) {
				collapse('input')
			}
		})

		# output$taxon_check <- renderText(input$taxon)
		#
		# output$collapse_check <- renderText(input$collapse)

		# output$tree_data <- DT::renderDT(head(vals$tree_data), options = list(pageLength = 5))

		output$tree <- renderPlot(
			{
				plot_tree_data(
					req(vals$tree_data),
					max_tips = 250
				)
			},
			width = function() input$width,
			height = function() input$height,
			res = 115
		)

		output$click_info <- renderPrint({
			# req(input$tree_click)
			# df = vals$tree_data
			# x <- input$tree_click$x
			# y <- input$tree_click$y
			# dist <- sqrt((df$from - x)^2 + (df$y - y)^2)
			# print(df[which.min(dist), ])
		})

		output$hover_info <- renderPrint({
			req(input$tree_hover)
			df = vals$tree_data
			x <- input$tree_hover$x
			y <- input$tree_hover$y
			dist <- sqrt((df$from - x)^2 + (df$y - y)^2)
			print(df[which.min(dist), ])
		})

		output$summary <- renderText(
			paste('n_tips:',  nrow(get_tips(req(vals$tree_data))))
			# tree_data_summary(req(vals$tree_data))
			# paste(
			# 	paste('n_roots:', nrow(get_roots(req(vals$tree_data)))),
			# 	paste('n_nodes:', nrow(get_nodes(req(vals$tree_data), ignore_roots=TRUE))),
			# 	paste('n_tips :',  nrow(get_tips(req(vals$tree_data)))),
			# 	sep = '\n'
			# )
		)



	}

	shinyApp(ui, server)

}
