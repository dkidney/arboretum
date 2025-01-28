library(shiny)
library(arboretum)

# do you really need to track input collapse choices?

options(shiny.reactlog = TRUE)

cat('pre-lauch code start ------------------------------------------------------\n')
taxon = 'archosauria'
# taxon = 'pterosauria'
df = arboretum:::load_tree_data()
arboretum:::tree_data_summary(df)
node_taxa = arboretum:::get_nodes(df)$taxon
df = arboretum:::subset_taxon(df, taxon)
default_collapsed = arboretum:::get_default_collapsed(df)
df <- arboretum:::collapse_taxa(df, default_collapsed)
arboretum:::tree_data_summary(df)
default_uncollapsed = arboretum:::get_nodes(df, ignore_roots = TRUE)$taxon

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
			width = 9,
			selectInput(
				inputId = 'collapse',
				label = 'collapse',
				choices = input_collapse_choices_default,
				selected = input_collapse_selected_default,
				# choices = '',
				multiple = TRUE,
				width = '100%'
			)
			# verbatimTextOutput("collapse_check")
			# ),
			# column(
			# 	width = 1,
			# 	actionButton(
			# 		inputId = 'expand',
			# 		label = 'expand all'
			# 	)
			# )
		)
	),
	fluidRow(
		plotOutput(
			outputId = 'tree'
		)
		# ),
		# fluidRow(
		# 	DT::DTOutput(
		# 		outputId="tree_data"
		# 	)
	)
)

server <- function(input, output, session) {

	# tree_data = reactiveVal()

	vals = reactiveValues(
		taxon_selected    = input_taxon_selected_default,
		collapse_choices  = input_collapse_choices_default,
		collapse_selected = input_collapse_selected_default,
		df = NULL
	)

	# observeEvent(
	# 	{input$expand},
	# 	{vals$collapse = NULL}
	# )

	# dummy <- eventReactive(
	# observeEvent(
	# 	{
	# 		input$taxon
	# 		input$collapse # didn't seem to react when this changed to NULL...
	# 	},
	observe(
		{

			taxon_updated = !(length(input$taxon) == length(vals$taxon_selected) &&
							  	input$taxon == vals$taxon_selected)

			collapse_updated = !(length(input$collapse) == length(vals$collapse_selected) &&
								 	all(input$collapse %in% vals$collapse_selected))

			# cat('\n')
			# cat('input$taxon:', input$taxon, '\n')
			# cat('vals$taxon_selected:', vals$taxon_selected, '\n')
			# cat('taxon_updated: ', taxon_updated, '\n')
			# cat('\n')
			# cat('input$collapse:', input$collapse, '\n')
			# cat('vals$collapse_selected:', vals$collapse_selected, '\n')
			# cat('collapse_updated: ', collapse_updated, '\n')
			# cat('\n')
			# cat('vals$collapse_choices:', vals$collapse_choices)

			# vals$taxon_selected
			# vals$collapse_selected
			# vals$collapse_choices
			# vals$tree_data

			# if (is.null(input$collapse)) {
			# 	browser()
			# } else if (is.na(input$collapse) || input$collapse == '') {
			# 	browser()
			# }

			if (is.null(vals$tree_data) | taxon_updated | collapse_updated) {

				# generate tree data
				df = arboretum:::load_tree_data()
				# arboretum:::tree_data_summary(df)
				df = arboretum:::subset_taxon(df, input$taxon)
				collapsed = input$collapse
				if (taxon_updated) {
					collapsed = arboretum:::get_default_collapsed(df)
				}
				df <- arboretum:::collapse_taxa(df, collapsed)
				# arboretum:::tree_data_summary(df)
				if (taxon_updated) {
					vals$taxon_selected = input$taxon
				}

				# update select input for collapse
				if (taxon_updated | collapse_updated) {
					uncollapsed = get_nodes(df, ignore_roots = TRUE)$taxon
					vals$collapse_selected = collapsed
					vals$collapse_choices = c(collapsed, uncollapsed)
					freezeReactiveValue(input, "collapse")
					updateSelectInput(
						inputId = 'collapse',
						selected = vals$collapse_selected,
						choices = vals$collapse_choices
					)
				}

				vals$tree_data = df
			}
		})

	# output$taxon_check <- renderText(input$taxon)
	#
	# output$collapse_check <- renderText(input$collapse)

	# output$tree_data <- DT::renderDT(head(vals$tree_data), options = list(pageLength = 5))

	output$tree <- renderPlot(
		{
			arboretum:::plot_tree_data(
				req(vals$tree_data),
				max_tips = 100
			)
		},
		width = 1500,
		height = 850,
		res = 96
	)



	# observe(
	# 	{
	# 		df = arboretum:::load_tree_data()
	# 		df = arboretum:::subset_taxon(df, input$taxon)
	# 		if (!is.null(input$collapse)) {
	# 			df <- arboretum:::collapse_taxa(df, input$collapse)
	# 		}
	# 		arboretum:::tree_data_summary(df)
	# 		tree_data(df)
	#
	# 		freezeReactiveValue(input, "collapse")
	# 		updateSelectInput(
	# 			inputId = 'collapse',
	# 			selected = collapse_selected,
	# 			choices = collapse_choices
	# 		)
	# 	}
	# )

	# observeEvent(
	# 	input$collapse,
	# 	{
	#
	# 		df = isolate(tree_data())
	# 		# collapsable = arboretum:::get_collapsable_taxa(df, isolate(input$taxon))
	# 		# collapsed = arboretum:::get_default_collapsed(df, collapsable)
	# 		df <- arboretum:::collapse_taxa(df, input$collapse)
	# 		arboretum:::tree_data_summary(df)
	# 		uncollapsed = get_nodes(df, ignore_roots = TRUE)$taxon
	# 		collapse_selected = collapsed
	# 		collapse_choices = c(collapsed, uncollapsed)
	# 		tree_data(df)
	#
	# 		freezeReactiveValue(input, "collapse")
	# 		updateSelectInput(
	# 			inputId = 'collapse',
	# 			selected = collapse_selected,
	# 			choices = collapse_choices
	# 		)
	# 	})

	# 	updateSelectInput(
	# 		inputId = 'collapse',
	# 		choices = collapse_choices,
	# 		selected = collapse_selected
	# 	)

	# # observe({
	# tree_data = eventReactive({
	# 	input$taxon
	# 	input$collapse
	# },
	# {
	# 	print('input$taxon has been updated')
	# 	print('input$taxon:')
	# 	print(isolate(input$taxon))
	# 	print('input$collapse:')
	# 	print(isolate(input$collapse))
	#
	# 	taxon <- input$taxon
	# 	df <- arboretum:::load_tree_data()
	# 	df <- arboretum:::subset_taxon(df, taxon)
	#
	# 	if (is.null(isolate(input$collapse))) {
	#
	# 		print('using default collapse')
	# 		collapse_choices <- arboretum:::get_collapsable_taxa(df, taxon)
	# 		collapse_selected <- arboretum:::get_default_collapsed(df, collapse_choices)
	# 		df <- arboretum:::collapse_taxa(df, collapse_selected)
	#
	# 	} else {
	#
	# 		print('using selected collapse')
	# 		# e.g. if saurpodomorpha is collapsed, then sauropoda shouldn't be in choices
	# 		df <- arboretum:::collapse_taxa(df, isolate(input$collapse))
	# 		collapse_choices <- get_nodes(df)$taxon
	# 		collapse_selected = isolate(input$collapse)
	#
	# 	}
	# 	# freezeReactiveValue(input, "collapse")
	# 	print('input$collapse choices:')
	# 	print(collapse_choices)
	# 	print('input$collapse selected:')
	# 	print(collapse_selected)
	# 	print('updating input$collapse')
	# 	updateSelectInput(
	# 		inputId = 'collapse',
	# 		choices = collapse_choices,
	# 		selected = collapse_selected
	# 	)
	# 	print(head(df))
	# 	return(df)
	# 	# print(isolate(input$taxon))
	# 	# print(isolate(input$collapse))
	# })
	#
	# # observeEvent(input$collapse, {
	# # 	print('input$collapse has been updated')
	# #
	# # 	print(isolate(input$taxon))
	# # 	print(isolate(input$collapse))
	# # 	taxon <- input$taxon
	# # 	df <- arboretum:::load_tree_data()
	# # 	df <- arboretum:::subset_taxon(df, taxon)
	# # 	df <- arboretum:::collapse_taxa(df, input$collapse)
	# # 	collapse_choices <- get_nodes(df)$taxon
	# # 	# freezeReactiveValue(input, "collapse")
	# # 	print('updating input$collapse choices')
	# # 	updateSelectInput(
	# # 		inputId = 'collapse',
	# # 		choices = collapse_choices,
	# # 		selected = isolate(input$collapse)
	# # 	)
	# # 	print(isolate(input$taxon))
	# # 	print(isolate(input$collapse))
	# # })
	#
	# output$tree <- renderPlot(
	# 	{
	# 		req(tree_data())
	# 		print('rendering plot')
	# 		print('a')
	# 		print(isolate(nrow(tree_data())))
	# 		print('b')
	# 		arboretum:::plot_tree_data(
	# 			tree_data(),
	# 			max_tips = 100
	# 		)
	# 		print('c')
	# 		# tree(
	# 		# 	taxon=req(input$taxon),
	# 		# 	collapse=input$collapse,
	# 		# 	max_tips = 100
	# 		# )
	# 	},
	# 	width = 1500,
	# 	height = 850,
	# 	res = 96
	# )
	#
}

shinyApp(ui, server)
