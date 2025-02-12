#' shiny app
#' @param taxon TODO
#' @import shiny
#' @export
tree_app <- function(taxon='tetrapodomorpha', collapse=NULL, ...) {
	#
	df = load_tree_data()
	input_taxon_choices = df$taxon[!df$is_tip]
	input_taxon_selected = if (missing(taxon)) df$taxon[df$is_root] else taxon

	xmin = if(exists('xmin')) xmin else NA
	xmax = if(exists('xmax')) xmax else NA

	df = tree_data(taxon=taxon, collapse=collapse)

	# ui <- bslib::page_navbar(
	# 	# theme = bslib::bs_theme(bootswatch = "cerulean"), # baby blue
	# 	# theme = bslib::bs_theme(bootswatch = "cosmo"), # light sky blue
	# 	# theme = bslib::bs_theme(bootswatch = "flatly"), # grey
	# 	theme = bslib::bs_theme(bootswatch = "spacelab"), # grey blue
	# 	# theme = bslib::bs_theme(bootswatch = "yeti"), # baby blue with a hint of green?
	# 	# theme = bslib::bs_theme(bootswatch = "zephyr"), # light sky blue
	# 	# theme = bslib::bs_theme(version = 5), # bootstrap
	# 	title = paste("arboretum", utils::packageVersion('arboretum')),
	# 	# navbar_options = bslib::navbar_options(
	# 		# 	# position = c("static-top", "fixed-top", "fixed-bottom"),
	# 		# bg = "#2D89C8",
	# 		# 	# theme = c("auto", "light", "dark"),
	# 		# collapsible = TRUE,
	# 		# 	inverse = TRUE,
	# 		# 	underline = TRUE
	# 	# ),
	# 	bslib::nav_panel(
	# 		title = "Descendants",
	# 		# icon = icon('seedling'),
	# 		# icon = icon('diagram-project'),
	# 		# icon = icon('tree'),
	# 		icon = icon('tree-conifer', lib="glyphicon"),
	# 		bslib::layout_sidebar(
	# 			sidebar = bslib::sidebar(
	# 				bslib::accordion_panel(
	# 					"Plot settings",
	# 					icon = icon('settings'),
	# 					numericInput("height", "height", min = 100, max = 5000, value = 900, step=50),
	# 					numericInput("width", "width", min = 100, max = 5000, value = 1250, step=50),
	# 					numericInput("xmin", "xmin", min = 0, max = 538.8, value = NA, step=10),
	# 					numericInput("xmax", "xmax", min = 0, max = 538.8, value = 0, step=10),
	# 				)
	# 			),
	# 			p("First page content.")
	# 		),
	# 	),
	# 	bslib::nav_panel(
	# 		title = "Ancestry",
	# 		# icon = icon('leaf'),
	# 		icon = icon('tree-deciduous', lib="glyphicon"),
	# 		# icon = icon('timeline'),
	# 		# icon = icon('person-cane'),
	# 		p("Second page content.")
	# 	),
	# 	bslib::nav_spacer(),
	# 	bslib::nav_item(
	# 		a(
	# 			href="https://github.com/dkidney/arboretum/blob/main/README.md",
	# 			icon('github', style="font-size: 30px")
	# 		)
	# 	)
	# 	# bslib::nav_menu(
	# 	# 	title = "Links",
	# 	# 	align = "right",
	# 	# bslib::nav_item(tags$a("Posit", href = "https://posit.co")),
	# 	# 	bslib::nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
	# 	# )
	# )
	# ui <- bslib::page_sidebar(
	ui <- bslib::page_sidebar(

		width = 200,

		# theme = bslib::bs_theme(bootswatch = "cerulean"), # baby blue
		# theme = bslib::bs_theme(bootswatch = "cosmo"), # light sky blue
		# theme = bslib::bs_theme(bootswatch = "flatly"), # grey
		# theme = bslib::bs_theme(bootswatch = "spacelab"), # grey blue
		# theme = bslib::bs_theme(bootswatch = "yeti"), # baby blue with a hint of green?
		# theme = bslib::bs_theme(bootswatch = "zephyr"), # light sky blue
		theme = bslib::bs_theme(bootswatch = "sandstone"),
		# theme = bslib::bs_theme(version = 5), # bootstrap

		# title = paste("arboretum", utils::packageVersion('arboretum')),
		# title = a(paste("arboretum", utils::packageVersion('arboretum')),
		# 		  style="font-size: 20px"),
		# href="https://github.com/dkidney/arboretum/blob/main/README.md"),

		sidebar = bslib::sidebar(
			bslib::accordion(
				open = c(
					"Configure tree",
					"Taxon info",
					"Tree summary",
					"About",
					NA_character_
				),
				bslib::accordion_panel(
					"Configure tree",
					icon = icon('leaf'),
					selectInput(
						inputId = 'taxon',
						label = 'taxon:',
						selected = input_taxon_selected,
						choices = input_taxon_choices
					),
					actionButton('expand', 'expand all'),
					HTML("<br/>"),
					actionButton('collapse', 'collapse default'),
					numericInput("xmin", "xmin", min = -550, max = 0, value = xmin, step=5),
					numericInput("xmax", "xmax", min = -550, max = 0, value = xmax, step=5)
				),
				bslib::accordion_panel(
					"Taxon info",
					icon = icon('circle-info'),
					htmlOutput(outputId = 'info')
				),
				bslib::accordion_panel(
					"Tree summary",
					icon = icon('list'),
					verbatimTextOutput(outputId = 'summary'),
				),
				bslib::accordion_panel(
					"Figure settings",
					icon = icon('cog'),
					numericInput("height", "height", min = 100, max = 5000, value = 850, step=50),
					numericInput("width", "width", min = 100, max = 5000, value = 1250, step=50),
				),
				bslib::accordion_panel(
					"About",
					a(
						href="https://github.com/dkidney/arboretum/blob/main/README.md",
						icon('github', style="font-size: 30px")
					)
				),
				# HTML("<br/>"),
				# br(),
				# # em('asfddasf'),
				# a(
				# 	href="https://github.com/dkidney/arboretum/blob/main/README.md",
				# 	icon('github', style="font-size: 30px")
				# )
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

		observeEvent(input$taxon, {
			vals$df = tree_data(taxon=input$taxon, collapse = 'default')
			updateNumericInput(inputId = 'xmin', value=NA)
			updateNumericInput(inputId = 'xmax', value=NA)
		})

		observeEvent(input$collapse, {
			vals$df = tree_data(taxon=input$taxon, collapse = 'default')
		})

		observeEvent(input$expand, {
			vals$df = tree_data(taxon=input$taxon, collapse = 'none')
		})

		# observeEvent(input$xmin, {
		# 	vals$xmin = input$xmin
		# })
		#
		# observeEvent(input$xmax, {
		# 	vals$xmin = input$xmax
		# })

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

		observe({
			req(vals$df)
			vals$plot = plot_tree_data(
				vals$df,
				xmin=input$xmin,
				xmax=input$xmax
			)
		})

		output$plot <- renderPlot({
			req(vals$plot)
			vals$plot
		},
		width = function() input$width,
		height = function() input$height,
		res = 115
		)

		output$info <- renderUI(HTML({
			req(input$plot_hover)
			req(vals$plot)
			df = isolate(vals$plot$data)
			taxon = closest_taxon(df, input$plot_hover)
			get_info(df, taxon) |> stringr::str_replace_all("\n", "<br/>")
		}))

		output$summary <- renderText({
			req(vals$df)
			print(summary(vals$df))
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



