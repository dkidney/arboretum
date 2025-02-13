#' plot tree data
#' @param taxon TODO
#' @param collapse TODO
#' @param max_tips TODO
#' @param ... TODO
#' @export
#' @examples
#' library(arboretum)
#' tree('tetrapodomorpha')
#' tree('reptiliomorpha', collapse=c('diapsida', 'therapsida'), xmin=-345, xmax=-252)
#' tree('therapsida', xmax=-200)
#' tree('ornithischia', collapse='none')
#' tree('sauropoda', collapse='none')
#' tree('tyrannosauroidea', collapse='avialae', xmin=-173)

tree = function(taxon=NULL, collapse=NULL, ...) {

	# df = prepare_tree_data(taxon=taxon, collapse=collapse)
	df = tree_data(taxon=taxon, collapse=collapse)

	print(summary(df))

	plot_tree_data(df, ...)

}

taxa = function(regex=NULL){
	taxa = sort(load_tree_data()$taxon)
	if (!is.null(regex)) {
		taxa = taxa |> purrr::keep(stringr::str_detect, regex)
	}
	writeLines(taxa)
}

# get_collapsable_taxa = function(df, taxon) {
# 	taxa = get_nodes(df)$taxon
# 	# if (is.null(taxon)) {
# 		taxa = taxa[taxa != get_roots(df)$taxon]
# 	# } else {
# 	# 	taxa = taxa[taxa != taxon]
# 	# }
# 	taxa
# }

# get_default_collapsed = function(df, collapsable_taxa) {
# taxa = collapsable_taxa

auto_xmin = function(df, xmin=NULL) {
	if (is.null(xmin) || is.na(xmin)) {
		xmin = floor(min(df$from) / 10) * 10
		message('using xmin = ', xmin)
	}
	stopifnot(inherits(xmin, c('integer', 'numeric')))
	xmin
}

auto_xmax = function(df, xmax=NULL) {
	if (is.null(xmax) || is.na(xmax)) {
		xmax = ceiling(max(df$to) / 10) * 10
		message('using xmax = ', xmax)
	}
	stopifnot(inherits(xmax, c('integer', 'numeric')))
	xmax
}

rescale_y = function(df) {
	df |> dplyr::mutate(y = .data$y |> rescale(0, 1))
}

plot_tree_data = function(df, max_tips=100, textsize=3.5, xmin=NULL, xmax=NULL) {

	# # check_xmin
	# # default = min(df$from) - (max(df$to) - min(df$from)) * 0.01
	# default = min(df$from)
	# if (is.null(xmin) || is.na(xmin)) {
	# 	xmin = default
	# } else {
	# 	stopifnot(inherits(xmin, c('integer', 'numeric')))
	# 	xmin = -abs(xmin)
	# 	# stopifnot(xmin >= default)
	# }
	#
	# # check_xmax
	# # default = max(df$to) + (max(df$to) - min(df$from)) * 0.01
	# default = max(df$to)
	# if (is.null(xmax) || is.na(xmax)) {
	# 	xmax = default
	# } else {
	# 	stopifnot(inherits(xmax, c('integer', 'numeric')))
	# 	# xmax = -abs(xmax)
	# 	# stopifnot(xmax <= default)
	# }

	# filter -------------------------------------------------------------------

	# browser()

	df = df |> reorder_tree_data()
	xmin = auto_xmin(df, xmin)
	xmax = auto_xmax(df, xmax)
	if (xmax <= xmin) {
		browser()
	}
	stopifnot(xmax > xmin)
	df = df |> dplyr::filter(.data$from < !!xmax | .data$to > !!xmin)
	df = df |> rescale_y()

	# df = check_n_tips(df, max_tips)
	# if(get_n_tips(df) > max_tips) {
	# 	warning('number of tips exceeds max_tips (', max_tips, ') - returning NULL')
	# 	return(NULL)
	# }

	# update labels ------------------------------------------------------------

	df = df |>
		dplyr::mutate(
			label = dplyr::case_when(
				.data$level == 'genus' ~ stringr::str_to_sentence(.data$taxon),
				.default=.data$taxon
			)
		) |>
		dplyr::mutate(
			label_x = dplyr::case_when(
				.data$is_tip ~ (pmax(.data$from, xmin) + pmin(.data$to, xmax)) / 2,
				.default = .data$from
			)
		)

	# base plot + zoom ---------------------------------------------------------

	# browser()
	plt = df |>
		base_plot() +
		ggplot2::coord_cartesian(xlim=c(xmin, xmax))

	# x breaks -----------------------------------------------------------------

	x_range = xmax - xmin
	if (x_range > 500) {
		step = 50
	} else 	if (x_range > 250) {
		step = 25
	} else if (x_range > 100) {
		step = 10
	} else {
		step = 5
	}

	x_breaks = rev(seq(0, -538.8, -step))

	# print(x_breaks)

	plt = plt +
		ggplot2::scale_x_continuous(
			breaks = x_breaks,
			labels = -x_breaks,
			expand = c(0.01, 0, 0.01, 0),
		)

	# relationships ------------------------------------------------------------

	parent_child_relationships = get_parent_child_relationships(df)
	if (!is.null(parent_child_relationships)) {
		plt = plt +
			geom_sigmoid(
				ggplot2::aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
				data = parent_child_relationships,
				col = 'grey50'
			)
	}

	# tips and nodes -----------------------------------------------------------

	pointsize = 1.0
	# browser()
	plt = plt +
		ggplot2::geom_segment(
			ggplot2::aes(xend=.data$to, yend=.data$y),
			data=df |> get_tips()
		) +
		# ggplot2::geom_text(
		# 	ggplot2::aes(x=.data$label_x, label=.data$taxon),
		# 	vjust=-0,
		# 	size = textsize,
		# ) +
		# ggplot2::geom_text(
		# 	ggplot2::aes(x=.data$label_x, label=.data$taxon),
		# 	vjust=0.5,
		# 	size = textsize,
		# ) +
		ggplot2::geom_text(
			ggplot2::aes(x=.data$label_x, label=.data$taxon),
			vjust=-0.5,
			size = textsize,
		) +
		ggplot2::geom_text(
			ggplot2::aes(x=.data$label_x, label=.data$taxon),
			vjust=-0.5,
			size = textsize,
		) +
		ggplot2::theme(
			panel.grid.major = ggplot2::element_blank(),
			panel.grid.minor = ggplot2::element_blank(),
			panel.background = ggplot2::element_blank(),
			axis.title.x = ggplot2::element_blank(),
			# axis.line.x = ggplot2::element_line(),
			axis.title.y = ggplot2::element_blank(),
			axis.text.y = ggplot2::element_blank(),
			axis.ticks.y = ggplot2::element_blank(),
			legend.position = "none"
		)
	# nodes from (no asterisk)
	tmp = df[!df$is_tip & !df$asterisk, ]
	plt = plt +
		ggplot2::geom_point(
			data=tmp,
			size=pointsize,
		)
	# nodes from (asterisk)
	tmp = df[!df$is_tip & df$asterisk, ]
	plt = plt +
		ggplot2::geom_point(
			data=tmp,
			size=pointsize,
			col='red',
			shape=21,
			fill=tmp$is_collapsed |> dplyr::if_else('white', 'red'),
		)
	# tips from (no asterisk)
	tmp = df[df$is_tip & !df$asterisk, ]
	plt = plt +
		ggplot2::geom_point(
			data=tmp,
			size = pointsize,
			col='black',
			shape=21,
			fill=tmp$is_collapsed |> dplyr::if_else('white', 'black'),
		)
	# tips from (asterisk)
	tmp = df[df$is_tip & df$asterisk, ]
	plt = plt +
		ggplot2::geom_point(
			data=tmp,
			size = pointsize,
			col='red',
			shape=21,
			fill=tmp$is_collapsed |> dplyr::if_else('white', 'red'),
		)
	# tips to (ignore extant)
	tmp = df[df$is_tip, ] |> dplyr::filter(.data$to < 0)
	if (nrow(tmp) > 0) {
		plt = plt +
			ggplot2::geom_point(
				ggplot2::aes(x=.data$to),
				data=tmp,
				size = pointsize,
			)
	}

	return(plt)
}

check_n_tips = function(df, max_tips=NULL) {
	n_tips = nrow(get_tips(df))
	# message('tree has ', n_tips, ' tips')
	if (is.null(max_tips)) return(df)
	stopifnot(is.numeric(max_tips), length(max_tips) == 1)
	max_tips = as.integer(max_tips)
	stopifnot(!is.na(max_tips), max_tips > 0)
	if(n_tips > max_tips) {
		stop('number of tips exceeds max_tips (', max_tips, ')')
	}
	invisible(df)
}

get_parent_child_relationships = function(df) {
	# browser()
	if (all(df$is_tip)) return(NULL)
	# nodes = get_nodes(df)
	# if (nrow(nodes) == 0) return(NULL)
	dplyr::inner_join(
		df |>
			dplyr::select(.data$taxon,
						  x=.data$from,
						  .data$y,
						  .data$children) |>
			dplyr::mutate(children = .data$children |> purrr::map(~dplyr::tibble(child=.x))) |>
			tidyr::unnest(.data$children),
		df |>
			dplyr::select(child=.data$taxon,
						  xend=.data$from,
						  yend=.data$y),
		by='child'
	) |>
		dplyr::arrange(.data$taxon,
					   .data$yend)
}

highlight_taxon = function(plt, taxon, col=1, alpha=0.25) {
	temp = plt$data |> subset(taxon)
	plt +
		ggplot2::annotate(
			"rect",
			xmin=min(temp$from, na.rm=TRUE) - 1,
			xmax=max(temp$to, na.rm=TRUE) + 1,
			ymin=min(temp$y, na.rm=TRUE) - 0.25,
			ymax=max(temp$y, na.rm=TRUE) + 0.5,
			alpha=alpha,
			fill=col,
			col=NA,
		)
}


