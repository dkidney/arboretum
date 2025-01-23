#' plot tree data
#' @param taxon TODO
#' @param collapse TODO
#' @export
#' @examples
#' library(arboretum)
#' tree()
#' tree('amniota', collapse=c('archosauromorpha',
#'                            'therapsida',
#'                            'lepidosauromorpha'))
#' tree('therapsida', collapse='mammalia')
#' tree('mammalia')
#' tree('archosauromorpha', collapse=c('dinosauria',
#'                                     'pterosauria',
#'                                     'pseudosuchia',
#'                                     'sauropterygia'))
#' tree('pseudosuchia')
#' tree('dinosauria', collapse = c('sauropoda',
#'                                 'ornithopoda',
#'                                 'theropoda',
#'                                 'ankylosauria',
#'                                 'ceratopsia'))
#' tree('theropoda', collapse=c('avialae'))

tree = function(taxon=NULL, collapse=NULL) {
	if (is.null(collapse)) {
		collapse = c(
			'archosauromorpha',
			'lepidosauromorpha',
			'parareptilia',
			'synapsida',
			'temnospondyli'
		)
	}

	df = load_tree_data() %>%
		subset_taxon(taxon) %>%
		collapse_taxa(collapse) %>%
		arrange_tree()

	relations = dplyr::inner_join(
		df %>%
			get_nodes() %>%
			dplyr::select(.data$taxon,
						  x=.data$from,
						  .data$y,
						  .data$children) %>%
			# dplyr::select(dplyr::one_of('taxon', 'from', 'y', 'children')) %>%
			# dplyr::rename(x=.data$from) %>%
			dplyr::mutate(children = .data$children %>% purrr::map(~dplyr::tibble(child=.x))) %>%
			tidyr::unnest(.data$children),
		df %>%
			dplyr::select(child=.data$taxon,
						  xend=.data$from,
						  yend=.data$y),
		by='child'
	) %>%
		dplyr::arrange(.data$taxon,
					   .data$yend)

	plt = df %>%
		ggplot2::ggplot(ggplot2::aes(x=.data$from,
									 y=.data$y))


	# geotime ------------------------------------------------------------------

	geotime = load_geotime()

	# filter periods to match tree data
	periods = geotime %>%
		dplyr::filter(.data$to > min(df$from, na.rm = TRUE)) %>%
		dplyr::select(.data$period) %>%
		dplyr::distinct()
	geotime %<>%
		dplyr::inner_join(periods, by='period')

	y_max = max(df$y, na.rm = TRUE)
	y_min = min(df$y, na.rm = TRUE)
	x_max = 0
	x_min = min(geotime$from)

	geotime = split_geotime_by_timescale(geotime)

	h = diff(range(df$y))
	one_pc = h / 100

	y_max_main = y_max + 0.5
	y_min_main = y_min - 0.5

	y_min_header  = y_max_main
	y_max_header  = y_min_header + one_pc * 3

	y_max_age    = y_min_main
	y_min_age    = y_max_age - one_pc * 8

	y_max_epoch  = y_min_age
	y_min_epoch  = y_max_epoch - one_pc * 3

	y_max_period = y_min_epoch
	y_min_period = y_max_period - one_pc * 4

	y_max_era    = y_min_period
	y_min_era    = y_max_era - one_pc * 4

	x_breaks = rev(seq(0, x_min, -25))

	plt = plt +
		ggplot2::scale_y_continuous(
			# breaks = 0:max(df$y),
			# labels = 0:sum(df$is_tip),
			# expand = c(bottom, ?, top, ?)
			expand = c(abs(y_min_era - y_min) / h, 0,
					   (y_max_header - y_max) / h, 0),
		) +
		ggplot2::scale_x_continuous(
			breaks = x_breaks,
			labels = -x_breaks,
			# expand = c(left, ?, right, ?)
			expand = c(0.001, 0, 0.001, 0),
		)

	age_linewidth    = 0.1
	epoch_linewidth  = 0.2
	period_linewidth = 0.3
	era_linewidth    = 0.4

	age_textsize    = 2
	epoch_textsize  = 2.5
	period_textsize = 3.5
	era_textsize    = 4

	age_linecolour    = 'grey60'
	epoch_linecolour  = 'grey50'
	period_linecolour = 'grey40'
	era_linecolour    = 'grey30'

	fill = TRUE

	plt = plt +
		ggplot2::geom_rect(
			ggplot2::aes(xmin=.data$from, xmax=.data$to),
			ymax = y_max_age,
			ymin = y_min_era,
			fill=NA,
			col = era_linecolour,
			linewidth = era_linewidth,
			data=geotime$era,
			inherit.aes = FALSE
		)

	# eras
	plt = plt +
		ggplot2::geom_rect(
			ggplot2::aes(xmin=.data$from, xmax=.data$to),
			ymax = y_max_era,
			ymin = y_min_era,
			fill=NA,
			col = era_linecolour,
			linewidth = era_linewidth,
			data=geotime$era,
			inherit.aes = FALSE
		) +
		ggplot2::geom_rect(
			ggplot2::aes(xmin=.data$from, xmax=.data$to),
			ymax = y_min_header,
			ymin = y_max_era,
			fill=NA,
			col = era_linecolour,
			linewidth = era_linewidth,
			data=geotime$era,
			inherit.aes = FALSE
		) +
		ggplot2::geom_text(
			ggplot2::aes(x = (.data$from + .data$to) / 2, label = .data$era),
			y = (y_max_era + y_min_era) / 2,
			data=geotime$era,
			size = era_textsize,
		)

	# periods
	plt = plt +
		ggplot2::geom_rect(
			ggplot2::aes(xmin=.data$from, xmax=.data$to),
			ymax = y_min_header,
			ymin = y_max_period,
			fill=NA,
			col = period_linecolour,
			linewidth = period_linewidth,
			data=geotime$period,
			inherit.aes = FALSE
		) +
		ggplot2::geom_rect(
			ggplot2::aes(xmin=.data$from, xmax=.data$to),
			ymax = y_min_epoch,
			ymin = y_min_period,
			fill = if (fill) geotime$period$fill else NA,
			alpha = 0.2,
			col = period_linecolour,
			linewidth = period_linewidth,
			data=geotime$period,
			inherit.aes = FALSE
		) +
		ggplot2::geom_text(
			ggplot2::aes(x = (.data$from + .data$to) / 2, label = .data$period),
			y = (y_max_period + y_min_period) / 2,
			data=geotime$period,
			size = period_textsize,
		)

	# epochs
	plt = plt +
		ggplot2::geom_rect(
			ggplot2::aes(xmin=.data$from, xmax=.data$to),
			ymax = y_min_header,
			ymin = y_max_epoch,
			fill = if (fill) geotime$epoch$fill else NA,
			alpha = 0.2,
			col = epoch_linecolour,
			linewidth = epoch_linewidth,
			data=geotime$epoch,
			inherit.aes = FALSE
		) +
		ggplot2::geom_rect(
			ggplot2::aes(xmin=.data$from, xmax=.data$to),
			ymax = y_max_epoch,
			ymin = y_min_epoch,
			fill = if (fill) geotime$epoch$fill else NA,
			alpha = 0.2,
			col = epoch_linecolour,
			linewidth = epoch_linewidth,
			data=geotime$epoch,
			inherit.aes = FALSE
		) +
		ggplot2::geom_text(
			ggplot2::aes(x = (.data$from + .data$to) / 2, label = .data$epoch),
			y = (y_max_epoch + y_min_epoch) / 2,
			data=geotime$epoch %>% dplyr::filter(!is.na(.data$epoch)),
			size = epoch_textsize,
		)

	# ages
	plt = plt +
		ggplot2::geom_rect(
			ggplot2::aes(xmin=.data$from, xmax=.data$to),
			ymax = y_min_header,
			ymin = y_max_age,
			fill=NA,
			col = age_linecolour,
			linewidth = age_linewidth,
			data=geotime$age,
			inherit.aes = FALSE
		) +
		ggplot2::geom_rect(
			ggplot2::aes(xmin=.data$from, xmax=.data$to),
			ymax = y_max_age,
			ymin = y_min_age,
			fill=NA,
			col = age_linecolour,
			linewidth = age_linewidth,
			data=geotime$age,
			inherit.aes = FALSE
		) +
		ggplot2::geom_text(
			ggplot2::aes(x = (.data$from + .data$to) / 2, label = .data$age),
			y = y_min_age + (y_max_age - y_min_age) * 0.05,
			data=geotime$age %>% dplyr::filter(!is.na(.data$age)),
			size = age_textsize,
			angle=90,
			hjust=0,
		)


	# events -------------------------------------------------------------------

	events = load_events() %>%
		dplyr::filter(.data$ma > min(geotime$period$from))

	plt = plt +
		ggplot2::geom_segment(
			ggplot2::aes(x=.data$ma, xend=.data$ma),
			data = events,
			y=y_max_epoch,
			yend=y_max_main,
			col=events$col,
			lty=1, lwd=0.5,
			inherit.aes = FALSE
		) +
		ggplot2::geom_text(
			ggplot2::aes(x=.data$ma, label=.data$label),
			data = events,
			# y=y_max_main,
			y = (y_min_header + y_max_header) / 2,
			col='grey50',
			# vjust = -0.5,
			inherit.aes = FALSE
		)


	# tips, nodes and relations ------------------------------------------------

	pointsize = 1.0
	textsize = 2.5
	plt = plt +
		geom_sigmoid(
			ggplot2::aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
			data = relations,
			col = 'grey50'
		) +
		ggplot2::geom_segment(
			ggplot2::aes(xend=.data$to, yend=.data$y),
			data=df %>% get_tips()
		) +
		ggplot2::geom_text(
			ggplot2::aes(x=(.data$from + .data$to) / 2, label=.data$taxon),
			data=df %>% get_tips(),
			vjust=-0.5,
			size = textsize,
		) +
		ggplot2::geom_point(
			data=df %>% get_tips(),
			size = pointsize,
		) +
		ggplot2::geom_point(
			ggplot2::aes(x=.data$to),
			data=df %>% get_tips() %>% dplyr::filter(.data$to < 0),
			size = pointsize,
		) +
		ggplot2::geom_point(
			data=df %>% get_nodes(),
			size = pointsize,
		) +
		ggplot2::geom_text(
			ggplot2::aes(label=.data$taxon),
			data=df %>% get_nodes(),
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


	return(plt)
}


highlight_taxon = function(plt, taxon, col=1, alpha=0.25) {
	temp = plt$data %>% subset_taxon(taxon)
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


