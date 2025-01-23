#' plot tree data
#' @export
#' @examples
#' tree()
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

	relations = inner_join(
		df %>%
			get_nodes() %>%
			select(taxon, x=from, y, children) %>%
			mutate(children = children %>% map(~tibble(child=.x))) %>%
			unnest(children),
		df %>%
			select(child=taxon, xend=from, yend=y),
		by='child'
	) %>%
		arrange(taxon, yend)

	plt = df %>%
		ggplot(aes(x=from, y=y))


	# geotime ------------------------------------------------------------------

	geotime = load_geotime()

	# filter periods to match tree data
	periods = geotime %>%
		filter(to > min(df$from, na.rm = TRUE)) %>%
		select(period) %>%
		distinct
	geotime %<>%
		inner_join(periods, by='period')

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
		scale_y_continuous(
			# breaks = 0:max(df$y),
			# labels = 0:sum(df$is_tip),
			# expand = c(bottom, ?, top, ?)
			expand = c(abs(y_min_era - y_min) / h, 0,
					   (y_max_header - y_max) / h, 0),
		) +
		scale_x_continuous(
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
		geom_rect(
			aes(xmin=from, xmax=to),
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
		geom_rect(
			aes(xmin=from, xmax=to),
			ymax = y_max_era,
			ymin = y_min_era,
			fill=NA,
			col = era_linecolour,
			linewidth = era_linewidth,
			data=geotime$era,
			inherit.aes = FALSE
		) +
		geom_rect(
			aes(xmin=from, xmax=to),
			ymax = y_min_header,
			ymin = y_max_era,
			fill=NA,
			col = era_linecolour,
			linewidth = era_linewidth,
			data=geotime$era,
			inherit.aes = FALSE
		) +
		geom_text(
			aes(x = (from + to) / 2, label = era),
			y = (y_max_era + y_min_era) / 2,
			data=geotime$era,
			size = era_textsize,
		)

	# periods
	plt = plt +
		geom_rect(
			aes(xmin=from, xmax=to),
			ymax = y_min_header,
			ymin = y_max_period,
			fill=NA,
			col = period_linecolour,
			linewidth = period_linewidth,
			data=geotime$period,
			inherit.aes = FALSE
		) +
		geom_rect(
			aes(xmin=from, xmax=to),
			ymax = y_min_epoch,
			ymin = y_min_period,
			fill = if (fill) geotime$period$fill else NA,
			alpha = 0.2,
			col = period_linecolour,
			linewidth = period_linewidth,
			data=geotime$period,
			inherit.aes = FALSE
		) +
		geom_text(
			aes(x = (from + to) / 2, label = period),
			y = (y_max_period + y_min_period) / 2,
			data=geotime$period,
			size = period_textsize,
		)

	# epochs
	plt = plt +
		geom_rect(
			aes(xmin=from, xmax=to),
			ymax = y_min_header,
			ymin = y_max_epoch,
			fill = if (fill) geotime$epoch$fill else NA,
			alpha = 0.2,
			col = epoch_linecolour,
			linewidth = epoch_linewidth,
			data=geotime$epoch,
			inherit.aes = FALSE
		) +
		geom_rect(
			aes(xmin=from, xmax=to),
			ymax = y_max_epoch,
			ymin = y_min_epoch,
			fill = if (fill) geotime$epoch$fill else NA,
			alpha = 0.2,
			col = epoch_linecolour,
			linewidth = epoch_linewidth,
			data=geotime$epoch,
			inherit.aes = FALSE
		) +
		geom_text(
			aes(x = (from + to) / 2, label = epoch),
			y = (y_max_epoch + y_min_epoch) / 2,
			data=geotime$epoch %>% filter(!is.na(epoch)),
			size = epoch_textsize,
		)

	# ages
	plt = plt +
		geom_rect(
			aes(xmin=from, xmax=to),
			ymax = y_min_header,
			ymin = y_max_age,
			fill=NA,
			col = age_linecolour,
			linewidth = age_linewidth,
			data=geotime$age,
			inherit.aes = FALSE
		) +
		geom_rect(
			aes(xmin=from, xmax=to),
			ymax = y_max_age,
			ymin = y_min_age,
			fill=NA,
			col = age_linecolour,
			linewidth = age_linewidth,
			data=geotime$age,
			inherit.aes = FALSE
		) +
		geom_text(
			aes(x = (from + to) / 2, label = age),
			y = y_min_age + (y_max_age - y_min_age) * 0.05,
			data=geotime$age %>% filter(!is.na(age)),
			size = age_textsize,
			angle=90,
			hjust=0,
		)


	# events -------------------------------------------------------------------

	events = load_events() %>%
		filter(ma > min(geotime$period$from))

	plt = plt +
		geom_segment(
			aes(x=ma, xend=ma),
			data = events,
			y=y_max_epoch,
			yend=y_max_main,
			col=events$col,
			lty=1, lwd=0.5,
			inherit.aes = FALSE
		) +
		geom_text(
			aes(x=ma, label=label),
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
			aes(x=x, y=y, xend=xend, yend=yend),
			data = relations,
			col = 'grey50'
		) +
		geom_segment(
			aes(xend=to, yend=y),
			data=df %>% get_tips()
		) +
		geom_text(
			aes(x=(from + to) / 2, label=taxon),
			data=df %>% get_tips(),
			vjust=-0.5,
			size = textsize,
		) +
		geom_point(
			data=df %>% get_tips(),
			size = pointsize,
		) +
		geom_point(
			aes(x=to),
			data=df %>% get_tips() %>% filter(to < 0),
			size = pointsize,
		) +
		geom_point(
			data=df %>% get_nodes(),
			size = pointsize,
		) +
		geom_text(
			aes(label=taxon),
			data=df %>% get_nodes(),
			vjust=-0.5,
			size = textsize,
		) +
		theme(
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank(),
			panel.background = element_blank(),
			axis.title.x=element_blank(),
			# axis.line.x=element_line(),
			axis.title.y=element_blank(),
			axis.text.y=element_blank(),
			axis.ticks.y=element_blank(),
			legend.position = "none"
		)


	return(plt)
}

highlight_clade = function(plt, clade, col=1, alpha=0.25) {
  temp = plt$data %>% get_clade(clade)
  plt +
    annotate(
      "rect",
      xmin=min(temp$from) - 1,
      xmax=max(temp$to, na.rm=TRUE) + 1,
      ymin=min(temp$index) - 0.25,
      ymax=max(temp$index) + 0.5,
      alpha=alpha,
      fill=col,
      col=NA,
    )
}


