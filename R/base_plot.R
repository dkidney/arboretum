
base_plot = function(df, textsize=3) {

	df$y = rescale(df$y, 0, 1)

	geotime = load_geotime() |>
		split_geotime_by_timescale()

	x_max = 0
	x_min = min(geotime$age$from)

	# y_max_header  = y_min_header + 0.03
	# y_min_age    = y_max_age - 0.08
	# y_min_epoch  = y_max_epoch - 0.03
	# y_min_period = y_max_period - 0.04
	# y_min_era    = y_max_era - 0.04
	# y_expand_bottom = abs(y_min_era - y_min) / h
	# y_expand_top = (y_max_header - y_max) / h

	# plt = plt +
	# 	ggplot2::scale_y_continuous(
	# 		# expand = c(y_expand_bottom, 0, y_expand_top, 0),
	# 	) +
	# 	ggplot2::scale_x_continuous(
	# 		breaks = x_breaks,
	# 		labels = -x_breaks,
	# 		# expand = c(left, ?, right, ?)
	# 		expand = c(0.001, 0, 0.001, 0),
	# 	)

	textsize=3

	age_linewidth    = 0.1
	epoch_linewidth  = 0.2
	period_linewidth = 0.3
	era_linewidth    = 0.3

	age_textsize    = textsize - 0.5
	epoch_textsize  = textsize
	period_textsize = textsize + 0.5
	era_textsize    = textsize + 1.0

	age_linecolour    = 'grey60'
	epoch_linecolour  = 'grey50'
	period_linecolour = 'grey40'
	era_linecolour    = 'grey30'

	fill = TRUE

	buffer = 0.05
	ys = -buffer # lower limit of main plotting area
	ys = c(ys, ys[1] - 0.08) # height of age label boxes
	ys = c(ys, ys[2] - 0.03) # height of epoch label boxes
	ys = c(ys, ys[3] - 0.04) # height of period label boxes
	ys = c(ys, ys[4] - 0.04) # height of era label boxes
	ys

	plt = df |>
		ggplot2::ggplot(ggplot2::aes(x=.data$from, y=.data$y)) +
		# ggplot2::geom_point() +
		# ggplot2::scale_x_continuous(
		# 	expand = c(0.01, 0, 0.01, 0),
		# ) +
		ggplot2::scale_y_continuous(
			# expand = c(-ys[5], 0, buffer * 1.5, 0),
			expand = c(abs(ys[5]), 0, buffer * 1.5, 0),
		)
	plt

	# epoch fill
	plt = plt +	ggplot2::geom_rect(
		ggplot2::aes(xmin=.data$from, xmax=.data$to),
		ymax = 1 + buffer,
		ymin = ys[3],
		fill = if (fill) geotime$epoch$fill else NA,
		alpha = 0.2,
		col = epoch_linecolour,
		linewidth = epoch_linewidth,
		data=geotime$epoch,
		inherit.aes = FALSE
	)
	plt

	# period fill
	plt = plt +	ggplot2::geom_rect(
		ggplot2::aes(xmin=.data$from, xmax=.data$to),
		ymax = ys[3],
		ymin = ys[4],
		fill = if (fill) geotime$period$fill else NA,
		alpha = 0.2,
		col = period_linecolour,
		linewidth = period_linewidth,
		data=geotime$period,
		inherit.aes = FALSE
	)
	plt

	# all label boxes
	plt = plt +	ggplot2::geom_rect(
		ggplot2::aes(xmin=.data$from, xmax=.data$to),
		ymax = ys[1],
		ymin = ys[5],
		fill=NA,
		col = era_linecolour,
		linewidth = era_linewidth,
		data=geotime$era,
		inherit.aes = FALSE
	)
	plt

	# age label boxes
	plt = plt +	ggplot2::geom_rect(
		ggplot2::aes(xmin=.data$from, xmax=.data$to),
		ymax = ys[1],
		ymin = ys[2],
		fill=NA,
		col = age_linecolour,
		linewidth = age_linewidth,
		data=geotime$age,
		inherit.aes = FALSE
	)
	plt

	# epoch label boxes
	plt = plt +	ggplot2::geom_rect(
		ggplot2::aes(xmin=.data$from, xmax=.data$to),
		ymax = ys[2],
		ymin = ys[3],
		fill=NA,
		col = epoch_linecolour,
		linewidth = epoch_linewidth,
		data=geotime$epoch,
		inherit.aes = FALSE
	)
	plt

	# # period label boxes
	# plt = plt +	ggplot2::geom_rect(
	# 	ggplot2::aes(xmin=.data$from, xmax=.data$to),
	# 	ymax = ys[3],
	# 	ymin = ys[4],
	# 	fill=NA,
	# 	col = period_linecolour,
	# 	linewidth = period_linewidth,
	# 	data=geotime$period,
	# 	inherit.aes = FALSE
	# )
	# plt

	# era label boxes
	plt = plt +	ggplot2::geom_rect(
		ggplot2::aes(xmin=.data$from, xmax=.data$to),
		ymax = ys[4],
		ymin = ys[5],
		fill=NA,
		col = era_linecolour,
		linewidth = era_linewidth,
		data=geotime$era,
		inherit.aes = FALSE
	)
	plt

	# age main boxes
	plt = plt +	ggplot2::geom_rect(
		ggplot2::aes(xmin=.data$from, xmax=.data$to),
		ymax = 1 + buffer,
		ymin = ys[1],
		fill = NA,
		col = age_linecolour,
		linewidth = age_linewidth,
		data = geotime$age,
		inherit.aes = FALSE
	)
	plt

	# # epoch main boxes
	# plt = plt +	ggplot2::geom_rect(
	# 	ggplot2::aes(xmin=.data$from, xmax=.data$to),
	# 	ymax = 1,
	# 	ymin = ys[2],
	# 	fill = NA,
	# 	col = epoch_linecolour,
	# 	linewidth = epoch_linewidth,
	# 	data = geotime$epoch,
	# 	inherit.aes = FALSE
	# )
	# plt

	# period main boxes
	plt = plt +	ggplot2::geom_rect(
		ggplot2::aes(xmin=.data$from, xmax=.data$to),
		ymax = 1 + buffer,
		ymin = ys[3],
		fill = NA,
		col = period_linecolour,
		linewidth = period_linewidth,
		data = geotime$period,
		inherit.aes = FALSE
	)
	plt

	# era main boxes
	plt = plt +	ggplot2::geom_rect(
		ggplot2::aes(xmin=.data$from, xmax=.data$to),
		ymax = 1 + buffer,
		ymin = ys[4],
		fill = NA,
		col = era_linecolour,
		linewidth = era_linewidth,
		data = geotime$era,
		inherit.aes = FALSE
	)
	plt

	# age labels
	plt = plt +	ggplot2::geom_text(
		ggplot2::aes(x = (.data$from + .data$to) / 2, label = .data$age),
		y = ys[2],
		data=geotime$age |> dplyr::filter(!is.na(.data$age)),
		size = age_textsize,
		angle=90,
		hjust=-0.1,
		# nudge_x = 0.1
	)
	plt

	# epoch labels
	plt = plt +	ggplot2::geom_text(
		ggplot2::aes(x = (.data$from + .data$to) / 2, label = .data$epoch),
		y = (ys[2] + ys[3]) / 2,
		data = geotime$epoch,
		size = epoch_textsize,
	)
	plt

	# period labels
	plt = plt +	ggplot2::geom_text(
		ggplot2::aes(x = (.data$from + .data$to) / 2, label = .data$period),
		y = (ys[3] + ys[4]) / 2,
		data = geotime$period,
		size = period_textsize,
	)
	plt

	# era labels
	plt = plt +	ggplot2::geom_text(
		ggplot2::aes(x = (.data$from + .data$to) / 2, label = .data$era),
		y = (ys[4] + ys[5]) / 2,
		data = geotime$era,
		size = era_textsize,
	)
	plt



	#
	#
	#
	# 	# periods
	# 	plt = plt +	ggplot2::geom_rect(
	# 		ggplot2::aes(xmin=.data$from, xmax=.data$to),
	# 		ymax = y_min_header,
	# 		ymin = y_max_period,
	# 		fill=NA,
	# 		col = period_linecolour,
	# 		linewidth = period_linewidth,
	# 		data=geotime$period,
	# 		inherit.aes = FALSE
	# 	)
	# 	plt
	#
	#
	# 	plt = plt +	ggplot2::geom_text(
	# 		ggplot2::aes(x = (.data$from + .data$to) / 2, label = .data$period),
	# 		y = (y_max_period + y_min_period) / 2,
	# 		data=geotime$period,
	# 		size = period_textsize,
	# 	)
	# 	plt
	#
	# 	# epochs
	# 	plt = plt + ggplot2::geom_rect(
	# 		ggplot2::aes(xmin=.data$from, xmax=.data$to),
	# 		ymax = y_min_header,
	# 		ymin = y_max_epoch,
	# 		fill = if (fill) geotime$epoch$fill else NA,
	# 		alpha = 0.2,
	# 		col = epoch_linecolour,
	# 		linewidth = epoch_linewidth,
	# 		data=geotime$epoch,
	# 		inherit.aes = FALSE
	# 	)
	# 	plt
	#

	#
	# 	plt = plt +	ggplot2::geom_text(
	# 		ggplot2::aes(x = (.data$from + .data$to) / 2, label = .data$epoch),
	# 		y = (y_max_epoch + y_min_epoch) / 2,
	# 		data=geotime$epoch |> dplyr::filter(!is.na(.data$epoch)),
	# 		size = epoch_textsize,
	# 	)
	# 	plt
	#
	#
	# 	# events -------------------------------------------------------------------
	#
		events = load_events()

		plt = plt +
			ggplot2::geom_segment(
				ggplot2::aes(x=.data$ma, xend=.data$ma),
				data = events,
				y=ys[4],
				yend=1 + buffer,
				col=events$col,
				lty=1, lwd=0.5,
				inherit.aes = FALSE
			) +
			ggplot2::geom_text(
				ggplot2::aes(x=.data$ma, label=.data$label),
				data = events,
				size=textsize,
				# y=y_max_main,
				y = 1 + buffer,
				col='grey50',
				vjust = -0.5,
				inherit.aes = FALSE
			)

	plt +
		ggplot2::theme(
			panel.grid.major = ggplot2::element_blank(),
			panel.grid.minor = ggplot2::element_blank(),
			panel.background = ggplot2::element_blank(),
			# axis.title.x = ggplot2::element_blank(),
			# axis.line.x = ggplot2::element_line(),
			# axis.title.y = ggplot2::element_blank(),
			# axis.text.y = ggplot2::element_blank(),
			# axis.ticks.y = ggplot2::element_blank(),
			legend.position = "none"
		)
}
