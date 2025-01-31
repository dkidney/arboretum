load_geotime = function() {
	period_fill = dplyr::tribble(
		~period        , ~fill,
		'quaternary'   , 'deeppink1',
		'neogene'      , 'darkorchid1',
		'paleogene'    , 'darkorange1',
		'cretaceous'   , 'gold1',
		'jurassic'     , 'steelblue1',
		'triassic'     , 'tomato1',
		'permian'      , 'darkorchid1',
		'carboniferous', 'palegreen1',
		'devonian'     , 'dodgerblue1',
		'silurian'     , 'wheat1',
		'ordovician'   , 'aquamarine',
		'cambrian'     , 'tan1',
	) %>%
		dplyr::mutate(dplyr::across(c(.data$period),
									stringr::str_to_sentence)) %>%
		truncate_geotime_labels()

	readr::read_tsv(
		system.file("extdata", "geotime.tsv", package="arboretum"),
		col_types = readr::cols(
			from = readr::col_double(),
			to = readr::col_double(),
			.default = readr::col_character()
		)
	) %>%
		dplyr::mutate(dplyr::across(c(.data$period, .data$epoch),
									stringr::str_to_sentence)) %>%
		truncate_geotime_labels() %>%
		dplyr::left_join(period_fill, by='period')
}

truncate_geotime_labels = function(df) {
	if (!is.null(df[['period']])) {
		df$period %<>%
			stringr::str_replace('Quaternary', 'Q')
	}
	if (!is.null(df[['epoch']])) {
		df$epoch %<>%
			stringr::str_replace('Pliocene', 'Pl.') %>%
			stringr::str_replace('Pleistocene', 'Pl.') %>%
			stringr::str_replace('Holocene', '') %>%
			stringr::str_replace('ocene$', 'oc.') %>%
			stringr::str_replace('ian$', '.')
	}
	if (!is.null(df[['age']])) {
		df$age %<>%
			stringr::str_replace('ian$', '.')
	}
	return(df)
}

split_geotime_by_timescale = function(geotime) {

	era = geotime %>%
		dplyr::group_by(.data$era) %>%
		dplyr::summarise(from=min(.data$from),
						 to=max(.data$to),
						 .groups = 'drop') %>%
		dplyr::arrange(dplyr::desc(.data$from))

	period = geotime %>%
		# dplyr::group_by(dplyr::across(dplyr::one_of('era', 'period', 'fill'))) %>%
		dplyr::group_by(.data$era,
						.data$period,
						.data$fill) %>%
		dplyr::summarise(from=min(.data$from),
						 to=max(.data$to),
						 .groups = 'drop') %>%
		# dplyr::select(dplyr::one_of('era', 'period', 'from', 'to', 'fill')) %>%
		dplyr::select(.data$era,
					  .data$period,
					  .data$from,
					  .data$to,
					  .data$fill) %>%
		dplyr::arrange(dplyr::desc(.data$from))

	epoch = geotime %>%
		# dplyr::group_by(dplyr::across(dplyr::one_of('era', 'period', 'epoch', 'fill'))) %>%
		dplyr::group_by(.data$era,
						.data$period,
						.data$epoch,
						.data$fill) %>%
		dplyr::summarise(from=min(.data$from),
						 to=max(.data$to),
						 .groups = 'drop') %>%
		dplyr::group_by(.data$era,
						.data$period) %>%
		dplyr::mutate(
			rank = rank(.data$from),
			amount = 1 / (max(.data$rank) + 1) * (.data$rank - 1),
			fill = colorspace::darken(.data$fill, .data$amount)
		) %>%
		dplyr::ungroup() %>%
		# dplyr::select(dplyr::one_of('era', 'period', 'epoch', 'from', 'to', 'fill')) %>%
		dplyr::select(.data$era,
					  .data$period,
					  .data$epoch,
					  .data$from,
					  .data$to,
					  .data$fill) %>%
		dplyr::arrange(dplyr::desc(.data$from))

	age = geotime %>%
		dplyr::group_by(.data$era,
						.data$period,
						.data$epoch,
						.data$age) %>%
		dplyr::summarise(from=min(.data$from),
						 to=max(.data$to),
						 .groups = 'drop') %>%
		dplyr::arrange(dplyr::desc(.data$from))

	list(
		era = era,
		period = period,
		epoch = epoch,
		age=age
	)
}

load_events = function() {
	dplyr::tribble(
		~label  , ~ma    , ~col,
		'GABI'  , 2.7    , 'blue',
		'K-Pg'  , 66     , 'red',
		'Tr-J'  , 201.3  , 'red',
		'P-Tr'  , 251.902, 'red',
		'CRC'   , 305    , 'blue',
		'Late D', 371.1  , 'red',
		'O-S'   , 445    , 'red',
	) %>%
		dplyr::mutate(ma = -abs(.data$ma))
}
