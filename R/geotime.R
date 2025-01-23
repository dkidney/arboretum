load_geotime = function() {
	period_fill = tribble(
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
		'ordovician'   , 'dodgerblue1',
		'cambrian'     , 'tan1',
	) %>%
		mutate(across(c(period), str_to_sentence)) %>%
		truncate_geotime_labels()

	readr::read_tsv(
		system.file("extdata", "geotime.tsv", package="arboretum"),
		col_types = cols(
			from = col_double(),
			to = col_double(),
			.default = col_character()
		)
	) %>%
		mutate(across(c(period, epoch), str_to_sentence)) %>%
		truncate_geotime_labels() %>%
		left_join(period_fill, by='period')
}

truncate_geotime_labels = function(df) {
	if (!is.null(df[['period']])) {
		df$period %<>%
			str_replace('Quaternary', 'Q')
	}
	if (!is.null(df[['epoch']])) {
		df$epoch %<>%
			str_replace('Pliocene', 'Pl.') %>%
			str_replace('Pleistocene', 'Pl.') %>%
			str_replace('Holocene', '') %>%
			str_replace('ocene$', 'oc.') %>%
			str_replace('ian$', '.')
	}
	if (!is.null(df[['age']])) {
		df$age %<>%
			str_replace('ian$', '.')
	}
	return(df)
}

split_geotime_by_timescale = function(geotime, timescale) {
	era = geotime %>%
		group_by(era) %>%
		summarise(from=min(from), to=max(to), .groups = 'drop') %>%
		arrange(desc(from))
	period = geotime %>%
		group_by(era, period, fill) %>%
		summarise(from=min(from), to=max(to), .groups = 'drop') %>%
		select(era, period, from, to, fill) %>%
		arrange(desc(from))
	epoch = geotime %>%
		group_by(era, period, epoch, fill) %>%
		summarise(from=min(from), to=max(to), .groups = 'drop') %>%
		group_by(era, period) %>%
		mutate(
			rank = rank(from),
			amount = 1 / (max(rank) + 1) * (rank - 1),
			fill = colorspace::darken(fill, amount)
		) %>%
		ungroup() %>%
		select(era, period, epoch, from, to, fill) %>%
		arrange(desc(from))
	age = geotime %>%
		group_by(era, period, epoch, age) %>%
		summarise(from=min(from), to=max(to), .groups = 'drop') %>%
		arrange(desc(from))
	list(
		era = era,
		period = period,
		epoch = epoch,
		age=age
	)
}

load_events = function() {
  tribble(
    ~label  , ~ma    , ~col,
    'GABI'  , 2.7    , 'blue',
    'K-Pg'  , 66     , 'red',
    'Tr-J'  , 201.3  , 'red',
    'P-Tr'  , 251.902, 'red',
    'Late D', 371.1  , 'red',
    'O-S'   , 445    , 'red',
  ) %>%
    mutate(ma = -abs(ma))
}
