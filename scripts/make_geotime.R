# suppressPackageStartupMessages({
# 	library(tidyverse)
# })

truncate_geotime_labels = function(df) {
	if (!is.null(df[['period']])) {
		df$period = df$period |>
			stringr::str_replace('Quaternary', 'Q')
	}
	if (!is.null(df[['epoch']])) {
		df$epoch = df$epoch |>
			stringr::str_replace('Pliocene', 'Pl.') |>
			stringr::str_replace('Pleistocene', 'Pl.') |>
			stringr::str_replace('Holocene', '') |>
			stringr::str_replace('ocene$', 'oc.') |>
			stringr::str_replace('ian$', '.')
	}
	if (!is.null(df[['age']])) {
		df$age = df$age |>
			stringr::str_replace('ian$', '.')
	}
	return(df)
}

period_fill = dplyr::tribble(
	~period        , ~fill,
	# 'quaternary'   , 'deeppink1',
	# 'neogene'      , 'darkorchid1',
	# 'paleogene'    , 'darkorange1',
	# 'cretaceous'   , 'gold1',
	# 'jurassic'     , 'steelblue1',
	# 'triassic'     , 'tomato1',
	# 'permian'      , 'darkorchid1',
	# 'carboniferous', 'palegreen1',
	# 'devonian'     , 'dodgerblue1',
	# 'silurian'     , 'wheat1',
	# 'ordovician'   , 'aquamarine',
	# 'cambrian'     , 'tan1',
	'quaternary'   , 'yellow2',
	'neogene'      , 'gold',
	'paleogene'    , 'darkorange1',
	'cretaceous'   , 'chartreuse3',
	'jurassic'     , 'deepskyblue2',
	'triassic'     , 'darkorchid',
	'permian'      , 'firebrick1',
	'carboniferous', 'palegreen1',
	'devonian'     , 'dodgerblue1',
	'silurian'     , 'wheat1',
	'ordovician'   , 'aquamarine',
	'cambrian'     , 'tan1',
) |>
	dplyr::mutate(dplyr::across('period', stringr::str_to_sentence)) |>
	truncate_geotime_labels()

geotime = tibble::tribble(
	~era       , ~period        , ~epoch         , ~age           , ~from   ,
	'cenozoic' , 'quaternary'   , 'holocene'     , NA_character_  ,   0.0117,
	'cenozoic' , 'quaternary'   , 'pleistocene'  , NA_character_  ,   2.58  ,
	'cenozoic' , 'neogene'      , 'pliocene'     , NA_character_  ,   5.333 ,
	'cenozoic' , 'neogene'      , 'miocene'      , 'messinian'    ,   7.246 ,
	'cenozoic' , 'neogene'      , 'miocene'      , 'tortonian'    ,  11.63  ,
	'cenozoic' , 'neogene'      , 'miocene'      , 'serravallian' ,  13.82  ,
	'cenozoic' , 'neogene'      , 'miocene'      , 'langhian'     ,  15.97  ,
	'cenozoic' , 'neogene'      , 'miocene'      , 'burdigalian'  ,  20.44  ,
	'cenozoic' , 'neogene'      , 'miocene'      , 'aquitanian'   ,  23.03  ,
	'cenozoic' , 'paleogene'    , 'oligocene'    , 'chattian'     ,  27.82  ,
	'cenozoic' , 'paleogene'    , 'oligocene'    , 'rupelian'     ,  33.9   ,
	'cenozoic' , 'paleogene'    , 'eocene'       , 'priabonian'   ,  37.71  ,
	'cenozoic' , 'paleogene'    , 'eocene'       , 'bartonian'    ,  41.2   ,
	'cenozoic' , 'paleogene'    , 'eocene'       , 'lutetian'     ,  47.8   ,
	'cenozoic' , 'paleogene'    , 'eocene'       , 'ypresian'     ,  56     ,
	'cenozoic' , 'paleogene'    , 'paleocene'    , 'thanetian'    ,  59.2   ,
	'cenozoic' , 'paleogene'    , 'paleocene'    , 'selandian'    ,  61.6   ,
	'cenozoic' , 'paleogene'    , 'paleocene'    , 'danian'       ,  66     ,
	'mesozoic' , 'cretaceous'   , 'late'         , 'maastrichtian',  72.1   ,
	'mesozoic' , 'cretaceous'   , 'late'         , 'campanian'    ,  83.6   ,
	'mesozoic' , 'cretaceous'   , 'late'         , 'santonian'    ,  86.3   ,
	'mesozoic' , 'cretaceous'   , 'late'         , 'coniacian'    ,  89.8   ,
	'mesozoic' , 'cretaceous'   , 'late'         , 'turonian'     ,  93.9   ,
	'mesozoic' , 'cretaceous'   , 'late'         , 'cenomanian'   , 100.5   ,
	'mesozoic' , 'cretaceous'   , 'early'        , 'albian'       , 113.0   ,
	'mesozoic' , 'cretaceous'   , 'early'        , 'aptian'       , 121.4   ,
	'mesozoic' , 'cretaceous'   , 'early'        , 'barremian'    , 125.77  ,
	'mesozoic' , 'cretaceous'   , 'early'        , 'hauterivian'  , 132.6   ,
	'mesozoic' , 'cretaceous'   , 'early'        , 'valanginian'  , 139.8   ,
	'mesozoic' , 'cretaceous'   , 'early'        , 'berriasian'   , 145     ,
	'mesozoic' , 'jurassic'     , 'late'         , 'tithonian'    , 149.2   ,
	'mesozoic' , 'jurassic'     , 'late'         , 'kimmeridgian' , 154.8   ,
	'mesozoic' , 'jurassic'     , 'late'         , 'oxfordian'    , 161.5   ,
	'mesozoic' , 'jurassic'     , 'middle'       , 'callovian'    , 165.3   ,
	'mesozoic' , 'jurassic'     , 'middle'       , 'bathonian'    , 168.2   ,
	'mesozoic' , 'jurassic'     , 'middle'       , 'bajocian'     , 170.9   ,
	'mesozoic' , 'jurassic'     , 'middle'       , 'aalenian'     , 174.7   ,
	'mesozoic' , 'jurassic'     , 'early'        , 'toarcian'     , 184.2   ,
	'mesozoic' , 'jurassic'     , 'early'        , 'pliensbachian', 192.9   ,
	'mesozoic' , 'jurassic'     , 'early'        , 'sinemurian'   , 199.5   ,
	'mesozoic' , 'jurassic'     , 'early'        , 'hettangian'   , 201.3   ,
	'mesozoic' , 'triassic'     , 'late'         , 'rhaetian'     , 205.7   ,
	'mesozoic' , 'triassic'     , 'late'         , 'norian'       , 227     ,
	'mesozoic' , 'triassic'     , 'late'         , 'carnian'      , 237     ,
	'mesozoic' , 'triassic'     , 'middle'       , 'ladinian'     , 242     ,
	'mesozoic' , 'triassic'     , 'middle'       , 'anisian'      , 247.2   ,
	'mesozoic' , 'triassic'     , 'early'        , 'olenekian'    , 251.2   ,
	'mesozoic' , 'triassic'     , 'early'        , 'induan'       , 251.902 ,
	'paleozoic', 'permian'      , 'lopingian'    , 'changhsingian', 254.14  ,
	'paleozoic', 'permian'      , 'lopingian'    , 'wuchiapingian', 259.51  ,
	'paleozoic', 'permian'      , 'guadalupian'  , 'capitanian'   , 264.28  ,
	'paleozoic', 'permian'      , 'guadalupian'  , 'wordian'      , 266.9   ,
	'paleozoic', 'permian'      , 'guadalupian'  , 'roadian'      , 273.01  ,
	'paleozoic', 'permian'      , 'cisuralian'   , 'kungurian'    , 283.5   ,
	'paleozoic', 'permian'      , 'cisuralian'   , 'artinskian'   , 290.1   ,
	'paleozoic', 'permian'      , 'cisuralian'   , 'sakmarian'    , 293.52  ,
	'paleozoic', 'permian'      , 'cisuralian'   , 'asselian'     , 298.9   ,
	'paleozoic', 'carboniferous', 'pennsylvanian', 'gzhelian'     , 303.7   ,
	'paleozoic', 'carboniferous', 'pennsylvanian', 'kasimovian'   , 307.0   ,
	'paleozoic', 'carboniferous', 'pennsylvanian', 'moscovian'    , 315.2   ,
	'paleozoic', 'carboniferous', 'pennsylvanian', 'bashkirian'   , 323.2   ,
	'paleozoic', 'carboniferous', 'mississippian', 'serpukhovian' , 330.9   ,
	'paleozoic', 'carboniferous', 'mississippian', 'viséan'       , 346.7   ,
	'paleozoic', 'carboniferous', 'mississippian', 'tournaisian'  , 358.9   ,
	'paleozoic', 'devonian'     , 'late'         , 'Famennian'    , 371.1   ,
	'paleozoic', 'devonian'     , 'late'         , 'Frasnian'     , 382.7   ,
	'paleozoic', 'devonian'     , 'middle'       , 'Givetian'     , 387.7   ,
	'paleozoic', 'devonian'     , 'middle'       , 'Eifelian'     , 393.3   ,
	'paleozoic', 'devonian'     , 'early'        , 'Emsian'       , 407.6   ,
	'paleozoic', 'devonian'     , 'early'        , 'Pragian'      , 410.8   ,
	'paleozoic', 'devonian'     , 'early'        , 'Lochkovian'   , 419.2   ,
	'paleozoic', 'silurian'     , 'pridoli'      , NA_character_  , 423.0   ,
	'paleozoic', 'silurian'     , 'ludlow'       , 'ludfordian'   , 425.6   ,
	'paleozoic', 'silurian'     , 'ludlow'       , 'gorstian'     , 427.4   ,
	'paleozoic', 'silurian'     , 'wenlock'      , 'homerian'     , 430.5   ,
	'paleozoic', 'silurian'     , 'wenlock'      , 'sheinwoodian' , 433.4   ,
	'paleozoic', 'silurian'     , 'llandovery'   , 'telychian'    , 438.5   ,
	'paleozoic', 'silurian'     , 'llandovery'   , 'aeronian'     , 440.8   ,
	'paleozoic', 'silurian'     , 'llandovery'   , 'rhuddanian'   , 443.8   ,
	'paleozoic', 'ordovician'   , NA_character_  , NA_character_  , 486.85  ,
	'paleozoic', 'cambrian'     , NA_character_  , NA_character_  , 538.8   ,
) |>
	dplyr::mutate(from = -abs(.data$from)) |>
	dplyr::mutate(to = .data$from |> dplyr::lag() |> tidyr::replace_na(0)) |>
	dplyr::mutate(dplyr::across(c('era', 'period', 'epoch', 'age'), stringr::str_to_sentence)) |>
	#
	# readr::read_tsv(
	# 	system.file("extdata", "geotime.tsv", package="arboretum"),
	# 	col_types = readr::cols(
	# 		from = readr::col_double(),
	# 		to = readr::col_double(),
	# 		.default = readr::col_character()
	# 	)
	# ) |>
	dplyr::mutate(dplyr::across(c('period', 'epoch'), stringr::str_to_sentence)) |>
	truncate_geotime_labels() |>
	dplyr::left_join(period_fill, by='period')

print(geotime)

geotime |> readr::write_tsv('inst/extdata/geotime.tsv')

geotime |> saveRDS('inst/extdata/geotime.rds')





