# Load tree data
#
# @return A tibble
# @export
#
# library(arboretum)
# df = load_tree_data()
# df %>% get_roots()
#
# taxon = 'theropoda'
#
# df %>% is_root(taxon)
# df %>% get_parent(taxon)
# df %>% get_siblings(taxon)
# df %>% get_children(taxon)
#
# df2 = df %>% subset_taxon(taxon)
# df2 %>% is_root(taxon)
# df2 %>% get_parent(taxon)
# df2 %>% get_siblings(taxon)
# df2 %>% get_children(taxon)
#
# df3 = df2 %>% collapse_taxa('tetanurae')
# df3 %>% print(n=Inf)
# df3 %>% get_tips()
# df3 %>% get_nodes()
# df3
# df3 %>% arrange_tree()
#
load_tree_data = function() {
	readr::read_tsv(
		system.file("extdata", "tree_data.tsv", package="arboretum"),
		col_types = readr::cols(
			from = readr::col_double(),
			to = readr::col_double(),
			.default = readr::col_character()
		)
	) %>%
		dplyr::mutate(dplyr::across(c(.data$from, .data$to), ~-abs(.x))) %>%
		dplyr::mutate(dplyr::across(c(.data$taxon, .data$children), stringr::str_to_lower)) %>%
		dplyr::mutate(children = .data$children %>% tidyr::replace_na('') %>% strsplit(split='\\s*,\\s*')) %>%
		dplyr::mutate(asterisk = .data$taxon %>% stringr::str_detect('\\*$')) %>%
		dplyr::mutate(taxon = .data$taxon %>% stringr::str_remove('\\*$')) %>%
		dplyr::arrange(.data$taxon) %>%
		check_tree_data() %>%
		identity()
}

check_taxon = function(df, taxon, warn=FALSE) {
	stopifnot(is.character(taxon), length(taxon) == 1)
	if(!taxon %in% df$taxon) {
		msg = stringr::str_c("invalid taxon: '", taxon, "'")
		if (warn) {
			warning(msg)
		} else {
			stop(msg)
		}
	}
	invisible(df)
}

get_roots = function(df) {
	df[!df$taxon %in% unlist(df$children), ]
}

get_tips =function(df) {
	df[purrr::map_int(df$children, length) == 0, ]
}

get_nodes =function(df) {
	df[purrr::map_int(df$children, length) > 0, ]
}

get_children = function(df, taxon) {
	check_taxon(df, taxon)
	children =  df$children[df$taxon == taxon][[1]]
	df[df$taxon %in% children, ]
}

get_siblings = function(df, taxon) {
	check_taxon(df, taxon)
	parent = get_parent(df, taxon)
	if (nrow(parent) == 0) {
		return(get_roots(df))
	}
	get_children(df, parent$taxon)
}

get_parent = function(df, taxon) {
	check_taxon(df, taxon)
	i = df$children %>% purrr::map_lgl(~taxon %in% .x)
	stopifnot(sum(i) <= 1)
	df[i, ]
}

is_root = function(df, taxon) {

	check_taxon(df, taxon)
	nrow(get_parent(df, taxon)) == 0
}

# Subset taxon
#
# Get a subset of tree data for a particular taxon
#
# @param df a data frame of tree data (tibble)
# @param taxon taxonomic name (string)
# @return A tibble
# @export
subset_taxon = function(df, taxon) {
	if (is.null(taxon)) return(df)
	taxa = c()
	temp_taxa = c(taxon)
	while (length(temp_taxa) > 0) {
		taxa = c(taxa, temp_taxa)
		temp_df = df[df$taxon %in% temp_taxa, ]
		temp_taxa = temp_df$children %>% unlist() #%>% purrr::discard(is.na)
	}
	df[df$taxon %in% taxa, ]
}

check_tree_data = function(df) {

	# check taxa unique
	errors = df$taxon %>% table
	errors = errors[errors > 1]
	if (length(errors) > 0) {
		stop('non-unique taxa: ', names(errors))
	}

	children = df$children %>% unlist #%>% purrr::discard(is.na)

	# check each child has only parent
	errors = children %>% table
	errors = errors[errors > 1]
	if (length(errors) > 0) {
		stop('children with multiple parents: ', stringr::str_c(names(errors), collapse = ', '))
	}

	# check children are in taxa
	errors = children[!children %in% df$taxon]
	if(length(errors) > 0) {
		stop('children not in taxa: ', stringr::str_c(errors, collapse = ', '))
	}

	# check from
	errors = c()
	for (i in 1:nrow(df)) { #i=1
		parent = df %>% get_parent(df$taxon[i])
		if (nrow(parent) == 0) next
		if (parent$from > df$from[i]) {
			error = stringr::str_glue('{parent$taxon} < {df$taxon[i]} ({parent$from} < {df$from[i]})')
			errors = c(errors, error)
		}
	}
	if(length(errors) > 0) {
		stop('parents younger than children:\n', stringr::str_c(errors, collapse = '\n'))
	}

	# check roots
	roots = get_roots(df)
	if (nrow(roots) > 1) {
		warning('multiple roots: ', stringr::str_c(sort(roots$taxon), collapse = '\n'))
	}

	invisible(df)

}

collapse_taxon = function(df, taxon) {
	if (is.null(taxon)) return(df)
	check_taxon(df, taxon, warn=TRUE)
	sub = subset_taxon(df, taxon)
	taxa_to_drop = sub$taxon[sub$taxon != taxon]
	df = df[!df$taxon %in% taxa_to_drop, ]
	i = df$taxon == taxon
	df$to[i] = max(c(sub$to, df$to[i]), na.rm=TRUE)
	df$children[i] = list(character(0))
	return(df)
}

collapse_taxa = function(df, taxa) {
	if (is.null(taxa)) return(df)
	taxa = taxa[taxa %in% df$taxon]
	for (taxon in taxa) {
		df = collapse_taxon(df, taxon)
	}
	return(df)
}

collapse_families = function(df) {
	families = df$taxon %>% purrr::keep(stringr::str_detect, 'idae$')
	collapse_taxa(df, taxa=families)
}

add_y = function(df) {
	df = dplyr::bind_rows(
		df %>% get_tips() %>% dplyr::mutate(y = 1:nrow(.)),
		df %>% get_nodes()
	)
	while (any(is.na(df$y))) {
		for (i in 1:nrow(df)) { #i=1
			if (!is.na(df$y[i])) next
			children = df$children[[i]]
			children_ys = df$y[df$taxon %in% children]
			if (any(is.na(children_ys))) next
			df$y[i] = mean(children_ys)
		}
	}
	df %>%
		dplyr::select(.data$y, dplyr::everything()) %>%
		# dplyr::arrange(desc(is_tip), desc(y)) %>%
		dplyr::arrange(dplyr::desc(.data$y)) %>%
		identity
}

# don't export this yet
add_depth = function(df) {
	depth = 1
	df$depth = NA_integer_
	children = get_roots(df)$taxon
	while (length(children) > 0) {
		df$depth[df$taxon %in% children] = depth
		children = unlist(df$children[df$taxon %in% children])
		depth = depth + 1
	}
	return(df)
}

arrange_tree = function(df) { # df = plt$data ; print(df, n=Inf)
	df$ranks = purrr::map(1:nrow(df), function(x) integer(0))
	for (i in 1:nrow(df)) { # i=44
		ranks = integer(0)
		taxon = df$taxon[i]
		while (length(taxon) > 0) {
			siblings = get_siblings(df, taxon)
			rank = rank(siblings$from)[siblings$taxon == taxon]
			ranks = c(rank, ranks)
			taxon = get_parent(df, taxon)$taxon
		}
		df$ranks[[i]] = ranks
	}
	max_depth = df$ranks %>% purrr::map_int(length) %>% max
	rank_taxa = stringr::str_c('rank', 1:max_depth)
	df %>%
		dplyr::mutate(
			ranks = .data$ranks %>%
				purrr::map(function(x) {
					zeros = rep(0, max_depth - length(x))
					c(x, zeros) %>%
						purrr::set_names(rank_taxa) %>%
						as.list() %>%
						dplyr::as_tibble()
				})
		) %>%
		tidyr::unnest(.data$ranks) %>%
		dplyr::arrange(dplyr::across(dplyr::one_of(!!rank_taxa))) %>%
		dplyr::select(-dplyr::one_of(!!rank_taxa))	%>%
		add_y()
}
