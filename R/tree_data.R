# if (FALSE) {
# 	tree_data()
# 	tree_data(collapse=NULL)
# 	tree_data(collapse=NA)
# 	tree_data(collapse='none')
# 	tree_data(collapse='default')
# }

tree_data = function(taxon, collapse='default') {
	load_tree_data() |>
		subset(taxon) |>
		collapse(collapse) |>
		reorder_tree_data() |>
		identity()
}

# prepare_tree_data = function(taxon=NULL, collapse='none') {
# 	df = load_tree_data()
# 	# if (is.null(taxon)) {
# 	# 	taxon = 'tetrapodomorpha'
# 	# }
# 	if (!is.null(taxon)) {
# 		df = subset_taxon(df, tolower(taxon))
# 	}
# 	if (is.null(collapse)) {
# 		# collapsable_taxa = get_collapsable_taxa(df, taxon)
# 		# collapse = get_default_collapsed(df, collapsable_taxa)
# 		collapse = get_default_collapsed(df)
# 	} else {
# 		if (length(collapse) == 1) {
# 			if (collapse == 'none') return(df)
# 		}
# 	}
# 	df = collapse_taxa(df, collapse)
# 	# df = check_n_tips(df, max_tips)
# 	df = arrange_tree(df)
#
# 	df
# }

load_tree_data = function() {
	readRDS(system.file("extdata", "tree_data.rds", package="arboretum"))
}

#' @export
summary.tree_data = function(object, ...) {
	df = object
	# browser()

	# 	n_rows = nrow(df)
	# 	roots = get_roots(df)
	# 	n_roots = nrow(roots)
	# 	n_nodes = nrow(get_nodes(df))
	# 	n_tips = nrow(get_tips(df))
	# 	stopifnot((n_roots + n_nodes + n_tips) == nrow(df))

	out = list(
		n_roots = get_n_roots(df),
		roots = get_roots(df)$taxon,
		n_nodes = get_n_nodes(df),
		nodes = get_nodes(df)$taxon,
		n_tips = get_n_tips(df),
		tips = get_tips(df)$taxon,
		n_collapsed = get_n_collapsed(df),
		collapsed = get_collapsed(df)$taxon
	)

	if ((out$n_roots + out$n_nodes + out$n_tips) != nrow(df)) {
		browser()
	}

	stopifnot((out$n_roots + out$n_nodes + out$n_tips) == nrow(df))

	structure(out, class = c('summary_tree_data', class(out)))
}

# @export
# print.tree_data = function(x) {
# 	tmp = x
# 	class(tmp) = class(tmp)[class(tmp) != 'tree_data']
# 	print(tmp)
# 	print(summary(x))
# }


#' @export
print.summary_tree_data = function(x, ...) {
	# browser()
	# cat('\n')

	roots = paste0(x$n_roots, ' root')
	if (x$n_roots > 1) roots = paste0(roots, 's')
	roots = paste0(roots, ': ', paste(x$roots, collapse=", "))

	nodes = paste0(x$n_nodes, ' node')
	if (x$n_nodes > 1) nodes = paste0(nodes, 's')

	tips = paste0(x$n_tips, ' tip')
	if (x$n_tips > 1) tips = paste0(tips, 's')

	collapsed = paste0(x$n_collapsed, ' collapsed')
	if (x$n_collapsed > 0) {
		s = '\n- '
		collapsed = paste0(collapsed, ':', s, paste(x$collapsed, collapse=s))
	}

	x = paste(roots, nodes, tips, collapsed, sep='\n')

	cat(x, '\n')

	invisible(x)

	# cat(stringr::str_glue(
	# 	roots,
	# '{x$n_roots} root',
	# if (x$n_roots > 1) 's',
	# '{paste(x$roots, collapse="\n ")}\n',
	# '{x$n_nodes} node(s)\n',
	# '{paste(x$nodes, collapse="\n        ")}\n',
	# '{x$n_tips} tip(s)\n',
	# '- {paste(x$tips, collapse="\n- ")}\n',
	# '{x$n_collapsed} collapsed',
	# if (x$n_collapsed > 0) '\n- {paste(x$collapsed, collapse="\n- ")}\n'
	# '{n_tips} tips\n',
	# '{length(roots)} root(s):\n- ',
	# '{paste(roots, collapse="\n- ")}\n',
	# '{length(collapsed)} collapsed node(s):\n- ',
	# '{paste(collapsed, collapse="\n- ")}',
	# ))
}

is_root = function(df, taxon) {
	taxon %in% get_roots(df)$taxon
}

n_roots = function(df) {
	nrow(get_roots(df))
}

get_info = function(df, taxon) {
	# browser()
	i = df$taxon == taxon
	notes = df$notes[i]
	if (is.na(notes)) notes = character(0)
	label = stringr::str_to_sentence(df$taxon[i])
	if (!is.na(df$level[i])) label = paste0(label, ' (', df$level[i], ')')
	paste0(
		c(
			label,
			paste0(abs(df$from[i]), '-', abs(df$to[i]), ' Ma'),
			paste('clade:', stringr::str_to_sentence(df$parent[i])),
			# paste0('children:\n- ', paste(df$children[i][[1]], collapse='\n- '))
			notes
		),
		collapse='\n'
	)
}

#' @export
subset.tree_data = function(x, subset, ...) {
	if (missing(subset) || length(subset) == 0 || is.na(subset)) return(x)
	stopifnot(length(subset) == 1)
	df = x
	taxon = subset
	check_valid_taxon(df, taxon)
	if (is_root(df, taxon) && n_roots(df) == 1) return(df)
	# taxon = stringr::str_to_lower(taxon)
	# if (is_sole_root(df, taxon)) return(df)
	# taxa = c()
	# temp_taxa = c(taxon)
	# while (length(temp_taxa) > 0) {
	# 	taxa = c(taxa, temp_taxa)
	# 	temp_df = df[df$taxon %in% temp_taxa, ]
	# 	temp_taxa = temp_df$children |> unlist() #|> purrr::discard(is.na)
	# }
	# df = df[df$taxon %in% taxa, ]
	generations = list()
	generations[[1]] = taxon
	for (i in 1:nrow(df)) {
		generation = df$children[df$taxon %in% generations[[i]]] |> unlist()
		if (length(generation) == 0) break
		generations[[i + 1]] = generation
	}
	# generations |> unlist()

	# taxa = c()
	# children = taxon
	# while (length(children) > 0) {
	# 	taxa = c(taxa, children)
	# 	children = df$children[df$taxon %in% children] |> unlist()
	# }

	# browser()

	df = df |> dplyr::filter(.data$taxon %in% unlist(!!generations))

	# update indicator columns
	# browser()
	# df$children[df$taxon == taxon][[1]] = character(0)

	df = update_indicator_columns(df)

	# df |>
	# 	dplyr::mutate(is_root = !.data$taxon %in% unlist(.data$children)) |>
	# 	dplyr::mutate(is_tip = purrr::map_int(.data$children, length) == 0) |>
	# 	dplyr::mutate(is_node = !.data$is_root & !.data$is_tip)

	# attr(df, 'root') = taxon
	# attr(df, 'collapsed') = attr(df, 'collapsed') |> purrr::keep(~.x %in% df$taxon)

	df
}

update_indicator_columns = function(df) {
	if (!'is_collapsed' %in% colnames(df)) df$is_collapsed = FALSE
	df |>
		dplyr::mutate(is_root = !.data$taxon %in% unlist(.data$children)) |>
		dplyr::mutate(is_tip = purrr::map_int(.data$children, length) == 0) |>
		dplyr::mutate(is_node = !.data$is_root & !.data$is_tip)
}

collapse = function(df, taxa=NULL, max_tips=100) {
	# browser()
	if (missing(taxa) || is.null(taxa) || (length(taxa) == 1 && is.na(taxa))) taxa = 'default'
	if (length(taxa) == 1 && taxa == 'none') return(df)
	node_taxa = get_nodes(df)$taxon
	# if (length(taxa) == 1 && taxa == 'default' && get_n_tips(df) > 20){
	if (length(taxa) == 1 && taxa == 'default'){
		# taxa = get_default_collapsed(df)
		taxa = node_taxa |> purrr::keep(stringr::str_detect, '(morpha|formes|oidea|idae|inae|ini|ina)$')
	} else {
		taxa = taxa |> purrr::keep(~.x %in% node_taxa)
	}
	if (length(taxa) == 0) return(df)
	# browser()
	# get_unnested_taxa(df, taxa)
	# taxa = taxa[!is_nested(df, taxa)]
	# only collapse node taxa - i.e. can't collapse tips or root(s)
	# taxa = taxa |> purrr::keep(~.x %in% get_nodes(df, ignore_roots = TRUE)$taxon)
	# if (length(taxa) == 0) return(df)
	# message('collapsing ', length(taxa), ' ',
	# 		if (length(taxa) == 1) 'taxon' else 'taxa',
	# 		':\n- ', stringr::str_c(taxa, collapse='\n- '))
	# for (taxon in taxa) df = collapse_taxon(df, taxon)


	# remove rows for decendants
	# for (taxon in taxa) {
	# 	# clade = subset(df, taxon)$taxon
	# 	# descendants = clade[clade != taxon]
	#
	# 	# remove rows for decendants
	# 	descendants = get_descendants_taxon(df, taxon)
	# 	df = df[!df$taxon %in% descendants$taxon,]
	# 	df$children[df$taxon == taxon][[1]] = character(0)
	# 	df$is_collapsed[df$taxon == taxon][[1]] = TRUE
	# 	# j = df$taxon %in% taxon
	# 	# df$plot[i] = FALSE
	# 	# df[i, c('collapsed', 'is_root', 'is_tip', 'is_node')] = NA
	# 	# df[j, c('collapsed', 'is_tip')] = TRUE
	# 	# df$is_node[j] = FALSE
	# }

	# remove rows for decendants
	# browser()
	descendants = get_descendants_taxa(df, taxa)
	df = df[!df$taxon %in% descendants$taxon,]
	df$children[df$taxon %in% taxa] = list(character(0))
	df$is_collapsed[df$taxon %in% taxa] = TRUE

	n_tips = get_n_tips(df)
	if(n_tips > max_tips) {
		stop('number of tips (', n_tips, ') exceeds max_tips (', max_tips, ')')
	}

	df = update_indicator_columns(df)

	# df = update_label_x(df)

	# df$is_collapsed[df$taxon %in% c(get_descendants(df, taxa)$taxon, taxa)] = TRUE
	# decendants_of_collapsed_taxa = get_descendants(df, taxa)$taxon
	# df$is_collapsed[df$taxon %in% decendants_of_collapsed_taxa] = TRUE
	# attr(df, 'collapsed') = taxa

	# df |> dplyr::arrange(dplyr::desc(.data$plot), .data$taxon)

	df
}

get_hierarchy = function(taxon){
	df = load_tree_data()
	hierarchy = stringr::str_to_lower(taxon)
	for (i in 1:nrow(df)) { # i=1
		parent = df$parent[df$taxon == hierarchy[i]]
		if (is.na(parent)) break
		hierarchy = c(hierarchy, parent)
	}
	hierarchy
}

# update_label_x = function(df, xmin=NULL, xmax=NULL) {
# 	xmin = if (is.numeric(xmin)) -abs(xmin) else df$from
# 	xmax = if (is.numeric(xmax)) -abs(xmax) else df$to
# 	df |>
# 		dplyr::mutate(
# 			label_x = dplyr::case_when(
# 				.data$is_tip ~ (pmax(.data$from, xmin) + pmin(.data$to, xmax)) / 2,
# 				.default = .data$from
# 			)
# 		)
# }

# # given a list
# is_nested = function(df, taxa) {
# 	nested = rep(FALSE, length(taxa))
# 	for (i in seq_along(taxa)) { # i=1
# 		if (nested[i]) next
# 		descendants = get_descendants_taxa(df, taxa[i])$taxon
# 		nested[taxa %in% descendants] = TRUE
# 	}
# 	nested
# }

# get_default_collapsed = function(df) {
# 	taxa = get_nodes(df, ignore_roots=TRUE)$taxon
# 	# taxa = taxa[taxa != get_roots(df)$taxon]
# 	taxa = taxa[stringr::str_detect(taxa, '(morpha|formes|oidea|ae)$')]
# 	if (length(taxa) == 0) return(character(0))
# 	nested = rep(FALSE, length(taxa))
# 	for (i in seq_along(taxa)) { # i=1
# 		if (nested[i]) next
# 		descendants = get_descendants_taxon(df, taxa[i])$taxon
# 		nested[taxa %in% descendants] = TRUE
# 	}
# 	taxa = sort(taxa[!nested])
# 	# message('collapsing ', length(taxa), ' taxa by default:\n\t',
# 	# 		stringr::str_c(taxa, collapse='\n\t'))
# 	taxa
# 	# selected_taxa = taxa
# 	# list(
# 	# 	choices = collapsable_taxa,
# 	# 	selected = selected_taxa
# 	# )
# }

reorder_tree_data = function(df) { # df = plt$data ; print(df, n=Inf)
	# browser()
	# collapsed = df |> dplyr::filter(.data$is_collapsed)
	# df = df |> dplyr::filter(!.data$is_collapsed)
	# print(class(df))
	df$ranks = purrr::map(1:nrow(df), function(x) integer(0))
	for (i in 1:nrow(df)) { # i=44
		ranks = integer(0)
		taxon = df$taxon[i]
		for (j in 1:nrow(df)) {
		# while (length(taxon) > 0) {
			# print(taxon)
			# if (taxon == 'tetrapodomorpha') browser()
			siblings = get_siblings(df, taxon)
			rank = rank(siblings$from, ties.method='first')[siblings$taxon == taxon]
			ranks = c(rank, ranks)
			if (is_root(df, taxon)) break
			taxon = get_parent(df, taxon)$taxon
		}
		df$ranks[[i]] = ranks
	}
	# browser()S
	# print(class(df))
	max_depth = df$ranks |> purrr::map_int(length) |> max()
	rank_taxa = stringr::str_c('rank', 1:max_depth)
	classes = class(df)
	df = df |>
		dplyr::mutate(
			ranks = .data$ranks |>
				purrr::map(function(x) {
					zeros = rep(0, max_depth - length(x))
					c(x, zeros) |>
						purrr::set_names(rank_taxa) |>
						as.list() |>
						dplyr::as_tibble()
				})
		) |>
		tidyr::unnest(.data$ranks) |>
		dplyr::arrange(dplyr::across(dplyr::one_of(!!rank_taxa))) |>
		dplyr::select(-dplyr::one_of(!!rank_taxa))|>
		add_y() |>
		# dplyr::bind_rows(collapsed) |>
		identity()
	structure(df, class = classes)
	# 	# print(class(df))
	# stopifnot(inherits(df, 'tree_data'))
	# df
}

check_valid_taxon = function(df, taxon, warn=FALSE) {
	stopifnot(is.character(taxon), length(taxon) == 1)
	if(taxon %in% df$taxon) return(invisible(df))
	msg = stringr::str_c("invalid taxon: '", taxon, "'")
	if (warn) {
		warning(msg)
	} else {
		stop(msg)
	}
}

get_roots = function(df) {
	df |> dplyr::filter(.data$is_root)
	# attr(df, 'roots')
}

get_collapsed = function(df, unnested=TRUE) {
	# browser()
	# # attr(df, 'collapsed')
	# df = df |> dplyr::filter(.data$is_collapsed)
	# if (unnested) {
	# 	browser()
	# 	df = get_unnested(df)
	# }
	# df
	df |> dplyr::filter(.data$is_collapsed)
}

# get_unnested = function(df) {
# 	df[!df$taxon %in% unlist(df$children), ]
# }
#
# get_unnested_taxa = function(df, taxa) {
# 	browser()
# 	df |>
# 		dplyr::filter(.data$taxon %in% !!taxa) |>
# 		dplyr::filter(!.data$taxon %in% unlist(.data$children))
# 	df = df[df$taxon %in% taxa, ]
# 	df$taxon[!df$taxon %in% unlist(df$children)]
# }

get_tips =function(df) {
	# include collapsed as tips
	# df = df |> dplyr::filter(!.data$is_collapsed)
	# df[!has_children(df), ]
	df |> dplyr::filter(.data$is_tip)
}

get_nodes =function(df) {
	# df = df |> dplyr::filter(!.data$is_collapsed)
	# if (!include_roots) {
	# 	df = df |> dplyr::filter(!is.na(.data$parent))
	# }
	# df[has_children(df), ]
	df |> dplyr::filter(.data$is_node)
}

get_n_roots = function(df) {
	sum(df$is_root)
}

get_n_nodes = function(df) {
	sum(df$is_node)
}

get_n_tips = function(df) {
	sum(df$is_tip)
}

get_n_collapsed = function(df) {
	sum(df$is_collapsed)
}

# has_children = function(df, include_collapsed=FALSE){
# 	# treat collapsed nodes as having no children
# 	# browser()
# 	i = purrr::map_int(df$children, length) > 0
# 	if (!include_collapsed) {
# 		i = i & !df$taxon %in% get_collapsed(df)
# 	}
# 	i
# }

get_children = function(df, taxon) {
	# check_valid_taxon(df, taxon)
	# children =  df$children[df$taxon == taxon][[1]]
	# df[df$taxon %in% children, ]
	df |> dplyr::filter(.data$parent %in% !!taxon)
}

get_descendants_taxon = function(df, taxon) {
	check_valid_taxon(df, taxon)
	df |>
		subset(taxon) |>
		dplyr::filter(.data$taxon != !!taxon)
}

get_descendants_taxa = function(df, taxa) {
	# todo: make this more efficient
	# browser()
	decendants = rep(FALSE, nrow(df))
	for (taxon in taxa) {
		i = df$taxon == taxon
		if (decendants[i]) next
		# i = df$taxon %in% subset(df, taxon)$taxon
		i = df$taxon %in% get_descendants_taxon(df, taxon)$taxon
		decendants[i] = TRUE
	}
	df[decendants, ]
	# taxa |>
	# 	purrr::map_df(function(taxon) get_descendants_taxon(df, taxon)) |>
	# 	dplyr::distinct()
}

get_parent = function(df, taxon) {
	check_valid_taxon(df, taxon)
	# i = df$children |> purrr::map_lgl(~taxon %in% .x)
	# stopifnot(sum(i) <= 1)
	# df[i, ]
	i =
		parent = df$parent[df$taxon == taxon]
	df |> dplyr::filter(.data$taxon %in% !!parent)
}

get_siblings = function(df, taxon) {
	# check_valid_taxon(df, taxon)
	if (is_root(df, taxon)) {
		return(get_roots(df))
	}
	parent = get_parent(df, taxon)
	get_children(df, parent$taxon)
}

# is_root = function(df, taxon) {
# 	# check_valid_taxon(df, taxon)
# 	nrow(get_parent(df, taxon)) == 0
# }

# is_sole_root = function(df, taxon) {
# 	# check_valid_taxon(df, taxon)
# 	roots = get_roots(df)
# 	return(length(roots) == 1 && taxon == roots)
# }

check_tree_data = function(df) {

	stopifnot(inherits(df, 'tree_data'))

	# check taxa unique
	errors = df$taxon |> table()
	errors = errors[errors > 1]
	if (length(errors) > 0) {
		stop('non-unique taxa: ', paste(names(errors), collapse=', '))
	}

	children = df$children |> unlist() #|> purrr::discard(is.na)

	# check each child has only parent
	errors = children |> table()
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
		parent = df |> get_parent(df$taxon[i])
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
		warning('multiple roots:\n- ', stringr::str_c(sort(roots$taxon), collapse = '\n- '))
	}

	invisible(df)

}

# tree_data_summary = function(df) {
# 	n_rows = nrow(df)
# 	roots = get_roots(df)
# 	n_roots = nrow(roots)
# 	n_nodes = nrow(get_nodes(df))
# 	n_tips = nrow(get_tips(df))
# 	stopifnot((n_roots + n_nodes + n_tips) == nrow(df))
# 	paste(
# 		'tree data summary:',
# 		paste0('- n rows : ', n_rows),
# 		paste0('- n roots: ', n_roots, ' (', paste(roots$taxon, collapse=','), ')'),
# 		paste0('- n nodes: ', n_nodes),
# 		paste0('- n tips : ', n_tips),
# 		sep = '\n'
# 	)
# 	# message('tree data summary:')
# 	# message('- n rows : ', n_rows)
# 	# message('- n roots: ', n_roots, ' (', paste(roots$taxon, collapse=','), ')')
# 	# message('- n nodes: ', n_nodes)
# 	# message('- n tips : ', n_tips)
# }

# collapse_taxon = function(df, taxon) {
# 	browser()
# 	if (is.null(taxon) | is.na(taxon) | taxon == '') return(df)
# 	if (!taxon %in% df$taxon) return(df)
# 	# check_valid_taxon(df, taxon, warn=TRUE)
#
# 	# sub = subset_taxon(df, taxon)
# 	# taxa_to_drop = sub$taxon[sub$taxon != taxon]
#
# 	# set collapsed column to 1 for all taxon descendants
# 	descendant_taxa = get_descendants_taxon(df, taxon)$taxon
# 	df$is_collapsed[df$taxon %in% descendant_taxa] = TRUE
# 	# df = df[!df$taxon %in% descendants$taxon, ]
#
# 	# update row for collapsed taxon
# 	# i = df$taxon == taxon
# 	# df$to[i] = max(c(descendants$to, df$to[i]), na.rm=TRUE)
# 	# df$children[i] = list(character(0))
# 	return(df)
# }

add_y = function(df) {
	# browser()
	# df = dplyr::bind_rows(
	# 	df |> get_tips() |> dplyr::mutate(y = dplyr::row_number()),
	# 	df |> get_nodes(include_roots=TRUE)
	# )
	df$y = NA_integer_
	# i = !df$is_tip

	# browser()

	n_tips = get_n_tips(df)
	df$y[df$is_tip] = 1:n_tips

	# new_ymin = 1 / n_tips
	# new_ymax = 1 - 1 / n_tips
	df$y = df$y |> rescale(0, 1)


	# df = df |>
	# 	dplyr::group_by(is_node) |>
	# 	dplyr::mutate(y = dplyr::row_number())
	# df$is_tip

	# browser()
	while (any(is.na(df$y))) {
		is = which(is.na(df$y))
		for (i in is) { #i=1
			# print(df)
			# print(i)
			# print(df$taxon[i])
			if (!is.na(df$y[i])) next
			# df[i, ]
			children = df$children[[i]]
			children_ys = df$y[df$taxon %in% children]
			if (any(is.na(children_ys))) next
			df$y[i] = mean(children_ys)
		}
	}

	# browser()
	df |>
		# dplyr::select(.data$y, dplyr::everything()) |>
		# dplyr::arrange(desc(is_tip), desc(y)) |>
		dplyr::arrange(dplyr::desc(.data$y)) |>
		rescale_y()
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

