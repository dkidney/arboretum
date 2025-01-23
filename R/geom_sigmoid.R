# Geom sigmoid
#
#@export
#
#@examples
# library(tidyverse)
# df = tribble(
#     ~x1, ~x2, ~y1, ~y2, ~col,
#     0, 1, 0, 1, 'blue',
#     0, 1, 0, -1, 'green',
# )
# df %>%
# ggplot() +
# geom_sigmoid(aes(x=x1, xend=x2, y=y1, yend=y2, col=col), c1=0.001, c2=.5, n=101) +
# scale_colour_manual(values = df$col %>% set_names(df$col)) +
# geom_point(aes(x=x1, y=y1)) +
# geom_point(aes(x=x2, y=y2), col='red')
geom_sigmoid <- function(
		mapping = NULL, data = NULL, geom = "line",
		position = "identity", na.rm = FALSE, show.legend = NA,
		n = 20, c1 = 0.1, c2 = 0, inherit.aes = TRUE,
		...
) {
	ggplot2::layer(
		stat = StatSigmoid, data = data, mapping = mapping, geom = geom,
		position = position, show.legend = show.legend, inherit.aes = inherit.aes,
		params = list(na.rm = na.rm, n = n, c1 = c1, c2 = c2,  ...)
	)
	# layer(
	# 	geom = GeomSigmoid, data = data, mapping = mapping, stat = stat,
	# 	position = position, show.legend = show.legend, inherit.aes = inherit.aes,
	# 	params = list(na.rm = na.rm, n = n, c1 = c1, ...)
	# )
}

StatSigmoid <- ggplot2::ggproto(
	"StatSigmoid", ggplot2::Stat,
	required_aes = c("x", "y", "xend", "yend"),
	setup_data = function(data, params) {
		data %>%
			dplyr::group_by(PANEL) %>%
			dplyr::mutate(group = dplyr::row_number())
	},
	compute_group = function(data, scales, n, c1, c2) {
		sigmoid(data$x, data$xend, data$y, data$yend, n = n, c1 = c1, c2 = c2)
	}
)

# GeomSigmoid <- ggproto(
# 	"GeomSigmoid", Geom,
# 	required_aes = c("x", "y", "xend", "yend"),
# 	setup_data = function(data, params) {
# 		data %>%
# 			group_by(PANEL) %>%
# 			mutate(group = row_number())
# 	},
# 	compute_group = function(data, scales, n, l) {
# 		sigmoid(data$x, data$xend, data$y, data$yend, n = n, c1 = c1)
# 	}
# )

# rescale(0:10, 0, 1)
# rescale(0:10, 1, 0)
rescale = function(x, new_min, new_max) {
	y = (x - min(x)) / diff(range(x))
	y = y * (new_max - new_min)
	y + new_min
}

# n=9
# plot(c(-1, 1), c(-1, 1), type='n', xlab='x', ylab='y')
#
# s1 = sigmoid(x1=0, y1=0, x2=1, y2=1, n=n)
# lines(s1, col=1)
# points(s1, pch=as.character(s1$i), col=1)
#
# s2 = sigmoid(x1=0, y1=.5, x2=.5, y2=-.5, n=n, c1=0.001)
# lines(s2, col=2)
# points(s2, pch=as.character(s2$i), col=2)
#
# s3 = sigmoid(x1=-.5, y1=.5, x2=-1, y2=1, n=n, c2=1)
# lines(s3, col=3)
# points(s3, pch=as.character(s3$i), col=3)
#
# s4 = sigmoid(x1=-.2, y1=-.2, x2=-.8, y2=-.8, n=n, c1=0.001, c2=-1)
# lines(s4, col=4)
# points(s4, pch=as.character(s4$i), col=4)
#
#
# plot(sigmoid(0, 10,  0, 10, n=n), type=type, pch=pch)
# plot(sigmoid(1,  0,  1,  0, n=n), type=type, pch=pch)
# plot(sigmoid(10, 0, 10,  0, n=n), type=type, pch=pch)
#
# pt='l'
# plot(sigmoid(0, 1, 0, 1, n=2), type=pt)
#
# plot(sigmoid(0, 1, 0, 1, n=1001, c1=1), type=pt)
# plot(sigmoid(0, 1, 0, 1, n=1001, c1=0.2), type=pt)
# plot(sigmoid(0, 1, 0, 1, n=1001, c1=0.1), type=pt)
# plot(sigmoid(0, 1, 0, 1, n=1001, c1=0.05), type=pt)
# plot(sigmoid(0, 1, 0, 1, n=1001, c1=0.001), type=pt)
#
# plot(sigmoid(0, 1, 0, 1, c2=1), type=pt)
# plot(sigmoid(0, 1, 0, 1, c2=0), type=pt)
# plot(sigmoid(0, 1, 0, 1, c2=-1), type=pt)
#
# plot(sigmoid(0, 1, 0, 1, c2= 1.0, c1=0.001, n=1001), type=pt)
# plot(sigmoid(0, 1, 0, 1, c2= 0.5, c1=0.001, n=1001), type=pt)
# plot(sigmoid(0, 1, 0, 1, c2= 0.0, c1=0.001, n=1001), type=pt)
# plot(sigmoid(0, 1, 0, 1, c2=-0.5, c1=0.001, n=1001), type=pt)
# plot(sigmoid(0, 1, 0, 1, c2=-1.0, c1=0.001, n=1001), type=pt)
#
# plot(sigmoid(0, 1, 0, 1, c1=0.01, c2=-1), type=pt)
# plot(sigmoid(0, 1, 0, 1, c1=0.01, c2=0), type=pt)
# plot(sigmoid(0, 1, 0, 1, c1=0.01, c2=1), type=pt)
#
# plot(sigmoid(0, 1, 0, 1, c2=-10), type=pt)
# plot(sigmoid(0, 1, 0, 1, c2=0), type=pt)
# plot(sigmoid(0, 1, 0, 1, c2=10), type=pt)
#
# plot(sigmoid(0, 1, 0, 1, n=3, c1=10, c2=100), type=pt)

sigmoid = function(x1, x2, y1, y2, n=21, c1=0.1, c2=0) {
	# x1=0; x2=1; y1=0; y2=1; n=21; c1=0.1; c2=0
	stopifnot(c1 > 0, c1 <= 10)
	stopifnot(c2 >= -1, c2 <= 1)
	n = as.integer(n)
	stopifnot(n > 1)
	xs = seq(-1, 1, length.out=n)
	# ys = 1 / (1 + exp(-c1 * (xs - c2)))
	ys = 1 / (1 + exp(-1/c1 * (xs - c2)))
	xs = rescale(xs, x1, x2)
	ys = rescale(ys, y1, y2)
	dplyr::tibble(x=xs, y=ys, i=seq_along(ys)) %>%
		dplyr::mutate(y = round(.data$y, 5)) %>%
		dplyr::group_by(.data$y) %>%
		dplyr::mutate(
			d1 = duplicated(.data$y),
			d2 = duplicated(.data$y, fromLast=TRUE)
		) %>%
		dplyr::ungroup() %>%
		dplyr::filter(!(.data$d1 & .data$d2)) %>%
		dplyr::select(.data$x, .data$y, .data$i)
}




