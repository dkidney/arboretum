#' \code{arboretum} package
#'
#' Package desc
#'
#' See \href{https://github.com/dkidney/arboretum#readme}{GitHub}
#'
#' Main function: TODO
#'
#' @name arboretum
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  {
	utils::globalVariables(c(
		".",
		".data"
	))
}
