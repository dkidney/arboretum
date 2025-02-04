for (pkg in c('tidyverse', 'devtools', 'usethis', 'shiny', 'bslib')) {
	if (!requireNamespace(pkg)) {
		install.packages(pkg)
	}
}
