#' Determines the host operating system
#' TODO: not clear if this works for windows
#' SOURCE: https://www.r-bloggers.com/identifying-the-os-from-r/
#'
#' @return
#' @export
#'
#' @examples
get_os <- function(){
	sysinf <- Sys.info()
	if (!is.null(sysinf)){
		os <- sysinf['sysname']
		if (os == 'Darwin')
			os <- "osx"
	} else { ## mystery machine
		os <- .Platform$OS.type
		if (grepl("^darwin", R.version$os))
			os <- "osx"
		if (grepl("linux-gnu", R.version$os))
			os <- "linux"
	}
	tolower(os)
}
