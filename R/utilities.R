library(dplyr)
library(glue)
library(readr)
library(jsonlite)

# Determines the host operating system
# TODO: not cler if this works for windows
# SOURCE: https://www.r-bloggers.com/identifying-the-os-from-r/
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


#' Title
#'
#' @return
#' @export
#'
#' @examples
get_shortcut_dir <- function() {
	os <- get_os()
	dir <- case_when(
		os == "osx"   ~ "~/.R/rstudio/keybindings",
		os == "linux" ~ "~/.R/rstudio/keybindings",
		TRUE          ~ "UNKNOWN")
	return(dir)
}

#' Title
#'
#' @param local
#'
#' @return
#' @export
#'
#' @examples
get_shortcut_filenames <- function(local=F) {
	if (local) {
		dir = getwd()
	} else {
		dir = get_shortcut_dir()
	}

	# TODO: make this windows friendly
	addinsFile <- glue("{dir}/{file}", file="addins.json")
	editorFile <- glue("{dir}/{file}", file="editor_bindings.json")
	rstudioFile <- glue("{dir}/{file}", file="rstudio_bindings.json")

	return(list(addinsFile=addinsFile,editorFile=editorFile,rstudioFile=rstudioFile))
}

#' Title
#'
#' @param local
#'
#' @return
#' @export
#'
#' @examples
get_keyboard_shortcuts <- function(local=F) {
	f = get_shortcut_filenames(local)

	addins <- fromJSON(f$addinsFile)
	editor <- fromJSON(f$editorFile)
	rstudio <- fromJSON(f$rstudioFile)
	return(list(addins=addins, editor=editor, rstudio=rstudio))
}

#' Title
#'
#' @param shortcuts
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
save_shortcuts_package <- function(shortcuts) {
	use_data(shortcuts, internal=F)
}

#' Title
#'
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
write_keyboard_shortcuts <- function(shortcuts, local=T, overwrite=F) {
	f = get_shortcut_filenames(local)

	if (!overwrite) {
		if (file.exists(f$addinsFile)){
			system(glue("/bin/cp {file} {file}.bkp", file=f$addinsFile))
		}
		if (file.exists(f$editorFile)){
			system(glue("/bin/cp {file} {file}.bkp", file=f$editorFile))
		}
		if (file.exists(f$rstudioFile)){
			system(glue("/bin/cp {file} {file}.bkp", file=f$rstudioFile))
		}
	}

	write_lines(toJSON(shortcuts$addins), f$addinsFile)
	write_lines(toJSON(shortcuts$editor), f$editorFile)
	write_lines(toJSON(shortcuts$rstudio), f$rstudioFile)
}
