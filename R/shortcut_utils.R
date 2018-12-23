#' Determine the directory name for RStudio keybindings
#'
#' @return a string containing the name of the directory containing the keybindings
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

#' Build a list which contains three named lists, which are the names of the three
#' files ued for RStudio keyboard shortcuts
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

#' Grab the RStudio keyboard shortcuts and return them as a named list
#'
#' @param local
#'
#' @return list containing three named lists
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

#' Grab the Rstudio keyboard shortcuts from the local user file structure
#' and save them to a variable in the package
#'
#' @param shortcuts
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
capture_shortcuts_package <- function(shortcuts) {
	use_data(shortcuts, internal=F)
}

#' restores RStudio keyboard shortcuts from the data saved to the package
#' in the 'shortcuts' variable
#'
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
restore_keyboard_shortcuts <- function(shortcuts, local=T, overwrite=F) {
	load_all()
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
