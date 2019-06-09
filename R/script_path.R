#' @title Script Path
#'
#' @description Gives, in RStudio, the path to the script file in which it is executed.
#' @param subdir String, optional. Subdirectory relative to the script's path.
#' @return Script file's path as string. If \code{subdir} is given, it is
#'   appended to the original path.
#' @examples
#' setwd( script_path() ) # sets working directory to the script's path, e.g. "C:/script_folder/"
#'
#' script_path('my_subdir/misc/') # returns "C:/script_folder/my_subdir/misc/"
#' @export
script_path = function( subdir = '' ) {
    return( paste0( dirname(rstudioapi::getActiveDocumentContext()$path), '/', subdir ) )
}