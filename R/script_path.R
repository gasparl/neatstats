#' Neat path
#'
#' This function gives the path to current script's path in RStudio.
#' @keywords path
#' @export
#' @examples
#' script_path()
script_path = function() {
    return( dirname(rstudioapi::getActiveDocumentContext()$path) )
}