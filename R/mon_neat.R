#' Monitor
#'
#' This function assigns a monitor object.
#' @examples
#' mon_neat()
#' @export
mon_neat = function( distance, mon_width_cm, mon_width_pixel ) {
    mon_obj = list( distance = distance, mon_width_cm = mon_width_cm, mon_width_pixel = mon_width_pixel )
    attr( mon_obj, 'class' ) = 'mon_neat'
    return( mon_obj )
}

#' Pixels to degrees
#'
#' Given a specific monitor object, converts pixels to degrees.
#' @examples
#' pix2deg()
#' @export
pix2deg = function( mon_obj, pixels ) {
    UseMethod('pix2deg')
}

#' @export
pix2deg.default = function( mon_obj = NULL, pixels = NULL ) {
    cat('This function is to be used with "mon_neat" objects.\n')
}

#' @export 
pix2deg.mon_neat = function( mon_obj, pixels ) {
    size_cm = pixels * mon_obj$mon_width_cm / mon_obj$mon_width_pixel
    deg_res = ( atan((size_cm / 2) / mon_obj$distance ) * (180 / pi ) * 2 )
    return( deg_res )
}

#' Degrees to pixels
#'
#' Given a specific monitor object, converts degrees to pixels.
#' @examples
#' deg2pix()
#' @rdname deg2pix
#' @export deg2pix
deg2pix = function( mon_obj, pixels ) {
    UseMethod('deg2pix')
}

#' @export
deg2pix.default = function( mon_obj = NULL, pixels = NULL ) {
    cat('This function is to be used with "mon_neat" objects.\n')
}

#' @export
deg2pix.mon_neat = function( mon_obj, degrees ) {
    size_cm = mon_obj$distance * tan( degrees * ( pi / 180) / 2 ) * 2
    pix_res = ( size_cm / mon_obj$mon_width_cm * mon_obj$mon_width_pixel )
    return( pix_res )
}

#' Monitor parameters
#'
#' Prints the parameters of a given monitor object.
#' @examples
#' params()
#' @export
params = function( mon_obj ) {
    UseMethod('params')
}

#' @export
params.default = function( mon_obj = NULL ) {
    cat('This function is to be used with "mon_neat" objects.\n')
}

#' @export
params.mon_neat = function( mon_obj ) {
    cat( 'distance: ', mon_obj$distance, 
         '\nmon_width_cm: ', mon_obj$mon_width_cm, 
         '\nmon_width_pixel: ', mon_obj$mon_width_pixel, sep = '', fill = T )
}