#' @title Monitor
#'
#' @description Assigns a monitor object, storing distance and width
#'   parameters.
#' @param distance Viewing distance in cm (from eyes to screen).
#' @param mon_width_cm Monitor screen width in cm.
#' @param mon_width_pixel Monitor screen width in pixels.
#' @return A monitor object with the specified parameters, to be used in the \code{\link{mon_pix2deg}}, \code{\link{mon_deg2pix}}, and \code{\link{mon_params}} functions.
#' @examples
#' # assign monitor with 57 cm viewing distance, and screen width 52 cm and 1920 pixels
#' my_mon = mon_neat(distance = 57, mon_width_cm = 52, mon_width_pixel = 1920)
#' @seealso \code{\link{mon_pix2deg}}, \code{\link{mon_deg2pix}}, \code{\link{mon_params}}
#' @export
mon_neat = function( distance, mon_width_cm, mon_width_pixel ) {
    mon_obj = list( distance = distance, mon_width_cm = mon_width_cm, mon_width_pixel = mon_width_pixel )
    attr( mon_obj, 'class' ) = 'mon_neat'
    return( mon_obj )
}

#' @title Pixels to degrees
#'
#' @description Given a specific monitor object, converts pixels to visual angle
#'   degrees.
#' @param mon_obj Monitor object, as assigned with \code{\link{mon_neat}}.
#' @param pixels Number of pixels.
#' @return Number of degrees for the given number of pixels.
#' @examples
#' # assign monitor with 57 cm distance, and screen width 52 cm and 1920 pixels
#' my_mon = mon_neat(distance = 57, mon_width_cm = 52, mon_width_pixel = 1920)
#'
#' # convert 30.4 pixels to degrees of visual angle, for the specified monitor
#' mon_pix2deg( my_mon, 30.4 ) # returns 0.8275913 (degrees)
#' @seealso \code{\link{mon_neat}}, \code{\link{mon_deg2pix}},
#'   \code{\link{mon_params}}
#' @export
mon_pix2deg = function( mon_obj, pixels ) {
    UseMethod('mon_pix2deg')
}

#' @export
mon_pix2deg.default = function( mon_obj = NULL, pixels = NULL ) {
    cat('This function is to be used with "mon_neat" objects.\n')
}

#' @export
mon_pix2deg.mon_neat = function( mon_obj, pixels ) {
    size_cm = pixels * mon_obj$mon_width_cm / mon_obj$mon_width_pixel
    deg_res = ( atan((size_cm / 2) / mon_obj$distance ) * (180 / pi ) * 2 )
    return( deg_res )
}


#' @title Degrees to pixels
#'
#' @description Given a specific monitor object, converts visual angle degrees
#'   to pixels.
#' @param mon_obj Monitor object, as assigned with \code{\link{mon_neat}}.
#' @param pixels Number of degrees.
#' @return Number of pixels for the given number of degrees.
#' @examples
#' # assign monitor with 57 cm distance, and screen width 52 cm and 1920 pixels
#' my_mon = mon_neat(distance = 57, mon_width_cm = 52, mon_width_pixel = 1920)
#'
#' # convert 0.8 degrees of visual angle to pixels, for the specified monitor
#' mon_deg2pix( my_mon, 0.8 ) # returns 29.38645 (pixels)
#' @seealso \code{\link{mon_neat}}, \code{\link{mon_deg2pix}},
#'   \code{\link{mon_params}}
#' @export
mon_deg2pix = function( mon_obj, degrees ) {
    UseMethod('mon_deg2pix')
}

#' @export
mon_deg2pix.default = function( ... ) {
    cat('This function is to be used with "mon_neat" objects.\n')
}

#' @export
mon_deg2pix.mon_neat = function( mon_obj, degrees ) {
    size_cm = mon_obj$distance * tan( degrees * ( pi / 180) / 2 ) * 2
    pix_res = ( size_cm / mon_obj$mon_width_cm * mon_obj$mon_width_pixel )
    return( pix_res )
}

#' @title Monitor parameters
#'
#' @description Prints the parameters of a given monitor object.
#' @return Returns nothing, just prints all parameters.
#' @examples
#' # assign monitor with 57 cm distance, and screen width 52 cm and 1920 pixels
#' my_mon = mon_neat(distance = 57, mon_width_cm = 52, mon_width_pixel = 1920)
#'
#' # print the above given parameters
#' mon_params( my_mon )
#' @export
mon_params = function( mon_obj ) {
    UseMethod('mon_params')
}

#' @export
mon_params.default = function( ... ) {
    cat('This function is to be used with "mon_neat" objects.\n')
}

#' @export
mon_params.mon_neat = function( mon_obj ) {
    cat( 'distance: ', mon_obj$distance,
         '\nmon_width_cm: ', mon_obj$mon_width_cm,
         '\nmon_width_pixel: ', mon_obj$mon_width_pixel, sep = '', fill = T )
}
