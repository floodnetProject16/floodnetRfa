###############################################################################
#' Floodnet model output
#'
#' All Floodnet functions create a standardized model output that can be used
#' for printing results or generating graphics.
#'
#' @name floodnetMdl
#'
#' @seealso \link{FloodnetAmax},\link{FloodnetPot}, \link{FloodnetPool},
#'   \link{floodnetGraphics}, \link{hist.floodnetMdl}.
#'
#' @export
#'
print.floodnetMdl <- function(x) print(as.data.frame(x))

#' @export
#' @rdname floodnetMdl
print.floodnetMdls <- function(x) print(as.data.frame(x))
