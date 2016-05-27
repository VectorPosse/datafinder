#' Finding good data examples
#'
#' The datafinder function will search a package for data and will return useful
#' information about that data like how many variables are present, what are their
#' types, are there missing values, and maybe more in the future.
#' @param pkg The name of a package (as a character string in quotation marks).
#'
#' @return A data frame. Each row corresponds to a variable in a data set and
#' indicates the type of variable, the number of missing observations, and
#' maybe more in the future.
#'
#' @author Sean Raleigh, \email{sraleigh@westminstercollege.edu}
#' @examples
#' datafinder("datasets")
#'
#' @export
datafinder <- function(pkg) {
    data(package = pkg)$results[,3]
}
