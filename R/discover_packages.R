#' Discover all the packages with data that are installed
#'
#' The discover_packages function finds the number of data frames in installed
#' packages.
#'
#' @return A data frame. Each row gives the counts of all the
#' data frames in each package. If there are no data frames in
#' the package, an empty data frame will be returned.
#'
#' @author Sean Raleigh, \email{sraleigh@westminstercollege.edu}
#' @author Jack Wilburn \email{jackwilburn@tutanota.com}
#'
#' @examples discover_packages()
#'
#' @importFrom utils data
#'
#' @export

discover_packages <- function() {

pkgs <- library()$results[, 1]
output <- data.frame(package = NA, num_datasets = NA)

for (i in 1:length(pkgs)) {
    pkg <- pkgs[i]
    num_datasets <- nrow(data(package = pkg)$results)
    df <- data.frame(package = pkg, num_datasets = num_datasets)
    output <- rbind(output, df)
}

output <- output[output$num_datasets > 0, ]

return(output)
}