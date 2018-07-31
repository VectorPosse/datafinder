#' Visualize the variables contained in a dataset.
#'
#' The visualize_dataset function helps visualize datasets with a reasonable
#' number of variables
#'
#' @param dataframe One data frame (as a character string).
#' @param override=FALSE Override default max variable and row counts
#'
#' @return A plot that plots each variable against another. This function
#' is dependent on the number of rows and variables
#'
#' @author Sean Raleigh, \email{sraleigh@westminstercollege.edu}
#'
#' @examples
#'   visualize_dataset("CO2")
#'
#' @importFrom dplyr %>% group_by_ summarise_ ungroup select_
#' @importFrom tidyr spread_
#'
#' @export
#'
visualize_dataset <- function(dataset, override = FALSE) {
    GGally::ggpairs(dataset)
}