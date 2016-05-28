#' Count the number of variables (by class) in data frames
#'
#' The count_vars function will count the number of the variables (by class) in
#' a data frame.
#'
#' @param dataframe One or more data frames (as a character string or
#' a vector of character strings).
#'
#' @return A data frame. Each row gives the counts (by class) of all the
#' variables in each data frame in the dataframe argument. If the input
#' dataframe is not actually a data frame, then an empty data frame
#' will be returned.
#'
#' @author Sean Raleigh, \email{sraleigh@westminstercollege.edu}
#'
#' @examples count_vars(CO2)
#'
#' @importFrom dplyr %>% group_by_ summarise_ ungroup select_
#' @importFrom tidyr spread_
#'
#' @export
count_vars <- function(dataframe) {
    list_vars_data <- do.call("rbind", lapply(dataframe,  list_vars))
    output <- get_counts(list_vars_data)
    return(output)
}
